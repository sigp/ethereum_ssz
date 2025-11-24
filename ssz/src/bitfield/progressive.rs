//! Provides `Bitfield<Progressive>` (ProgressiveBitlist)
//! for encoding and decoding bitlists that have no capacity limit.
use crate::{
    bitfield::{bytes_for_bit_len, Bitfield, BitfieldBehaviour, Error, SMALLVEC_LEN},
    Decode, DecodeError, Encode,
};
use core::marker::PhantomData;
use serde::de::{Deserialize, Deserializer};
use serde::ser::{Serialize, Serializer};
use serde_utils::hex::{encode as hex_encode, PrefixedHexVisitor};
use smallvec::{smallvec, SmallVec, ToSmallVec};

/// A marker struct used to declare progressive (uncapped) behaviour on a Bitfield.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Progressive;

impl BitfieldBehaviour for Progressive {}

/// A heap-allocated, ordered, variable-length collection of `bool` values, without a capacity limit.
pub type ProgressiveBitlist = Bitfield<Progressive>;

impl Bitfield<Progressive> {
    /// Instantiate with capacity for `num_bits` boolean values. The length cannot be grown or
    /// shrunk after instantiation.
    ///
    /// All bits are initialized to `false`.
    pub fn with_capacity(num_bits: usize) -> Result<Self, Error> {
        Ok(Self {
            bytes: smallvec![0; bytes_for_bit_len(num_bits)],
            len: num_bits,
            _phantom: PhantomData,
        })
    }

    /// Consumes `self`, returning a serialized representation.
    ///
    /// The output is faithful to the SSZ encoding of `self`, such that a leading `true` bit is
    /// used to indicate the length of the bitfield.
    ///
    /// ## Example
    /// ```
    /// use ssz::ProgressiveBitlist;
    /// use smallvec::SmallVec;
    ///
    /// let b = ProgressiveBitlist::with_capacity(4).unwrap();
    ///
    /// assert_eq!(b.into_bytes(), SmallVec::from_buf([0b0001_0000]));
    /// ```
    pub fn into_bytes(self) -> SmallVec<[u8; SMALLVEC_LEN]> {
        let len = self.len();
        let mut bytes = self.bytes;

        bytes.resize(bytes_for_bit_len(len + 1), 0);

        let mut bitfield: Bitfield<Progressive> = Bitfield::from_raw_bytes(bytes, len + 1)
            .unwrap_or_else(|_| {
                unreachable!(
                    "Bitfield with {} bytes must have enough capacity for {} bits.",
                    bytes_for_bit_len(len + 1),
                    len + 1
                )
            });
        bitfield
            .set(len, true)
            .expect("len must be in bounds for bitfield.");

        bitfield.bytes
    }

    /// Instantiates a new instance from `bytes`. Consumes the same format that `self.into_bytes()`
    /// produces (SSZ).
    ///
    /// Returns `Err` if `bytes` are not a valid encoding.
    pub fn from_bytes(bytes: SmallVec<[u8; SMALLVEC_LEN]>) -> Result<Self, Error> {
        let bytes_len = bytes.len();
        let mut initial_bitfield: Bitfield<Progressive> = {
            let num_bits = bytes.len() * 8;
            Bitfield::from_raw_bytes(bytes, num_bits)?
        };

        let len = initial_bitfield
            .highest_set_bit()
            .ok_or(Error::MissingLengthInformation)?;

        // The length bit should be in the last byte, or else it means we have too many bytes.
        if len / 8 + 1 != bytes_len {
            return Err(Error::InvalidByteCount {
                given: bytes_len,
                expected: len / 8 + 1,
            });
        }

        initial_bitfield
            .set(len, false)
            .expect("Bit has been confirmed to exist");

        let mut bytes = initial_bitfield.into_raw_bytes();

        bytes.truncate(bytes_for_bit_len(len));

        Self::from_raw_bytes(bytes, len)
    }

    /// Compute the intersection of two ProgressiveBitlists of potentially different lengths.
    ///
    /// Return a new ProgressiveBitlist with length equal to the shorter of the two inputs.
    pub fn intersection(&self, other: &Self) -> Self {
        let min_len = std::cmp::min(self.len(), other.len());
        let mut result = Self::with_capacity(min_len).expect("min len is always valid");
        // Bitwise-and the bytes together, starting from the left of each vector. This takes care
        // of masking out any entries beyond `min_len` as well, assuming the bitfield doesn't
        // contain any set bits beyond its length.
        for i in 0..result.bytes.len() {
            result.bytes[i] = self.bytes[i] & other.bytes[i];
        }
        result
    }

    /// Compute the union of two ProgressiveBitlists of potentially different lengths.
    ///
    /// Return a new ProgressiveBitlist with length equal to the longer of the two inputs.
    pub fn union(&self, other: &Self) -> Self {
        let max_len = std::cmp::max(self.len(), other.len());
        let mut result = Self::with_capacity(max_len).expect("max len is always valid");
        for i in 0..result.bytes.len() {
            result.bytes[i] =
                self.bytes.get(i).copied().unwrap_or(0) | other.bytes.get(i).copied().unwrap_or(0);
        }
        result
    }

    /// Returns `true` if `self` is a subset of `other` and `false` otherwise.
    pub fn is_subset(&self, other: &Self) -> bool {
        self.difference(other).is_zero()
    }
}

impl Encode for Bitfield<Progressive> {
    fn is_ssz_fixed_len() -> bool {
        false
    }

    fn ssz_bytes_len(&self) -> usize {
        // We could likely do better than turning this into bytes and reading the length, however
        // it is kept this way for simplicity.
        self.clone().into_bytes().len()
    }

    fn ssz_append(&self, buf: &mut Vec<u8>) {
        buf.extend_from_slice(&self.clone().into_bytes())
    }
}

impl Decode for Bitfield<Progressive> {
    fn is_ssz_fixed_len() -> bool {
        false
    }

    fn from_ssz_bytes(bytes: &[u8]) -> Result<Self, DecodeError> {
        Self::from_bytes(bytes.to_smallvec()).map_err(|e| {
            DecodeError::BytesInvalid(format!("ProgressiveBitlist failed to decode: {:?}", e))
        })
    }
}

impl Serialize for Bitfield<Progressive> {
    /// Serde serialization is compliant with the Ethereum YAML test format.
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&hex_encode(self.as_ssz_bytes()))
    }
}

impl<'de> Deserialize<'de> for Bitfield<Progressive> {
    /// Serde serialization is compliant with the Ethereum YAML test format.
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let bytes = deserializer.deserialize_str(PrefixedHexVisitor)?;
        Self::from_ssz_bytes(&bytes)
            .map_err(|e| serde::de::Error::custom(format!("ProgressiveBitlist {:?}", e)))
    }
}

#[cfg(feature = "arbitrary")]
impl arbitrary::Arbitrary<'_> for Bitfield<Progressive> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'_>) -> arbitrary::Result<Self> {
        // Pick a reasonable maximum for testing.
        const MAX_BYTES: usize = 512;

        let rand = usize::arbitrary(u)?;
        let size = std::cmp::min(rand, MAX_BYTES);
        let mut vec = smallvec![0u8; size];
        u.fill_buffer(&mut vec)?;
        Self::from_bytes(vec).map_err(|_| arbitrary::Error::IncorrectFormat)
    }
}

#[cfg(test)]
mod progressive_bitlist {
    use super::*;
    use crate::ProgressiveBitlist;

    #[test]
    fn ssz_encode() {
        assert_eq!(
            ProgressiveBitlist::with_capacity(0).unwrap().as_ssz_bytes(),
            vec![0b0000_0001],
        );

        assert_eq!(
            ProgressiveBitlist::with_capacity(1).unwrap().as_ssz_bytes(),
            vec![0b0000_0010],
        );

        assert_eq!(
            ProgressiveBitlist::with_capacity(8).unwrap().as_ssz_bytes(),
            vec![0b0000_0000, 0b0000_0001],
        );

        assert_eq!(
            ProgressiveBitlist::with_capacity(7).unwrap().as_ssz_bytes(),
            vec![0b1000_0000]
        );

        let mut b = ProgressiveBitlist::with_capacity(8).unwrap();
        for i in 0..8 {
            b.set(i, true).unwrap();
        }
        assert_eq!(b.as_ssz_bytes(), vec![255, 0b0000_0001]);

        let mut b = ProgressiveBitlist::with_capacity(8).unwrap();
        for i in 0..4 {
            b.set(i, true).unwrap();
        }
        assert_eq!(b.as_ssz_bytes(), vec![0b0000_1111, 0b0000_0001]);

        assert_eq!(
            ProgressiveBitlist::with_capacity(16)
                .unwrap()
                .as_ssz_bytes(),
            vec![0b0000_0000, 0b0000_0000, 0b0000_0001]
        );
    }

    #[test]
    fn ssz_decode() {
        assert!(ProgressiveBitlist::from_ssz_bytes(&[]).is_err());
        assert!(ProgressiveBitlist::from_ssz_bytes(&[0b0000_0000]).is_err());
        assert!(ProgressiveBitlist::from_ssz_bytes(&[0b0000_0000, 0b0000_0000]).is_err());

        assert!(ProgressiveBitlist::from_ssz_bytes(&[0b0000_0001]).is_ok());
        assert!(ProgressiveBitlist::from_ssz_bytes(&[0b0000_0010]).is_ok());
        assert!(ProgressiveBitlist::from_ssz_bytes(&[0b0000_0100]).is_ok());

        assert!(ProgressiveBitlist::from_ssz_bytes(&[0b0000_0001, 0b0000_0001]).is_ok());
        assert!(ProgressiveBitlist::from_ssz_bytes(&[0b0000_0001, 0b0000_0010]).is_ok());
        assert!(ProgressiveBitlist::from_ssz_bytes(&[0b0000_0001, 0b0000_0100]).is_ok());
    }

    #[test]
    fn ssz_decode_extra_bytes() {
        assert!(ProgressiveBitlist::from_ssz_bytes(&[0b0000_0001, 0b0000_0000]).is_err());
        assert!(ProgressiveBitlist::from_ssz_bytes(&[0b1000_0000, 0]).is_err());
        assert!(ProgressiveBitlist::from_ssz_bytes(&[0b1000_0000, 0, 0]).is_err());
        assert!(ProgressiveBitlist::from_ssz_bytes(&[0b1000_0000, 0, 0, 0, 0]).is_err());
    }

    #[test]
    fn ssz_round_trip() {
        assert_round_trip(ProgressiveBitlist::with_capacity(0).unwrap());

        for i in 0..17 {
            assert_round_trip(ProgressiveBitlist::with_capacity(i).unwrap());
        }

        let mut b = ProgressiveBitlist::with_capacity(1).unwrap();
        b.set(0, true).unwrap();
        assert_round_trip(b);

        for i in 0..16 {
            let mut b = ProgressiveBitlist::with_capacity(i).unwrap();
            for j in 0..i {
                if j % 2 == 0 {
                    b.set(j, true).unwrap();
                }
            }
            assert_round_trip(b);

            let mut b = ProgressiveBitlist::with_capacity(i).unwrap();
            for j in 0..i {
                b.set(j, true).unwrap();
            }
            assert_round_trip(b);
        }
    }

    fn assert_round_trip<T: Encode + Decode + PartialEq + std::fmt::Debug>(t: T) {
        assert_eq!(T::from_ssz_bytes(&t.as_ssz_bytes()).unwrap(), t);
    }

    #[test]
    fn from_raw_bytes() {
        assert!(ProgressiveBitlist::from_raw_bytes(smallvec![0b0000_0000], 0).is_ok());
        assert!(ProgressiveBitlist::from_raw_bytes(smallvec![0b0000_0001], 1).is_ok());
        assert!(ProgressiveBitlist::from_raw_bytes(smallvec![0b0000_0011], 2).is_ok());
        assert!(ProgressiveBitlist::from_raw_bytes(smallvec![0b0000_0111], 3).is_ok());
        assert!(ProgressiveBitlist::from_raw_bytes(smallvec![0b0000_1111], 4).is_ok());
        assert!(ProgressiveBitlist::from_raw_bytes(smallvec![0b0001_1111], 5).is_ok());
        assert!(ProgressiveBitlist::from_raw_bytes(smallvec![0b0011_1111], 6).is_ok());
        assert!(ProgressiveBitlist::from_raw_bytes(smallvec![0b0111_1111], 7).is_ok());
        assert!(ProgressiveBitlist::from_raw_bytes(smallvec![0b1111_1111], 8).is_ok());

        assert!(ProgressiveBitlist::from_raw_bytes(smallvec![0b1111_1111, 0b0000_0001], 9).is_ok());
        assert!(
            ProgressiveBitlist::from_raw_bytes(smallvec![0b1111_1111, 0b0000_0011], 10).is_ok()
        );
        assert!(
            ProgressiveBitlist::from_raw_bytes(smallvec![0b1111_1111, 0b0000_0111], 11).is_ok()
        );
        assert!(
            ProgressiveBitlist::from_raw_bytes(smallvec![0b1111_1111, 0b0000_1111], 12).is_ok()
        );
        assert!(
            ProgressiveBitlist::from_raw_bytes(smallvec![0b1111_1111, 0b0001_1111], 13).is_ok()
        );
        assert!(
            ProgressiveBitlist::from_raw_bytes(smallvec![0b1111_1111, 0b0011_1111], 14).is_ok()
        );
        assert!(
            ProgressiveBitlist::from_raw_bytes(smallvec![0b1111_1111, 0b0111_1111], 15).is_ok()
        );
        assert!(
            ProgressiveBitlist::from_raw_bytes(smallvec![0b1111_1111, 0b1111_1111], 16).is_ok()
        );

        for i in 0..8 {
            assert!(ProgressiveBitlist::from_raw_bytes(smallvec![], i).is_err());
            assert!(ProgressiveBitlist::from_raw_bytes(smallvec![0b1111_1111], i).is_err());
            assert!(
                ProgressiveBitlist::from_raw_bytes(smallvec![0b0000_0000, 0b1111_1110], i).is_err()
            );
        }

        assert!(ProgressiveBitlist::from_raw_bytes(smallvec![0b0000_0001], 0).is_err());
        assert!(ProgressiveBitlist::from_raw_bytes(smallvec![0b0000_0011], 1).is_err());
        assert!(ProgressiveBitlist::from_raw_bytes(smallvec![0b0000_0111], 2).is_err());
        assert!(ProgressiveBitlist::from_raw_bytes(smallvec![0b0000_1111], 3).is_err());
        assert!(ProgressiveBitlist::from_raw_bytes(smallvec![0b0001_1111], 4).is_err());
        assert!(ProgressiveBitlist::from_raw_bytes(smallvec![0b0011_1111], 5).is_err());
        assert!(ProgressiveBitlist::from_raw_bytes(smallvec![0b0111_1111], 6).is_err());
        assert!(ProgressiveBitlist::from_raw_bytes(smallvec![0b1111_1111], 7).is_err());

        assert!(
            ProgressiveBitlist::from_raw_bytes(smallvec![0b1111_1111, 0b0000_0001], 8).is_err()
        );
        assert!(
            ProgressiveBitlist::from_raw_bytes(smallvec![0b1111_1111, 0b0000_0011], 9).is_err()
        );
        assert!(
            ProgressiveBitlist::from_raw_bytes(smallvec![0b1111_1111, 0b0000_0111], 10).is_err()
        );
        assert!(
            ProgressiveBitlist::from_raw_bytes(smallvec![0b1111_1111, 0b0000_1111], 11).is_err()
        );
        assert!(
            ProgressiveBitlist::from_raw_bytes(smallvec![0b1111_1111, 0b0001_1111], 12).is_err()
        );
        assert!(
            ProgressiveBitlist::from_raw_bytes(smallvec![0b1111_1111, 0b0011_1111], 13).is_err()
        );
        assert!(
            ProgressiveBitlist::from_raw_bytes(smallvec![0b1111_1111, 0b0111_1111], 14).is_err()
        );
        assert!(
            ProgressiveBitlist::from_raw_bytes(smallvec![0b1111_1111, 0b1111_1111], 15).is_err()
        );
    }

    fn test_set_unset(num_bits: usize) {
        let mut bitfield = ProgressiveBitlist::with_capacity(num_bits).unwrap();

        for i in 0..=num_bits {
            if i < num_bits {
                // Starts as false
                assert_eq!(bitfield.get(i), Ok(false));
                // Can be set true.
                assert!(bitfield.set(i, true).is_ok());
                assert_eq!(bitfield.get(i), Ok(true));
                // Can be set false
                assert!(bitfield.set(i, false).is_ok());
                assert_eq!(bitfield.get(i), Ok(false));
            } else {
                assert!(bitfield.get(i).is_err());
                assert!(bitfield.set(i, true).is_err());
                assert!(bitfield.get(i).is_err());
            }
        }
    }

    fn test_bytes_round_trip(num_bits: usize) {
        for i in 0..num_bits {
            let mut bitfield = ProgressiveBitlist::with_capacity(num_bits).unwrap();
            bitfield.set(i, true).unwrap();

            let bytes = bitfield.clone().into_raw_bytes();
            assert_eq!(bitfield, Bitfield::from_raw_bytes(bytes, num_bits).unwrap());
        }
    }

    #[test]
    fn set_unset() {
        for i in 0..8 * 5 {
            test_set_unset(i)
        }
    }

    #[test]
    fn bytes_round_trip() {
        for i in 0..8 * 5 {
            test_bytes_round_trip(i)
        }
    }

    /// Type-specialised `smallvec` macro for testing.
    macro_rules! bytevec {
        ($($x : expr),* $(,)*) => {
            {
                let __smallvec: SmallVec<[u8; SMALLVEC_LEN]> = smallvec!($($x),*);
                __smallvec
            }
        };
    }

    #[test]
    fn into_raw_bytes() {
        let mut bitfield = ProgressiveBitlist::with_capacity(9).unwrap();
        bitfield.set(0, true).unwrap();
        assert_eq!(
            bitfield.clone().into_raw_bytes(),
            bytevec![0b0000_0001, 0b0000_0000]
        );
        bitfield.set(1, true).unwrap();
        assert_eq!(
            bitfield.clone().into_raw_bytes(),
            bytevec![0b0000_0011, 0b0000_0000]
        );
        bitfield.set(2, true).unwrap();
        assert_eq!(
            bitfield.clone().into_raw_bytes(),
            bytevec![0b0000_0111, 0b0000_0000]
        );
        bitfield.set(3, true).unwrap();
        assert_eq!(
            bitfield.clone().into_raw_bytes(),
            bytevec![0b0000_1111, 0b0000_0000]
        );
        bitfield.set(4, true).unwrap();
        assert_eq!(
            bitfield.clone().into_raw_bytes(),
            bytevec![0b0001_1111, 0b0000_0000]
        );
        bitfield.set(5, true).unwrap();
        assert_eq!(
            bitfield.clone().into_raw_bytes(),
            bytevec![0b0011_1111, 0b0000_0000]
        );
        bitfield.set(6, true).unwrap();
        assert_eq!(
            bitfield.clone().into_raw_bytes(),
            bytevec![0b0111_1111, 0b0000_0000]
        );
        bitfield.set(7, true).unwrap();
        assert_eq!(
            bitfield.clone().into_raw_bytes(),
            bytevec![0b1111_1111, 0b0000_0000]
        );
        bitfield.set(8, true).unwrap();
        assert_eq!(
            bitfield.into_raw_bytes(),
            bytevec![0b1111_1111, 0b0000_0001]
        );
    }

    #[test]
    fn highest_set_bit() {
        assert_eq!(
            ProgressiveBitlist::with_capacity(16)
                .unwrap()
                .highest_set_bit(),
            None
        );

        assert_eq!(
            ProgressiveBitlist::from_raw_bytes(smallvec![0b0000_0001, 0b0000_0000], 16)
                .unwrap()
                .highest_set_bit(),
            Some(0)
        );

        assert_eq!(
            ProgressiveBitlist::from_raw_bytes(smallvec![0b0000_0010, 0b0000_0000], 16)
                .unwrap()
                .highest_set_bit(),
            Some(1)
        );

        assert_eq!(
            ProgressiveBitlist::from_raw_bytes(smallvec![0b0000_1000], 8)
                .unwrap()
                .highest_set_bit(),
            Some(3)
        );

        assert_eq!(
            ProgressiveBitlist::from_raw_bytes(smallvec![0b0000_0000, 0b1000_0000], 16)
                .unwrap()
                .highest_set_bit(),
            Some(15)
        );
    }

    #[test]
    fn intersection() {
        let a = ProgressiveBitlist::from_raw_bytes(smallvec![0b1100, 0b0001], 16).unwrap();
        let b = ProgressiveBitlist::from_raw_bytes(smallvec![0b1011, 0b1001], 16).unwrap();
        let c = ProgressiveBitlist::from_raw_bytes(smallvec![0b1000, 0b0001], 16).unwrap();

        assert_eq!(a.intersection(&b), c);
        assert_eq!(b.intersection(&a), c);
        assert_eq!(a.intersection(&c), c);
        assert_eq!(b.intersection(&c), c);
        assert_eq!(a.intersection(&a), a);
        assert_eq!(b.intersection(&b), b);
        assert_eq!(c.intersection(&c), c);
    }

    #[test]
    fn subset() {
        let a = ProgressiveBitlist::from_raw_bytes(smallvec![0b1000, 0b0001], 16).unwrap();
        let b = ProgressiveBitlist::from_raw_bytes(smallvec![0b1100, 0b0001], 16).unwrap();
        let c = ProgressiveBitlist::from_raw_bytes(smallvec![0b1100, 0b1001], 16).unwrap();

        assert_eq!(a.len(), 16);
        assert_eq!(b.len(), 16);
        assert_eq!(c.len(), 16);

        // a vector is always a subset of itself
        assert!(a.is_subset(&a));
        assert!(b.is_subset(&b));
        assert!(c.is_subset(&c));

        assert!(a.is_subset(&b));
        assert!(a.is_subset(&c));
        assert!(b.is_subset(&c));

        assert!(!b.is_subset(&a));
        assert!(!c.is_subset(&a));
        assert!(!c.is_subset(&b));

        let d = ProgressiveBitlist::from_raw_bytes(smallvec![0b1100, 0b1001, 0b1010], 24).unwrap();
        assert!(d.is_subset(&d));

        assert!(a.is_subset(&d));
        assert!(b.is_subset(&d));
        assert!(c.is_subset(&d));

        // A bigger length bitlist cannot be a subset of a smaller length bitlist
        assert!(!d.is_subset(&a));
        assert!(!d.is_subset(&b));
        assert!(!d.is_subset(&c));

        let e = ProgressiveBitlist::from_raw_bytes(smallvec![0b1100, 0b1001, 0b0000], 24).unwrap();
        assert!(e.is_subset(&c));
        assert!(c.is_subset(&e));
    }

    #[test]
    fn intersection_diff_length() {
        let a = ProgressiveBitlist::from_bytes(smallvec![0b0010_1110, 0b0010_1011]).unwrap();
        let b = ProgressiveBitlist::from_bytes(smallvec![0b0010_1101, 0b0000_0001]).unwrap();
        let c = ProgressiveBitlist::from_bytes(smallvec![0b0010_1100, 0b0000_0001]).unwrap();
        let d = ProgressiveBitlist::from_bytes(smallvec![0b0010_1110, 0b1111_1111, 0b1111_1111])
            .unwrap();

        assert_eq!(a.len(), 13);
        assert_eq!(b.len(), 8);
        assert_eq!(c.len(), 8);
        assert_eq!(d.len(), 23);
        assert_eq!(a.intersection(&b), c);
        assert_eq!(b.intersection(&a), c);
        assert_eq!(a.intersection(&d), a);
        assert_eq!(d.intersection(&a), a);
    }

    #[test]
    fn union() {
        let a = ProgressiveBitlist::from_raw_bytes(smallvec![0b1100, 0b0001], 16).unwrap();
        let b = ProgressiveBitlist::from_raw_bytes(smallvec![0b1011, 0b1001], 16).unwrap();
        let c = ProgressiveBitlist::from_raw_bytes(smallvec![0b1111, 0b1001], 16).unwrap();

        assert_eq!(a.union(&b), c);
        assert_eq!(b.union(&a), c);
        assert_eq!(a.union(&a), a);
        assert_eq!(b.union(&b), b);
        assert_eq!(c.union(&c), c);
    }

    #[test]
    fn union_diff_length() {
        let a = ProgressiveBitlist::from_bytes(smallvec![0b0010_1011, 0b0010_1110]).unwrap();
        let b = ProgressiveBitlist::from_bytes(smallvec![0b0000_0001, 0b0010_1101]).unwrap();
        let c = ProgressiveBitlist::from_bytes(smallvec![0b0010_1011, 0b0010_1111]).unwrap();
        let d = ProgressiveBitlist::from_bytes(smallvec![0b0010_1011, 0b1011_1110, 0b1000_1101])
            .unwrap();

        assert_eq!(a.len(), c.len());
        assert_eq!(a.union(&b), c);
        assert_eq!(b.union(&a), c);
        assert_eq!(a.union(&d), d);
        assert_eq!(d.union(&a), d);
    }

    #[test]
    fn difference() {
        let a = ProgressiveBitlist::from_raw_bytes(smallvec![0b1100, 0b0001], 16).unwrap();
        let b = ProgressiveBitlist::from_raw_bytes(smallvec![0b1011, 0b1001], 16).unwrap();
        let a_b = ProgressiveBitlist::from_raw_bytes(smallvec![0b0100, 0b0000], 16).unwrap();
        let b_a = ProgressiveBitlist::from_raw_bytes(smallvec![0b0011, 0b1000], 16).unwrap();

        assert_eq!(a.difference(&b), a_b);
        assert_eq!(b.difference(&a), b_a);
        assert!(a.difference(&a).is_zero());
    }

    #[test]
    fn difference_diff_length() {
        let a = ProgressiveBitlist::from_raw_bytes(smallvec![0b0110, 0b1100, 0b0011], 24).unwrap();
        let b = ProgressiveBitlist::from_raw_bytes(smallvec![0b1011, 0b1001], 16).unwrap();
        let a_b =
            ProgressiveBitlist::from_raw_bytes(smallvec![0b0100, 0b0100, 0b0011], 24).unwrap();
        let b_a = ProgressiveBitlist::from_raw_bytes(smallvec![0b1001, 0b0001], 16).unwrap();

        assert_eq!(a.difference(&b), a_b);
        assert_eq!(b.difference(&a), b_a);
    }

    #[test]
    fn shift_up() {
        let mut a =
            ProgressiveBitlist::from_raw_bytes(smallvec![0b1100_1111, 0b1101_0110], 16).unwrap();
        let b =
            ProgressiveBitlist::from_raw_bytes(smallvec![0b1001_1110, 0b1010_1101], 16).unwrap();

        a.shift_up(1).unwrap();
        assert_eq!(a, b);
        a.shift_up(15).unwrap();
        assert!(a.is_zero());

        let mut c =
            ProgressiveBitlist::from_raw_bytes(smallvec![0b1100_1111, 0b1101_0110], 16).unwrap();
        c.shift_up(16).unwrap();
        assert!(c.is_zero());
        assert!(c.shift_up(17).is_err());
    }

    #[test]
    fn num_set_bits() {
        let a = ProgressiveBitlist::from_raw_bytes(smallvec![0b1100, 0b0001], 16).unwrap();
        let b = ProgressiveBitlist::from_raw_bytes(smallvec![0b1011, 0b1001], 16).unwrap();

        assert_eq!(a.num_set_bits(), 3);
        assert_eq!(b.num_set_bits(), 5);
    }

    #[test]
    fn iter() {
        let mut bitfield = ProgressiveBitlist::with_capacity(9).unwrap();
        bitfield.set(2, true).unwrap();
        bitfield.set(8, true).unwrap();

        assert_eq!(
            bitfield.iter().collect::<Vec<bool>>(),
            vec![false, false, true, false, false, false, false, false, true]
        );
    }

    #[test]
    fn ssz_bytes_len() {
        for i in 1..64 {
            let mut bitfield = ProgressiveBitlist::with_capacity(i).unwrap();
            for j in 0..i {
                bitfield.set(j, true).expect("should set bit in bounds");
            }
            let bytes = bitfield.as_ssz_bytes();
            assert_eq!(bitfield.ssz_bytes_len(), bytes.len(), "i = {}", i);
        }
    }

    // Ensure that the stack size of a ProgressiveBitlist is manageable.
    #[test]
    fn size_of() {
        assert_eq!(std::mem::size_of::<ProgressiveBitlist>(), SMALLVEC_LEN + 24);
    }

    #[test]
    fn display() {
        let bitlist =
            ProgressiveBitlist::from_raw_bytes(smallvec![0b0011_1111, 0b0001_0101], 15).unwrap();
        assert_eq!("111111001010100", bitlist.to_string());
    }
}
