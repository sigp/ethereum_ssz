//! Provides a SSZ decoder which deserializes fields from last to first. I.e.
//! the last field that is defined in a struct is deserialized first.
//!
//! This decoder does not allocate internally during decoding, providing speed
//! and memory benefits.
use super::{read_offset, Decode, DecodeError, BYTES_PER_LENGTH_OFFSET};

/// Builds a [`ReverseSszDecoder`].
///
/// This builder must receive a "register" call for each field that the
/// resulting decoder will decode. Fields may be registered in any order.
/// Duplicate fields of the same type should be registered as many types as the
/// type is duplicated.
pub struct ReverseSszDecoderBuilder<'a> {
    bytes: &'a [u8],
    is_ssz_fixed_len: bool,
    ssz_fixed_len: usize,
}

impl<'a> ReverseSszDecoderBuilder<'a> {
    /// Create a new builder which will decode the given SSZ `bytes`.
    pub fn new(bytes: &'a [u8]) -> Self {
        Self {
            bytes,
            is_ssz_fixed_len: true,
            ssz_fixed_len: 0,
        }
    }

    /// Register a field some type `T` to be decoded.
    pub fn register_field<T: Decode>(&mut self) -> Result<(), DecodeError> {
        self.register_field_parameterized(T::is_ssz_fixed_len(), T::ssz_fixed_len())
    }

    /// Register a field some type `T` to be decoded, providing specific values.
    pub fn register_field_parameterized(
        &mut self,
        is_ssz_fixed_len: bool,
        ssz_fixed_len: usize,
    ) -> Result<(), DecodeError> {
        self.is_ssz_fixed_len &= is_ssz_fixed_len;
        self.ssz_fixed_len = self
            .ssz_fixed_len
            .checked_add(ssz_fixed_len)
            .ok_or(DecodeError::FixedBytesOverflow)?;
        Ok(())
    }

    /// Builds a [`ReverseSszDecoder`], returning errors if the given SSZ
    /// `bytes` are invalid.
    pub fn build(self) -> Result<ReverseSszDecoder<'a>, DecodeError> {
        let (fixed, variable) = if self.is_ssz_fixed_len {
            if self.bytes.len() == self.ssz_fixed_len {
                (self.bytes, &[][..])
            } else {
                return Err(DecodeError::InvalidByteLength {
                    len: self.bytes.len(),
                    expected: self.ssz_fixed_len,
                });
            }
        } else {
            self.bytes.split_at_checked(self.ssz_fixed_len).ok_or(
                DecodeError::InvalidByteLength {
                    len: self.bytes.len(),
                    expected: self.ssz_fixed_len,
                },
            )?
        };

        Ok(ReverseSszDecoder {
            fixed,
            variable,
            variable_offset: fixed.len(),
        })
    }
}

/// Decodes individual fields of an SSZ "container" (struct). Use a
/// [`ReverseSszDecoderBuilder`] to generate a decoder.
///
/// The user of this `Decoder` must:
///
/// - Decode each field based on the *reverse* order in which the fields are
///   defined.
/// - Call a "decode" function on all fields.
/// - Call [`Self::finish`] at the end of decoding to check for leftover bytes.
pub struct ReverseSszDecoder<'a> {
    fixed: &'a [u8],
    variable: &'a [u8],
    variable_offset: usize,
}

impl<'a> ReverseSszDecoder<'a> {
    /// Decode the last remaining field of the struct.
    pub fn decode_last<I: Decode>(&mut self) -> Result<I, DecodeError> {
        self.decode_last_parameterized(I::is_ssz_fixed_len(), I::ssz_fixed_len(), I::from_ssz_bytes)
    }

    /// Decode the last remaining field of the struct, providing specific
    /// values.
    pub fn decode_last_parameterized<I, F: Fn(&[u8]) -> Result<I, DecodeError>>(
        &mut self,
        is_ssz_fixed_len: bool,
        ssz_fixed_len: usize,
        decoder: F,
    ) -> Result<I, DecodeError> {
        if is_ssz_fixed_len {
            let (remaining, item) = self.split_last_fixed(ssz_fixed_len)?;
            self.fixed = remaining;
            decoder(item)
        } else {
            let (remaining, offset) = self.split_last_fixed(BYTES_PER_LENGTH_OFFSET)?;
            self.fixed = remaining;
            let offset = read_offset(offset)?;
            let offset = offset
                .checked_sub(self.variable_offset)
                .ok_or(DecodeError::OffsetIntoFixedPortion(offset))?;
            let (remaining, value) = self.split_variable(offset)?;
            self.variable = remaining;
            decoder(value)
        }
    }

    /// Split `self.fixed` at `self.fixed.len() - n`.
    fn split_last_fixed(&self, n: usize) -> Result<(&'a [u8], &'a [u8]), DecodeError> {
        let mid = self
            .fixed
            .len()
            .checked_sub(n)
            .ok_or(DecodeError::InvalidByteLength {
                len: self.fixed.len(),
                expected: n,
            })?;
        self.fixed
            .split_at_checked(mid)
            .ok_or(DecodeError::InvalidByteLength {
                len: self.fixed.len(),
                expected: mid,
            })
    }

    /// Split `self.variable` at `mid`.
    fn split_variable(&self, mid: usize) -> Result<(&'a [u8], &'a [u8]), DecodeError> {
        self.variable
            .split_at_checked(mid)
            .ok_or(DecodeError::OffsetOutOfBounds(mid))
    }

    /// Check to ensure there are no remaining bytes left to decode.
    ///
    /// This function must be called once all fields have been deserialized.
    /// Failing to do so can allow trailing SSZ bytes to be ignored, resulting
    /// in the deserialization of invalid SSZ which can result in consensus
    /// splits.
    pub fn finish(&self) -> Result<(), DecodeError> {
        if !self.fixed.is_empty() {
            Err(DecodeError::ExcessFixedBytes(self.fixed.len()))
        } else if !self.variable.is_empty() {
            Err(DecodeError::ExcessVariableBytes(self.variable.len()))
        } else {
            Ok(())
        }
    }
}
