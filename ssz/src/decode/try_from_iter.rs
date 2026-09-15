use smallvec::SmallVec;
use std::collections::{BTreeMap, BTreeSet};
use std::convert::Infallible;
use std::fmt::Debug;

/// The largest buffer we pre-allocate from an untrusted length hint, in bytes.
///
/// SSZ decoding derives a list length from the input before it reads the elements. A hostile input
/// can name a huge length, so we bound the up-front reservation to this budget. The collection
/// still grows to fit, so a valid list is never rejected.
const MAX_BYTES_TO_PRE_ALLOCATE: usize = 1_048_576;

/// Returns the capacity to reserve for a length hint, bounded by `MAX_BYTES_TO_PRE_ALLOCATE`.
fn capped_capacity<T>(len_hint: Option<usize>) -> usize {
    len_hint
        .unwrap_or(0)
        .min(MAX_BYTES_TO_PRE_ALLOCATE / std::mem::size_of::<T>().max(1))
}

/// Partial variant of `std::iter::FromIterator`.
///
/// This trait is implemented for types which can be constructed from an iterator of decoded SSZ
/// values, but which may refuse values once a length limit is reached.
pub trait TryFromIter<T>: Sized {
    type Error: Debug;

    fn try_from_iter<I>(iter: I) -> Result<Self, Self::Error>
    where
        I: IntoIterator<Item = T>;
}

// It would be nice to be able to do a blanket impl, e.g.
//
// `impl TryFromIter<T> for C where C: FromIterator<T>`
//
// However this runs into trait coherence issues due to the type parameter `T` on `TryFromIter`.
//
// E.g. If we added an impl downstream for `List<T, N>` then another crate downstream of that
// could legally add an impl of `FromIterator<Local> for List<Local, N>` which would create
// two conflicting implementations for `List<Local, N>`. Hence the `List<T, N>` impl is disallowed
// by the compiler in the presence of the blanket impl. That's obviously annoying, so we opt to
// abandon the blanket impl in favour of impls for selected types.
impl<T> TryFromIter<T> for Vec<T> {
    type Error = Infallible;

    fn try_from_iter<I>(values: I) -> Result<Self, Self::Error>
    where
        I: IntoIterator<Item = T>,
    {
        // The length hint comes from the SSZ input and is not yet checked, so bound the
        // reservation to a fixed budget. The Vec still grows to fit every element.
        let iter = values.into_iter();
        let (_, opt_max_len) = iter.size_hint();
        let mut vec = Vec::with_capacity(capped_capacity::<T>(opt_max_len));
        vec.extend(iter);
        Ok(vec)
    }
}

impl<T, const N: usize> TryFromIter<T> for SmallVec<[T; N]> {
    type Error = Infallible;

    fn try_from_iter<I>(iter: I) -> Result<Self, Self::Error>
    where
        I: IntoIterator<Item = T>,
    {
        // Bound the reservation from the untrusted length hint, as in the `Vec` impl.
        let iter = iter.into_iter();
        let (_, opt_max_len) = iter.size_hint();
        let mut out = SmallVec::with_capacity(capped_capacity::<T>(opt_max_len));
        out.extend(iter);
        Ok(out)
    }
}

impl<K, V> TryFromIter<(K, V)> for BTreeMap<K, V>
where
    K: Ord,
{
    type Error = Infallible;

    fn try_from_iter<I>(iter: I) -> Result<Self, Self::Error>
    where
        I: IntoIterator<Item = (K, V)>,
    {
        Ok(Self::from_iter(iter))
    }
}

impl<T> TryFromIter<T> for BTreeSet<T>
where
    T: Ord,
{
    type Error = Infallible;

    fn try_from_iter<I>(iter: I) -> Result<Self, Self::Error>
    where
        I: IntoIterator<Item = T>,
    {
        Ok(Self::from_iter(iter))
    }
}

/// Partial variant of `collect`.
pub trait TryCollect: Iterator {
    fn try_collect<C>(self) -> Result<C, C::Error>
    where
        C: TryFromIter<Self::Item>;
}

impl<I> TryCollect for I
where
    I: Iterator,
{
    fn try_collect<C>(self) -> Result<C, C::Error>
    where
        C: TryFromIter<Self::Item>,
    {
        C::try_from_iter(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// An iterator that overstates its length through `size_hint`.
    struct Wonky<I> {
        hint: usize,
        iter: I,
    }

    impl<I: Iterator> Iterator for Wonky<I> {
        type Item = I::Item;

        fn next(&mut self) -> Option<Self::Item> {
            self.iter.next()
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            (0, Some(self.hint))
        }
    }

    #[test]
    fn vec_try_from_iter_caps_preallocation() {
        // A raw `Vec::with_capacity(hint)` for this hint aborts with a capacity overflow.
        let wonky = Wonky {
            hint: usize::MAX / 8,
            iter: std::iter::repeat(1u64).take(5),
        };
        let vec: Vec<u64> = wonky.try_collect().unwrap();
        assert_eq!(vec, vec![1u64; 5]);
    }

    #[test]
    fn smallvec_try_from_iter_caps_preallocation() {
        let wonky = Wonky {
            hint: usize::MAX / 8,
            iter: std::iter::repeat(1u64).take(5),
        };
        let sv: SmallVec<[u64; 4]> = wonky.try_collect().unwrap();
        assert_eq!(sv.as_slice(), &[1u64; 5]);
    }

    #[test]
    fn small_lists_still_reserve_exactly() {
        let exact: Vec<u64> = (0..3u64)
            .collect::<Vec<_>>()
            .into_iter()
            .try_collect()
            .unwrap();
        assert_eq!(exact, vec![0, 1, 2]);
    }
}
