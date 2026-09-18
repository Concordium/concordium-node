use std::ops::{Range, RangeBounds};
use tinyvec::TinyVec;

/// Path
struct Path<const INLINE_KEY_LENGTH: usize>(TinyVec<[u8; INLINE_KEY_LENGTH]>);

impl<const INLINE_KEY_LENGTH: usize> Path<INLINE_KEY_LENGTH> {
    fn as_slice(&self) -> PathSliceRef<'_> {
        PathSliceRef::LowHigh(self.0.as_slice())
    }
}

/// Reference to slice of [`Path`]
#[derive(Debug)]
enum PathSliceRef<'a> {
    LowLow(&'a [u8]),
    LowHigh(&'a [u8]),
    HighLow(&'a [u8]),
    HighHigh(&'a [u8]),
}

impl<'a> PathSliceRef<'a> {
    fn index(&self, index: Range<usize>) -> PathSliceRef<'a> {
        todo!()
    }
}

/// Stores the nibble in the first 4 bits of the `u8`
#[derive(Debug)]
struct PathNibble(u8);

/// Reference to nibble.
#[derive(Debug)]
enum PathNibbleRef<'a> {
    Lower(&'a u8),
    Higher(&'a u8),
}

/// Reference to nibble.
#[derive(Debug)]
enum PathNibbleRefMut<'a> {
    Lower(&'a mut u8),
    Higher(&'a mut u8),
}

#[cfg(test)]
mod test {
    use super::*;
    use tinyvec::tiny_vec;

    #[test]
    fn test_index_path() {
        let path = Path(tiny_vec![1, 2, 3]);
    }
}
