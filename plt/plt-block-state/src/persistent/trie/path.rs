use concordium_base::common::{Buffer, Deserial, Get, ParseResult, Put, ReadBytesExt, Serial};
use std::ops::RangeBounds;
use tinyvec::TinyVec;

/// Path represents a path in a trie, or a path to look up in the trie. A path is represented
/// as a sequence of bytes, but the length is in granularity of nibbles (4 bits).
#[derive(Debug, Clone)]
pub struct Path<const INLINE_KEY_LENGTH: usize>(TinyVec<[u8; INLINE_KEY_LENGTH]>);

impl<const INLINE_KEY_LENGTH: usize> Path<INLINE_KEY_LENGTH> {
    /// Borrow as path slice.
    pub fn as_path_slice(&self) -> PathSliceRef<'_> {
        PathSliceRef::LowHigh(self.0.as_slice())
    }

    /// Returns the path represented as bytes, if the path is an even number of nibbles.
    /// If the path is an odd number of nibbles, `None` is returned.
    pub fn as_byte_slice(&self) -> Option<&[u8]> {
        todo!()
    }

    /// Extend the path with the given path slice.
    pub fn extend_from_path_slice(&mut self, slice: PathSliceRef<'_>) {
        todo!()
    }

    /// Create path from the bytes in the given vector.
    pub fn from_tiny_vec(tiny_vec: TinyVec<[u8; INLINE_KEY_LENGTH]>) -> Self {
        todo!()
    }

    /// Index into the path using nibbles as index.
    pub fn index_path_slice(&self, index: impl RangeBounds<usize>) -> PathSliceRef<'_> {
        self.as_path_slice().index_path_slice(index)
    }

    /// Index into the path using nibbles as index.
    pub fn index_path_nibble(&self, index: usize) -> PathNibble {
        self.as_path_slice().index_path_nibble(index)
    }

    /// Length of the path slice in nibbles.
    pub fn len(&self) -> usize {
        todo!()
    }
}

impl<const INLINE_KEY_LENGTH: usize> Serial for Path<INLINE_KEY_LENGTH> {
    fn serial<B: Buffer>(&self, out: &mut B) {
        // todo ar encode less than 8 bytes
        out.put(self.len() as u64);
        out.write_all(self.0.as_slice())
            .expect("Writing to a buffer should not fail.");
    }
}

impl<const INLINE_KEY_LENGTH: usize> Deserial for Path<INLINE_KEY_LENGTH> {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> ParseResult<Self> {
        let size: u64 = source.get()?;
        let mut vec = TinyVec::with_initial_len(size as usize);
        source.read_exact(&mut vec)?;
        Ok(Path(vec))
    }
}

/// Reference to slice of [`Path`]
#[derive(Debug, Copy, Clone)]
pub enum PathSliceRef<'a> {
    LowLow(&'a [u8]),
    LowHigh(&'a [u8]),
    HighLow(&'a [u8]),
    HighHigh(&'a [u8]),
}

impl<'a> PathSliceRef<'a> {
    /// Create empty path slice
    pub fn empty() -> Self {
        todo!()
    }

    /// If path slice is empty.
    pub fn is_empty(&self) -> bool {
        todo!()
    }

    /// Index into the path slice using nibbles as index.
    pub fn index_path_slice(&self, index: impl RangeBounds<usize>) -> PathSliceRef<'a> {
        todo!()
    }

    /// Index into the path slice using nibbles as index.
    pub fn index_path_nibble(&self, index: usize) -> PathNibble {
        todo!()
    }

    /// Create path slice from given byte slice.
    pub fn from_byte_slice(slice: &'a [u8]) -> Self {
        Self::LowHigh(slice)
    }

    /// The first nibble in the path slice.
    pub fn first_nibble(&self) -> Option<PathNibble> {
        todo!()
    }

    /// Create [`Path`] from the path slice.
    pub fn to_path<const INLINE_KEY_LENGTH: usize>(&self) -> Path<INLINE_KEY_LENGTH> {
        todo!()
    }

    /// Length of the path slice in nibbles.
    pub fn len(&self) -> usize {
        todo!()
    }
}

/// Length of common prefix in nibbles of the two path slices.
pub fn common_prefix_len(path_ref1: PathSliceRef<'_>, path_ref2: PathSliceRef<'_>) -> usize {
    todo!()
    // let mut i = 0;
    // while i < a.len() && i < b.len() && a[i] == b[i] {
    //     i += 1;
    // }
    // &a[0..i]
}

/// Stores the nibble in the first 4 bits of the `u8`
#[derive(Debug, Copy, Clone, Ord, PartialOrd, PartialEq, Eq)]
pub struct PathNibble(u8);

impl PathNibble {
    pub fn from_byte(byte: u8) -> Self {
        // todo ar validate
        Self(byte)
    }

    pub fn as_byte(&self) -> u8 {
        self.0
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use tinyvec::tiny_vec;

    #[test]
    fn test_index_path() {
        // let path = Path(tiny_vec![1, 2, 3]);
    }
}
