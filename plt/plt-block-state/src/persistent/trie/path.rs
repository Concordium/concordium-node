use concordium_base::common::{Buffer, Deserial, Get, ParseResult, Put, ReadBytesExt, Serial};
use std::collections::Bound;
use std::ops::RangeBounds;
use tinyvec::TinyVec;

/// Path represents a path in a trie, or a path to look up in the trie. A path is represented
/// as a sequence of bytes, but the length is in granularity of nibbles (4 bits).
#[derive(Debug, Clone)]
pub struct Path<const INLINE_KEY_LENGTH: usize> {
    /// Bytes in the path. The last nibble is not part of the path,
    /// if `odd_length` is `true`.
    bytes: TinyVec<[u8; INLINE_KEY_LENGTH]>,
    /// If `0`, the path is `bytes`; if `1`,
    /// the path is `bytes` except for the last nibble.
    odd_end: u8,
}

impl<const INLINE_KEY_LENGTH: usize> Path<INLINE_KEY_LENGTH> {
    /// If path slice is empty.
    #[allow(unused)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Create empty path
    pub fn empty() -> Self {
        Self {
            bytes: TinyVec::new(),
            odd_end: 0,
        }
    }

    /// Borrow as path slice.
    pub fn as_path_slice(&self) -> PathSliceRef<'_> {
        PathSliceRef {
            byte_slice: &self.bytes,
            odd_start: 0,
            odd_end: self.odd_end,
        }
    }

    /// Returns the path represented as bytes, if the path is an even number of nibbles.
    /// If the path is an odd number of nibbles, `None` is returned.
    pub fn as_byte_slice(&self) -> Option<&[u8]> {
        if self.odd_end == 0 {
            Some(&self.bytes)
        } else {
            None
        }
    }

    /// Extend the path with the given path slice.
    pub fn extend_from_path_slice(&mut self, slice: &PathSliceRef<'_>) {
        todo!()
    }

    /// Create path from the bytes in the given vector.
    pub fn from_tiny_vec(tiny_vec: TinyVec<[u8; INLINE_KEY_LENGTH]>) -> Self {
        Self {
            bytes: tiny_vec,
            odd_end: 0,
        }
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
        (self.bytes.len() << 1) - self.odd_end as usize
    }
}

impl<const INLINE_KEY_LENGTH: usize> Serial for Path<INLINE_KEY_LENGTH> {
    fn serial<B: Buffer>(&self, out: &mut B) {
        // todo ar encode less than 8 bytes
        out.put(self.len() as u64);
        out.write_all(self.bytes.as_slice())
            .expect("Writing to a buffer should not fail.");
    }
}

impl<const INLINE_KEY_LENGTH: usize> Deserial for Path<INLINE_KEY_LENGTH> {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> ParseResult<Self> {
        let len: u64 = source.get()?;
        let odd_length = len & 1;
        let mut vec = TinyVec::with_initial_len(((len + odd_length) >> 1) as usize);
        source.read_exact(&mut vec)?;
        Ok(Self {
            bytes: vec,
            odd_end: odd_length as u8,
        })
    }
}

/// Reference to slice of [`Path`]
#[derive(Debug, Copy, Clone)]
pub struct PathSliceRef<'a> {
    byte_slice: &'a [u8],
    odd_start: u8,
    odd_end: u8,
}

impl<'a> PathSliceRef<'a> {
    /// Create empty path slice
    #[allow(unused)]
    pub fn empty() -> Self {
        Self {
            byte_slice: &[],
            odd_start: 0,
            odd_end: 0,
        }
    }

    /// If path slice is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Index into the path slice using nibbles as index.
    pub fn index_path_slice(&self, range_bounds: impl RangeBounds<usize>) -> PathSliceRef<'a> {
        let start_slice_index = match range_bounds.start_bound() {
            Bound::Included(&index) => index,
            Bound::Excluded(&index) => index + 1,
            Bound::Unbounded => 0,
        };
        let start_nibble_index = start_slice_index + self.odd_start as usize;
        let start_byte_index = start_nibble_index << 1;

        let end_slice_index = match range_bounds.end_bound() {
            Bound::Included(&index) => index + 1,
            Bound::Excluded(&index) => index,
            Bound::Unbounded => self.len(),
        };
        let end_nibble_index = end_slice_index + self.odd_end as usize;
        let end_byte_index = end_nibble_index << 1;

        Self {
            byte_slice: &self.byte_slice[start_byte_index..end_byte_index],
            odd_start: (start_nibble_index & 1) as u8,
            odd_end: (end_nibble_index & 1) as u8,
        }
    }

    /// Index into the path slice using nibbles as index.
    pub fn index_path_nibble(&self, slice_index: usize) -> PathNibble {
        let nibble_index = slice_index + self.odd_start as usize;
        let byte_index = nibble_index << 1;

        if nibble_index & 1 == 0 {
            PathNibble::from_byte_start(self.byte_slice[byte_index])
        } else {
            PathNibble::from_byte_end(self.byte_slice[byte_index])
        }
    }

    /// Create path slice from given byte slice.
    pub fn from_byte_slice(slice: &'a [u8]) -> Self {
        Self {
            byte_slice: slice,
            odd_start: 0,
            odd_end: 0,
        }
    }

    /// The first nibble in the path slice.
    pub fn first_nibble(&self) -> Option<PathNibble> {
        if self.is_empty() {
            None
        } else {
            Some(self.index_path_nibble(0))
        }
    }

    /// Create [`Path`] from the path slice.
    pub fn to_path<const INLINE_KEY_LENGTH: usize>(self) -> Path<INLINE_KEY_LENGTH> {
        let mut path = Path::empty();
        path.extend_from_path_slice(&self);
        path
    }

    /// Length of the path slice in nibbles.
    pub fn len(&self) -> usize {
        (self.byte_slice.len() << 1) - self.odd_start as usize - self.odd_end as usize
    }

    pub fn iter(&self) -> PathSliceIter<'_> {
        todo!()
    }
}

pub struct PathSliceIter<'a> {
    bytes: &'a [u8],
    odd_start: u8,
}

impl<'a> Iterator for PathSliceIter<'a> {
    type Item = PathNibble;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

/// Length of common prefix in nibbles of the two path slices.
pub fn common_prefix_len(path_ref1: PathSliceRef<'_>, path_ref2: PathSliceRef<'_>) -> usize {
    let mut iter1 = path_ref1.iter();
    let mut iter2 = path_ref2.iter();
    let mut i = 0;
    while let Some(elm1) = iter1.next()
        && let Some(elm2) = iter2.next()
        && elm1 == elm2
    {
        i += 1;
    }
    i
}

/// Stores the nibble in the first 4 bits of the `u8`
#[derive(Debug, Copy, Clone, Ord, PartialOrd, PartialEq, Eq)]
pub struct PathNibble(u8);

impl PathNibble {
    pub fn from_byte_raw(byte: u8) -> Self {
        // todo ar validate
        Self(byte)
    }

    pub fn from_byte_start(byte: u8) -> Self {
        // todo ar validate
        Self((byte & 0b11110000) >> 4)
    }

    pub fn from_byte_end(byte: u8) -> Self {
        Self(byte & 0b1111)
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
