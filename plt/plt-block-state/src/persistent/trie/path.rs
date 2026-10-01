use concordium_base::common::{Buffer, Deserial, Get, ParseResult, Put, ReadBytesExt, Serial};
use std::collections::Bound;
use std::ops::RangeBounds;
use std::{iter, slice};
use tinyvec::TinyVec;

/// Path represents a path in a trie, or a path to look up in the trie. A path is represented
/// as a sequence of bytes, but the length is in granularity of 4 bit chunks.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Path<const INLINE_KEY_LENGTH: usize> {
    /// Bytes in the path. The last chunk is not part of the path,
    /// if `odd_length` is `1` - in this case the last chunk is always `0`.
    bytes: TinyVec<[u8; INLINE_KEY_LENGTH]>,
    /// If `0`, the path is `bytes`; if `1`,
    /// the path is `bytes` except for the last chunk.
    odd_end: u8,
}

impl<const INLINE_KEY_LENGTH: usize> Path<INLINE_KEY_LENGTH> {
    /// If path is empty.
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

    /// Returns the path represented as bytes, if the path is an even number of chunks.
    /// If the path is an odd number of chunks, `None` is returned.
    pub fn as_byte_slice(&self) -> Option<&[u8]> {
        if self.odd_end == 0 {
            Some(&self.bytes)
        } else {
            None
        }
    }

    /// Extend the path with the given path slice.
    pub fn extend_from_path_slice(&mut self, slice: &PathSliceRef<'_>) {
        if self.odd_end == 0 && slice.odd_start == 0 {
            self.bytes.extend_from_slice(slice.byte_slice);
            self.odd_end = slice.odd_end;
        } else if self.odd_end == 1 && slice.odd_start == 1 {
            *self.bytes.last_mut().unwrap() =
                PathChunk::from_byte_start(*self.bytes.last().unwrap())
                    .splice_to_byte(PathChunk::from_byte_end(slice.byte_slice[0]));
            self.bytes.extend_from_slice(&slice.byte_slice[1..]);
            self.odd_end = slice.odd_end;
        } else if self.odd_end == 0 && slice.odd_start == 1 {
            self.bytes.reserve(slice.byte_slice.len());
            let mut buffered_chunk = PathChunk::from_byte_end(slice.byte_slice[0]);
            for &byte in &slice.byte_slice[1..] {
                self.bytes
                    .push(buffered_chunk.splice_to_byte(PathChunk::from_byte_start(byte)));
                buffered_chunk = PathChunk::from_byte_end(byte);
            }
            if slice.odd_end == 0 {
                self.bytes
                    .push(buffered_chunk.splice_to_byte(PathChunk::zero()));
            }
            self.odd_end = slice.odd_end ^ 1;
        } else if self.odd_end == 1 && slice.odd_start == 0 {
            self.bytes.reserve(slice.byte_slice.len());
            let mut buffered_chunk =
                PathChunk::from_byte_start(self.bytes.last().copied().unwrap());
            self.bytes.pop().unwrap();
            for &byte in slice.byte_slice {
                self.bytes
                    .push(buffered_chunk.splice_to_byte(PathChunk::from_byte_start(byte)));
                buffered_chunk = PathChunk::from_byte_end(byte);
            }
            if slice.odd_end == 0 {
                self.bytes
                    .push(buffered_chunk.splice_to_byte(PathChunk::zero()));
            }
            self.odd_end = slice.odd_end ^ 1;
        }

        if self.odd_end == 1 {
            *self.bytes.last_mut().unwrap() &= !0b1111;
        }
    }

    /// Index into the path using chunks as index.
    pub fn index_path_slice(&self, index: impl RangeBounds<usize>) -> PathSliceRef<'_> {
        self.as_path_slice().index_path_slice(index)
    }

    /// Index into the path using chunks as index.
    pub fn index_path_chunk(&self, index: usize) -> PathChunk {
        self.as_path_slice().index_path_chunk(index)
    }

    /// Length of the path slice in chunks.
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
    /// Slice of bytes that defines the path chunks. If `odd_start` is `1`,
    /// the first chunk in the first byte is undefined and not part of the slice. The same the
    /// last chunk in the last byte if `odd_end` is `1`.
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

    /// Index into the path slice using chunks as index.
    pub fn index_path_slice(&self, range_bounds: impl RangeBounds<usize>) -> PathSliceRef<'a> {
        let start_slice_index = match range_bounds.start_bound() {
            Bound::Included(&index) => index,
            Bound::Excluded(&index) => index + 1,
            Bound::Unbounded => 0,
        };
        let start_chunk_index = start_slice_index + self.odd_start as usize;
        let start_byte_index = start_chunk_index >> 1;

        let end_slice_index = match range_bounds.end_bound() {
            Bound::Included(&index) => index + 1,
            Bound::Excluded(&index) => index,
            Bound::Unbounded => self.len(),
        };
        assert!(end_slice_index <= self.len());
        let end_chunk_index = end_slice_index + self.odd_start as usize;
        let end_byte_index = (end_chunk_index + 1) >> 1;

        Self {
            byte_slice: &self.byte_slice[start_byte_index..end_byte_index],
            odd_start: (start_chunk_index & 1) as u8,
            odd_end: (end_chunk_index & 1) as u8,
        }
    }

    /// Index into the path slice using chunks as index.
    pub fn index_path_chunk(&self, slice_index: usize) -> PathChunk {
        assert!(slice_index < self.len());

        let chunk_index = slice_index + self.odd_start as usize;
        let byte_index = chunk_index >> 1;

        if chunk_index & 1 == 0 {
            PathChunk::from_byte_start(self.byte_slice[byte_index])
        } else {
            if self.odd_end == 1 && byte_index + 1 == self.byte_slice.len() {
                panic!("slice index out of bounds");
            }
            PathChunk::from_byte_end(self.byte_slice[byte_index])
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

    /// The first chunk in the path slice.
    pub fn first_chunk(&self) -> Option<PathChunk> {
        if self.is_empty() {
            None
        } else {
            Some(self.index_path_chunk(0))
        }
    }

    /// Create [`Path`] from the path slice.
    pub fn to_path<const INLINE_KEY_LENGTH: usize>(self) -> Path<INLINE_KEY_LENGTH> {
        let mut path = Path::empty();
        path.extend_from_path_slice(&self);
        path
    }

    /// Length of the path slice in chunks.
    pub fn len(&self) -> usize {
        (self.byte_slice.len() << 1) - self.odd_start as usize - self.odd_end as usize
    }

    pub fn iter(&self) -> PathSliceIter<'_> {
        PathSliceIter {
            buffered_chunk: None,
            bytes_iter: self.byte_slice.iter().peekable(),
            odd_start: self.odd_start,
            odd_end: self.odd_end,
        }
    }
}

pub struct PathSliceIter<'a> {
    buffered_chunk: Option<PathChunk>,
    bytes_iter: iter::Peekable<slice::Iter<'a, u8>>,
    odd_start: u8,
    odd_end: u8,
}

impl<'a> Iterator for PathSliceIter<'a> {
    type Item = PathChunk;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(buffered_byte) = self.buffered_chunk.take() {
            return Some(buffered_byte);
        }

        if let Some(&byte) = self.bytes_iter.next() {
            let start_chunk = PathChunk::from_byte_start(byte);
            let end_chunk = if self.bytes_iter.peek().is_some() || self.odd_end == 0 {
                Some(PathChunk::from_byte_end(byte))
            } else {
                None
            };

            if self.odd_start == 1 {
                self.odd_start = 0;

                end_chunk
            } else {
                self.buffered_chunk = end_chunk;
                Some(start_chunk)
            }
        } else {
            None
        }
    }
}

/// Length of common prefix in chunks of the two path slices.
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

/// Chunk of a path. A chunk is 4 bits.
#[derive(Debug, Copy, Clone, Ord, PartialOrd, PartialEq, Eq)]
pub struct PathChunk(u8);

impl PathChunk {
    pub fn zero() -> Self {
        Self(0)
    }

    pub fn from_byte_raw(byte: u8) -> Self {
        assert_eq!(byte & !0b1111, 0);
        Self(byte)
    }

    pub fn from_byte_start(byte: u8) -> Self {
        Self(byte >> 4)
    }

    pub fn from_byte_end(byte: u8) -> Self {
        Self(byte & 0b1111)
    }

    pub fn splice_to_byte(self, other: Self) -> u8 {
        self.0 << 4 | other.0
    }

    pub fn to_byte_raw(self) -> u8 {
        self.0
    }
}

#[cfg(test)]
mod test {
    use super::*;

    type TestPath = Path<4>;

    fn path_from_chunks(iter: impl IntoIterator<Item = PathChunk>) -> TestPath {
        let vec: Vec<_> = iter.into_iter().collect();
        let mut bytes = TinyVec::with_initial_len((vec.len() + 1) >> 1);

        for (index, chunk) in vec.chunks(2).enumerate() {
            bytes[index] =
                chunk[0].splice_to_byte(chunk.get(1).copied().unwrap_or(PathChunk::zero()));
        }

        TestPath {
            bytes,
            odd_end: (vec.len() & 1) as u8,
        }
    }

    #[test]
    fn test_index_path_chunk() {
        let path = path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(3), PathChunk(4)]);
        assert_eq!(path.index_path_chunk(0), PathChunk::from_byte_raw(1u8));
        assert_eq!(path.index_path_chunk(1), PathChunk::from_byte_raw(2u8));
        assert_eq!(path.index_path_chunk(2), PathChunk::from_byte_raw(3u8));
        assert_eq!(path.index_path_chunk(3), PathChunk::from_byte_raw(4u8));

        let path_ref = path.index_path_slice(1..);
        assert_eq!(path_ref.index_path_chunk(0), PathChunk::from_byte_raw(2u8));
        assert_eq!(path_ref.index_path_chunk(1), PathChunk::from_byte_raw(3u8));
        assert_eq!(path_ref.index_path_chunk(2), PathChunk::from_byte_raw(4u8));

        let path_ref = path.index_path_slice(..3);
        assert_eq!(path_ref.index_path_chunk(0), PathChunk::from_byte_raw(1u8));
        assert_eq!(path_ref.index_path_chunk(1), PathChunk::from_byte_raw(2u8));
        assert_eq!(path_ref.index_path_chunk(2), PathChunk::from_byte_raw(3u8));
    }

    #[test]
    fn test_first_chunk() {
        let path = path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(3), PathChunk(4)]);

        let path_ref = path.index_path_slice(0..);
        assert_eq!(path_ref.first_chunk(), Some(PathChunk::from_byte_raw(1u8)));

        let path_ref = path.index_path_slice(1..);
        assert_eq!(path_ref.first_chunk(), Some(PathChunk::from_byte_raw(2u8)));
    }

    /// Tests `index_path_slice` using `iter` for assertions, so
    /// effectively testing both at the same time.
    #[test]
    fn test_index_path_slice_using_iter() {
        let path = path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(3), PathChunk(4)]);
        let path_chunks: Vec<_> = path.as_path_slice().iter().collect();
        assert_eq!(
            path_chunks,
            vec![
                PathChunk(1u8),
                PathChunk(2u8),
                PathChunk(3u8),
                PathChunk(4u8)
            ]
        );

        let path_chunks: Vec<_> = path.index_path_slice(1..).iter().collect();
        assert_eq!(
            path_chunks,
            vec![PathChunk(2u8), PathChunk(3u8), PathChunk(4u8)]
        );

        let path_chunks: Vec<_> = path.index_path_slice(..3).iter().collect();
        assert_eq!(
            path_chunks,
            vec![PathChunk(1u8), PathChunk(2u8), PathChunk(3u8)]
        );

        let path_chunks: Vec<_> = path.index_path_slice(..=2).iter().collect();
        assert_eq!(
            path_chunks,
            vec![PathChunk(1u8), PathChunk(2u8), PathChunk(3u8)]
        );

        let path_chunks: Vec<_> = path.index_path_slice(1..1).iter().collect();
        assert_eq!(path_chunks, vec![]);

        let path_chunks: Vec<_> = path.index_path_slice(2..2).iter().collect();
        assert_eq!(path_chunks, vec![]);

        let path_chunks: Vec<_> = path.index_path_slice(1..2).iter().collect();
        assert_eq!(path_chunks, vec![PathChunk(2u8),]);

        let path_chunks: Vec<_> = path.index_path_slice(0..1).iter().collect();
        assert_eq!(path_chunks, vec![PathChunk(1u8),]);
    }

    #[test]
    fn test_extend_from_path_slice() {
        // Test with both ends of splicing byte aligned

        let ref_path = path_from_chunks([
            PathChunk(1),
            PathChunk(2),
            PathChunk(3),
            PathChunk(4),
            PathChunk(5),
        ]);

        let mut path = path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(3), PathChunk(4)]);
        path.extend_from_path_slice(&ref_path.index_path_slice(0..2));
        assert_eq!(
            path,
            path_from_chunks([
                PathChunk(1),
                PathChunk(2),
                PathChunk(3),
                PathChunk(4),
                PathChunk(1),
                PathChunk(2),
            ])
        );

        let mut path = path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(3), PathChunk(4)]);
        path.extend_from_path_slice(&ref_path.index_path_slice(0..1));
        assert_eq!(
            path,
            path_from_chunks([
                PathChunk(1),
                PathChunk(2),
                PathChunk(3),
                PathChunk(4),
                PathChunk(1),
            ])
        );

        // Test with neither ends of splicing byte aligned

        let mut path = path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(3)]);
        path.extend_from_path_slice(&ref_path.index_path_slice(1..3));
        assert_eq!(
            path,
            path_from_chunks([
                PathChunk(1),
                PathChunk(2),
                PathChunk(3),
                PathChunk(2),
                PathChunk(3),
            ])
        );

        let mut path = path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(3)]);
        path.extend_from_path_slice(&ref_path.index_path_slice(1..2));
        assert_eq!(
            path,
            path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(3), PathChunk(2),])
        );

        // Test with left end of splicing byte aligned but right not

        let mut path = path_from_chunks([PathChunk(1), PathChunk(2)]);
        path.extend_from_path_slice(&ref_path.index_path_slice(1..3));
        assert_eq!(
            path,
            path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(2), PathChunk(3),])
        );

        let mut path = path_from_chunks([PathChunk(1), PathChunk(2)]);
        path.extend_from_path_slice(&ref_path.index_path_slice(1..2));
        assert_eq!(
            path,
            path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(2),])
        );

        // Test with right end of splicing byte aligned but left not

        let mut path = path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(3)]);
        path.extend_from_path_slice(&ref_path.index_path_slice(0..2));
        assert_eq!(
            path,
            path_from_chunks([
                PathChunk(1),
                PathChunk(2),
                PathChunk(3),
                PathChunk(1),
                PathChunk(2),
            ])
        );

        let mut path = path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(3)]);
        path.extend_from_path_slice(&ref_path.index_path_slice(0..1));
        assert_eq!(
            path,
            path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(3), PathChunk(1),])
        );

        let mut path = path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(3)]);
        path.extend_from_path_slice(&ref_path.index_path_slice(0..0));
        assert_eq!(
            path,
            path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(3)])
        );
    }

    #[test]
    fn test_len() {
        let path = path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(3), PathChunk(4)]);
        assert_eq!(path.len(), 4);

        let path_ref = path.as_path_slice();
        assert_eq!(path_ref.len(), 4);

        let path_ref = path.index_path_slice(1..);
        assert_eq!(path_ref.len(), 3);

        let path_ref = path.index_path_slice(..3);
        assert_eq!(path_ref.len(), 3);

        let path_ref = path.index_path_slice(1..3);
        assert_eq!(path_ref.len(), 2);

        let path_ref = path.index_path_slice(1..1);
        assert_eq!(path_ref.len(), 0);

        let path_ref = path.index_path_slice(2..2);
        assert_eq!(path_ref.len(), 0);

        let path = path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(3)]);
        assert_eq!(path.len(), 3);
    }

    #[test]
    fn test_is_empty() {
        let path = TestPath::empty();
        assert!(path.is_empty());

        let path_ref = PathSliceRef::empty();
        assert!(path_ref.is_empty());

        let path = path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(3), PathChunk(4)]);
        assert!(!path.is_empty());

        let path_ref = path.as_path_slice();
        assert!(!path_ref.is_empty());

        let path_ref = path.index_path_slice(1..1);
        assert!(path_ref.is_empty());

        let path_ref = path.index_path_slice(1..2);
        assert!(!path_ref.is_empty());
    }

    #[test]
    fn test_as_byte_slice() {
        let path = TestPath::empty();
        assert_eq!(path.as_byte_slice(), Some([0u8; 0].as_slice()));

        let path = path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(3), PathChunk(4)]);
        assert_eq!(
            path.as_byte_slice(),
            Some([1u8 << 4 | 2u8, 3u8 << 4 | 4u8].as_slice())
        );

        let path = path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(3)]);
        assert_eq!(path.as_byte_slice(), None);
    }

    #[test]
    fn test_common_prefix_len() {
        let path1 = path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(3), PathChunk(4)]);
        let path2 = path_from_chunks([PathChunk(1), PathChunk(2), PathChunk(4), PathChunk(5)]);

        // Test identical paths

        assert_eq!(
            common_prefix_len(path1.as_path_slice(), path1.as_path_slice()),
            4
        );

        // Test path prefix of other path

        assert_eq!(
            common_prefix_len(path1.as_path_slice(), path1.index_path_slice(0..3)),
            3
        );

        assert_eq!(
            common_prefix_len(path1.index_path_slice(0..3), path1.as_path_slice()),
            3
        );

        // Test paths not identical

        assert_eq!(
            common_prefix_len(path1.as_path_slice(), path2.as_path_slice()),
            2
        );

        assert_eq!(
            common_prefix_len(path1.as_path_slice(), path1.index_path_slice(1..)),
            0
        );
    }
}
