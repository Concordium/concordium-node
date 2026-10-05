use crate::failure::{BlockStateFailure, BlockStateResult};
use concordium_base::common::{Buffer, Deserial, Get, ParseResult, Put, ReadBytesExt, Serial};
use std::ops::RangeBounds;
use tinyvec::TinyVec;

/// Path represents a path in a trie, or a path to look up in the trie. A path is represented
/// as a sequence of bytes.
#[derive(Debug, Clone)]
pub struct Path<const INLINE_KEY_LENGTH: usize>(
    /// Invariant: Length is always less or equal to [`MAX_PATH_LENGTH`]
    TinyVec<[u8; INLINE_KEY_LENGTH]>,
);

/// Max length of a path. Must be kept in sync with `Serial` and `Deserial` implementations.
const MAX_PATH_LENGTH: usize = u16::MAX as usize;

impl<const INLINE_KEY_LENGTH: usize> Path<INLINE_KEY_LENGTH> {
    /// Create empty path
    pub fn empty() -> Self {
        Self(TinyVec::new())
    }

    /// If path is empty.
    #[allow(unused)]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Borrow as path slice.
    pub fn as_path_slice(&self) -> PathSliceRef<'_> {
        PathSliceRef(self.0.as_slice())
    }

    /// Returns the path represented as bytes.
    pub fn as_byte_slice(&self) -> &[u8] {
        self.0.as_slice()
    }

    /// Extend the path with the given path slice.
    pub fn extend_from_path_slice(&mut self, slice: &PathSliceRef<'_>) -> BlockStateResult<()> {
        check_length(self.0.len() + slice.0.len())?;
        self.0.extend_from_slice(slice.0);
        Ok(())
    }

    /// Extend the path with the given chunk.
    pub fn extend_with_chunk(&mut self, chunk: PathChunk) -> BlockStateResult<()> {
        check_length(self.0.len() + 1)?;
        self.0.push(chunk.to_byte());
        Ok(())
    }

    /// Index into the path.
    pub fn index_path_slice(
        &self,
        index: impl RangeBounds<usize> + std::slice::SliceIndex<[u8], Output = [u8]>,
    ) -> PathSliceRef<'_> {
        self.as_path_slice().index_path_slice(index)
    }

    /// Index into the path.
    pub fn index_path_chunk(&self, index: usize) -> PathChunk {
        self.as_path_slice().index_path_chunk(index)
    }

    /// Length of the path in bytes.
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

fn check_length(length: usize) -> BlockStateResult<()> {
    if length > MAX_PATH_LENGTH {
        Err(BlockStateFailure::Invariant(format!(
            "trie path max length exceeded: {}",
            length
        )))
    } else {
        Ok(())
    }
}

impl<const INLINE_KEY_LENGTH: usize> Serial for Path<INLINE_KEY_LENGTH> {
    fn serial<B: Buffer>(&self, out: &mut B) {
        out.put(u16::try_from(self.len()).expect("path length invariant broken"));
        out.write_all(self.0.as_slice())
            .expect("Writing to a buffer should not fail.");
    }
}

impl<const INLINE_KEY_LENGTH: usize> Deserial for Path<INLINE_KEY_LENGTH> {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> ParseResult<Self> {
        let size: u16 = source.get()?;
        let mut vec = TinyVec::with_initial_len(size as usize);
        source.read_exact(&mut vec)?;
        Ok(Path(vec))
    }
}

/// Reference to slice of [`Path`]
#[derive(Debug, Copy, Clone)]
pub struct PathSliceRef<'a>(
    /// Invariant: Length is always less or equal to [`MAX_PATH_LENGTH`]
    &'a [u8],
);

impl<'a> PathSliceRef<'a> {
    /// Create empty path slice
    #[allow(unused)]
    pub fn empty() -> Self {
        Self(&[])
    }

    /// Index into the path slice.
    pub fn index_path_slice(
        &self,
        index: impl RangeBounds<usize> + std::slice::SliceIndex<[u8], Output = [u8]>,
    ) -> PathSliceRef<'a> {
        Self(&self.0[index])
    }

    /// Index into the path slice.
    pub fn index_path_chunk(&self, index: usize) -> PathChunk {
        PathChunk(self.0[index])
    }

    /// Create path slice from given byte slice.
    pub fn from_byte_slice(slice: &'a [u8]) -> BlockStateResult<Self> {
        check_length(slice.len())?;
        Ok(Self(slice))
    }

    /// The first chunk in the path slice.
    pub fn first_chunk(&self) -> Option<PathChunk> {
        self.0.first().copied().map(PathChunk)
    }

    /// Create [`Path`] from the path slice.
    pub fn to_path<const INLINE_KEY_LENGTH: usize>(self) -> Path<INLINE_KEY_LENGTH> {
        Path(self.0.into())
    }

    /// Length of the path slice in bytes.
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

/// Length of common prefix in bytes of the two path slices.
pub fn common_prefix_len(path_ref1: PathSliceRef<'_>, path_ref2: PathSliceRef<'_>) -> usize {
    let mut i = 0;
    while i < path_ref1.len() && i < path_ref2.len() && path_ref1.0[i] == path_ref2.0[i] {
        i += 1;
    }
    i
}

/// Chunk of a path. Currently, a chunk is a single byte.
#[derive(Debug, Copy, Clone, Ord, PartialOrd, PartialEq, Eq)]
pub struct PathChunk(u8);

impl PathChunk {
    pub fn from_byte(byte: u8) -> Self {
        Self(byte)
    }

    pub fn to_byte(self) -> u8 {
        self.0
    }
}
