use concordium_base::common::{Buffer, Deserial, Get, ParseResult, Put, ReadBytesExt, Serial};
use std::ops::RangeBounds;
use tinyvec::TinyVec;

/// Path
#[derive(Debug, Clone)]
pub struct Path<const INLINE_KEY_LENGTH: usize>(TinyVec<[u8; INLINE_KEY_LENGTH]>);

impl<const INLINE_KEY_LENGTH: usize> Path<INLINE_KEY_LENGTH> {
    pub fn as_path_slice(&self) -> PathSliceRef<'_> {
        PathSliceRef(self.0.as_slice())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn as_byte_slice(&self) -> Option<&[u8]> {
        Some(self.0.as_slice())
    }

    pub fn extend_from_path_slice(&mut self, slice: PathSliceRef<'_>) {
        self.0.extend_from_slice(&slice.0)
    }

    pub fn from_tiny_vec(tiny_vec: TinyVec<[u8; INLINE_KEY_LENGTH]>) -> Self {
        Self(tiny_vec)
    }

    pub fn index_path_slice(
        &self,
        index: impl RangeBounds<usize> + std::slice::SliceIndex<[u8], Output = [u8]>,
    ) -> PathSliceRef<'_> {
        self.as_path_slice().index_path_slice(index)
    }

    pub fn index_path_nibble(&self, index: usize) -> PathNibble {
        self.as_path_slice().index_path_nibble(index)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl<const INLINE_KEY_LENGTH: usize> Serial for Path<INLINE_KEY_LENGTH> {
    fn serial<B: Buffer>(&self, out: &mut B) {
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
pub struct PathSliceRef<'a>(&'a [u8]);

impl<'a> PathSliceRef<'a> {
    pub fn empty() -> Self {
        Self(&[])
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn index_path_slice(
        &self,
        index: impl RangeBounds<usize> + std::slice::SliceIndex<[u8], Output = [u8]>,
    ) -> PathSliceRef<'a> {
        Self(&self.0[index])
    }

    pub fn index_path_nibble(&self, index: usize) -> PathNibble {
        PathNibble(self.0[index])
    }

    pub fn from_byte_slice(slice: &'a [u8]) -> Self {
        Self(slice)
    }

    pub fn first_nibble(&self) -> Option<PathNibble> {
        self.0.first().copied().map(PathNibble)
    }

    pub fn to_path<const INLINE_KEY_LENGTH: usize>(&self) -> Path<INLINE_KEY_LENGTH> {
        Path(self.0.into())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

pub fn common_prefix_len(path_ref1: PathSliceRef<'_>, path_ref2: PathSliceRef<'_>) -> usize {
    let mut i = 0;
    while i < path_ref1.len() && i < path_ref2.len() && path_ref1.0[i] == path_ref2.0[i] {
        i += 1;
    }
    i
}

/// Stores the nibble in the first 4 bits of the `u8`
#[derive(Debug, Copy, Clone, Ord, PartialOrd, PartialEq, Eq)]
pub struct PathNibble(u8);

impl PathNibble {
    pub fn from_byte(byte: u8) -> Self {
        Self(byte)
    }

    pub fn as_byte(&self) -> u8 {
        self.0
    }
}
