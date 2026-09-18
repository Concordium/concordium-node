use concordium_base::common::{Buffer, Deserial, Get, ParseResult, Put, ReadBytesExt, Serial};
use std::ops::RangeBounds;
use tinyvec::TinyVec;

/// Path
#[derive(Debug, Clone)]
pub struct Path<const INLINE_KEY_LENGTH: usize>(TinyVec<[u8; INLINE_KEY_LENGTH]>);

impl<const INLINE_KEY_LENGTH: usize> Path<INLINE_KEY_LENGTH> {
    pub fn as_path_slice(&self) -> PathSliceRef<'_> {
        PathSliceRef::LowHigh(self.0.as_slice())
    }

    pub fn as_byte_slice(&self) -> Option<&[u8]> {
        todo!()
    }

    pub fn extend_from_path_slice(&mut self, slice: PathSliceRef<'_>) {
        todo!()
    }

    pub fn from_tiny_vec(tiny_vec: TinyVec<[u8; INLINE_KEY_LENGTH]>) -> Self {
        todo!()
    }

    pub fn index_path_slice(&self, index: impl RangeBounds<usize>) -> PathSliceRef<'_> {
        self.as_path_slice().index_path_slice(index)
    }

    pub fn index_path_nibble(&self, index: usize) -> PathNibble {
        self.as_path_slice().index_path_nibble(index)
    }

    pub fn len(&self) -> usize {
        todo!()
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
pub enum PathSliceRef<'a> {
    LowLow(&'a [u8]),
    LowHigh(&'a [u8]),
    HighLow(&'a [u8]),
    HighHigh(&'a [u8]),
}

impl<'a> PathSliceRef<'a> {
    pub fn empty() -> Self {
        todo!()
    }

    pub fn is_empty(&self) -> bool {
        todo!()
    }

    pub fn index_path_slice(&self, index: impl RangeBounds<usize>) -> PathSliceRef<'a> {
        todo!()
    }

    pub fn index_path_nibble(&self, index: usize) -> PathNibble {
        todo!()
    }

    pub fn from_byte_slice(slice: &'a [u8]) -> Self {
        Self::LowHigh(slice)
    }

    pub fn first_nibble(&self) -> Option<PathNibble> {
        todo!()
    }

    pub fn to_path<const INLINE_KEY_LENGTH: usize>(&self) -> Path<INLINE_KEY_LENGTH> {
        todo!()
    }

    pub fn len(&self) -> usize {
        todo!()
    }

    // pub fn as_byte_slice(&self) -> Option<&[u8]> {
    //     match self {
    //         PathSliceRef::LowHigh(byte_slice) => Some(byte_slice),
    //         _ => None,
    //     }
    // }
}

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
