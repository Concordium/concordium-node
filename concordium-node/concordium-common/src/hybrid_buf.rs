#[cfg(feature = "s11n_serde")]
use serde::{
    de::{self, Deserializer, Visitor},
    ser::{self, SerializeStruct, Serializer},
};
use tempfile::tempfile;

use std::{
    borrow::Cow,
    cmp,
    convert::TryFrom,
    fs::File,
    io::{self, Cursor, Read, Result, Seek, SeekFrom, Write},
    mem,
};

const DEFAULT_MEM_SIZE: usize = 4 * 1024;
const MAX_MEM_SIZE: usize = 4 * 1024 * 1024;

type MemBuf = Cursor<Vec<u8>>;

#[derive(Debug)]
pub enum HybridBuf {
    Mem(MemBuf),
    File(File),
}

impl HybridBuf {
    pub fn new() -> Self { Default::default() }

    pub fn new_on_disk() -> Result<Self> { Ok(HybridBuf::File(tempfile()?)) }

    pub fn with_capacity(cap: usize) -> Result<Self> {
        if cap <= MAX_MEM_SIZE {
            Ok(HybridBuf::Mem(Cursor::new(Vec::with_capacity(cap))))
        } else {
            Self::new_on_disk()
        }
    }

    pub fn len(&self) -> Result<u64> {
        match self {
            Self::Mem(cursor) => Ok(cursor.get_ref().len() as u64),
            Self::File(file) => file.metadata().map(|md| md.len()),
        }
    }

    pub fn is_empty(&self) -> Result<bool> { self.len().map(|len| len == 0) }

    pub fn position(&mut self) -> Result<u64> {
        match self {
            Self::Mem(cursor) => Ok(cursor.position()),
            Self::File(file) => file.seek(SeekFrom::Current(0)),
        }
    }

    #[allow(clippy::wrong_self_convention)] // doesn't actually mutate
    pub fn is_eof(&mut self) -> Result<bool> { Ok(self.position()? == self.len()?) }

    pub fn swap_to_disk(&mut self) -> Result<()> {
        if let Self::Mem(cursor) = self {
            let mut file_buf = Self::new_on_disk()?;
            file_buf.write_all(cursor.get_ref())?;
            let _ = mem::replace(self, file_buf);
        }
        Ok(())
    }

    pub fn remaining_len(&mut self) -> Result<u64> { Ok(self.len()? - self.position()?) }

    pub fn remaining_bytes(&mut self) -> Result<Cow<'_, [u8]>> {
        if let Self::Mem(ref mut cursor) = self {
            let pos = cursor.position() as usize;
            cursor.seek(SeekFrom::End(0))?; // exhaust the cursor
            let bytes = (&cursor.get_ref()[pos..]).into();
            Ok(bytes)
        } else {
            let size = cmp::max(self.len().unwrap_or(0) as usize, MAX_MEM_SIZE);
            let mut bytes = Vec::with_capacity(size);
            self.read_to_end(&mut bytes)?;
            Ok(bytes.into())
        }
    }

    pub fn rewind(&mut self) -> Result<()> {
        self.seek(SeekFrom::Start(0))?;
        Ok(())
    }
}

impl Default for HybridBuf {
    fn default() -> Self {
        let buf = Vec::with_capacity(DEFAULT_MEM_SIZE);
        HybridBuf::Mem(Cursor::new(buf))
    }
}

impl Clone for HybridBuf {
    fn clone(&self) -> Self {
        match self {
            Self::Mem(cursor) => HybridBuf::Mem(cursor.clone()),
            Self::File(file) => HybridBuf::File(file.try_clone().expect(
                "Can't duplicate a buffer file's handle/descriptor! The operation is unsupported \
                 or the disk is full.",
            )),
        }
    }
}

impl Read for HybridBuf {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        match self {
            Self::Mem(cursor) => cursor.read(buf),
            Self::File(file) => file.read(buf),
        }
    }

    fn read_exact(&mut self, buf: &mut [u8]) -> Result<()> {
        match self {
            Self::Mem(cursor) => cursor.read_exact(buf),
            Self::File(file) => file.read_exact(buf),
        }
    }
}

impl Write for HybridBuf {
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        match self {
            Self::Mem(cursor) => {
                let target_len = cursor.get_ref().len() + buf.len();

                if target_len <= MAX_MEM_SIZE {
                    cursor.write(buf)
                } else {
                    self.swap_to_disk()?;
                    self.write(buf)
                }
            }
            Self::File(file) => file.write(buf),
        }
    }

    fn write_all(&mut self, buf: &[u8]) -> Result<()> {
        match self {
            Self::Mem(cursor) => {
                let target_len = cursor.get_ref().len() + buf.len();

                if target_len <= MAX_MEM_SIZE {
                    cursor.write_all(buf)
                } else {
                    self.swap_to_disk()?;
                    self.write_all(buf)
                }
            }
            Self::File(file) => file.write_all(buf),
        }
    }

    fn flush(&mut self) -> Result<()> {
        match self {
            Self::Mem(_) => Ok(()),
            Self::File(file) => file.flush(),
        }
    }
}

impl TryFrom<Vec<u8>> for HybridBuf {
    type Error = io::Error;

    fn try_from(vec: Vec<u8>) -> Result<Self> {
        if vec.len() <= MAX_MEM_SIZE {
            Ok(HybridBuf::Mem(Cursor::new(vec)))
        } else {
            let mut ret = HybridBuf::new_on_disk()?;
            ret.write_all(&vec)?;
            ret.rewind()?;
            Ok(ret)
        }
    }
}

impl<'a> TryFrom<&'a [u8]> for HybridBuf {
    type Error = io::Error;

    fn try_from(buf: &[u8]) -> Result<Self> {
        if buf.len() <= MAX_MEM_SIZE {
            Ok(HybridBuf::Mem(Cursor::new(buf.to_owned())))
        } else {
            let mut ret = HybridBuf::new_on_disk()?;
            ret.write_all(buf)?;
            ret.rewind()?;
            Ok(ret)
        }
    }
}

impl TryFrom<HybridBuf> for Vec<u8> {
    type Error = io::Error;

    fn try_from(mut buf: HybridBuf) -> Result<Self> {
        match buf {
            HybridBuf::Mem(cursor) => Ok(cursor.into_inner()),
            HybridBuf::File(_) => {
                let mut ret = Vec::with_capacity(buf.len()? as usize);
                buf.read_to_end(&mut ret)?;
                Ok(ret)
            }
        }
    }
}

impl Seek for HybridBuf {
    fn seek(&mut self, tgt: SeekFrom) -> Result<u64> {
        match self {
            Self::Mem(ref mut cursor) => cursor.seek(tgt),
            Self::File(ref mut file) => file.seek(tgt),
        }
    }
}

#[cfg(feature = "s11n_serde")]
impl serde::ser::Serialize for HybridBuf {
    fn serialize<S>(&self, serializer: S) -> core::result::Result<S::Ok, S::Error>
    where
        S: Serializer, {
        let mut state = serializer.serialize_struct("HybridBuf", 1)?;
        let potential_err = ser::Error::custom("can't read a HybridBuf into memory");
        state.serialize_field("hb", &self.clone().into_vec().map_err(|_| potential_err)?)?;
        state.end()
    }
}

#[cfg(feature = "s11n_serde")]
impl<'de> serde::de::Deserialize<'de> for HybridBuf {
    fn deserialize<D>(deserializer: D) -> core::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>, {
        struct HybridBufVisitor;

        impl<'de> Visitor<'de> for HybridBufVisitor {
            type Value = HybridBuf;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a HybridBuf struct")
            }

            fn visit_bytes<E>(self, buf: &[u8]) -> core::result::Result<Self::Value, E>
            where
                E: serde::de::Error, {
                let potential_err = de::Error::custom("can't write a HybridBuf to disk");
                let mut ret = HybridBuf::new();
                ret.write_all(&buf).map_err(|_| potential_err)?;
                Ok(ret)
            }
        }

        deserializer.deserialize_bytes(HybridBufVisitor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn position_and_resource_swapping() {
        const CHUNK_SIZE: usize = 4 * 1024;
        let mut hb = HybridBuf::default();

        while hb.len().expect("Can't read a HybridBuffer's length!") <= MAX_MEM_SIZE as u64 {
            let _ = hb.write_all(&[0u8; CHUNK_SIZE]);
        }

        assert_eq!(
            hb.position().expect("Can't read a HybridBuffer's pos!"),
            (MAX_MEM_SIZE + CHUNK_SIZE) as u64
        );

        if let HybridBuf::Mem(_) = hb {
            panic!("HybridBuf doesn't swap to disk!");
        }
    }
}
