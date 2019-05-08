use std::{
    fs::File,
    io::{BufReader, Cursor, Read, Result, Seek, SeekFrom, Write},
    sync::Arc,
};

use failure::Fallible;
use tempfile::NamedTempFile;

use crate::ContainerView;

#[derive(Debug)]
pub struct UCursorFile {
    pub src_temp_file: Arc<NamedTempFile>,
    pub file:          Box<BufReader<File>>,
    pub len:           u64,
    pub pos:           u64,
    pub offset:        u64,
}

impl UCursorFile {
    pub fn build() -> Result<Self> {
        let tmp_file = NamedTempFile::new()?;
        let file = tmp_file.reopen()?;

        Ok(UCursorFile {
            src_temp_file: Arc::new(tmp_file),
            file:          Box::new(BufReader::new(file)),
            len:           0,
            pos:           0,
            offset:        0,
        })
    }

    pub fn read_all_into(&mut self, out: &mut Vec<u8>) -> Fallible<()> {
        self.file.seek(SeekFrom::Start(self.offset))?;
        self.file.read_to_end(out)?;
        self.file.seek(SeekFrom::Start(self.pos))?;
        Ok(())
    }

    fn try_clone(&self) -> Result<Self> {
        Ok(UCursorFile {
            src_temp_file: Arc::clone(&self.src_temp_file),
            file:          Box::new(BufReader::new(self.src_temp_file.reopen()?)),
            len:           self.len,
            pos:           0,
            offset:        self.offset,
        })
    }
}

/// # TODO
///  * Add len to `File` in order to improve `len()` func.
#[derive(Debug)]
pub enum UCursor {
    Memory(Cursor<ContainerView>),
    File(UCursorFile),
}

impl std::cmp::PartialEq for UCursor {
    fn eq(&self, other: &UCursor) -> bool {
        match self {
            UCursor::Memory(ref cursor) => {
                if let UCursor::Memory(ref other_cursor) = other {
                    cursor.get_ref() == other_cursor.get_ref()
                } else {
                    false
                }
            }
            UCursor::File(ref uc_file) => {
                if let UCursor::File(ref uc_other_file) = other {
                    uc_file.src_temp_file.path() == uc_other_file.src_temp_file.path()
                        && uc_file.offset == uc_other_file.offset
                } else {
                    false
                }
            }
        }
    }
}

impl UCursor {
    pub fn build_from_temp_file() -> Result<Self> { Ok(UCursor::File(UCursorFile::build()?)) }

    #[inline]
    pub fn build_from_view(view: ContainerView) -> Self { UCursor::Memory(Cursor::new(view)) }

    #[inline]
    pub fn sub(&self, offset: u64) -> Result<Self> { self.sub_range(offset, self.len() - offset) }

    pub fn sub_range(&self, offset: u64, len: u64) -> Result<Self> {
        let ic = match self {
            UCursor::Memory(ref cursor) => {
                UCursor::build_from_view(cursor.get_ref().sub_range(offset as usize, len as usize))
            }
            UCursor::File(ref uc_file) => {
                let mut other = uc_file.try_clone()?;
                other.offset += offset;
                other.len = std::cmp::min(len, other.len - offset);

                UCursor::File(other)
            }
        };
        Ok(ic)
    }

    #[inline]
    pub fn position(&self) -> u64 {
        match self {
            UCursor::Memory(ref cursor) => cursor.position(),
            UCursor::File(ref uc_file) => uc_file.pos,
        }
    }

    #[inline]
    pub fn set_position(&mut self, pos: u64) -> u64 {
        let prev_position = self.position();
        match self {
            UCursor::Memory(ref mut cursor) => cursor.set_position(pos),
            UCursor::File(ref mut uc_file) => {
                uc_file.pos = pos;
                let _ = uc_file.file.seek(SeekFrom::Start(pos));
            }
        };
        prev_position
    }

    #[inline]
    pub fn len(&self) -> u64 {
        match self {
            UCursor::Memory(ref cursor) => cursor.get_ref().len() as u64,
            UCursor::File(ref uc_file) => uc_file.len,
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool { self.len() == 0 }

    pub fn read_into_view(&mut self, size: usize) -> Result<ContainerView> {
        match self {
            UCursor::Memory(ref mut cursor) => {
                let curr_pos = cursor.position() as usize;
                if curr_pos + size <= cursor.get_ref().len() {
                    let view = cursor.get_ref().sub_range(curr_pos, size);
                    cursor.set_position((curr_pos + size) as u64);
                    Ok(view)
                } else {
                    Err(std::io::Error::from(std::io::ErrorKind::UnexpectedEof))
                }
            }
            UCursor::File(ref mut uc_file) => {
                // Generate view and write into it.
                let mut view_content = Vec::with_capacity(size);
                unsafe { view_content.set_len(size) };

                uc_file.file.read_exact(view_content.as_mut_slice())?;
                let view = ContainerView::from(view_content);
                debug_assert_eq!(view.len(), size);

                uc_file.pos += size as u64;
                Ok(view)
            }
        }
    }

    pub fn read_all_into_view(&mut self) -> Result<ContainerView> {
        let self_len = self.len() as usize;
        let curr_pos = self.set_position(0);
        let view = self.read_into_view(self_len).or_else(|e| {
            self.set_position(curr_pos);
            Err(e)
        })?;
        self.set_position(curr_pos);
        Ok(view)
    }

    pub fn clear(&mut self) {
        match self {
            UCursor::Memory(ref mut cursor) => {
                cursor.set_position(0);
            }
            UCursor::File(..) => {
                let mut other =
                    UCursor::build_from_view(ContainerView::from(Vec::with_capacity(4 * 1024)));
                std::mem::swap(self, &mut other);
            }
        }
    }

    pub fn swap_to_file(&mut self) -> Result<()> {
        if let UCursor::Memory(ref mut cursor) = self {
            cursor.set_position(0);
        }

        if let UCursor::Memory(..) = self {
            let mut other = UCursor::build_from_temp_file()?;
            std::io::copy(self, &mut other)?;
            other.flush()?;
            std::mem::swap(self, &mut other);
            self.set_position(0);
        }
        Ok(())
    }

    pub fn swap_to_memory(&mut self) -> Result<()> {
        let mut data_opt = None;

        if let UCursor::File(ref mut uc_file) = self {
            // Sync
            uc_file.file.get_mut().sync_all()?;
            uc_file.file.seek(SeekFrom::Start(uc_file.offset))?;

            // Copy into mem
            let mut data = Vec::with_capacity(uc_file.len as usize);
            std::io::copy(&mut uc_file.file, &mut data)?;

            data_opt = Some(data);
        };

        if let Some(data_opt) = data_opt {
            let mut other = UCursor::build_from_view(ContainerView::from(data_opt));
            std::mem::swap(self, &mut other);
        };
        Ok(())
    }
}

impl std::clone::Clone for UCursor {
    /// # TODO
    /// Double-check that `flush` is not needed before clone from a `file`,
    /// because current implementation is using its `file_path` to open a
    /// new OS descriptor. It will imply `Internal mutability` for files.
    fn clone(&self) -> Self {
        let c = match self {
            UCursor::Memory(ref cursor) => UCursor::build_from_view(cursor.get_ref().clone()),
            UCursor::File(ref uc_file) => {
                let other_uc_file = uc_file.try_clone().expect(
                    "Unable to clone file cursor, check you have free space on temporary folder",
                );

                UCursor::File(other_uc_file)
            }
        };

        debug_assert_eq!(c.position(), 0);
        c
    }
}

impl std::io::Read for UCursor {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        match self {
            UCursor::Memory(ref mut cursor) => cursor.read(buf),
            UCursor::File(ref mut uc_file) => {
                let bytes_read = uc_file.file.read(buf)?;
                uc_file.pos += bytes_read as u64;
                Ok(bytes_read)
            }
        }
    }
}

impl std::io::Write for UCursor {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        match self {
            UCursor::Memory(_) => unimplemented!(), // we don't write to memory
            UCursor::File(ref mut uc_file) => {
                let bytes = uc_file.file.get_mut().write(buf)?;
                uc_file.pos += bytes as u64;
                uc_file.len = std::cmp::max(uc_file.len, uc_file.pos + uc_file.offset);
                Ok(bytes)
            }
        }
    }

    #[inline]
    fn write_all(&mut self, buf: &[u8]) -> Result<()> {
        match self {
            UCursor::Memory(_) => unimplemented!(), // we don't write to memory
            UCursor::File(ref mut uc_file) => {
                uc_file.file.get_mut().write_all(buf)?;
                uc_file.pos += buf.len() as u64;
                uc_file.len = std::cmp::max(uc_file.len, uc_file.pos + uc_file.offset);
                Ok(())
            }
        }
    }

    #[inline]
    fn flush(&mut self) -> Result<()> {
        if let UCursor::File(ref mut uc_file) = self {
            uc_file.file.get_mut().flush()
        } else {
            Ok(())
        }
    }
}

impl std::io::Seek for UCursor {
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        match self {
            UCursor::Memory(ref mut cursor) => cursor.seek(pos),
            UCursor::File(ref mut uc_file) => {
                let filtered_pos = match pos {
                    refpos @ SeekFrom::Current(..) => refpos,
                    refpos @ SeekFrom::End(..) => refpos,
                    SeekFrom::Start(offset_from_start) => {
                        SeekFrom::Start(offset_from_start + uc_file.offset)
                    }
                };
                uc_file.pos = uc_file.file.seek(filtered_pos)? - uc_file.offset as u64;
                Ok(uc_file.pos)
            }
        }
    }
}

impl From<Vec<u8>> for UCursor {
    #[inline]
    fn from(data: Vec<u8>) -> Self { UCursor::build_from_view(ContainerView::from(data)) }
}

impl From<ContainerView> for UCursor {
    #[inline]
    fn from(view: ContainerView) -> Self { UCursor::build_from_view(view) }
}

#[cfg(feature = "s11n_serde")]
use serde::ser::{SerializeStruct, Serializer};

#[cfg(feature = "s11n_serde")]
impl serde::ser::Serialize for UCursor {
    /// # TODO
    /// Implement using a kind of 'reader' or as 'chunks' in order to avoid all
    /// file into memory.
    fn serialize<S>(&self, serializer: S) -> core::result::Result<S::Ok, S::Error>
    where
        S: Serializer, {
        let content = match self {
            UCursor::Memory(ref cursor) => cursor.get_ref(),
            UCursor::File(..) => {
                return Err(serde::ser::Error::custom("Cannot serialize from file"));
            }
        };

        let mut state = serializer.serialize_struct("UCursor", 1)?;
        state.serialize_field("content", content.as_slice())?;
        state.end()
    }
}

#[cfg(feature = "s11n_serde")]
use serde::de::{Deserialize, Deserializer, MapAccess, Visitor};

#[cfg(feature = "s11n_serde")]
impl<'de> serde::de::Deserialize<'de> for UCursor {
    fn deserialize<D>(deserializer: D) -> core::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>, {
        enum Field {
            Content,
        }

        impl<'de> Deserialize<'de> for Field {
            fn deserialize<D>(deserializer: D) -> core::result::Result<Field, D::Error>
            where
                D: Deserializer<'de>, {
                struct FieldVisistor;

                impl<'de> Visitor<'de> for FieldVisistor {
                    type Value = Field;

                    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                        formatter.write_str("`content`")
                    }

                    fn visit_str<E>(self, value: &str) -> core::result::Result<Field, E>
                    where
                        E: serde::de::Error, {
                        match value {
                            "content" => Ok(Field::Content),
                            _ => Err(serde::de::Error::unknown_field(value, FIELDS)),
                        }
                    }
                }

                deserializer.deserialize_identifier(FieldVisistor)
            }
        }

        struct UCursorVisitor;

        impl<'de> Visitor<'de> for UCursorVisitor {
            type Value = UCursor;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("struct UCursor")
            }

            fn visit_map<V>(self, mut map: V) -> core::result::Result<UCursor, V::Error>
            where
                V: MapAccess<'de>, {
                let mut data_content: Option<Vec<u8>> = None;

                while let Some(field) = map.next_key()? {
                    match field {
                        Field::Content => {
                            if data_content.is_some() {
                                return Err(serde::de::Error::duplicate_field("content"));
                            }
                            data_content = Some(map.next_value()?);
                        }
                    }
                }

                let content =
                    data_content.ok_or_else(|| serde::de::Error::missing_field("content"))?;

                Ok(UCursor::build_from_view(ContainerView::from(content)))
            }
        }

        const FIELDS: &[&str] = &["content"];
        deserializer.deserialize_struct("UCursor", FIELDS, UCursorVisitor)
    }
}

#[cfg(test)]
mod unit_test {
    use failure::Fallible;
    use rand::{distributions::Standard, thread_rng, Rng};
    use std::io::{Cursor, Read};

    use super::UCursor;
    use crate::ContainerView;

    fn make_content_with_size(content_size: usize) -> Vec<u8> {
        thread_rng()
            .sample_iter(&Standard)
            .take(content_size)
            .collect::<Vec<u8>>()
    }

    #[test]
    fn in_memory_1_k() -> Fallible<()> { in_memory(1024) }

    #[test]
    fn in_memory_512_k() -> Fallible<()> { in_memory(512 * 1024) }

    #[test]
    fn in_memory_1_m() -> Fallible<()> { in_memory(1024 * 1024) }

    fn in_memory(content_size: usize) -> Fallible<()> {
        let content = make_content_with_size(content_size);
        let test_content = content[..3].to_vec();

        let view = ContainerView::from(content.clone());

        let mut buff = [0; 5];
        let c1 = UCursor::build_from_view(view.clone());
        let mut t1 = c1.take(3);
        t1.read(&mut buff)?;
        assert_eq!(&buff[..3], test_content.as_slice());

        let mut out = Vec::new();
        let mut c2 = UCursor::build_from_view(view);
        c2.read_to_end(&mut out)?;
        assert_eq!(out, content);

        Ok(())
    }

    #[test]
    fn in_file_1_k() -> Fallible<()> { in_file(1024) }

    #[test]
    fn in_file_512_k() -> Fallible<()> { in_file(512 * 1024) }

    #[test]
    fn in_file_1_m() -> Fallible<()> { in_file(1024 * 1024) }

    fn in_file(content_size: usize) -> Fallible<()> {
        let content = make_content_with_size(content_size);

        let mut c2 = {
            let mut c1 = UCursor::build_from_temp_file()?;
            std::io::copy(&mut Cursor::new(content.as_slice()), &mut c1)?;
            c1.clone()
        };

        let mut out = Vec::with_capacity(content_size);
        c2.read_to_end(&mut out)?;

        assert_eq!(out, content);

        Ok(())
    }

    #[test]
    fn from_memory_to_file_1_m() -> Fallible<()> { from_memory_to_file(1024 * 1024) }

    #[test]
    fn from_memory_to_file_4_m() -> Fallible<()> { from_memory_to_file(4 * 1024 * 1024) }

    #[test]
    fn from_memory_to_file_32_m() -> Fallible<()> { from_memory_to_file(32 * 1024 * 1024) }

    fn from_memory_to_file(content_size: usize) -> Fallible<()> {
        let content = make_content_with_size(content_size);
        let view = ContainerView::from(content);

        let mut cur = UCursor::build_from_view(view);
        cur.swap_to_file().map_err(|e| failure::Error::from(e))?;
        Ok(())
    }

}
