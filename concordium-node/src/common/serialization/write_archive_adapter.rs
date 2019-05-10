use crate::common::serialization::WriteArchive;

use byteorder::{LittleEndian, WriteBytesExt};
use failure::Fallible;

use std::io::Write;

pub struct WriteArchiveAdapter<T>
where
    T: Write, {
    io_writer: T,
}

impl<T> WriteArchiveAdapter<T>
where
    T: Write,
{
    #[inline]
    pub fn into_inner(self) -> T { self.io_writer }

    #[inline]
    pub fn inner(&self) -> &T { &self.io_writer }

    #[inline]
    pub fn inner_mut(&mut self) -> &mut T { &mut self.io_writer }
}

impl<T> std::convert::From<T> for WriteArchiveAdapter<T>
where
    T: Write,
{
    #[inline]
    fn from(io: T) -> Self { WriteArchiveAdapter { io_writer: io } }
}

impl<T> WriteArchive for WriteArchiveAdapter<T>
where
    T: Write,
{
    #[inline]
    fn write_u8(&mut self, data: u8) -> Fallible<()> { into_err!(self.io_writer.write_u8(data)) }

    #[inline]
    fn write_u16(&mut self, data: u16) -> Fallible<()> {
        into_err!(self.io_writer.write_u16::<LittleEndian>(data))
    }

    #[inline]
    fn write_u32(&mut self, data: u32) -> Fallible<()> {
        into_err!(self.io_writer.write_u32::<LittleEndian>(data))
    }

    #[inline]
    fn write_u64(&mut self, data: u64) -> Fallible<()> {
        into_err!(self.io_writer.write_u64::<LittleEndian>(data))
    }
}

impl<T> std::io::Write for WriteArchiveAdapter<T>
where
    T: Write,
{
    #[inline]
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> { self.io_writer.write(buf) }

    #[inline]
    fn flush(&mut self) -> std::io::Result<()> { self.io_writer.flush() }
}
