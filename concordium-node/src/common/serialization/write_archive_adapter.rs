use crate::common::{ serialization::WriteArchive, UCursor };

use byteorder::{ByteOrder, LittleEndian};
use failure::Fallible;

use std::io::Write;

pub struct WriteArchiveAdapter<T>
where
    T: Write
{
    io_writer: T,
    io_payload: Option<UCursor>
}

impl<T> WriteArchiveAdapter<T>
where
    T: Write
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
    T: Write
{
    #[inline]
    fn from(io: T) -> Self { WriteArchiveAdapter {
        io_writer: io,
        io_payload: None
        }
    }
}

macro_rules! write_into_writer {
    ($write_func:expr, $buf_size:expr, $data:expr, $writer:expr) => {{
        let mut buf: [u8; $buf_size] = unsafe { std::mem::uninitialized() };
        $write_func(&mut buf, $data);
        $writer.write(&buf)?;
        Ok(())
    }};
}

impl<T> WriteArchive for WriteArchiveAdapter<T>
where
    T: Write,
{
    #[inline]
    fn write_u8(&mut self, data: u8) -> Fallible<()> {
        let buf = [data];
        self.io_writer.write(&buf)?;
        Ok(())
    }

    #[inline]
    fn write_u16(&mut self, data: u16) -> Fallible<()> {
        write_into_writer!(LittleEndian::write_u16, 2, data, self.io_writer)
    }

    #[inline]
    fn write_u32(&mut self, data: u32) -> Fallible<()> {
        write_into_writer!(LittleEndian::write_u32, 4, data, self.io_writer)
    }

    #[inline]
    fn write_u64(&mut self, data: u64) -> Fallible<()> {
        write_into_writer!(LittleEndian::write_u64, 8, data, self.io_writer)
    }

    #[inline]
    fn set_payload(&mut self, payload: UCursor) {
        self.io_payload = Some(payload);
    }

    #[inline]
    fn payload(&self) -> Option<UCursor> {
       self.io_payload.clone()
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
