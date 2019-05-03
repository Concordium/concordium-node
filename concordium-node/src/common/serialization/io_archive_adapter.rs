use crate::common::{
    serialization::{ReadArchive, WriteArchive},
    RemotePeer,
};

use byteorder::{ByteOrder, NetworkEndian};
use failure::Fallible;

use std::{
    io::{Read, Write},
    net::IpAddr,
};

pub struct IOWriteArchiveAdapter<T>
where
    T: Write, {
    io_writer: T,
}

impl<T> IOWriteArchiveAdapter<T>
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

impl<T> std::convert::From<T> for IOWriteArchiveAdapter<T>
where
    T: Write,
{
    fn from(io: T) -> Self { IOWriteArchiveAdapter { io_writer: io } }
}

macro_rules! write_into_writer {
    ($write_func:expr, $buf_size:expr, $data:expr, $writer:expr) => {{
        let mut buf: [u8; $buf_size] = unsafe { std::mem::uninitialized() };
        $write_func(&mut buf, $data);
        $writer.write(&buf)?;
        Ok(())
    }};
}

impl<T> WriteArchive for IOWriteArchiveAdapter<T>
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
        write_into_writer!(NetworkEndian::write_u16, 2, data, self.io_writer)
    }

    #[inline]
    fn write_u32(&mut self, data: u32) -> Fallible<()> {
        write_into_writer!(NetworkEndian::write_u32, 4, data, self.io_writer)
    }

    #[inline]
    fn write_u64(&mut self, data: u64) -> Fallible<()> {
        write_into_writer!(NetworkEndian::write_u64, 8, data, self.io_writer)
    }

    #[inline]
    fn write_slice(&mut self, data: &[u8]) -> Fallible<()> {
        self.io_writer.write(data)?;
        Ok(())
    }
}

impl<T> std::io::Write for IOWriteArchiveAdapter<T>
where
    T: Write,
{
    #[inline]
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> { self.io_writer.write(buf) }

    #[inline]
    fn flush(&mut self) -> std::io::Result<()> { self.io_writer.flush() }
}

pub struct IOReadArchiveAdapter<T>
where
    T: Read, {
    io_reader:   T,
    remote_peer: RemotePeer,
    ip:          IpAddr,
}

impl<T> IOReadArchiveAdapter<T>
where
    T: Read,
{
    pub fn new(io_reader: T, remote_peer: RemotePeer, ip: IpAddr) -> Self {
        IOReadArchiveAdapter {
            io_reader,
            remote_peer,
            ip,
        }
    }

    #[inline]
    pub fn into_inner(self) -> T { self.io_reader }

    #[inline]
    pub fn inner(&self) -> &T { &self.io_reader }
}

macro_rules! read_from_reader {
    ($read_func:expr, $buf_size:expr, $reader:expr) => {{
        let mut buf: [u8; $buf_size] = unsafe { std::mem::uninitialized() };
        $reader.read(&mut buf)?;
        Ok($read_func(&buf))
    }};
}

impl<T> ReadArchive for IOReadArchiveAdapter<T>
where
    T: Read,
{
    #[inline]
    fn remote_peer(&self) -> &RemotePeer { &self.remote_peer }

    #[inline]
    fn ip(&self) -> IpAddr { self.ip }

    #[inline]
    fn read_u8(&mut self) -> Fallible<u8> {
        let mut buf: [u8; 1] = unsafe { std::mem::uninitialized() };
        self.io_reader.read(&mut buf)?;
        Ok(buf[0])
    }

    #[inline]
    fn read_u16(&mut self) -> Fallible<u16> {
        read_from_reader!(NetworkEndian::read_u16, 2, self.io_reader)
    }

    #[inline]
    fn read_u32(&mut self) -> Fallible<u32> {
        read_from_reader!(NetworkEndian::read_u32, 4, self.io_reader)
    }

    #[inline]
    fn read_u64(&mut self) -> Fallible<u64> {
        read_from_reader!(NetworkEndian::read_u64, 8, self.io_reader)
    }

    #[inline]
    fn read_n_bytes(&mut self, len: u32) -> Fallible<Vec<u8>> {
        let mut buf = Vec::with_capacity(len as usize);
        unsafe { buf.set_len(len as usize) };
        self.io_reader.read_exact(buf.as_mut_slice())?;
        Ok(buf)
    }

    #[inline]
    fn read_into_byte_slice(&mut self, output: &mut [u8]) -> Fallible<()> {
        into_err!(self.io_reader.read_exact(output))
    }
}

impl<T> std::io::Read for IOReadArchiveAdapter<T>
where
    T: Read,
{
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> { self.io_reader.read(buf) }
}
