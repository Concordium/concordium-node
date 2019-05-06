use crate::common::{
    serialization::{ReadArchive},
    RemotePeer,
};

use byteorder::{ByteOrder, NetworkEndian};
use failure::Fallible;

use std::{
    io::{Read, },
    net::IpAddr,
};

pub struct ReadArchiveAdapter<T>
where
    T: Read, {
    io_reader:   T,
    remote_peer: RemotePeer,
    ip:          IpAddr,
}

impl<T> ReadArchiveAdapter<T>
where
    T: Read,
{
    pub fn new(io_reader: T, remote_peer: RemotePeer, ip: IpAddr) -> Self {
        ReadArchiveAdapter {
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

impl<T> ReadArchive for ReadArchiveAdapter<T>
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
}

impl<T> std::io::Read for ReadArchiveAdapter<T>
where
    T: Read,
{
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> { self.io_reader.read(buf) }
}
