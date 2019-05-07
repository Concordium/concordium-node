use crate::common::{ UCursor, ContainerView, serialization::ReadArchive, RemotePeer};

use byteorder::{ByteOrder, NetworkEndian};
use failure::Fallible;

use std::{io::Read, net::IpAddr};

pub struct ReadArchiveAdapter {
    io_reader:   UCursor,
    remote_peer: RemotePeer,
    ip:          IpAddr,
}

impl ReadArchiveAdapter
{
    pub fn new(io_reader: UCursor, remote_peer: RemotePeer, ip: IpAddr) -> Self {
        ReadArchiveAdapter {
            io_reader,
            remote_peer,
            ip,
        }
    }

    #[inline]
    pub fn into_inner(self) -> UCursor { self.io_reader }

    #[inline]
    pub fn inner(&self) -> &UCursor { &self.io_reader }
}

macro_rules! read_from_reader {
    ($read_func:expr, $buf_size:expr, $reader:expr) => {{
        let vw = $reader.read_into_view($buf_size)?;
        Ok($read_func(vw.as_slice()))
    }};
}

impl ReadArchive for ReadArchiveAdapter
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
    fn read_n_bytes(&mut self, len: u32) -> Fallible<ContainerView> {
        into_err!(self.io_reader.read_into_view( len as usize))
    }

    #[inline]
    fn payload(&mut self, len: u64) -> Option<UCursor> {
        self.io_reader.sub_range( self.io_reader.position(), len).ok()
    }
}

impl std::io::Read for ReadArchiveAdapter
{
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> { self.io_reader.read(buf) }
}
