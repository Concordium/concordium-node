use crate::common::{ serialization::ReadArchive, RemotePeer};

use concordium_common::{ UCursor, ContainerView};
use byteorder::{LittleEndian, ReadBytesExt};
use failure::Fallible;

use std::{net::IpAddr};

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

impl ReadArchive for ReadArchiveAdapter
{
    #[inline]
    fn remote_peer(&self) -> &RemotePeer { &self.remote_peer }

    #[inline]
    fn ip(&self) -> IpAddr { self.ip }

    #[inline]
    fn read_u8(&mut self) -> Fallible<u8> {
        into_err!(self.io_reader.read_u8())
    }

    #[inline]
    fn read_u16(&mut self) -> Fallible<u16> {
        into_err!(self.io_reader.read_u16::<LittleEndian>())
    }

    #[inline]
    fn read_u32(&mut self) -> Fallible<u32> {
        into_err!(self.io_reader.read_u32::<LittleEndian>())
    }

    #[inline]
    fn read_u64(&mut self) -> Fallible<u64> {
        into_err!(self.io_reader.read_u64::<LittleEndian>())
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
