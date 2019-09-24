use crate::common::{serialization::ReadArchive, RemotePeer};

use byteorder::{LittleEndian, ReadBytesExt};
use concordium_common::{hybrid_buf::HybridBuf, into_err};
use failure::Fallible;

use std::{io::Read, mem};

pub struct ReadArchiveAdapter {
    io_reader:   HybridBuf,
    remote_peer: RemotePeer,
}

impl ReadArchiveAdapter {
    pub fn new(io_reader: HybridBuf, remote_peer: RemotePeer) -> Self {
        ReadArchiveAdapter {
            io_reader,
            remote_peer,
        }
    }
}

impl ReadArchive for ReadArchiveAdapter {
    #[inline]
    fn remote_peer(&self) -> &RemotePeer { &self.remote_peer }

    #[inline]
    fn read_u8(&mut self) -> Fallible<u8> { into_err!(self.io_reader.read_u8()) }

    #[inline]
    fn read_u16(&mut self) -> Fallible<u16> { into_err!(self.io_reader.read_u16::<LittleEndian>()) }

    #[inline]
    fn read_u32(&mut self) -> Fallible<u32> { into_err!(self.io_reader.read_u32::<LittleEndian>()) }

    #[inline]
    fn read_u64(&mut self) -> Fallible<u64> { into_err!(self.io_reader.read_u64::<LittleEndian>()) }

    #[inline]
    fn read_n_bytes(&mut self, len: u32) -> Fallible<Box<[u8]>> {
        ensure!(
            u64::from(len) <= into_err!(self.inner().remaining_len())?,
            "Insufficent bytes in this archive"
        );
        let len = len as usize;
        let mut target = vec![0u8; len];
        self.read_exact(&mut target)?;
        Ok(target.into_boxed_slice())
    }

    #[inline]
    fn payload(&mut self) -> HybridBuf { mem::replace(&mut self.io_reader, Default::default()) }

    fn inner(&mut self) -> &mut HybridBuf { &mut self.io_reader }
}

impl std::io::Read for ReadArchiveAdapter {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> { self.io_reader.read(buf) }
}
