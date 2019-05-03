use crate::common::{ P2PPeer, RemotePeer };

use failure::{ Fallible, err_msg };

use std::{
    net::IpAddr,
    str,
};

pub trait WriteArchive: Sized {
    // Write
    fn write_u8(&mut self, data: u8) -> Fallible<()>;
    fn write_u16(&mut self, data: u16) -> Fallible<()>;
    fn write_u32(&mut self, data: u32) -> Fallible<()>;
    fn write_u64(&mut self, data: u64) -> Fallible<()>;

    fn write_slice(&mut self, data: &[u8]) -> Fallible<()>;

    fn write_str<T: AsRef<str>>(&mut self, s: T) -> Fallible<()> {
        let s_ref = s.as_ref();
        self.write_u32(s_ref.len() as u32)?;
        self.write_slice(s_ref.as_bytes())
    }
}

pub trait ReadArchive: Sized {
    fn post_handshake_peer(&self) -> Fallible<P2PPeer> {
        self.remote_peer().clone().post_handshake_peer_or_else( ||{
            err_msg( "Message requires handshake to be completed first")
        })
    }

    fn remote_peer(&self) -> &RemotePeer;
    fn ip(&self) -> IpAddr;

    // Read
    fn read_u8(&mut self) -> Fallible<u8>;
    fn read_u16(&mut self) -> Fallible<u16>;
    fn read_u32(&mut self) -> Fallible<u32>;
    fn read_u64(&mut self) -> Fallible<u64>;

    fn read_n_bytes(&mut self, len: u32) -> Fallible<Vec<u8>>;
    fn read_into_byte_slice(&mut self, output: &mut [u8]) -> Fallible<()>;

    /// #TODO
    /// Should it be read as 'str'?
    fn read_string(&mut self) -> Fallible<String> {
        let str_len = self.read_u32()?;
        let str_buf = String::from_utf8(self.read_n_bytes(str_len)?)?;
        Ok(str_buf)
    }

    // Utilitis for parsing.
    #[inline]
    fn tag_str<T: AsRef<str>>(&mut self, tag: T) -> Fallible<()> {
        let value = self.read_string()?;
        if value.as_str() == tag.as_ref() {
            Ok(())
        } else {
            bail!("Expected tag `{}` but found `{}`", tag.as_ref(), value)
        }
    }
}

