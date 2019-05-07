use crate::common::{
    serialization::Deserializable,
    P2PPeer, RemotePeer
};

use concordium_common::{ UCursor, ContainerView };
use failure::{err_msg, Fallible};

use std::{fmt::Display, net::IpAddr, str};

pub trait WriteArchive: Sized + std::io::Write {
    // Write
    fn write_u8(&mut self, data: u8) -> Fallible<()>;
    fn write_u16(&mut self, data: u16) -> Fallible<()>;
    fn write_u32(&mut self, data: u32) -> Fallible<()>;
    fn write_u64(&mut self, data: u64) -> Fallible<()>;

    fn set_payload(&mut self, payload: UCursor);
    fn payload(&self) -> Option<UCursor>;

    fn write_str<T: AsRef<str>>(&mut self, s: T) -> Fallible<()> {
        let s_ref = s.as_ref();
        self.write_u32(s_ref.len() as u32)?;
        self.write(s_ref.as_bytes())?;
        Ok(())
    }
}

pub trait ReadArchive: Sized + std::io::Read {
    fn post_handshake_peer(&self) -> Fallible<P2PPeer> {
        self.remote_peer().clone().post_handshake_peer_or_else(|| {
            err_msg("Message requires handshake to be completed first")
        })
    }

    fn remote_peer(&self) -> &RemotePeer;
    fn ip(&self) -> IpAddr;

    // Read
    fn read_u8(&mut self) -> Fallible<u8>;
    fn read_u16(&mut self) -> Fallible<u16>;
    fn read_u32(&mut self) -> Fallible<u32>;
    fn read_u64(&mut self) -> Fallible<u64>;

    fn read_n_bytes(&mut self, len: u32) -> Fallible<ContainerView>;

    fn payload(&mut self, len: u64) -> Option<UCursor>;

    /// #TODO
    /// Should it be read as 'str'?
    fn read_string(&mut self) -> Fallible<String> {
        let len = self.read_u32()?;
        let vw = self.read_n_bytes(len)?;
        Ok(str::from_utf8(vw.as_slice())?.to_owned())
    }

    // Utilitis for parsing.
    fn tag<T>(&mut self, tag: T) -> Fallible<()>
    where
        T: Deserializable + PartialEq + Display, {
        let other: T = T::deserialize(self)?;
        if tag == other {
            Ok(())
        } else {
            bail!("Expected tag `{}` but found `{}`", tag, other)
        }
    }

    fn tag_slice(&mut self, tag: &[u8]) -> Fallible<()> {
        let vw = self.read_n_bytes( tag.len() as u32)?;
        if tag == vw.as_slice() {
            Ok(())
        } else {
            bail!("Expected tag `{:?}` but found `{:?}`", tag, vw.as_slice())
        }
    }
}
