use crate::common::{serialization::Deserializable, P2PPeer, RemotePeer};

use failure::{err_msg, Fallible};

use std::{fmt::Display, net::IpAddr, str};

pub trait WriteArchive: Sized + std::io::Write {
    // Write
    fn write_u8(&mut self, data: u8) -> Fallible<()>;
    fn write_u16(&mut self, data: u16) -> Fallible<()>;
    fn write_u32(&mut self, data: u32) -> Fallible<()>;
    fn write_u64(&mut self, data: u64) -> Fallible<()>;

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

    fn read_n_bytes(&mut self, len: u32) -> Fallible<Vec<u8>>;

    /// #TODO
    /// Should it be read as 'str'?
    fn read_string(&mut self) -> Fallible<String> {
        let len = self.read_u32()?;
        let mut buf = vec![0u8; len as usize];
        self.read(buf.as_mut_slice())?;

        Ok(String::from_utf8(buf)?)
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
        let mut buf = vec![0u8; tag.len()];
        self.read(buf.as_mut_slice())?;
        if tag == buf.as_slice() {
            Ok(())
        } else {
            bail!("Expected tag `{:?}` but found `{:?}`", tag, buf.as_slice())
        }
    }
}
