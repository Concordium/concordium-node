use crate::common::{serialization::Deserializable, P2PPeer, RemotePeer};

use concordium_common::{ContainerView, UCursor};
use failure::{err_msg, Fallible};

use std::{fmt::Display, net::IpAddr, str};

/// It is the archive trait for serialization.
///
/// WriteArchive is one of the key concept of serialization process.
/// Its main target is to define 'How' types should be serialized. That means
/// this class have to cover 'how' to serialize basic types, because composed
/// types can be serialized by composition of simple types:
/// This archive is able to write the following types:
///  - All unsigned integer types: u8, u16, u32, u64
///  - Any type supported by `std::io::Write`, like `&[u8]`.
///  - Any type which can be converted to a `str` reference.
///
/// On the other hand, `Serializable` trait describes **What** have to be
/// serialized in a composed data type like: any struct, Vec, etc.
pub trait WriteArchive: Sized + std::io::Write {
    // Write
    fn write_u8(&mut self, data: u8) -> Fallible<()>;
    fn write_u16(&mut self, data: u16) -> Fallible<()>;
    fn write_u32(&mut self, data: u32) -> Fallible<()>;
    fn write_u64(&mut self, data: u64) -> Fallible<()>;

    fn write_str<T: AsRef<str>>(&mut self, s: T) -> Fallible<()> {
        let s_ref = s.as_ref();
        self.write_u32(s_ref.len() as u32)?;
        self.write_all(s_ref.as_bytes())?;
        Ok(())
    }
}

/// It is the archive trait for deserialization.
///
/// ReadArchive is one of the key concept of deserialization process.
/// Its main target is to define the following:
///  - 'How' types should be deserialized into memory object. Only native types
///    because complex types are defined by composition of natives.
///  - Set a deserialization 'context'. Sometimes, deserialization process
///    requires more
///  information than just load from data stream. Our case is a good example of
/// that. We need some  remote peer information (P2PPeer and IpAddr), and that
/// info is not stored in source streamed  data. That 'context' info is supplied
/// by archive itself.
///  - Any other optimization. For instance, `payload` function allow you to
///    access directly to
///  pending streamed data. It is extremely useful for messages that contains
/// huge binary payloads  that would not need to be processed.
///
/// On the other hand, `Deserializable` trait describes **What** have to be
/// deserialized in a composed data type like: any struct, Vec, etc.
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

    /// It gets a shadow-copy of pending streamed data.
    fn payload(&mut self, len: u64) -> Option<UCursor>;

    // Utilities for parsing.
    // ===========================

    /// It returns pending bytes to reach end-of-file.
    /// This function should be used to ensure if you are able to load and
    /// specific amount of bytes.
    fn pending_bytes(&self) -> u64;

    /// It checks that streamed data is deserialized into `T` object and that is
    /// equal to `tag`.
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

    /// It checks that streamed data is equal to `tag`.
    fn tag_slice(&mut self, tag: &[u8]) -> Fallible<()> {
        let vw = self.read_n_bytes(tag.len() as u32)?;
        if tag == vw.as_slice() {
            Ok(())
        } else {
            bail!("Expected tag `{:?}` but found `{:?}`", tag, vw.as_slice())
        }
    }
}
