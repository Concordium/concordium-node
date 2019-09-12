use crate::common::{P2PPeer, RemotePeer};

use concordium_common::hybrid_buf::HybridBuf;
use failure::{format_err, Fallible};

use std::str;

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
        self.remote_peer()
            .clone()
            .peer()
            .ok_or_else(|| format_err!("Message requires handshake to be completed first"))
    }

    fn remote_peer(&self) -> &RemotePeer;

    // Read
    fn read_u8(&mut self) -> Fallible<u8>;
    fn read_u16(&mut self) -> Fallible<u16>;
    fn read_u32(&mut self) -> Fallible<u32>;
    fn read_u64(&mut self) -> Fallible<u64>;

    fn read_n_bytes(&mut self, len: u32) -> Fallible<Box<[u8]>>;

    fn payload(&mut self) -> HybridBuf;

    // Utilities for parsing.
    // ===========================

    /// It returns the number of bytes left to reach end-of-file.
    /// This function should be used to ensure if you are able to load
    /// a specific amount of bytes.
    fn remaining_bytes_count(&mut self) -> std::io::Result<u64>;
}
