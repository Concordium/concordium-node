pub mod archive;
pub mod deserializable;
pub mod read_archive_adapter;
pub mod serializable;
pub mod write_archive_adapter;

pub use self::{
    archive::{ReadArchive, WriteArchive},
    deserializable::Deserializable,
    read_archive_adapter::ReadArchiveAdapter,
    serializable::Serializable,
    write_archive_adapter::WriteArchiveAdapter,
};

use crate::common::RemotePeer;
use concordium_common::hybrid_buf::HybridBuf;

use failure::Fallible;

/// Helper function to serialize `src` into memory.
/// It uses `capacity` as initial capacity for target vector.
pub fn serialize_into_memory<T>(src: &T, capacity: usize) -> Fallible<HybridBuf>
where
    T: Serializable, {
    let mut archive = WriteArchiveAdapter::from(HybridBuf::with_capacity(capacity)?);
    src.serialize(&mut archive)?;
    let mut ret = archive.into_inner();
    ret.rewind()?;
    Ok(ret)
}

/// Helper function to deserialize `src` from memory.
pub fn deserialize_from_memory<T>(src: HybridBuf, peer: RemotePeer) -> Fallible<T>
where
    T: Deserializable, {
    let mut archive = ReadArchiveAdapter::new(src, peer);
    T::deserialize(&mut archive)
}
