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
use concordium_common::UCursor;

use failure::Fallible;

/// Helper function to serialize `src` into memory.
/// It uses `capacity` as initial capacity for target vector.
pub fn serialize_into_memory<T>(src: &T, capacity: usize) -> Fallible<Vec<u8>>
where
    T: Serializable, {
    let mut archive = WriteArchiveAdapter::from(Vec::with_capacity(capacity));
    src.serialize(&mut archive)?;
    Ok(archive.into_inner())
}

/// Helper function to deserialize `src` from memory.
pub fn deserialize_from_memory<T>(src: Vec<u8>, peer: RemotePeer) -> Fallible<T>
where
    T: Deserializable, {
    let cursor = UCursor::from(src);
    let mut archive = ReadArchiveAdapter::new(cursor, peer);
    T::deserialize(&mut archive)
}
