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

use concordium_common::{hybrid_buf::HybridBuf, Serial};

use failure::Fallible;

/// Helper function to serialize `src` into memory.
/// It uses `capacity` as initial capacity for target vector.
pub fn serialize_into_memory<T>(src: &T, capacity: usize) -> Fallible<HybridBuf>
where
    T: Serial, {
    let mut buffer = HybridBuf::with_capacity(capacity)?;
    src.serial(&mut buffer)?;
    buffer.rewind()?;

    Ok(buffer)
}

/// Helper function to deserialize `src` from memory.
pub fn deserialize_from_memory<T>(mut src: &mut HybridBuf) -> Fallible<T>
where
    T: Serial, {
    T::deserial(&mut src)
}
