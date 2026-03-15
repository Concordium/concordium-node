use concordium_base::common::Serialize;

pub mod hashed_buffered_reference;

/// Reference to a storage location in the backing store where a value is located.
#[derive(Default, Debug, Clone, Copy, Eq, PartialEq, Serialize)]
#[repr(transparent)]
pub struct BlobReference(pub u64);
