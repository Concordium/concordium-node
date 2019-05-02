#[cfg(feature = "s11n_nom")]
pub mod nom;

#[cfg(feature = "s11n_serde_json")]
pub mod json;

#[cfg(feature = "s11n_serde_cbor")]
pub mod cbor;

#[cfg(feature = "s11n_capnp")]
pub mod cap;

pub mod archive;
pub mod deserializable;
pub mod serializable;

#[macro_export]
macro_rules! serialize_into_memory {
    ($src:ident) => {
        (|| -> Fallible<Vec<u8>> {
            let mut archive = IOWriteArchiveAdapter::from(Vec::new());
            $src.serialize(&mut archive)?;
            Ok(archive.into_inner())
        })()
    };
    ($src:ident, $capacity:expr) => {
        (|| -> Fallible<Vec<u8>> {
            let mut archive = IOWriteArchiveAdapter::from(Vec::with_capacity($capacity));
            $src.serialize(&mut archive)?;
            Ok(archive.into_inner())
        })()
    };
}

pub use self::{
    archive::{IOReadArchiveAdapter, IOWriteArchiveAdapter, ReadArchive, WriteArchive},
    deserializable::Deserializable,
    serializable::Serializable,
};
