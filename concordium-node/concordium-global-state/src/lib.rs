#[macro_use]
extern crate log;

// pub use concordium_consensus::consensus as consensus;

// (de)serialization macros

macro_rules! check_serialization {
    ($target:expr, $cursor:expr) => {
        debug_assert_eq!($cursor.position(), $cursor.get_ref().len() as u64);

        debug_assert_eq!(
            &&*$target.serialize(),
            $cursor.get_ref(),
            "Invalid serialization of {:?}",
            $target
        );
    };
}

macro_rules! debug_deserialization {
    ($target:expr, $bytes:expr) => {
        info!("Deserializing an object: {} ({}B)", $target, $bytes.len());
    };
}

macro_rules! debug_serialization {
    ($object:expr) => {
        info!("Serializing an object: {:?}", $object);
    };
}

macro_rules! read_const_sized {
    ($source:expr, $size:expr) => {{
        let mut buf = [0u8; $size as usize];
        $source.read_exact(&mut buf)?;

        buf
    }};
}

macro_rules! read_sized {
    ($source:expr, $size:expr) => {{
        let mut buf = vec![0u8; $size as usize];
        $source.read_exact(&mut buf)?;

        buf
    }};
}

pub mod block;
pub mod common;
pub mod finalization;
pub mod parameters;
pub mod transaction;
pub mod tree;
