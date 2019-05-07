#![allow(dead_code)]

#[cfg(unix)]
extern crate curryrsunix as curryrs;
#[cfg(windows)]
extern crate curryrswin as curryrs;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;
#[macro_use]
extern crate concordium_common;

macro_rules! check_serialization {
    ($target:expr, $source:expr) => {
        debug_assert_eq!(
            $target.serialize().as_slice(),
            $source,
            "Invalid serialization of {:?}",
            $target
        );
    };
}

macro_rules! debug_deserialization {
    ($target:expr, $bytes:expr) => {
        debug!("Deserializing an object: {} ({}B)", $target, $bytes.len());
    };
}

macro_rules! debug_serialization {
    ($object:expr) => {
        debug!("Serializing an object: {:?}", $object);
    };
}

macro_rules! read_const_sized {
    ($source:expr, $size:expr) => {{
        let mut buf = [0u8; $size];
        $source.read_exact(&mut buf)?;

        buf
    }};
}

macro_rules! read_sized {
    ($source:expr, $size:expr) => {{
        let mut buf = vec![0u8; $size];
        $source.read_exact(&mut buf)?;

        buf
    }};
}

macro_rules! read_all {
    ($source:expr) => {{
        let size = $source.get_ref().len() - $source.position() as usize;
        let mut buf = vec![0u8; size];
        $source.read_exact(&mut buf)?;

        buf
    }};
}

#[macro_use]
mod fails;
pub mod block;
pub mod common;
pub mod consensus;
pub mod finalization;
pub mod parameters;
pub mod transaction;
