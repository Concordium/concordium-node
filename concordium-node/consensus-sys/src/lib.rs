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
        info!("Deserializing an object: {} ({}B)", $target, $bytes.len());
    };
}

macro_rules! debug_serialization {
    ($object:expr) => {
        info!("Serializing an object: {:?}", $object);
    };
}

#[macro_use]
mod fails;
pub mod block;
pub mod common;
pub mod consensus;
pub mod finalization;
pub mod parameters;
pub mod transaction;
