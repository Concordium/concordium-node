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
    ($target:expr, $cursor:expr) => {
        //debug_assert_eq!($cursor.position(), $cursor.get_ref().len() as u64);

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

#[macro_use]
mod fails;
pub mod block;
pub mod common;
pub mod consensus;
pub mod finalization;
pub mod parameters;
pub mod transaction;
