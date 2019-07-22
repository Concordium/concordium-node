use crate::common::{fails::InvalidIpType, serialization::ReadArchive};

use concordium_common::{HashBytes, UCursor, SHA256};
use failure::{err_msg, Error, Fallible};

use std::{
    cmp::Eq,
    collections::HashSet,
    hash::Hash,
    net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr},
};

/// On the other hand, `Deserializable` trait describes **What** have to be
/// deserialized in a composed data type like: any struct, Vec, etc.
///
/// see [`ReadArchive`]
///
/// [`ReadArchive`]: ../archive/trait.ReadArchive.html
pub trait Deserializable: Sized {
    fn deserialize<A>(archive: &mut A) -> Fallible<Self>
    where
        A: ReadArchive;
}

// Basic types
// ==============================================================================================

impl Deserializable for u8 {
    #[inline]
    fn deserialize<A>(archive: &mut A) -> Fallible<u8>
    where
        A: ReadArchive, {
        archive.read_u8()
    }
}

impl Deserializable for u16 {
    #[inline]
    fn deserialize<A>(archive: &mut A) -> Fallible<u16>
    where
        A: ReadArchive, {
        archive.read_u16()
    }
}

impl Deserializable for u32 {
    #[inline]
    fn deserialize<A>(archive: &mut A) -> Fallible<u32>
    where
        A: ReadArchive, {
        archive.read_u32()
    }
}

impl Deserializable for u64 {
    #[inline]
    fn deserialize<A>(archive: &mut A) -> Fallible<u64>
    where
        A: ReadArchive, {
        archive.read_u64()
    }
}

impl<T> Deserializable for Box<T>
where
    T: Deserializable,
{
    #[inline]
    fn deserialize<A>(archive: &mut A) -> Fallible<Box<T>>
    where
        A: ReadArchive, {
        Ok(Box::new(T::deserialize(archive)?))
    }
}

// Common std types
// ==============================================================================================

impl Deserializable for IpAddr {
    fn deserialize<A>(archive: &mut A) -> Fallible<Self>
    where
        A: ReadArchive, {
        let ip_type = u8::deserialize(archive)?;
        match ip_type {
            4u8 => Ok(IpAddr::from(Ipv4Addr::deserialize(archive)?)),
            6u8 => Ok(IpAddr::from(Ipv6Addr::deserialize(archive)?)),
            _ => Err(Error::from(InvalidIpType::new(ip_type.to_string()))),
        }
    }
}

impl Deserializable for Ipv4Addr {
    fn deserialize<A>(archive: &mut A) -> Fallible<Self>
    where
        A: ReadArchive, {
        // IPs are deserialized very frequently, so `mem::unitialized` is used to
        // improve performance. Safety is ensured by the early return in case
        // `read_exact` fails.
        let mut octects: [u8; 4] = unsafe { std::mem::uninitialized() };
        archive.read_exact(&mut octects)?;
        Ok(Ipv4Addr::from(octects))
    }
}

impl Deserializable for Ipv6Addr {
    fn deserialize<A>(archive: &mut A) -> Fallible<Self>
    where
        A: ReadArchive, {
        // IPs are deserialized very frequently, so `mem::unitialized` is used to
        // improve performance. Safety is ensured by the early return in case
        // `read_exact` fails.
        let mut segments: [u16; 8] = unsafe { std::mem::uninitialized() };
        for segment in &mut segments {
            *segment = u16::deserialize(archive)?;
        }

        Ok(Ipv6Addr::from(segments))
    }
}

impl Deserializable for SocketAddr {
    #[inline]
    fn deserialize<A>(archive: &mut A) -> Fallible<Self>
    where
        A: ReadArchive, {
        Ok(SocketAddr::new(
            IpAddr::deserialize(archive)?,
            u16::deserialize(archive)?,
        ))
    }
}

impl Deserializable for HashBytes {
    #[inline]
    fn deserialize<A>(archive: &mut A) -> Fallible<Self>
    where
        A: ReadArchive, {
        let vw = archive.read_n_bytes(u32::from(SHA256))?;
        Ok(HashBytes::new(vw.as_slice()))
    }
}

// Standard collections
// ==============================================================================================

impl<T, S: ::std::hash::BuildHasher + Default> Deserializable for HashSet<T, S>
where
    T: Deserializable + Eq + Hash,
{
    fn deserialize<A>(archive: &mut A) -> Fallible<Self>
    where
        A: ReadArchive, {
        // This ensures safety if an corrupted package tries to reserve more memory than
        // real-data contains.
        // Message size is limited by P2P protocol.
        const MAX_INITIAL_CAPACITY: usize = 4096;

        let len = u32::deserialize(archive)?;

        let mut out = HashSet::with_capacity_and_hasher(
            std::cmp::min(len as usize, MAX_INITIAL_CAPACITY),
            Default::default(),
        );

        for _i in 0..len {
            out.insert(T::deserialize(archive)?);
        }
        Ok(out)
    }
}

impl<T> Deserializable for Vec<T>
where
    T: Deserializable,
{
    fn deserialize<A>(archive: &mut A) -> Fallible<Self>
    where
        A: ReadArchive, {
        // This ensures safety if an corrupted package tries to reserve more memory than
        // real-data contains.
        // Message size is limited by P2P protocol.
        const MAX_INITIAL_CAPACITY: usize = 4096;

        let len = u32::deserialize(archive)?;
        let mut out = Vec::with_capacity(std::cmp::min(len as usize, MAX_INITIAL_CAPACITY));
        for _i in 0..len {
            out.push(T::deserialize(archive)?);
        }
        Ok(out)
    }
}

// Concordium-common
// ==============================================================================================

/// As `UCursor` maintains compatability with a broad range of `std::io`, we
/// simply enforce a max size of u32 only during serialization-phases.
impl Deserializable for UCursor {
    /// It returns a `Shadow-copy` of the payload.
    fn deserialize<A>(archive: &mut A) -> Fallible<UCursor>
    where
        A: ReadArchive, {
        let len = u32::deserialize(archive)?;
        archive
            .payload(u64::from(len))
            .ok_or_else(|| err_msg("No payload on this archive"))
    }
}
