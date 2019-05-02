use crate::network::serialization::ReadArchive;

use failure::Fallible;

use std::{
    cmp::Eq,
    collections::HashSet,
    hash::Hash,
    net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr},
};

/// # TODO
/// Move to other file.
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

// Std common types
// ==============================================================================================

impl Deserializable for IpAddr {
    fn deserialize<A>(archive: &mut A) -> Fallible<Self>
    where
        A: ReadArchive, {
        let ip = match archive.read_u8()? {
            4u8 => IpAddr::from(Ipv4Addr::deserialize(archive)?),
            6u8 => IpAddr::from(Ipv6Addr::deserialize(archive)?),
            _ => bail!("Unsupported version of `IpAddr`"),
        };

        Ok(ip)
    }
}

impl Deserializable for Ipv4Addr {
    fn deserialize<A>(archive: &mut A) -> Fallible<Self>
    where
        A: ReadArchive, {
        let mut octects: [u8; 4] = unsafe { std::mem::uninitialized() };
        archive.read_into_byte_slice(&mut octects)?;
        Ok(Ipv4Addr::from(octects))
    }
}

impl Deserializable for Ipv6Addr {
    fn deserialize<A>(archive: &mut A) -> Fallible<Self>
    where
        A: ReadArchive, {
        let mut segments: [u16; 8] = unsafe { std::mem::uninitialized() };
        for segment in &mut segments {
            *segment = archive.read_u16()?;
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
            archive.read_u16()?,
        ))
    }
}

// Standar collections
// ==============================================================================================

impl<T> Deserializable for HashSet<T>
where
    T: Deserializable + Eq + Hash,
{
    #[inline]
    fn deserialize<A>(archive: &mut A) -> Fallible<Self>
    where
        A: ReadArchive, {
        let len = archive.read_u32()?;
        let mut out = HashSet::with_capacity(len as usize);
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
    #[inline]
    fn deserialize<A>(archive: &mut A) -> Fallible<Self>
    where
        A: ReadArchive, {
        let len = archive.read_u32()?;
        let mut out = Vec::with_capacity(len as usize);
        for _i in 0..len {
            out.push(T::deserialize(archive)?);
        }
        Ok(out)
    }
}
