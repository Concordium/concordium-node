use crate::network::serialization::WriteArchive;
use failure::Fallible;

pub trait Serializable<T: ?Sized = Self> {
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive;
}

// Basic types
// ==============================================================================================

impl Serializable for u8 {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        archive.write_u8(*self)
    }
}

impl Serializable for String {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        archive.write_str(self.as_str())
    }
}

impl<T> Serializable for &T
where
    T: Serializable,
{
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        (*self).serialize(archive)
    }
}

impl<T> Serializable for Box<T>
where
    T: Serializable,
{
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        (*self).serialize(archive)
    }
}

// Std common types
// ==============================================================================================

use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};

impl Serializable for Ipv4Addr {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        archive.write_u8(4u8)?;
        archive.write_slice(&self.octets())
    }
}

impl Serializable for Ipv6Addr {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        archive.write_u8(6u8)?;
        for segment in &self.segments() {
            archive.write_u16(*segment)?;
        }
        Ok(())
    }
}

impl Serializable for IpAddr {
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        match self {
            IpAddr::V4(ip4) => ip4.serialize(archive),
            IpAddr::V6(ip6) => ip6.serialize(archive),
        }
    }
}

use std::net::SocketAddr;
impl Serializable for SocketAddr {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        self.ip().serialize(archive)?;
        archive.write_u16(self.port())
    }
}

// Standar collections
// ==============================================================================================

#[inline]
fn serialize_from_iterator<I, A, T>(mut iterator: I, archive: &mut A) -> Fallible<()>
where
    I: Iterator<Item = T>,
    T: Serializable,
    A: WriteArchive, {
    while let Some(ref v) = iterator.next() {
        (*v).serialize(archive)?;
    }
    Ok(())
}

use std::collections::HashSet;
impl<T> Serializable for HashSet<T>
where
    T: Serializable,
{
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        archive.write_u32(self.len() as u32)?;
        serialize_from_iterator(self.iter(), archive)
    }
}

impl<T> Serializable for Vec<T>
where
    T: Serializable,
{
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        archive.write_u32(self.len() as u32)?;
        serialize_from_iterator(self.iter(), archive)
    }
}
