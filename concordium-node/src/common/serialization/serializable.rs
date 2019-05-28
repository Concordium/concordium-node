use crate::common::serialization::WriteArchive;

use concordium_common::{HashBytes, UCursor};
use failure::Fallible;

use std::{
    collections::HashSet,
    net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr},
    ops::Deref,
};

/// `Serializable` trait describes **What** have to be
/// serialized in a composed data type like: any struct, Vec, etc.
///
/// see [`WriteArchive`]
///
/// [`WriteArchive`]: ../archive/trait.WriteArchive.html
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

impl Serializable for u16 {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        archive.write_u16(*self)
    }
}

impl Serializable for u32 {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        archive.write_u32(*self)
    }
}

impl Serializable for u64 {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        archive.write_u64(*self)
    }
}

impl Serializable for HashBytes {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        into_err!(archive.write(&self).map(|_| ()))
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
        self.deref().serialize(archive)
    }
}

// Common std types
// ==============================================================================================

impl Serializable for Ipv4Addr {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        4u8.serialize(archive)?;
        archive.write_all(&self.octets())?;
        Ok(())
    }
}

impl Serializable for Ipv6Addr {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        6u8.serialize(archive)?;
        for segment in &self.segments() {
            (*segment).serialize(archive)?;
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

impl Serializable for SocketAddr {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        self.ip().serialize(archive)?;
        self.port().serialize(archive)
    }
}

// Standard collections
// ==============================================================================================

impl<T, S: ::std::hash::BuildHasher> Serializable for HashSet<T, S>
where
    T: Serializable,
{
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        (self.len() as u32).serialize(archive)?;
        self.iter()
            .map(|ref item| item.serialize(archive))
            .collect::<Fallible<()>>()
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
        (self.len() as u32).serialize(archive)?;
        self.iter()
            .map(|ref item| item.serialize(archive))
            .collect::<Fallible<()>>()
    }
}

// Concordium-common
// ==============================================================================================

impl Serializable for UCursor {
    /// It makes a `deep-copy` of the `UCursor` into `Archive`.
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
    where
        A: WriteArchive, {
        let mut self_from = self.sub(self.position())?;
        let self_from_len = self_from.len();

        self_from_len.serialize(archive)?;
        std::io::copy(&mut self_from, archive)?;

        Ok(())
    }
}
