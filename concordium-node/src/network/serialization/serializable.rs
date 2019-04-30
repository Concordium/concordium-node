use failure::Fallible;
use std::{ str, fmt::Display };

pub trait Archive: Sized {
    // Write

    fn write_u8(&mut self, data: u8) -> Fallible<()>;
    fn write_u16(&mut self, data: u16) -> Fallible<()>;
    fn write_u32(&mut self, data: u32) -> Fallible<()>;
    fn write_u64(&mut self, data: u64) -> Fallible<()>;

    fn write_slice(&mut self, data: &[u8]) -> Fallible<()>;

    fn write_str<T: AsRef<str>>(&mut self, s: T) -> Fallible<()> {
        let s_ref = s.as_ref();
        self.write_u32( s_ref.len() as u32)?;
        self.write_slice( s_ref.as_bytes())
    }

    // Read
    fn read_u8(&mut self) -> Fallible<u8>;
    fn read_u16(&mut self) -> Fallible<u16>;
    fn read_u32(&mut self) -> Fallible<u32>;
    fn read_u64(&mut self) -> Fallible<u64>;

    fn read_slice(&mut self, len: u32) -> Fallible<&[u8]>;

    /// #TODO
    /// Should it be read as 'str'?
    fn read_string(&mut self) -> Fallible<String> {
        let str_len = self.read_u32()?;
        let str_buf = str::from_utf8( self.read_slice( str_len)?)?;
        Ok( str_buf.to_owned())
    }

    // Other containers

    // Utilitis for parsing.

    #[inline]
    fn tag_str<T: AsRef<str>>(&mut self, tag: T) -> Fallible<()> {
        let value = self.read_string()?;
        if value.as_str() == tag.as_ref() {
            Ok(())
        } else {
            bail!("Expected tag `{}` but found `{}`", tag.as_ref(), value)
        }
    }

    fn tag_value<T>(&mut self, tag: T) -> Fallible<()>  where T: Deserializable + Display + PartialEq {
        let value = T::deserialize( self)?;
        if value == tag {
            Ok(())
        } else {
            bail!("Expected tag value `{}` but found `{}`", tag, value)
        }
    }
}

pub trait Serializable<T: ?Sized = Self>  {
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()>
        where A: Archive;
}

// Basic types
// ==============================================================================================

impl Serializable for u8 {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()> where A: Archive {
        archive.write_u8( *self)
    }
}

impl Serializable for String {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()> where A: Archive {
        archive.write_str( self.as_str())
    }
}

impl<T> Serializable for &T where T: Serializable {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()> where A: Archive {
        (*self).serialize( archive)
    }
}

impl<T> Serializable for Box<T> where T: Serializable {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()> where A: Archive {
        (*self).serialize( archive)
    }
}

// Std common types
// ==============================================================================================

use std::net::{ IpAddr, Ipv4Addr, Ipv6Addr};

impl Serializable for Ipv4Addr {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()> where A: Archive {
        archive.write_slice( &self.octets())
    }
}

impl Serializable for Ipv6Addr {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()> where A: Archive {
        for segment in &self.segments() {
            archive.write_u16( *segment)?;
        }
        Ok(())
    }
}

impl Serializable for IpAddr {
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()> where A: Archive {
        match self {
            IpAddr::V4(ip4) => ip4.serialize( archive),
            IpAddr::V6(ip6) => ip6.serialize( archive)
        }
    }
}

use std::net::{ SocketAddr };
impl Serializable for SocketAddr {
    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()> where A: Archive {
        self.ip().serialize( archive)?;
        archive.write_u16( self.port())
    }
}

// Standar collections
// ==============================================================================================

#[inline]
fn serialize_from_iterator<I,A,T>( mut iterator: I, archive: &mut A) -> Fallible<()>
    where I: Iterator<Item=T>, T: Serializable, A: Archive,
{
    while let Some(ref v) = iterator.next() {
        (*v).serialize( archive)?;
    }
    Ok(())
}

use std::collections::HashSet;
impl<T> Serializable for HashSet<T> where T: Serializable {

    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()> where A: Archive {
        archive.write_u32( self.len() as u32)?;
        serialize_from_iterator( self.iter(), archive)
    }
}

impl<T> Serializable for Vec<T> where T: Serializable {

    #[inline]
    fn serialize<A>(&self, archive: &mut A) -> Fallible<()> where A: Archive {
        archive.write_u32( self.len() as u32)?;
        serialize_from_iterator( self.iter(), archive)
    }
}



/// # TODO
/// Move to other file.
pub trait Deserializable : Sized {
    fn deserialize<A>(archive: &mut A) -> Fallible<Self>
        where A: Archive;
}
