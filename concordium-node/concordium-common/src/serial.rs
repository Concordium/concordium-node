use byteorder::{ReadBytesExt, WriteBytesExt, BE};
use failure::{bail, Fallible};

// desired endianness
pub type E = BE;

// the actual trait; might need 2 lifetimes

pub trait Serial
where
    Self: Sized, {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self>;
    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()>;
}

// implementation for std types

use std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr};

impl Serial for u8 {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> { Ok(source.read_u8()?) }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        Ok(target.write_u8(*self)?)
    }
}

impl Serial for u16 {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> { Ok(source.read_u16::<E>()?) }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        Ok(target.write_u16::<E>(*self)?)
    }
}

impl Serial for u32 {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> { Ok(source.read_u32::<E>()?) }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        Ok(target.write_u32::<E>(*self)?)
    }
}

impl Serial for u64 {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> { Ok(source.read_u64::<E>()?) }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        Ok(target.write_u64::<E>(*self)?)
    }
}

impl Serial for Ipv4Addr {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let mut octects = [0u8; 4];
        source.read_exact(&mut octects)?;
        Ok(Ipv4Addr::from(octects))
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        target.write_u8(4)?;
        Ok(target.write_all(&self.octets())?)
    }
}

impl Serial for Ipv6Addr {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let mut segments = [0u16; 8];
        for segment in &mut segments {
            *segment = source.read_u16::<E>()?;
        }

        Ok(Ipv6Addr::from(segments))
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        target.write_u8(6)?;
        for segment in &self.segments() {
            target.write_u16::<E>(*segment)?;
        }
        Ok(())
    }
}

impl Serial for IpAddr {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        match source.read_u8()? {
            4 => Ok(IpAddr::V4(Ipv4Addr::deserial(source)?)),
            6 => Ok(IpAddr::V6(Ipv6Addr::deserial(source)?)),
            x => bail!("Can't deserialize an IpAddr (unknown type: {})", x),
        }
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        match self {
            IpAddr::V4(ip4) => ip4.serial(target),
            IpAddr::V6(ip6) => ip6.serial(target),
        }
    }
}

impl Serial for SocketAddr {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        Ok(SocketAddr::new(
            IpAddr::deserial(source)?,
            source.read_u16::<E>()?,
        ))
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        self.ip().serial(target)?;
        Ok(target.write_u16::<E>(self.port())?)
    }
}

impl<T: Serial> Serial for Vec<T> {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        const MAX_INITIAL_CAPACITY: usize = 4096;

        let len = source.read_u32::<E>()?;
        let mut out = Vec::with_capacity(std::cmp::min(len as usize, MAX_INITIAL_CAPACITY));
        for _i in 0..len {
            out.push(T::deserial(source)?);
        }
        Ok(out)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        (self.len() as u32).serial(target)?;
        self.iter()
            .map(|ref item| item.serial(target))
            .collect::<Fallible<()>>()
    }
}

use std::{
    collections::HashSet,
    hash::{BuildHasher, Hash},
};

impl<T: Serial + Eq + Hash, S: BuildHasher + Default> Serial for HashSet<T, S> {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        const MAX_INITIAL_CAPACITY: usize = 4096;

        let len = source.read_u32::<E>()?;
        let mut out = HashSet::with_capacity_and_hasher(
            std::cmp::min(len as usize, MAX_INITIAL_CAPACITY),
            Default::default(),
        );

        for _i in 0..len {
            out.insert(T::deserial(source)?);
        }
        Ok(out)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        (self.len() as u32).serial(target)?;
        self.iter()
            .map(|ref item| item.serial(target))
            .collect::<Fallible<()>>()
    }
}

use crate::hybrid_buf::HybridBuf;

impl Serial for HybridBuf {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let len = u32::deserial(source)? as usize; // advance the cursor
        let mut ret = HybridBuf::with_capacity(len)?;
        std::io::copy(source, &mut ret)?;
        ret.rewind()?;

        Ok(ret)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        let mut self_clone = self.to_owned(); // FIXME!
        let len = self_clone.len()? - self_clone.position()?;

        (len as u32).serial(target)?;
        std::io::copy(&mut self_clone, target)?;

        Ok(())
    }
}

pub fn serialize_into_buffer<T: Serial>(src: &T, capacity: usize) -> Fallible<HybridBuf> {
    let mut buffer = HybridBuf::with_capacity(capacity)?;
    src.serial(&mut buffer)?;
    buffer.rewind()?;

    Ok(buffer)
}

pub fn deserialize_from_memory<T: Serial>(mut src: &mut HybridBuf) -> Fallible<T> {
    T::deserial(&mut src)
}
