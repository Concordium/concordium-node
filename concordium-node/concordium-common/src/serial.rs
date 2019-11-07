use byteorder::{NetworkEndian, ReadBytesExt, WriteBytesExt};
use failure::{bail, Fallible};

// desired endianness
pub type Endianness = NetworkEndian;

// an empty struct used to indicate there is no serialization parameter
pub struct NoParam;

pub trait Serial
where
    Self: Sized, {
    // FIXME: use a default NoParam value when Rust#29661 is closed
    type Param;

    fn deserial<R: ReadBytesExt>(_source: &mut R) -> Fallible<Self> {
        unimplemented!();
    }
    fn deserial_with_param<R: ReadBytesExt>(
        _source: &mut R,
        _param: Self::Param,
    ) -> Fallible<Self> {
        unimplemented!();
    }
    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()>;
}

// implementation for std types

use std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr};

impl Serial for u8 {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> { Ok(source.read_u8()?) }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        Ok(target.write_u8(*self)?)
    }
}

impl Serial for u16 {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        Ok(source.read_u16::<Endianness>()?)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        Ok(target.write_u16::<Endianness>(*self)?)
    }
}

impl Serial for u32 {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        Ok(source.read_u32::<Endianness>()?)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        Ok(target.write_u32::<Endianness>(*self)?)
    }
}

impl Serial for u64 {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        Ok(source.read_u64::<Endianness>()?)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        Ok(target.write_u64::<Endianness>(*self)?)
    }
}

impl Serial for Ipv4Addr {
    type Param = NoParam;

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
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let mut octets = [0u8; 16];
        source.read_exact(&mut octets)?;
        Ok(Ipv6Addr::from(octets))
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        target.write_u8(6)?;
        Ok(target.write_all(&self.octets())?)
    }
}

impl Serial for IpAddr {
    type Param = NoParam;

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
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        Ok(SocketAddr::new(
            IpAddr::deserial(source)?,
            source.read_u16::<Endianness>()?,
        ))
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        self.ip().serial(target)?;
        Ok(target.write_u16::<Endianness>(self.port())?)
    }
}

impl<T: Serial> Serial for Vec<T> {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        const MAX_INITIAL_CAPACITY: usize = 4096;

        let len = source.read_u32::<Endianness>()?;
        let mut out = Vec::with_capacity(std::cmp::min(len as usize, MAX_INITIAL_CAPACITY));
        while let Ok(object) = T::deserial(source) {
            out.push(object);
        }
        Ok(out)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        (self.len() as u32).serial(target)?;
        for item in self {
            item.serial(target)?
        }
        Ok(())
    }
}

use std::{
    collections::HashSet,
    hash::{BuildHasher, Hash},
};

impl<T: Serial + Eq + Hash, S: BuildHasher + Default> Serial for HashSet<T, S> {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        const MAX_INITIAL_CAPACITY: usize = 4096;

        let len = source.read_u32::<Endianness>()?;
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
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let mut ret = HybridBuf::new();
        std::io::copy(source, &mut ret)?;
        ret.rewind()?;

        Ok(ret)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        let mut self_clone = self.to_owned(); // FIXME
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::{Cursor, Seek, SeekFrom};

    #[test]
    fn serial_vec_s11n() {
        let vec = vec![13462436u64, 34615462, 736853];
        let mut serialized = Cursor::new(Vec::new());
        vec.serial(&mut serialized).unwrap();
        serialized.seek(SeekFrom::Start(0)).unwrap();
        let deserialized = <Vec<u64>>::deserial(&mut serialized).unwrap();

        assert_eq!(deserialized, vec)
    }
}
