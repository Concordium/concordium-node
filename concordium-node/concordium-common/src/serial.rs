use byteorder::{ReadBytesExt, WriteBytesExt, LE};
use failure::{bail, Fallible};

// the actual trait; might need 2 lifetimes

pub trait Serial
where
    Self: Sized, {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self>;
    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()>;
}

// implementation for std types

use std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr};

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
            *segment = source.read_u16::<LE>()?;
        }

        Ok(Ipv6Addr::from(segments))
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        target.write_u8(6)?;
        for segment in &self.segments() {
            target.write_u16::<LE>(*segment)?;
        }
        Ok(())
    }
}

impl Serial for IpAddr {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let ip_type = source.read_u8()?;
        match ip_type {
            4 => Ok(IpAddr::V4(Ipv4Addr::deserial(source)?)),
            6 => Ok(IpAddr::V6(Ipv6Addr::deserial(source)?)),
            e => bail!("Can't deserialize {} as an IpAddr: {}", ip_type, e),
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
            source.read_u16::<LE>()?,
        ))
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        self.ip().serial(target)?;
        Ok(target.write_u16::<LE>(self.port())?)
    }
}
