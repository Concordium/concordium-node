use failure::Fallible;
#[cfg(feature = "s11n_serde_msgpack")]
use rmp_serde::{encode::write as to_writer, from_slice};
#[cfg(feature = "s11n_serde_cbor")]
use serde_cbor::{from_slice, to_writer};

use crate::network::NetworkMessage;

use std::io::{Seek, SeekFrom, Write};

impl NetworkMessage {
    pub fn deserialize(input: &[u8]) -> Fallible<Self> {
        from_slice::<NetworkMessage>(input).map_err(|e| e.into())
    }

    pub fn serialize<T: Write + Seek>(&mut self, target: &mut T) -> Fallible<()> {
        to_writer(target, self)?;
        target.seek(SeekFrom::Start(0))?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn s11n_size_serde() {
        use crate::test_utils::create_random_packet;

        let payload_size = 1000;
        let mut msg = create_random_packet(payload_size);
        let mut buffer = std::io::Cursor::new(Vec::with_capacity(payload_size));

        msg.serialize(&mut buffer).unwrap();
        println!(
            "serde {} s11n ratio: {}",
            if cfg!(feature = "s11n_serde_msgpack") {
                "msgpack"
            } else {
                "CBOR"
            },
            buffer.get_ref().len() as f64 / payload_size as f64
        );
    }
}
