use std::convert::{From, TryFrom, TryInto};

use concordium_common::hybrid_buf::HybridBuf;
use nom::IResult;

use crate::{
    common::P2PNodeId,
    network::{
        NetworkId, NetworkMessage, NetworkPacket, NetworkPacketType, ProtocolMessageType,
        ProtocolPacketType, PROTOCOL_NAME,
    },
};

named!(
    s11n_network_packet_direct<&[u8], NetworkPacket>,
    do_parse!(
        receiver_id: take!(8) >>
        network_id: take!(2) >>
        content: take!(12) >>
        (
            {
                let receiver_id = P2PNodeId(u64::from_be_bytes(receiver_id.try_into().unwrap()));
                let network_id = NetworkId::from(u16::from_be_bytes(network_id.try_into().unwrap()));

                NetworkPacket {
                    packet_type: NetworkPacketType::DirectMessage(receiver_id),
                    network_id,
                    message: HybridBuf::try_from(content.to_owned()).unwrap(),
                }
            }
        )
    )
);

fn s11n_message<'a>(input: &'a [u8]) -> IResult<&'a [u8], NetworkMessage> {
    let (input, msg_type) = take!(input, 1)?;
    if let Ok(msg_type) = ProtocolMessageType::try_from(msg_type[0]) {
        match msg_type {
            ProtocolMessageType::Packet => {
                let (input, packet_type_slice) = take!(input, 1)?;
                if let Ok(packet_type) = ProtocolPacketType::try_from(packet_type_slice[0]) {
                    if packet_type == ProtocolPacketType::Direct {
                        let (input, packet_direct) = s11n_network_packet_direct(input)?;
                        Ok((
                            input,
                            NetworkMessage::NetworkPacket(packet_direct, None, None),
                        ))
                    } else {
                        unimplemented!()
                    }
                } else {
                    panic!("Invalid ProtocolPacketType: {:?}", packet_type_slice[0]);
                }
            }
            _ => unimplemented!(),
        }
    } else {
        panic!("Invalid ProtocolMessageType: {:?}", msg_type[0]);
    }
}

named!(s11n_network_message_parse<&[u8], NetworkMessage>,
    do_parse!(
        tag!(PROTOCOL_NAME) >>
        take!(1) >>
        take!(8) >>
        msg: s11n_message >>
        (msg)
    )
);

/// See `s11n_network_message_parse` documentation
pub fn s11n_network_message(input: &[u8]) -> IResult<&[u8], NetworkMessage> {
    s11n_network_message_parse(input)
}

#[cfg(test)]
mod unit_test {
    use byteorder::{WriteBytesExt, BE};
    use nom::IResult;

    use super::*;
    use crate::network::{
        NetworkId, NetworkMessage, ProtocolMessageType, ProtocolPacketType, PROTOCOL_NAME,
        PROTOCOL_VERSION,
    };

    use std::io::{Cursor, Write};

    fn ut_s11n_nom_001_data() -> Vec<(Vec<u8>, IResult<&'static [u8], NetworkMessage>)> {
        let raw_msg = b"Hello world!";
        let mut direct_message_content = HybridBuf::try_from(raw_msg.to_vec()).unwrap();
        direct_message_content.rewind().unwrap();

        vec![(
            {
                let mut raw = Cursor::new(Vec::new());
                raw.write_all(PROTOCOL_NAME.as_bytes()).unwrap();
                raw.write_u8(PROTOCOL_VERSION).unwrap();
                raw.write_u64::<BE>(0).unwrap();
                raw.write_u8(ProtocolMessageType::Packet as u8).unwrap();
                raw.write_u8(ProtocolPacketType::Direct as u8).unwrap();
                raw.write_u64::<BE>(9999).unwrap();
                raw.write_u16::<BE>(111).unwrap();
                raw.write_all(raw_msg).unwrap();
                raw.into_inner()
            },
            Ok((
                &b""[..],
                NetworkMessage::NetworkPacket(
                    NetworkPacket {
                        packet_type: NetworkPacketType::DirectMessage(P2PNodeId(9999)),
                        network_id:  NetworkId::from(111u16),
                        message:     direct_message_content,
                    },
                    None,
                    None,
                ),
            )),
        )]
    }

    #[test]
    fn ut_s11n_nom_001() {
        let data = ut_s11n_nom_001_data();
        for (i, (input, expected)) in data.into_iter().enumerate() {
            let output = s11n_network_message(&input);
            assert_eq!(
                format!("{:?}", output),
                format!("{:?}", expected),
                "invalid case {}",
                i
            );
        }
    }
}
