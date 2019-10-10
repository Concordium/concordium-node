mod s11n {
    use capnp::serialize_packed;
    use failure::Fallible;

    use concordium_common::hybrid_buf::HybridBuf;

    use crate::{
        common::P2PNodeId,
        network::{
            NetworkId, NetworkMessage, NetworkPacket, NetworkPacketType, NetworkRequest,
            NetworkResponse,
        },
        p2p_capnp,
    };

    use std::{convert::TryFrom, io::Cursor};

    #[inline(always)]
    fn load_p2p_node_id(
        p2p_node_id: &p2p_capnp::p2_p_node_id::Reader,
    ) -> ::capnp::Result<P2PNodeId> {
        Ok(P2PNodeId(p2p_node_id.get_id()))
    }

    #[inline(always)]
    fn load_packet_type(
        packet_type: &p2p_capnp::packet_type::Reader,
    ) -> ::capnp::Result<NetworkPacketType> {
        match packet_type.which()? {
            p2p_capnp::packet_type::Which::Direct(target_id) => {
                let target_id = load_p2p_node_id(&target_id?)?;
                Ok(NetworkPacketType::DirectMessage(target_id))
            }
            p2p_capnp::packet_type::Which::Broadcast(ids_to_exclude) => {
                let ids_to_exclude = ids_to_exclude?;
                let mut ids = Vec::with_capacity(ids_to_exclude.len() as usize);
                for id in ids_to_exclude.iter() {
                    ids.push(load_p2p_node_id(&id)?);
                }
                Ok(NetworkPacketType::BroadcastedMessage(ids))
            }
        }
    }

    #[inline(always)]
    fn load_network_packet(
        packet: &p2p_capnp::network_packet::Reader,
    ) -> ::capnp::Result<NetworkMessage> {
        let packet_type = load_packet_type(&packet.get_packet_type()?)?;
        let network_id = NetworkId::from(packet.get_network_id());
        let message = HybridBuf::try_from(packet.get_message()?)?;
        let timestamp = packet.get_timestamp();

        let packet = NetworkPacket {
            packet_type,
            network_id,
            message,
        };

        Ok(NetworkMessage::NetworkPacket(packet, Some(timestamp), None))
    }

    #[inline(always)]
    fn load_network_request(
        request: &p2p_capnp::network_request::Reader,
    ) -> ::capnp::Result<NetworkRequest> {
        match request.which()? {
            p2p_capnp::network_request::Which::Ping(_) => Ok(NetworkRequest::Ping),
            _ => Err(capnp::Error::unimplemented(
                "Network request type not implemented".to_owned(),
            )),
        }
    }

    #[inline(always)]
    fn load_network_response(
        response: &p2p_capnp::network_response::Reader,
    ) -> ::capnp::Result<NetworkResponse> {
        match response.which()? {
            p2p_capnp::network_response::Which::Pong(_) => Ok(NetworkResponse::Pong),
            _ => Err(capnp::Error::unimplemented(
                "Network response type not implemented".to_owned(),
            )),
        }
    }

    pub fn load_from_slice(input: &[u8]) -> ::capnp::Result<NetworkMessage> {
        let mut buff = Cursor::new(input);
        let reader =
            serialize_packed::read_message(&mut buff, capnp::message::ReaderOptions::default())?;

        let nm = reader.get_root::<p2p_capnp::network_message::Reader>()?;

        match nm.which()? {
            p2p_capnp::network_message::Which::Packet(packet) => load_network_packet(&packet?),
            p2p_capnp::network_message::Which::Request(request_reader_res) => {
                if let Ok(request_reader) = request_reader_res {
                    let request = load_network_request(&request_reader)?;
                    let timestamp = Some(request_reader.get_timestamp());
                    let other = None;

                    Ok(NetworkMessage::NetworkRequest(request, timestamp, other))
                } else {
                    Ok(NetworkMessage::InvalidMessage)
                }
            }
            p2p_capnp::network_message::Which::Response(response_reader_res) => {
                if let Ok(response_reader) = response_reader_res {
                    let response = load_network_response(&response_reader);
                    let timestamp = Some(response_reader.get_timestamp());
                    let other = None;

                    Ok(NetworkMessage::NetworkResponse(response?, timestamp, other))
                } else {
                    Ok(NetworkMessage::InvalidMessage)
                }
            }
            _ => Ok(NetworkMessage::InvalidMessage),
        }
    }

    #[inline(always)]
    fn write_packet_type(
        builder: &mut p2p_capnp::packet_type::Builder,
        packet_type: &NetworkPacketType,
    ) {
        match packet_type {
            NetworkPacketType::DirectMessage(target_id) => {
                let mut builder = builder.reborrow().init_direct();
                builder.set_id(target_id.as_raw());
            }
            NetworkPacketType::BroadcastedMessage(ids_to_exclude) => {
                let mut builder = builder
                    .reborrow()
                    .init_broadcast(ids_to_exclude.len() as u32);
                for (i, id) in ids_to_exclude.iter().enumerate() {
                    builder.reborrow().get(i as u32).set_id(id.as_raw());
                }
            }
        }
    }

    #[inline(always)]
    fn write_network_packet(
        builder: &mut p2p_capnp::network_packet::Builder,
        packet: &mut NetworkPacket,
        timestamp: u64,
    ) -> Fallible<()> {
        let message = packet.message.remaining_bytes()?;

        write_packet_type(
            &mut builder.reborrow().init_packet_type(),
            &packet.packet_type,
        );
        builder.set_network_id(packet.network_id.id);
        builder.set_message(&message);
        builder.set_timestamp(timestamp);

        packet.message.rewind().unwrap(); // FIXME; doesn't need to be here, it's just for the test

        Ok(())
    }

    #[inline(always)]
    fn write_network_request(
        builder: &mut p2p_capnp::network_request::Builder,
        request: &NetworkRequest,
        timestamp: u64,
    ) {
        match request {
            NetworkRequest::Ping => builder.set_ping(()),
            _ => panic!("Network request is not yet supported"),
        };
        builder.set_timestamp(timestamp);
    }

    #[inline(always)]
    fn write_network_response(
        builder: &mut p2p_capnp::network_response::Builder,
        response: &NetworkResponse,
        timestamp: u64,
    ) {
        match response {
            NetworkResponse::Pong => builder.set_pong(()),
            _ => panic!("Network response is not yet supported"),
        };
        builder.set_timestamp(timestamp);
    }

    #[inline(always)]
    pub fn write_network_message(
        builder: &mut p2p_capnp::network_message::Builder,
        nm: &mut NetworkMessage,
    ) {
        match nm {
            NetworkMessage::NetworkPacket(ref mut np, ref timestamp_opt, _) => {
                let timestamp: u64 = timestamp_opt.unwrap_or(0 as u64);
                let mut packet_builder = builder.reborrow().init_packet();
                write_network_packet(&mut packet_builder, np, timestamp).unwrap(); // FIXME
            }
            NetworkMessage::NetworkRequest(ref request, ref timestamp_opt, _) => {
                let timestamp: u64 = timestamp_opt.unwrap_or(0 as u64);
                let mut request_builder = builder.reborrow().init_request();
                write_network_request(&mut request_builder, request, timestamp);
            }
            NetworkMessage::NetworkResponse(ref response, ref timestamp_opt, _) => {
                let timestamp: u64 = timestamp_opt.unwrap_or(0 as u64);
                let mut response_builder = builder.reborrow().init_response();
                write_network_response(&mut response_builder, response, timestamp);
            }
            _ => panic!("Network message is not yet supported"),
        }
    }
}

use crate::{network::NetworkMessage, p2p_capnp};

pub fn deserialize(input: &[u8]) -> NetworkMessage {
    match s11n::load_from_slice(input) {
        Ok(nm) => nm,
        Err(e) => {
            warn!("CAPNP error: {:?}", e);
            NetworkMessage::InvalidMessage
        }
    }
}

pub fn save_network_message(nm: &mut NetworkMessage) -> Vec<u8> {
    let mut message = ::capnp::message::Builder::new_default();
    {
        let mut builder = message.init_root::<p2p_capnp::network_message::Builder>();
        s11n::write_network_message(&mut builder, nm);
    }
    let mut buffer = vec![];
    assert!(capnp::serialize_packed::write_message(&mut buffer, &message).is_ok());

    buffer
}

#[cfg(test)]
mod unit_test {
    use super::{deserialize, save_network_message};
    use concordium_common::hybrid_buf::HybridBuf;
    use std::{convert::TryFrom, str::FromStr};

    use crate::{
        common::P2PNodeId,
        network::{
            NetworkId, NetworkMessage, NetworkPacket, NetworkPacketType, NetworkRequest,
            NetworkResponse,
        },
    };

    fn ut_s11n_001_data() -> Vec<(Vec<u8>, NetworkMessage)> {
        let payload = HybridBuf::try_from(b"Hello world!".to_vec()).unwrap();

        let mut messages = vec![
            NetworkMessage::NetworkRequest(NetworkRequest::Ping, Some(0 as u64), None),
            NetworkMessage::NetworkRequest(NetworkRequest::Ping, Some(11529215046068469760), None),
            NetworkMessage::NetworkResponse(NetworkResponse::Pong, Some(u64::max_value()), None),
            NetworkMessage::NetworkPacket(
                NetworkPacket {
                    packet_type: NetworkPacketType::DirectMessage(
                        P2PNodeId::from_str(&"2A").unwrap(),
                    ),
                    network_id:  NetworkId::from(111u16),
                    message:     payload,
                },
                Some(10),
                None,
            ),
        ];

        let mut messages_data: Vec<(Vec<u8>, NetworkMessage)> = Vec::with_capacity(messages.len());
        for mut message in &mut messages {
            let data: Vec<u8> = save_network_message(&mut message);
            messages_data.push((data, message.clone()));
        }

        messages_data
    }

    #[test]
    fn ut_s11n_capnp_001() {
        let test_params = ut_s11n_001_data();
        for (data, expected) in test_params {
            let output = deserialize(data.as_slice());
            assert_eq!(format!("{:?}", output), format!("{:?}", expected));
        }
    }
}
