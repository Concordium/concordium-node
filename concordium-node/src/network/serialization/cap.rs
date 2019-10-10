use capnp::serialize;
use failure::Fallible;

use concordium_common::hybrid_buf::HybridBuf;

use crate::{
    common::{get_current_stamp, P2PNodeId},
    network::{
        NetworkId, NetworkMessage, NetworkMessagePayload, NetworkPacket, NetworkPacketType,
        NetworkRequest, NetworkResponse,
    },
    p2p_capnp,
};

use std::{
    convert::TryFrom,
    io::{BufReader, Read, Write},
};

#[inline(always)]
fn load_p2p_node_id(p2p_node_id: &p2p_capnp::p2_p_node_id::Reader) -> capnp::Result<P2PNodeId> {
    Ok(P2PNodeId(p2p_node_id.get_id()))
}

#[inline(always)]
fn load_packet_type(
    packet_type: &p2p_capnp::packet_type::Reader,
) -> capnp::Result<NetworkPacketType> {
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
fn load_network_packet(packet: &p2p_capnp::network_packet::Reader) -> capnp::Result<NetworkPacket> {
    let packet_type = load_packet_type(&packet.get_packet_type()?)?;
    let network_id = NetworkId::from(packet.get_network_id());
    let message = HybridBuf::try_from(packet.get_message()?)?;

    Ok(NetworkPacket {
        packet_type,
        network_id,
        message,
    })
}

#[inline(always)]
fn load_network_request(
    request: &p2p_capnp::network_request::Reader,
) -> capnp::Result<NetworkRequest> {
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
) -> capnp::Result<NetworkResponse> {
    match response.which()? {
        p2p_capnp::network_response::Which::Pong(_) => Ok(NetworkResponse::Pong),
        _ => Err(capnp::Error::unimplemented(
            "Network response type not implemented".to_owned(),
        )),
    }
}

fn load_from_reader<T: Read>(input: &mut T) -> capnp::Result<NetworkMessage> {
    let mut input = BufReader::new(input);
    let reader = serialize::read_message(&mut input, capnp::message::ReaderOptions::default())?;

    let nm = reader.get_root::<p2p_capnp::network_message::Reader>()?;
    let timestamp = nm.get_timestamp();

    match nm.which()? {
        p2p_capnp::network_message::Which::Packet(packet) => {
            if let Ok(packet) = load_network_packet(&packet?) {
                Ok(NetworkMessage {
                    timestamp1: Some(timestamp),
                    timestamp2: Some(get_current_stamp()),
                    payload:    NetworkMessagePayload::NetworkPacket(packet),
                })
            } else {
                Err(capnp::Error::failed("invalid network packet".to_owned()))
            }
        }
        p2p_capnp::network_message::Which::Request(request_reader) => {
            if let Ok(request) = load_network_request(&request_reader?) {
                Ok(NetworkMessage {
                    timestamp1: Some(timestamp),
                    timestamp2: Some(get_current_stamp()),
                    payload:    NetworkMessagePayload::NetworkRequest(request),
                })
            } else {
                Err(capnp::Error::failed("invalid network request".to_owned()))
            }
        }
        p2p_capnp::network_message::Which::Response(response_reader) => {
            if let Ok(response) = load_network_response(&response_reader?) {
                Ok(NetworkMessage {
                    timestamp1: Some(timestamp),
                    timestamp2: Some(get_current_stamp()),
                    payload:    NetworkMessagePayload::NetworkResponse(response),
                })
            } else {
                Err(capnp::Error::failed("invalid network response".to_owned()))
            }
        }
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
) -> Fallible<()> {
    let message = packet.message.remaining_bytes()?;

    write_packet_type(
        &mut builder.reborrow().init_packet_type(),
        &packet.packet_type,
    );
    builder.set_network_id(packet.network_id.id);
    builder.set_message(&message);

    packet.message.rewind().unwrap(); // FIXME; doesn't need to be here, it's just for the test

    Ok(())
}

#[inline(always)]
fn write_network_request(
    builder: &mut p2p_capnp::network_request::Builder,
    request: &NetworkRequest,
) {
    match request {
        NetworkRequest::Ping => builder.set_ping(()),
        _ => panic!("Network request is not yet supported"),
    };
}

#[inline(always)]
fn write_network_response(
    builder: &mut p2p_capnp::network_response::Builder,
    response: &NetworkResponse,
) {
    match response {
        NetworkResponse::Pong => builder.set_pong(()),
        _ => panic!("Network response is not yet supported"),
    };
}

#[inline(always)]
pub fn write_network_message(
    builder: &mut p2p_capnp::network_message::Builder,
    message: &mut NetworkMessage,
) {
    builder.set_timestamp(message.timestamp1.unwrap_or(0));

    match message.payload {
        NetworkMessagePayload::NetworkPacket(ref mut packet) => {
            let mut packet_builder = builder.reborrow().init_packet();
            write_network_packet(&mut packet_builder, packet).unwrap(); // FIXME
        }
        NetworkMessagePayload::NetworkRequest(ref request) => {
            let mut request_builder = builder.reborrow().init_request();
            write_network_request(&mut request_builder, request);
        }
        NetworkMessagePayload::NetworkResponse(ref response) => {
            let mut response_builder = builder.reborrow().init_response();
            write_network_response(&mut response_builder, response);
        }
    }
}

pub fn deserialize<T: Read>(input: &mut T) -> Fallible<NetworkMessage> {
    match load_from_reader(input) {
        Ok(msg) => Ok(msg),
        Err(e) => bail!(e),
    }
}

pub fn save_network_message<T: Write>(buffer: &mut T, nm: &mut NetworkMessage) {
    let mut message = capnp::message::Builder::new_default();
    {
        let mut builder = message.init_root::<p2p_capnp::network_message::Builder>();
        write_network_message(&mut builder, nm);
    }
    capnp::serialize::write_message(buffer, &message).unwrap(); // FIXME
}

#[cfg(test)]
mod unit_test {
    use super::*;
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
        let messages = vec![
            NetworkMessage {
                timestamp1: Some(0 as u64),
                timestamp2: None,
                payload:    NetworkMessagePayload::NetworkRequest(NetworkRequest::Ping),
            },
            NetworkMessage {
                timestamp1: Some(11529215046068469760),
                timestamp2: None,
                payload:    NetworkMessagePayload::NetworkRequest(NetworkRequest::Ping),
            },
            NetworkMessage {
                timestamp1: Some(u64::max_value()),
                timestamp2: None,
                payload:    NetworkMessagePayload::NetworkResponse(NetworkResponse::Pong),
            },
            NetworkMessage {
                timestamp1: Some(10),
                timestamp2: None,
                payload:    NetworkMessagePayload::NetworkPacket(NetworkPacket {
                    packet_type: NetworkPacketType::DirectMessage(
                        P2PNodeId::from_str(&"2A").unwrap(),
                    ),
                    network_id:  NetworkId::from(111u16),
                    message:     HybridBuf::try_from(b"Hello world!".to_vec()).unwrap(),
                }),
            },
        ];

        let mut messages_data: Vec<(Vec<u8>, NetworkMessage)> = Vec::with_capacity(messages.len());
        for mut message in messages.into_iter() {
            let mut data = Vec::new();
            save_network_message(&mut data, &mut message);
            messages_data.push((data, message));
        }

        messages_data
    }

    #[test]
    fn ut_s11n_capnp_001() {
        let test_params = ut_s11n_001_data();
        for (data, expected) in test_params {
            let output = deserialize(&mut data.as_slice()).unwrap();
            assert_eq!(
                format!("{:?}", output.payload),
                format!("{:?}", expected.payload)
            );
        }
    }
}
