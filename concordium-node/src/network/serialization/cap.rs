use capnp;
use failure::Fallible;

use crate::{
    common::{get_current_stamp, P2PNodeId},
    network::*,
    p2p_capnp,
};

use std::io::{BufRead, BufReader, Write};

impl NetworkMessage {
    pub fn deserialize(buffer: &[u8]) -> Fallible<Self> {
        _deserialize(&mut BufReader::new(buffer), false).map_err(std::convert::Into::into)
    }

    pub fn serialize<T: Write>(&self, target: &mut T) -> Fallible<()> {
        let mut message = capnp::message::Builder::new_default();

        let mut builder = message.init_root::<p2p_capnp::network_message::Builder>();
        write_network_message(&mut builder, self)?;

        capnp::serialize::write_message(target, &message).map_err(std::convert::Into::into)
    }
}

#[inline(always)]
fn load_p2p_node_id(p2p_node_id: &p2p_capnp::p2_p_node_id::Reader) -> capnp::Result<P2PNodeId> {
    Ok(P2PNodeId(p2p_node_id.get_id()))
}

#[inline(always)]
fn load_packet_destination(
    packet_type: &p2p_capnp::packet_destination::Reader,
) -> capnp::Result<PacketDestination> {
    match packet_type.which()? {
        p2p_capnp::packet_destination::Which::Direct(target_id) => {
            let target_id = load_p2p_node_id(&target_id?)?;
            Ok(PacketDestination::Direct(target_id))
        }
        p2p_capnp::packet_destination::Which::Broadcast(()) => {
            Ok(PacketDestination::Broadcast(Vec::with_capacity(1)))
        }
    }
}

#[inline(always)]
fn load_network_packet(packet: &p2p_capnp::network_packet::Reader) -> capnp::Result<NetworkPacket> {
    let destination = load_packet_destination(&packet.get_packet_destination()?)?;
    let network_id = NetworkId::from(packet.get_network_id());
    let message = packet.get_message()?.to_vec();

    Ok(NetworkPacket {
        network_id,
        message,
        destination,
    })
}

#[inline(always)]
fn load_network_request(
    request: &p2p_capnp::network_request::Reader,
) -> capnp::Result<NetworkRequest> {
    match request.which()? {
        p2p_capnp::network_request::Which::Ping(_) => Ok(NetworkRequest::Ping),
        _ => Err(capnp::Error::unimplemented("Network request type not implemented".to_owned())),
    }
}

#[inline(always)]
fn load_network_response(
    response: &p2p_capnp::network_response::Reader,
) -> capnp::Result<NetworkResponse> {
    match response.which()? {
        p2p_capnp::network_response::Which::Pong(_) => Ok(NetworkResponse::Pong),
        _ => Err(capnp::Error::unimplemented("Network response type not implemented".to_owned())),
    }
}

fn _deserialize<T: BufRead>(input: &mut T, packed: bool) -> capnp::Result<NetworkMessage> {
    let reader = if packed {
        capnp::serialize_packed::read_message(input, capnp::message::ReaderOptions::default())?
    } else {
        capnp::serialize::read_message(input, capnp::message::ReaderOptions::default())?
    };

    let nm = reader.get_root::<p2p_capnp::network_message::Reader>()?;
    let created = nm.get_created();

    match nm.which()? {
        p2p_capnp::network_message::Which::Packet(packet) => {
            if let Ok(packet) = load_network_packet(&packet?) {
                Ok(NetworkMessage {
                    created,
                    received: Some(get_current_stamp()),
                    payload: NetworkPayload::NetworkPacket(packet),
                })
            } else {
                Err(capnp::Error::failed("invalid network packet".to_owned()))
            }
        }
        p2p_capnp::network_message::Which::Request(request_reader) => {
            if let Ok(request) = load_network_request(&request_reader?) {
                Ok(NetworkMessage {
                    created,
                    received: Some(get_current_stamp()),
                    payload: NetworkPayload::NetworkRequest(request),
                })
            } else {
                Err(capnp::Error::failed("invalid network request".to_owned()))
            }
        }
        p2p_capnp::network_message::Which::Response(response_reader) => {
            if let Ok(response) = load_network_response(&response_reader?) {
                Ok(NetworkMessage {
                    created,
                    received: Some(get_current_stamp()),
                    payload: NetworkPayload::NetworkResponse(response),
                })
            } else {
                Err(capnp::Error::failed("invalid network response".to_owned()))
            }
        }
    }
}

#[inline(always)]
fn write_packet_destination(
    builder: &mut p2p_capnp::packet_destination::Builder,
    packet_destination: &PacketDestination,
) {
    match packet_destination {
        PacketDestination::Direct(target_id) => {
            let mut builder = builder.reborrow().init_direct();
            builder.set_id(target_id.as_raw());
        }
        PacketDestination::Broadcast(..) => {
            unimplemented!();
        }
    }
}

#[inline(always)]
fn write_network_packet(
    builder: &mut p2p_capnp::network_packet::Builder,
    packet: &NetworkPacket,
) -> Fallible<()> {
    write_packet_destination(
        &mut builder.reborrow().init_packet_destination(),
        &packet.destination,
    );
    builder.set_network_id(packet.network_id.id);
    builder.set_message(&packet.message);

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
fn write_network_message(
    builder: &mut p2p_capnp::network_message::Builder,
    message: &NetworkMessage,
) -> Fallible<()> {
    builder.set_created(message.created);

    match message.payload {
        NetworkPayload::NetworkPacket(ref packet) => {
            let mut packet_builder = builder.reborrow().init_packet();
            write_network_packet(&mut packet_builder, packet)?;
        }
        NetworkPayload::NetworkRequest(ref request) => {
            let mut request_builder = builder.reborrow().init_request();
            write_network_request(&mut request_builder, request);
        }
        NetworkPayload::NetworkResponse(ref response) => {
            let mut response_builder = builder.reborrow().init_response();
            write_network_response(&mut response_builder, response);
        }
    }
    Ok(())
}

#[cfg(test)]
mod unit_test {
    use super::*;
    use std::{
        io::{Cursor, Seek, SeekFrom},
        str::FromStr,
    };

    use crate::{
        common::P2PNodeId,
        network::{NetworkId, NetworkMessage, NetworkPacket, NetworkRequest, NetworkResponse},
    };

    fn ut_s11n_001_data() -> Vec<(Cursor<Vec<u8>>, NetworkMessage)> {
        let messages = vec![
            NetworkMessage {
                created:  0 as u64,
                received: None,
                payload:  NetworkPayload::NetworkRequest(NetworkRequest::Ping),
            },
            NetworkMessage {
                created:  11529215046068469760,
                received: None,
                payload:  NetworkPayload::NetworkRequest(NetworkRequest::Ping),
            },
            NetworkMessage {
                created:  u64::max_value(),
                received: None,
                payload:  NetworkPayload::NetworkResponse(NetworkResponse::Pong),
            },
            NetworkMessage {
                created:  10,
                received: None,
                payload:  NetworkPayload::NetworkPacket(NetworkPacket {
                    destination: PacketDestination::Direct(P2PNodeId::from_str(&"2A").unwrap()),
                    network_id:  NetworkId::from(111u16),
                    message:     b"Hello world!".to_vec(),
                }),
            },
        ];

        let mut messages_data: Vec<(Cursor<Vec<u8>>, NetworkMessage)> =
            Vec::with_capacity(messages.len());
        for message in messages.into_iter() {
            let mut data = Cursor::new(Vec::new());
            message.serialize(&mut data).unwrap();
            data.seek(SeekFrom::Start(0)).unwrap();
            messages_data.push((data, message));
        }

        messages_data
    }

    #[test]
    fn ut_s11n_capnp_001() {
        let test_params = ut_s11n_001_data();
        for (data, expected) in test_params {
            let output = NetworkMessage::deserialize(&data.get_ref()).unwrap();
            assert_eq!(output.payload, expected.payload);
        }
    }

    #[test]
    fn s11n_size_capnp() {
        use crate::test_utils::create_random_packet;

        let payload_size = 1000;
        let msg = create_random_packet(payload_size);
        let mut buffer = std::io::Cursor::new(Vec::with_capacity(payload_size));

        msg.serialize(&mut buffer).unwrap();
        println!(
            "capnp (unpacked) s11n ratio: {}",
            buffer.get_ref().len() as f64 / payload_size as f64
        );
    }
}
