use std::{
    convert::{From, TryFrom},
    net::{IpAddr, Ipv4Addr, SocketAddr},
    str::{self, FromStr},
};

use concordium_common::UCursor;
use nom::{verbose_errors::Context, IResult};

use crate::{
    common::{get_current_stamp, P2PNodeId, P2PPeer, P2PPeerBuilder, PeerType},
    network::{
        NetworkId, NetworkMessage, NetworkPacket, NetworkPacketBuilder, NetworkRequest,
        NetworkResponse, ProtocolMessageType, ProtocolPacketType, ProtocolRequestType,
        ProtocolResponseType, PROTOCOL_NAME,
    },
};

pub const PROTOCOL_VERSION_STR: &str = "001";
pub const PROTOCOL_MESSAGE_ID_LENGTH: usize = 64;
pub const PROTOCOL_NETWORK_CONTENT_SIZE_LENGTH: usize = 10;
pub const PROTOCOL_NETWORK_ID_LENGTH: usize = 5;
pub const PROTOCOL_NODE_ID_LENGTH: usize = 16;
pub const PROTOCOL_SENT_TIMESTAMP_LENGTH: usize = 12;
pub const PROTOCOL_MESSAGE_TYPE_LENGTH: usize = 2;

fn localhost_peer() -> P2PPeer {
    P2PPeerBuilder::default()
        .peer_type(PeerType::Node)
        .addr(SocketAddr::new(
            IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)),
            8888,
        ))
        .id(P2PNodeId(9999))
        .build()
        .unwrap()
}

fn s11n_to_timestamp(input: &[u8]) -> Result<u64, std::num::ParseIntError> {
    let input_utf8 = String::from_utf8(input.to_vec()).unwrap();
    let decoded_b64 = base64::decode(&input_utf8).unwrap();
    let mut bytes = [0u8; 8];
    bytes.clone_from_slice(&decoded_b64[..8]);
    Ok(u64::from_le_bytes(bytes))
}

fn s11n_msg_id(input: &[u8]) -> IResult<&[u8], &str> {
    let (ref input, ref msg_id_slice) = take!(input, PROTOCOL_MESSAGE_ID_LENGTH)?;
    let msg_id_str: &str = str::from_utf8(&msg_id_slice).unwrap_or("");

    Ok((input, msg_id_str))
}

fn s11n_network_id(input: &[u8]) -> IResult<&[u8], u16> {
    let (ref input, ref net_id_slice) = take!(input, PROTOCOL_NETWORK_ID_LENGTH)?;
    let net_id_str: &str = str::from_utf8(&net_id_slice).unwrap_or("0");
    let net_id: u16 = net_id_str.parse::<u16>().unwrap_or(0 as u16);

    Ok((input, net_id))
}

fn s11n_content_size(input: &[u8]) -> IResult<&[u8], usize> {
    let (ref input, ref cs_slice) = take!(input, PROTOCOL_NETWORK_CONTENT_SIZE_LENGTH)?;
    let cs_str: &str = str::from_utf8(&cs_slice).unwrap_or("0");
    let cs = cs_str.parse::<usize>().unwrap_or(0 as usize);

    Ok((input, cs))
}

named!(
    s11n_p2p_node_id<&[u8], P2PNodeId>,
    do_parse!(
        sid_slice: take!(PROTOCOL_NODE_ID_LENGTH) >>
        (
            str::from_utf8(&sid_slice)
                .map( |id| P2PNodeId::from_str( id)
                      .unwrap_or_else( |_| P2PNodeId::default()))
                .unwrap()

        )
    )
);

named!(
    s11n_network_packet_direct<&[u8], NetworkPacket>,
    do_parse!(
        receiver_id: s11n_p2p_node_id >>
        msg_id: s11n_msg_id >>
        network_id: s11n_network_id >>
        content_size: s11n_content_size >>
        content: take!(content_size) >>
        (
            NetworkPacketBuilder::default()
                .peer( localhost_peer())
                .message_id( msg_id.to_string())
                .network_id( NetworkId::from(network_id))
                .message(UCursor::from( content.to_vec()))
                .build_direct( receiver_id )
                .unwrap()
        )
    )
);

fn s11n_message_id(input: &[u8], timestamp: u64) -> IResult<&[u8], NetworkMessage> {
    let (ref input, ref msg_id_slice) = take!(input, PROTOCOL_MESSAGE_TYPE_LENGTH)?;
    let msg_id_str = str::from_utf8(&msg_id_slice).unwrap_or("");
    if let Ok(msg_id) = ProtocolMessageType::try_from(msg_id_str) {
        match msg_id {
            ProtocolMessageType::Request => {
                let (ref input, ref request_type_slice) =
                    take!(*input, PROTOCOL_MESSAGE_TYPE_LENGTH)?;
                let request_type_str = str::from_utf8(&request_type_slice).unwrap_or("");
                if let Ok(request_type) = ProtocolRequestType::try_from(request_type_str) {
                    if request_type == ProtocolRequestType::Ping {
                        return Ok((
                            &input,
                            NetworkMessage::NetworkRequest(
                                NetworkRequest::Ping(localhost_peer()),
                                Some(timestamp),
                                Some(get_current_stamp()),
                            ),
                        ));
                    }
                }
            }
            ProtocolMessageType::Response => {
                let (ref input, ref response_type_slice) =
                    take!(*input, PROTOCOL_MESSAGE_TYPE_LENGTH)?;
                let response_type_str = str::from_utf8(&response_type_slice).unwrap_or("");
                if let Ok(request_type) = ProtocolResponseType::try_from(response_type_str) {
                    if request_type == ProtocolResponseType::Pong {
                        return Ok((
                            &input,
                            NetworkMessage::NetworkResponse(
                                NetworkResponse::Pong(localhost_peer()),
                                Some(timestamp),
                                Some(get_current_stamp()),
                            ),
                        ));
                    }
                }
            }
            ProtocolMessageType::Packet => {
                let (ref input, ref packet_type_slice) =
                    take!(*input, PROTOCOL_MESSAGE_TYPE_LENGTH)?;
                let packet_type_str = str::from_utf8(&packet_type_slice).unwrap_or("");
                if let Ok(packet_type) = ProtocolPacketType::try_from(packet_type_str) {
                    if packet_type == ProtocolPacketType::Direct {
                        return s11n_network_packet_direct(input).map(|result_packet_direct| {
                            let (input, packet_direct) = result_packet_direct;
                            (
                                input,
                                NetworkMessage::NetworkPacket(
                                    packet_direct,
                                    Some(timestamp),
                                    Some(get_current_stamp()),
                                ),
                            )
                        });
                    }
                }
            }
        };

        return Ok((input, NetworkMessage::InvalidMessage));
    }

    Err(nom::Err::Error(Context::Code(
        msg_id_slice,
        nom::ErrorKind::Tag,
    )))
}

// It parses time-stamp from a 16 bytes array as hexadecimal value.
named!(
    s11n_timestamp<&[u8], u64>,
    map_res!(
        // complete!( apply!( s11n_take_n_hex_digits, 16)),
        take!(PROTOCOL_SENT_TIMESTAMP_LENGTH),
        s11n_to_timestamp
    )
);

named!(s11n_network_request_ping, eof!());

named!(s11n_network_response_pong, eof!());

// It parses stream of bytes into `NetworkMessage`.
// Expected stream format is:
// * Protocol name: 13 bytes: "CONCORDIUMP2P"
// * Protocol version: 3 bytes: "001"
// * Timestamp: 16bytes: In hexadecimal format
// * Message Id: 4bytes: Decimal string format
// * Variable content based on previous `Message Id`.
named!( s11n_network_message_parse<&[u8], NetworkMessage>,
    do_parse!(
        tag!( PROTOCOL_NAME)        >>
        tag!( PROTOCOL_VERSION_STR) >>
        timestamp: s11n_timestamp   >>
        msg: apply!( s11n_message_id, timestamp) >>
        (msg)
    )
);

/// See `s11n_network_message_parse` documentation
pub fn s11n_network_message(input: &[u8]) -> IResult<&[u8], NetworkMessage> {
    s11n_network_message_parse(input)
}

#[cfg(test)]
mod unit_test {
    use nom::{verbose_errors::Context, IResult};

    use super::{localhost_peer, s11n_network_message, PROTOCOL_VERSION_STR};
    use crate::network::{
        NetworkId, NetworkMessage, NetworkPacket, NetworkPacketBuilder, NetworkRequest,
        NetworkResponse, ProtocolMessageType, ProtocolPacketType, ProtocolRequestType,
        ProtocolResponseType, PROTOCOL_NAME,
    };
    use concordium_common::{ContainerView, UCursor};

    fn ut_s11n_nom_001_data() -> Vec<(String, IResult<&'static [u8], NetworkMessage>)> {
        let direct_message_content = ContainerView::from(b"Hello world!".to_vec());
        let direct_message_message_id = NetworkPacket::generate_message_id();

        vec![
            (
                format!(
                    "{}{}{}{}{}",
                    PROTOCOL_NAME,
                    PROTOCOL_VERSION_STR,
                    base64::encode(&0u64.to_le_bytes()[..]),
                    ProtocolMessageType::Request,
                    ProtocolRequestType::Ping,
                ),
                Ok((
                    &b""[..],
                    NetworkMessage::NetworkRequest(
                        NetworkRequest::Ping(localhost_peer()),
                        Some(0u64),
                        None,
                    ),
                )),
            ),
            (
                format!(
                    "{}{}{}{}{}",
                    PROTOCOL_NAME,
                    PROTOCOL_VERSION_STR,
                    base64::encode(&11529215046068469760u64.to_le_bytes()[..]),
                    ProtocolMessageType::Request,
                    ProtocolRequestType::Ping
                ),
                Ok((
                    &b""[..],
                    NetworkMessage::NetworkRequest(
                        NetworkRequest::Ping(localhost_peer()),
                        Some(11529215046068469760),
                        None,
                    ),
                )),
            ),
            (
                // Fail: Empty message
                format!(""),
                Err(nom::Err::Incomplete(nom::Needed::Size(PROTOCOL_NAME.len()))),
            ),
            (
                // Fail: Wrong protocol name
                format!("CONCORDIUMP3P"),
                Err(nom::Err::Error(Context::Code(
                    &b"CONCORDIUMP3P"[..],
                    nom::ErrorKind::Tag,
                ))),
            ),
            (
                // Fail: Wrong protocol version: Invalid digit
                format!("{}{}", PROTOCOL_NAME, "0X1"),
                Err(nom::Err::Error(Context::Code(
                    &b"0X1"[..],
                    nom::ErrorKind::Tag,
                ))),
            ),
            (
                // Limit: Max timestamp
                format!(
                    "{}{}{}{}{}",
                    PROTOCOL_NAME,
                    PROTOCOL_VERSION_STR,
                    base64::encode(&u64::max_value().to_le_bytes()[..]),
                    ProtocolMessageType::Response,
                    ProtocolResponseType::Pong
                ),
                Ok((
                    &b""[..],
                    NetworkMessage::NetworkResponse(
                        NetworkResponse::Pong(localhost_peer()),
                        Some(u64::max_value()),
                        None,
                    ),
                )),
            ),
            (
                format!(
                    "{}{}{}{}{}{}{}{}{:010}{}",
                    PROTOCOL_NAME,
                    PROTOCOL_VERSION_STR,
                    base64::encode(&10u64.to_le_bytes()[..]),
                    ProtocolMessageType::Packet,
                    ProtocolPacketType::Direct,
                    localhost_peer().id(),
                    direct_message_message_id,
                    NetworkId::from(111u16),
                    direct_message_content.len(),
                    std::str::from_utf8(direct_message_content.as_slice()).unwrap()
                ),
                Ok((
                    &b""[..],
                    NetworkMessage::NetworkPacket(
                        NetworkPacketBuilder::default()
                            .peer(localhost_peer())
                            .message_id(direct_message_message_id)
                            .network_id(NetworkId::from(111u16))
                            .message(UCursor::from(direct_message_content))
                            .build_direct(localhost_peer().id())
                            .unwrap(),
                        Some(10),
                        None,
                    ),
                )),
            ),
        ]
    }

    #[test]
    fn ut_s11n_nom_001() {
        let data = ut_s11n_nom_001_data();
        for (input, expected) in &data {
            let output = s11n_network_message(input.as_bytes());
            assert_eq!(output, *expected);
        }
    }
}
