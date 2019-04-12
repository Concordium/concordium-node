use std::{
    net::{IpAddr, Ipv4Addr},
    str,
};

use nom::IResult;

use crate::{
    common::{get_current_stamp, ConnectionType, P2PNodeId, P2PPeer, P2PPeerBuilder, UCursor},
    network::{
        NetworkMessage, NetworkPacket, NetworkPacketBuilder, NetworkRequest, NetworkResponse,
        PROTOCOL_MESSAGE_ID_LENGTH, PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE,
        PROTOCOL_MESSAGE_TYPE_REQUEST_PING, PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG, PROTOCOL_NAME,
        PROTOCOL_NETWORK_CONTENT_SIZE_LENGTH, PROTOCOL_NETWORK_ID_LENGTH, PROTOCOL_NODE_ID_LENGTH,
        PROTOCOL_VERSION,
    },
};

fn localhost_peer() -> P2PPeer {
    P2PPeerBuilder::default()
        .connection_type(ConnectionType::Node)
        .ip(IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)))
        .port(8888)
        .build()
        .unwrap()
}

/// *IMPORTANT*: Input HAVE TO be digits because it uses *unchecked*
/// transformation to utf8.
fn s11n_to_timestamp(input: &[u8]) -> Result<u64, std::num::ParseIntError> {
    let input_utf8: &str = str::from_utf8(&input).unwrap_or("0");
    u64::from_str_radix(input_utf8, 16)
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
                .map( |id| P2PNodeId::from_string( id)
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
                .network_id( network_id)
                .message( UCursor::from( content.to_vec()))
                .build_direct( receiver_id )
                .unwrap()
        )
    )
);

fn s11n_message_id(input: &[u8], timestamp: u64) -> IResult<&[u8], NetworkMessage> {
    let (ref input, ref msg_id_slice) = take!(input, 4)?;
    let msg_id: &str = str::from_utf8(&msg_id_slice).unwrap_or("");

    match msg_id {
        PROTOCOL_MESSAGE_TYPE_REQUEST_PING => Ok((
            &input,
            NetworkMessage::NetworkRequest(
                NetworkRequest::Ping(localhost_peer()),
                Some(timestamp),
                Some(get_current_stamp()),
            ),
        )),
        PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG => Ok((
            &input,
            NetworkMessage::NetworkResponse(
                NetworkResponse::Pong(localhost_peer()),
                Some(timestamp),
                Some(get_current_stamp()),
            ),
        )),
        PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE => {
            s11n_network_packet_direct(input).map(|result_packet_direct| {
                let (input, packet_direct) = result_packet_direct;
                (
                    input,
                    NetworkMessage::NetworkPacket(
                        packet_direct,
                        Some(timestamp),
                        Some(get_current_stamp()),
                    ),
                )
            })
        }
        _ => Ok((&input, NetworkMessage::InvalidMessage)),
    }
}

// It parses time-stamp from a 16 bytes array as hexadecimal value.
named!(
    s11n_timestamp<&[u8], u64>,
    map_res!(
        // complete!( apply!( s11n_take_n_hex_digits, 16)),
        take!(16),
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
        tag!( PROTOCOL_VERSION)     >>
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

    use super::{localhost_peer, s11n_network_message};

    use crate::{
        common::{ContainerView, P2PNodeId, UCursor},
        network::{
            NetworkMessage, NetworkPacketBuilder, NetworkRequest, NetworkResponse,
            PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE, PROTOCOL_MESSAGE_TYPE_REQUEST_PING,
            PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG, PROTOCOL_NAME, PROTOCOL_VERSION,
        },
    };

    fn ut_s11n_001_data() -> Vec<(String, IResult<&'static [u8], NetworkMessage>)> {
        let direct_message_content = ContainerView::from(b"Hello world!".to_vec());

        vec![
            (
                format!(
                    "{}{}{}{}",
                    PROTOCOL_NAME,
                    PROTOCOL_VERSION,
                    "0000000000000000",
                    PROTOCOL_MESSAGE_TYPE_REQUEST_PING
                ),
                Ok((
                    &b""[..],
                    NetworkMessage::NetworkRequest(
                        NetworkRequest::Ping(localhost_peer()),
                        Some(0 as u64),
                        None,
                    ),
                )),
            ),
            (
                format!(
                    "{}{}{}{}",
                    PROTOCOL_NAME,
                    PROTOCOL_VERSION,
                    "A000000000000000",
                    PROTOCOL_MESSAGE_TYPE_REQUEST_PING
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
            /*(
                // Fail: Wrong timestamp: Invalid digit
                format!("{}{}{}", PROTOCOL_NAME, PROTOCOL_VERSION, "000000000Y00000A"),
                Err( nom::Err::Error( Context::Code(
                    &b"000000000Y00000A"[..],
                    nom::ErrorKind::TakeWhileMN)))
            ),*/
            (
                // Limit: Max timestamp
                format!(
                    "{}{}{}{}",
                    PROTOCOL_NAME,
                    PROTOCOL_VERSION,
                    "FFFFFFFFFFFFFFFF",
                    PROTOCOL_MESSAGE_TYPE_RESPONSE_PONG
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
                    "{}{:03}{:016X}{:03}{:064X}{:064}{:05}{:010}{}",
                    PROTOCOL_NAME,
                    PROTOCOL_VERSION,
                    10,
                    PROTOCOL_MESSAGE_TYPE_DIRECT_MESSAGE,
                    42,
                    100,
                    111, // Network Id
                    direct_message_content.len(),
                    std::str::from_utf8(direct_message_content.as_slice()).unwrap()
                ),
                Ok((
                    &b""[..],
                    NetworkMessage::NetworkPacket(
                        NetworkPacketBuilder::default()
                            .peer(localhost_peer())
                            .message_id(format!("{:064}", 100))
                            .network_id(111)
                            .message(UCursor::from(direct_message_content))
                            .build_direct(P2PNodeId::from_string("2A").unwrap())
                            .unwrap(),
                        Some(10),
                        None,
                    ),
                )),
            ),
        ]
    }

    #[test]
    fn ut_s11n_001() {
        let data = ut_s11n_001_data();
        for (input, expected) in &data {
            let output = s11n_network_message(input.as_bytes());
            assert_eq!(output, *expected);
        }
    }

}
