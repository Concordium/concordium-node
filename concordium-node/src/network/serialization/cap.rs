mod s11n {
    use crate::common::{ UCursor, P2PPeer, P2PNodeId, ConnectionType };
    use crate::network::{ NetworkMessage, NetworkPacket, NetworkPacketBuilder, NetworkPacketType,
        NetworkRequest, NetworkResponse };
    use crate::p2p_capnp;

    use ::capnp::{ serialize };
    use num_bigint::{ BigUint };
    use std::io::{ Cursor };
    use std::net::{ IpAddr, Ipv4Addr, Ipv6Addr };

    #[inline(always)]
    fn load_p2p_node_id(
        p2p_node_id: &p2p_capnp::p2_p_node_id::Reader) -> ::capnp::Result<P2PNodeId>
    {
        let id = BigUint::from_bytes_le( p2p_node_id.get_id()?);
        Ok( P2PNodeId{ id: id })
    }

    #[inline(always)]
    fn load_ip_addr(
        ip_addr: &p2p_capnp::ip_addr::Reader) -> ::capnp::Result<IpAddr>
    {
        match ip_addr.which()? {
            p2p_capnp::ip_addr::Which::V4(v4_res) => {
                let v4 = &v4_res?;
                Ok(IpAddr::V4( Ipv4Addr::new(
                    v4.get_a(), v4.get_b(), v4.get_c(), v4.get_d())))
            },
            p2p_capnp::ip_addr::Which::V6(v6_res) => {
                let v6 = &v6_res?;
                Ok( IpAddr::V6( Ipv6Addr::new(
                    v6.get_a(), v6.get_b(), v6.get_c(), v6.get_d(),
                    v6.get_e(), v6.get_f(), v6.get_g(), v6.get_h())))
            }
        }
    }

    #[inline(always)]
    fn load_connection_type(
        conn_type: &p2p_capnp::ConnectionType ) -> ConnectionType
    {
        match conn_type {
            p2p_capnp::ConnectionType::Node => ConnectionType::Node,
            p2p_capnp::ConnectionType::Bootstrapper => ConnectionType::Bootstrapper
        }
    }

    #[inline(always)]
    fn load_p2p_peer(
        p2p_peer: &p2p_capnp::p2_p_peer::Reader) -> ::capnp::Result<P2PPeer>
    {
        Ok( P2PPeer::from(
            load_connection_type( &p2p_peer.get_connection_type()?),
            load_p2p_node_id( &p2p_peer.get_id()?)?,
            load_ip_addr( &p2p_peer.get_ip()?)?,
            p2p_peer.get_port()
        ))
    }

    #[inline(always)]
    fn load_network_packet_direct(
            direct :&p2p_capnp::network_packet_direct::Reader,
            peer: &P2PPeer,
            timestamp :u64) -> ::capnp::Result<NetworkMessage>
    {
        let msg_id = direct.get_msg_id()?;
        let network_id = direct.get_network_id();
        let receiver_id = load_p2p_node_id( &direct.get_receiver()?)?;
        let msg = direct.get_msg()?;

        let packet = NetworkPacketBuilder::default()
            .peer( peer.clone())
            .message_id( msg_id.to_string())
            .network_id( network_id)
            .message( UCursor::from( msg.to_vec()))
            .build_direct( receiver_id)
            .map_err( |err| ::capnp::Error::failed( err.to_string()))?;

        Ok( NetworkMessage::NetworkPacket( packet, Some(timestamp), None))
    }

    #[inline(always)]
    fn load_network_packet(
       packet: &p2p_capnp::network_packet::Reader,
       peer: &P2PPeer,
       _ip: &IpAddr) -> ::capnp::Result<NetworkMessage>
    {
        match packet.which()? {
            p2p_capnp::network_packet::Which::Direct(direct) => {
                let timestamp = packet.get_timestamp();
                load_network_packet_direct(&direct?, peer, timestamp)
            },
            _ => Ok( NetworkMessage::InvalidMessage)
        }
    }

    #[inline(always)]
    fn load_request_ping(
        ping: &p2p_capnp::request_ping::Reader) -> ::capnp::Result<NetworkRequest>
    {
        let peer = load_p2p_peer( &ping.get_peer()?);
        Ok( NetworkRequest::Ping( peer?))
    }

    #[inline(always)]
    fn load_network_request(
        request: &p2p_capnp::network_request::Reader) -> ::capnp::Result<NetworkRequest>
    {
        match request.which()? {
            p2p_capnp::network_request::Which::Ping(ping_reader) => {
                load_request_ping( &ping_reader?)
            },
            _ => Err( capnp::Error::unimplemented("Network request type not implemented".to_owned()))
        }
    }

    #[inline(always)]
    fn load_response_pong(
        pong: &p2p_capnp::response_pong::Reader) -> ::capnp::Result<NetworkResponse>
    {
        let peer = load_p2p_peer( & pong.get_peer()?);
        Ok( NetworkResponse::Pong( peer?))
    }

    #[inline(always)]
    fn load_network_response(
        response: &p2p_capnp::network_response::Reader) -> ::capnp::Result<NetworkResponse>
    {
        match response.which()? {
            p2p_capnp::network_response::Which::Pong(pong_reader) => {
                load_response_pong( &pong_reader?)
            },
            _ => Err( capnp::Error::unimplemented("Network response type not implemented".to_owned()))
        }
    }

    pub fn load_from_slice(
        peer: &P2PPeer,
        ip: &IpAddr,
        input: &[u8]) -> ::capnp::Result<NetworkMessage>
    {
        let mut buff = Cursor::new( input);
        // let reader = serialize_packed::read_message(
        let reader = serialize::read_message(
                &mut buff,
                capnp::message::ReaderOptions::default())?;

        let nm = reader.get_root::<p2p_capnp::network_message::Reader>()?;

        match nm.which()? {
            p2p_capnp::network_message::Which::Packet(packet) => {
                load_network_packet( & packet?, peer, ip)
            },
            p2p_capnp::network_message::Which::Request(request_reader_res) => {
                if let Ok(request_reader) = request_reader_res {
                    let request = load_network_request(& request_reader)?;
                    let timestamp = Some( request_reader.get_timestamp());
                    let other = None;

                    Ok( NetworkMessage::NetworkRequest(
                        request, timestamp, other))
                } else {
                    Ok( NetworkMessage::InvalidMessage)
                }
            },
            p2p_capnp::network_message::Which::Response(response_reader_res) => {
                if let Ok(response_reader) = response_reader_res {
                    let response = load_network_response(& response_reader);
                    let timestamp = Some( response_reader.get_timestamp());
                    let other = None;

                    Ok( NetworkMessage::NetworkResponse(
                        response?, timestamp, other))
                } else {
                    Ok( NetworkMessage::InvalidMessage)
                }
            },
            p2p_capnp::network_message::Which::Unknown(()) => {
                Ok( NetworkMessage::UnknownMessage)
            },
            _ => Ok( NetworkMessage::InvalidMessage)
        }
    }

    #[inline(always)]
    fn write_p2p_node_id(
        node_id_builder: &mut p2p_capnp::p2_p_node_id::Builder,
        p2p_node_id: &P2PNodeId)
    {
        let id : Vec<u8> = p2p_node_id.id.to_bytes_le();
        node_id_builder.set_id( id.as_slice());
    }

    #[inline(always)]
    fn write_connection_type( conn_type: ConnectionType) -> p2p_capnp::ConnectionType
    {
        match conn_type {
            ConnectionType::Node => p2p_capnp::ConnectionType::Node,
            ConnectionType::Bootstrapper => p2p_capnp::ConnectionType::Bootstrapper
        }
    }

    #[inline(always)]
    fn write_ip_addr_v4(
        ip_addr_v4_builder : &mut p2p_capnp::ip_addr_v4::Builder,
        ip_addr_v4: &Ipv4Addr)
    {
        let octets : [u8;4] = ip_addr_v4.octets();
        ip_addr_v4_builder.set_a( octets[0]);
        ip_addr_v4_builder.set_b( octets[1]);
        ip_addr_v4_builder.set_c( octets[2]);
        ip_addr_v4_builder.set_d( octets[3]);
    }

    #[inline(always)]
    fn write_ip_addr_v6(
        ip_addr_v6_builder : &mut p2p_capnp::ip_addr_v6::Builder,
        ip_addr_v6: &Ipv6Addr)
    {
        let segments :[u16;8] = ip_addr_v6.segments();
        ip_addr_v6_builder.set_a( segments[0]);
        ip_addr_v6_builder.set_b( segments[1]);
        ip_addr_v6_builder.set_c( segments[2]);
        ip_addr_v6_builder.set_d( segments[3]);
        ip_addr_v6_builder.set_e( segments[4]);
        ip_addr_v6_builder.set_f( segments[5]);
        ip_addr_v6_builder.set_g( segments[6]);
        ip_addr_v6_builder.set_h( segments[7]);
    }

    #[inline(always)]
    fn write_ip_addr(
        builder: &mut p2p_capnp::ip_addr::Builder,
        ip_addr: &IpAddr)
    {
        match ip_addr {
            IpAddr::V4(ip_addr_v4) => {
                let mut v4_builder = builder.reborrow().init_v4();
                write_ip_addr_v4( &mut v4_builder, &ip_addr_v4);
            },
            IpAddr::V6(ip_addr_v6) => {
                let mut v6_builder = builder.reborrow().init_v6();
                write_ip_addr_v6( &mut v6_builder, &ip_addr_v6);
            }
        }
    }

    #[inline(always)]
    fn write_p2p_peer(
        builder: &mut p2p_capnp::p2_p_peer::Builder,
        peer: &P2PPeer)
    {
        {
            let mut ip_builder = builder.reborrow().init_ip();
            let ip = peer.ip();
            write_ip_addr( &mut ip_builder, &ip);
        }

        builder.set_port( peer.port());
        {
            let mut builder_id = builder.reborrow().init_id();
            let id = peer.id();
            write_p2p_node_id( &mut builder_id, &id);
        }
        builder.set_connection_type(
            write_connection_type( peer.connection_type()));
    }

    #[inline(always)]
    fn write_network_packet_direct(
        builder: &mut p2p_capnp::network_packet_direct::Builder,
        peer: &P2PPeer,
        msg_id: &String,
        receiver: &P2PNodeId,
        network_id: u16,
        msg: &[u8])
    {
        {
            let mut peer_builder = builder.reborrow().init_peer();
            write_p2p_peer( &mut peer_builder, peer);
        }
        builder.set_msg_id( msg_id);

        {
            let mut node_id_builder = builder.reborrow().init_receiver();
            write_p2p_node_id( &mut node_id_builder, receiver);
        }

        builder.set_network_id( network_id);
        builder.set_msg( msg);
    }

    #[inline(always)]
    fn write_network_packet(
        builder: &mut p2p_capnp::network_packet::Builder,
        np: &mut NetworkPacket,
        timestamp: u64)
    {
        match np.packet_type
        {
            NetworkPacketType::DirectMessage( ref receiver) =>
            {
                let view = np.message.read_all_into_view()
                    .expect("Not enought memory to serialize using CAPNP");

                let mut direct_builder = builder.reborrow().init_direct();
                write_network_packet_direct(
                    &mut direct_builder,
                    &np.peer, &np.message_id, receiver, np.network_id,
                    view.as_slice());
            },
            _ => panic!("Network Packet is not yet supported")
        };
        builder.set_timestamp(timestamp);
    }

    #[inline(always)]
    fn write_request_ping(
        builder: &mut p2p_capnp::request_ping::Builder,
        peer: &P2PPeer)
    {
        let mut peer_builder = builder.reborrow().init_peer();
        write_p2p_peer( &mut peer_builder, peer);
    }

    #[inline(always)]
    fn write_network_request(
        builder: &mut p2p_capnp::network_request::Builder,
        request: &NetworkRequest,
        timestamp: u64)
    {
        match request {
            NetworkRequest::Ping( ref peer) => {
                let mut ping_builder = builder.reborrow().init_ping();
                write_request_ping(
                    &mut ping_builder,
                    peer);
            },
            _ => panic!("Network request is not yet supported")
        };
        builder.set_timestamp(timestamp);
    }

    #[inline(always)]
    fn write_response_pong(
        builder: &mut p2p_capnp::response_pong::Builder,
        peer: &P2PPeer)
    {
        let mut peer_builder = builder.reborrow().init_peer();
        write_p2p_peer( &mut peer_builder, peer);
    }

    #[inline(always)]
    fn write_network_response(
        builder: &mut p2p_capnp::network_response::Builder,
        response: &NetworkResponse,
        timestamp: u64)
    {
        match response {
            NetworkResponse::Pong( ref peer) => {
                let mut pong_builder = builder.reborrow().init_pong();
                write_response_pong(
                    &mut pong_builder,
                    peer);
            },
            _ => panic!("Network response is not yet supported")
        };
        builder.set_timestamp(timestamp);
    }

    pub fn write_network_message(
        builder: &mut p2p_capnp::network_message::Builder,
        nm: &mut NetworkMessage)
    {
        match nm {
            NetworkMessage::NetworkPacket( ref mut np, ref timestamp_opt, _) => {
                let timestamp : u64 = timestamp_opt.unwrap_or(0 as u64);
                let mut packet_builder = builder.reborrow().init_packet();
                write_network_packet(
                    &mut packet_builder,
                    np, timestamp);
            },
            NetworkMessage::NetworkRequest(ref request, ref timestamp_opt, _) => {
                let timestamp : u64 = timestamp_opt.unwrap_or(0 as u64);
                let mut request_builder = builder.reborrow().init_request();
                write_network_request(
                    &mut request_builder,
                    request, timestamp);
            },
            NetworkMessage::NetworkResponse(ref response, ref timestamp_opt, _) => {
                let timestamp : u64 = timestamp_opt.unwrap_or(0 as u64);
                let mut response_builder = builder.reborrow().init_response();
                write_network_response(
                    &mut response_builder,
                    response, timestamp);

            },
            _ => panic!("Network message is not yet supported")
        }
    }
}

use crate::common::{ P2PPeer };
use crate::network::{ NetworkMessage };
use crate::p2p_capnp;
use std::net::{ IpAddr };


pub fn deserialize(
        peer: &P2PPeer,
        ip: &IpAddr,
        input: &[u8]) -> NetworkMessage
{
    match s11n::load_from_slice( peer, ip, input) {
        Ok(nm) => nm,
        Err(e) => {
            warn!("CAPNP error: {:?}", e);
            NetworkMessage::InvalidMessage
        }
    }
}

pub fn save_network_message( nm: &mut NetworkMessage) -> Vec<u8> {
    let mut message = ::capnp::message::Builder::new_default();
    {
        let mut builder = message.init_root::<p2p_capnp::network_message::Builder>();
        s11n::write_network_message( &mut builder, nm);
    }
    let mut buffer = vec![];
    capnp::serialize::write_message( &mut buffer, &message).unwrap();

    buffer
}

#[cfg(test)]
mod unit_test {
    use std::net::{ IpAddr, Ipv4Addr };

    use super::{ deserialize, save_network_message };

    use crate::network::{ NetworkMessage, NetworkRequest, NetworkResponse, NetworkPacketBuilder};
    use crate::common::{ UCursor, P2PPeer, P2PPeerBuilder, P2PNodeId, ConnectionType,};

    fn localhost_peer() -> P2PPeer
    {
        P2PPeerBuilder::default()
            .connection_type( ConnectionType::Node)
            .ip( IpAddr::V4(Ipv4Addr::new(127,0,0,1)))
            .port( 8888)
            .build().unwrap()
    }

    fn ut_s11n_001_data() -> Vec<( Vec<u8>,  NetworkMessage)> {
        let direct_message_content = b"Hello world!".to_vec();
        let mut messages = vec![
            NetworkMessage::NetworkRequest(
                NetworkRequest::Ping( localhost_peer()),
                Some(0 as u64),
                None),
            NetworkMessage::NetworkRequest(
                NetworkRequest::Ping( localhost_peer()),
                Some(11529215046068469760),
                None),
            NetworkMessage::NetworkResponse(
                NetworkResponse::Pong( localhost_peer()),
                Some( u64::max_value()),
                None),
            NetworkMessage::NetworkPacket(
                NetworkPacketBuilder::default()
                    .peer( localhost_peer())
                    .message_id( format!("{:064}",100))
                    .network_id( 111)
                    .message( UCursor::from( direct_message_content))
                    .build_direct( P2PNodeId::from_string("2A").unwrap()).unwrap(),
                 Some(10), None)
        ];

        let mut messages_data: Vec<(Vec<u8>, NetworkMessage)> = vec![];
        for mut message in &mut messages {
            let data: Vec<u8> = save_network_message( &mut message);
            messages_data.push((data, message.clone()));
        }

        messages_data
    }

    #[test]
    fn ut_s11n_001(){
        let local_ip = IpAddr::V4( Ipv4Addr::new(127,0,0,1));
        let local_peer = P2PPeerBuilder::default()
                .connection_type( ConnectionType::Node)
                .ip( local_ip.clone())
                .port( 8888)
                .build().unwrap();

        let test_params = ut_s11n_001_data();
        for (data, expected) in &test_params {
            let output = deserialize( &local_peer, &local_ip, data.as_slice());
            assert_eq!( output, *expected)
        }
    }
}
