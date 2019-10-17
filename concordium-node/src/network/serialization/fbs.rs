use failure::{Error, Fallible};
use flatbuffers::{FlatBufferBuilder, Push};

use crate::flatbuffers_shim::network;

use crate::{
    common::{get_current_stamp, p2p_peer::P2PPeer, P2PNodeId},
    network::{
        NetworkId, NetworkMessage, NetworkMessagePayload, NetworkPacket, NetworkPacketType,
        NetworkRequest, NetworkResponse, PROTOCOL_VERSION,
    },
    p2p::banned_nodes::BannedNode,
};
use concordium_common::{hybrid_buf::HybridBuf, serial::Serial};

use std::{
    convert::TryFrom,
    io::{self, Cursor, Seek, SeekFrom, Write},
    net::IpAddr,
};

// misc. helpers

impl Push for P2PPeer {
    type Output = [u8; 15];

    fn push(&self, dest: &mut [u8], _rest: &[u8]) {
        flatbuffers::emplace_scalar::<u64>(dest, self.id.as_raw());
        match self.addr.ip() {
            IpAddr::V4(ip) => {
                for &byte in &ip.octets() {
                    flatbuffers::emplace_scalar::<u8>(dest, byte);
                }
            }
            IpAddr::V6(ip) => {
                for &byte in &ip.octets() {
                    flatbuffers::emplace_scalar::<u8>(dest, byte);
                }
            }
        }
        flatbuffers::emplace_scalar::<u16>(dest, self.addr.port());
        flatbuffers::emplace_scalar::<u8>(dest, self.peer_type as u8);
    }
}

// deserialization

fn deserialize_packet(root: &network::NetworkMessage) -> Fallible<NetworkMessagePayload> {
    let packet = if let Some(payload) = root.payload() {
        network::NetworkPacket::init_from_table(payload)
    } else {
        bail!("missing network message payload (expected a packet)")
    };

    let packet_type = if let Some(direct) = packet.destination_as_direct() {
        NetworkPacketType::DirectMessage(P2PNodeId(direct.target_id()))
    } else if let Some(broadcast) = packet.destination_as_broadcast() {
        let ids_to_exclude = if let Some(ids) = broadcast.ids_to_exclude() {
            ids.safe_slice().iter().copied().map(P2PNodeId).collect()
        } else {
            bail!("FIXME: missing values or an encoding issue?")
        };
        NetworkPacketType::BroadcastedMessage(ids_to_exclude)
    } else {
        bail!("invalid network packet type")
    };

    let network_id = NetworkId::from(packet.networkId());

    let payload = if let Some(payload) = packet.payload() {
        HybridBuf::try_from(payload)?
    } else {
        bail!("missing packet payload")
    };

    Ok(NetworkMessagePayload::NetworkPacket(NetworkPacket {
        packet_type,
        network_id,
        message: payload,
    }))
}

fn deserialize_request(root: &network::NetworkMessage) -> Fallible<NetworkMessagePayload> {
    let request = if let Some(payload) = root.payload() {
        network::NetworkRequest::init_from_table(payload)
    } else {
        bail!("missing network message payload (expected a request)")
    };

    match request.variant() {
        network::RequestVariant::Ping => {
            Ok(NetworkMessagePayload::NetworkRequest(NetworkRequest::Ping))
        }
        network::RequestVariant::GetPeers => {
            if let Some(network_ids) = request
                .payload()
                .map(network::NetworkIds::init_from_table)
                .and_then(|payload| payload.ids())
            {
                let network_ids = network_ids
                    .safe_slice()
                    .iter()
                    .copied()
                    .map(NetworkId::from)
                    .collect();
                Ok(NetworkMessagePayload::NetworkRequest(
                    NetworkRequest::GetPeers(network_ids),
                ))
            } else {
                bail!("missing network ids in a GetPeers request")
            }
        }
        network::RequestVariant::Handshake => {
            if let Some(handshake) = request.payload().map(network::Handshake::init_from_table) {
                let node_id = P2PNodeId(handshake.nodeId());
                let port = handshake.port();
                let network_ids = if let Some(network_ids) = handshake.networkIds() {
                    network_ids
                        .safe_slice()
                        .iter()
                        .copied()
                        .map(NetworkId::from)
                        .collect()
                } else {
                    bail!("missing network ids in a Handshake")
                };
                // the zero-knowledge proof will be added in the future

                Ok(NetworkMessagePayload::NetworkRequest(
                    NetworkRequest::Handshake(node_id, port, network_ids, Vec::new()),
                ))
            } else {
                bail!("missing handshake payload")
            }
        }
        network::RequestVariant::BanNode | network::RequestVariant::UnbanNode => {
            if let Some(id) = request
                .payload()
                .map(network::BanUnban::init_from_table)
                .and_then(|ban| ban.id())
            {
                let tgt = BannedNode::deserial(&mut Cursor::new(id))?;

                Ok(NetworkMessagePayload::NetworkRequest(
                    match request.variant() {
                        network::RequestVariant::BanNode => NetworkRequest::BanNode(tgt),
                        network::RequestVariant::UnbanNode => NetworkRequest::UnbanNode(tgt),
                        _ => unreachable!(),
                    },
                ))
            } else {
                bail!("missing ban target in a ban/unban request")
            }
        }
        network::RequestVariant::JoinNetwork | network::RequestVariant::LeaveNetwork => {
            if let Some(id) = request
                .payload()
                .map(network::NetworkId::init_from_table)
                .map(|id| NetworkId::from(id.id()))
            {
                Ok(NetworkMessagePayload::NetworkRequest(
                    match request.variant() {
                        network::RequestVariant::JoinNetwork => NetworkRequest::JoinNetwork(id),
                        network::RequestVariant::LeaveNetwork => NetworkRequest::LeaveNetwork(id),
                        _ => unreachable!(),
                    },
                ))
            } else {
                bail!("missing network id in a join/leave network request")
            }
        }
        // TODO: network::RequestVariant::Retransmit => ,
        _ => unimplemented!(),
    }
}

fn deserialize_response(root: &network::NetworkMessage) -> Fallible<NetworkMessagePayload> {
    let response = if let Some(payload) = root.payload() {
        network::NetworkResponse::init_from_table(payload)
    } else {
        bail!("missing network message payload (expected a request)")
    };

    match response.variant() {
        network::ResponseVariant::Pong => Ok(NetworkMessagePayload::NetworkResponse(
            NetworkResponse::Pong,
        )),
        network::ResponseVariant::PeerList => {
            if let Some(blob) = response
                .payload_as_peer_list()
                .and_then(|peers| peers.blob())
            {
                let mut blob = Cursor::new(blob);
                let list = <Vec<P2PPeer>>::deserial(&mut blob)?;

                Ok(NetworkMessagePayload::NetworkResponse(
                    NetworkResponse::PeerList(list),
                ))
            } else {
                bail!("missing peers in a PeerList response")
            }
        }
        network::ResponseVariant::Handshake => {
            if let Some(handshake) = response.payload().map(network::Handshake::init_from_table) {
                let node_id = P2PNodeId(handshake.nodeId());
                let port = handshake.port();
                let network_ids = if let Some(network_ids) = handshake.networkIds() {
                    network_ids
                        .safe_slice()
                        .iter()
                        .copied()
                        .map(NetworkId::from)
                        .collect()
                } else {
                    bail!("missing network ids in a Handshake")
                };
                // the zero-knowledge proof will be added in the future

                Ok(NetworkMessagePayload::NetworkResponse(
                    NetworkResponse::Handshake(node_id, port, network_ids, Vec::new()),
                ))
            } else {
                bail!("missing handshake payload")
            }
        }
    }
}

pub fn deserialize(buffer: &[u8]) -> Fallible<NetworkMessage> {
    if !network::network_message_size_prefixed_buffer_has_identifier(buffer) {
        bail!("unrecognized protocol name")
    }

    let root = network::get_size_prefixed_root_as_network_message(buffer);

    if root.version() != PROTOCOL_VERSION {
        bail!("invalid protocol version")
    }

    let timestamp1 = Some(root.timestamp());

    let payload = match root.payload_type() {
        network::NetworkMessagePayload::NetworkPacket => deserialize_packet(&root)?,
        network::NetworkMessagePayload::NetworkRequest => deserialize_request(&root)?,
        network::NetworkMessagePayload::NetworkResponse => deserialize_response(&root)?,
        _ => bail!("Invalid network message payload type"),
    };

    Ok(NetworkMessage {
        timestamp1,
        timestamp2: Some(get_current_stamp()),
        payload,
    })
}

// serialization

fn serialize_packet(
    builder: &mut FlatBufferBuilder,
    packet: &mut NetworkPacket,
) -> io::Result<flatbuffers::WIPOffset<flatbuffers::UnionWIPOffset>> {
    let (destination_offset, destination_type) = match packet.packet_type {
        NetworkPacketType::DirectMessage(target_id) => {
            let offset = network::Direct::create(builder, &network::DirectArgs {
                target_id: target_id.as_raw(),
            });

            (offset.as_union_value(), network::Destination::Direct)
        }
        NetworkPacketType::BroadcastedMessage(ref ids_to_exclude) => {
            builder.start_vector::<u64>(ids_to_exclude.len());
            for id in ids_to_exclude {
                builder.push(id.as_raw());
            }
            let ids_offset = builder.end_vector(ids_to_exclude.len());
            let offset = network::Broadcast::create(builder, &network::BroadcastArgs {
                ids_to_exclude: Some(ids_offset),
            });

            (offset.as_union_value(), network::Destination::Broadcast)
        }
    };

    let payload_offset = builder.create_vector_direct::<u8>(&packet.message.remaining_bytes()?);

    let packet_offset = network::NetworkPacket::create(builder, &network::NetworkPacketArgs {
        destination_type,
        destination: Some(destination_offset),
        networkId: packet.network_id.id,
        payload: Some(payload_offset),
    })
    .as_union_value();

    Ok(packet_offset)
}

fn serialize_request(
    builder: &mut FlatBufferBuilder,
    request: &NetworkRequest,
) -> io::Result<flatbuffers::WIPOffset<flatbuffers::UnionWIPOffset>> {
    let (variant, payload_type, payload) = match request {
        NetworkRequest::Ping => (
            network::RequestVariant::Ping,
            network::RequestPayload::NONE,
            None,
        ),
        NetworkRequest::GetPeers(nets) => {
            builder.start_vector::<u16>(nets.len());
            for net in nets {
                builder.push(net.id);
            }
            let nets_offset = Some(builder.end_vector(nets.len()));
            let offset = Some(
                network::NetworkIds::create(builder, &network::NetworkIdsArgs { ids: nets_offset })
                    .as_union_value(),
            );
            (
                network::RequestVariant::GetPeers,
                network::RequestPayload::NetworkIds,
                offset,
            )
        }
        NetworkRequest::Handshake(id, port, nets, _zk) => {
            builder.start_vector::<u16>(nets.len());
            for net in nets {
                builder.push(net.id);
            }
            let nets_offset = Some(builder.end_vector(nets.len()));
            let offset = Some(
                network::Handshake::create(builder, &network::HandshakeArgs {
                    nodeId:     id.as_raw(),
                    port:       *port,
                    networkIds: nets_offset,
                    zk:         None,
                })
                .as_union_value(),
            );
            (
                network::RequestVariant::Handshake,
                network::RequestPayload::Handshake,
                offset,
            )
        }
        NetworkRequest::BanNode(id) | NetworkRequest::UnbanNode(id) => {
            let mut buf = Vec::new();
            id.serial(&mut buf).expect("can't serialize a ban/unban");
            builder.start_vector::<u8>(buf.len());
            for &byte in buf.iter().rev() {
                builder.push(byte);
            }
            let id_offset = Some(builder.end_vector(buf.len()));
            let offset = Some(
                network::BanUnban::create(builder, &network::BanUnbanArgs { id: id_offset })
                    .as_union_value(),
            );

            let ban_unban = match request {
                NetworkRequest::BanNode(_) => network::RequestVariant::BanNode,
                NetworkRequest::UnbanNode(_) => network::RequestVariant::UnbanNode,
                _ => unreachable!(),
            };

            (ban_unban, network::RequestPayload::BanUnban, offset)
        }
        NetworkRequest::JoinNetwork(id) => {
            let offset = Some(
                network::NetworkId::create(builder, &network::NetworkIdArgs { id: id.id })
                    .as_union_value(),
            );
            (
                network::RequestVariant::JoinNetwork,
                network::RequestPayload::NetworkId,
                offset,
            )
        }
        NetworkRequest::LeaveNetwork(id) => {
            let offset = Some(
                network::NetworkId::create(builder, &network::NetworkIdArgs { id: id.id })
                    .as_union_value(),
            );
            (
                network::RequestVariant::LeaveNetwork,
                network::RequestPayload::NetworkId,
                offset,
            )
        }
        // TODO: Retransmit
        _ => unimplemented!(),
    };

    let request_offset = network::NetworkRequest::create(builder, &network::NetworkRequestArgs {
        variant,
        payload_type,
        payload,
    })
    .as_union_value();

    Ok(request_offset)
}

fn serialize_response(
    builder: &mut FlatBufferBuilder,
    response: &NetworkResponse,
) -> io::Result<flatbuffers::WIPOffset<flatbuffers::UnionWIPOffset>> {
    let (variant, payload_type, payload) = match response {
        NetworkResponse::Pong => (
            network::ResponseVariant::Pong,
            network::ResponsePayload::NONE,
            None,
        ),
        NetworkResponse::PeerList(peerlist) => {
            let mut buf = Vec::new();
            peerlist
                .serial(&mut buf)
                .expect("can't serialize a list of peers");
            builder.start_vector::<u8>(buf.len());
            for &byte in buf.iter().rev() {
                builder.push(byte);
            }
            let blob_offset = Some(builder.end_vector(buf.len()));
            let offset = Some(
                network::PeerList::create(builder, &network::PeerListArgs { blob: blob_offset })
                    .as_union_value(),
            );

            (
                network::ResponseVariant::PeerList,
                network::ResponsePayload::PeerList,
                offset,
            )
        }
        NetworkResponse::Handshake(id, port, nets, _zk) => {
            builder.start_vector::<u16>(nets.len());
            for net in nets {
                builder.push(net.id);
            }
            let nets_offset = Some(builder.end_vector(nets.len()));
            let offset = Some(
                network::Handshake::create(builder, &network::HandshakeArgs {
                    nodeId:     id.as_raw(),
                    port:       *port,
                    networkIds: nets_offset,
                    zk:         None,
                })
                .as_union_value(),
            );
            (
                network::ResponseVariant::Handshake,
                network::ResponsePayload::Handshake,
                offset,
            )
        }
    };

    let response_offset =
        network::NetworkResponse::create(builder, &network::NetworkResponseArgs {
            variant,
            payload_type,
            payload,
        })
        .as_union_value();

    Ok(response_offset)
}

pub fn serialize<T: Write + Seek>(source: &mut NetworkMessage, target: &mut T) -> Fallible<()> {
    let capacity = if let NetworkMessagePayload::NetworkPacket(ref packet) = source.payload {
        packet.message.len()? as usize + 64 // FIXME: fine-tune the overhead
    } else {
        256
    };
    let mut builder = FlatBufferBuilder::new_with_capacity(capacity);

    let (payload_type, payload_offset) = match source.payload {
        NetworkMessagePayload::NetworkPacket(ref mut packet) => (
            network::NetworkMessagePayload::NetworkPacket,
            serialize_packet(&mut builder, packet)?,
        ),
        NetworkMessagePayload::NetworkRequest(ref request) => (
            network::NetworkMessagePayload::NetworkRequest,
            serialize_request(&mut builder, request)?,
        ),
        NetworkMessagePayload::NetworkResponse(ref response) => (
            network::NetworkMessagePayload::NetworkResponse,
            serialize_response(&mut builder, response)?,
        ),
    };

    let message_offset =
        network::NetworkMessage::create(&mut builder, &network::NetworkMessageArgs {
            version: PROTOCOL_VERSION,
            timestamp: get_current_stamp(),
            payload_type,
            payload: Some(payload_offset),
        });

    network::finish_size_prefixed_network_message_buffer(&mut builder, message_offset);

    target
        .write_all(builder.finished_data())
        .map_err(Error::from)?;

    target.seek(SeekFrom::Start(0))?;

    Ok(())
}

// tests

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{common::PeerType, test_utils::create_random_packet};
    use std::{io::Cursor, net::SocketAddr};

    #[test]
    fn s11n_size_fbs() {
        use crate::test_utils::create_random_packet;

        let payload_size = 1000;
        let mut msg = create_random_packet(payload_size);
        let mut buffer = std::io::Cursor::new(Vec::with_capacity(payload_size));

        serialize(&mut msg, &mut buffer).unwrap();
        println!(
            "flatbuffers s11n ratio: {}",
            buffer.get_ref().len() as f64 / payload_size as f64
        );
    }

    #[test]
    fn fbs_serde_request_ping() {
        let mut message = NetworkMessage {
            timestamp1: None,
            timestamp2: None,
            payload:    NetworkMessagePayload::NetworkRequest(NetworkRequest::Ping),
        };
        let mut buffer = Cursor::new(Vec::new());

        serialize(&mut message, &mut buffer).unwrap();
        let deserialized = deserialize(&buffer.get_ref()).unwrap();
        assert_eq!(deserialized.payload, message.payload);
    }

    #[test]
    fn fbs_serde_request_get_peers() {
        let mut message = NetworkMessage {
            timestamp1: None,
            timestamp2: None,
            payload:    NetworkMessagePayload::NetworkRequest(NetworkRequest::GetPeers(
                [100u16, 1000, 1234, 9999]
                    .iter()
                    .copied()
                    .map(NetworkId::from)
                    .collect(),
            )),
        };
        let mut buffer = Cursor::new(Vec::new());

        serialize(&mut message, &mut buffer).unwrap();
        let deserialized = deserialize(&buffer.get_ref()).unwrap();
        assert_eq!(deserialized.payload, message.payload);
    }

    #[test]
    fn fbs_serde_request_handshake() {
        let mut message = NetworkMessage {
            timestamp1: None,
            timestamp2: None,
            payload:    NetworkMessagePayload::NetworkRequest(NetworkRequest::Handshake(
                P2PNodeId(77),
                1234,
                [100u16, 1000, 1234, 9999]
                    .iter()
                    .copied()
                    .map(NetworkId::from)
                    .collect(),
                Vec::new(),
            )),
        };
        let mut buffer = Cursor::new(Vec::new());

        serialize(&mut message, &mut buffer).unwrap();
        let deserialized = deserialize(&buffer.get_ref()).unwrap();
        assert_eq!(deserialized.payload, message.payload);
    }

    #[test]
    fn fbs_serde_request_ban_node_by_id() {
        let mut message = NetworkMessage {
            timestamp1: None,
            timestamp2: None,
            payload:    NetworkMessagePayload::NetworkRequest(NetworkRequest::BanNode(
                BannedNode::ById(P2PNodeId(1337)),
            )),
        };
        let mut buffer = Cursor::new(Vec::new());

        serialize(&mut message, &mut buffer).unwrap();
        let deserialized = deserialize(&buffer.get_ref()).unwrap();
        assert_eq!(deserialized.payload, message.payload);
    }

    #[test]
    fn fbs_serde_request_unban_node_by_id() {
        let mut message = NetworkMessage {
            timestamp1: None,
            timestamp2: None,
            payload:    NetworkMessagePayload::NetworkRequest(NetworkRequest::UnbanNode(
                BannedNode::ById(P2PNodeId(1337)),
            )),
        };
        let mut buffer = Cursor::new(Vec::new());

        serialize(&mut message, &mut buffer).unwrap();
        let deserialized = deserialize(&buffer.get_ref()).unwrap();
        assert_eq!(deserialized.payload, message.payload);
    }

    #[test]
    fn fbs_serde_request_ban_node_by_addr_v4() {
        let mut message = NetworkMessage {
            timestamp1: None,
            timestamp2: None,
            payload:    NetworkMessagePayload::NetworkRequest(NetworkRequest::BanNode(
                BannedNode::ByAddr(IpAddr::from([255, 255, 255, 255])),
            )),
        };
        let mut buffer = Cursor::new(Vec::new());

        serialize(&mut message, &mut buffer).unwrap();
        let deserialized = deserialize(&buffer.get_ref()).unwrap();
        assert_eq!(deserialized.payload, message.payload);
    }

    #[test]
    fn fbs_serde_request_ban_node_by_addr_v6() {
        let mut message = NetworkMessage {
            timestamp1: None,
            timestamp2: None,
            payload:    NetworkMessagePayload::NetworkRequest(NetworkRequest::BanNode(
                BannedNode::ByAddr(IpAddr::from([1, 2, 3, 4, 5, 6, 7, 8])),
            )),
        };
        let mut buffer = Cursor::new(Vec::new());

        serialize(&mut message, &mut buffer).unwrap();
        let deserialized = deserialize(&buffer.get_ref()).unwrap();
        assert_eq!(deserialized.payload, message.payload);
    }

    #[test]
    fn fbs_serde_request_join_network() {
        let mut message = NetworkMessage {
            timestamp1: None,
            timestamp2: None,
            payload:    NetworkMessagePayload::NetworkRequest(NetworkRequest::JoinNetwork(
                NetworkId::from(1337),
            )),
        };
        let mut buffer = Cursor::new(Vec::new());

        serialize(&mut message, &mut buffer).unwrap();
        let deserialized = deserialize(&buffer.get_ref()).unwrap();
        assert_eq!(deserialized.payload, message.payload);
    }

    #[test]
    fn fbs_serde_request_leave_network() {
        let mut message = NetworkMessage {
            timestamp1: None,
            timestamp2: None,
            payload:    NetworkMessagePayload::NetworkRequest(NetworkRequest::LeaveNetwork(
                NetworkId::from(1337),
            )),
        };
        let mut buffer = Cursor::new(Vec::new());

        serialize(&mut message, &mut buffer).unwrap();
        let deserialized = deserialize(&buffer.get_ref()).unwrap();
        assert_eq!(deserialized.payload, message.payload);
    }

    // TODO: Retransmit (Requests)

    #[test]
    fn fbs_serde_response_pong() {
        let mut message = NetworkMessage {
            timestamp1: None,
            timestamp2: None,
            payload:    NetworkMessagePayload::NetworkResponse(NetworkResponse::Pong),
        };
        let mut buffer = Cursor::new(Vec::new());

        serialize(&mut message, &mut buffer).unwrap();
        let deserialized = deserialize(&buffer.get_ref()).unwrap();
        assert_eq!(deserialized.payload, message.payload);
    }

    #[test]
    fn fbs_serde_response_peer_list() {
        let mut message = NetworkMessage {
            timestamp1: None,
            timestamp2: None,
            payload:    NetworkMessagePayload::NetworkResponse(NetworkResponse::PeerList(
                [
                    P2PPeer {
                        id:        P2PNodeId(1234567890123),
                        addr:      SocketAddr::new(IpAddr::from([1, 2, 3, 4]), 8000),
                        peer_type: PeerType::Bootstrapper,
                    },
                    P2PPeer {
                        id:        P2PNodeId(1),
                        addr:      SocketAddr::new(IpAddr::from([8, 7, 6, 5, 4, 3, 2, 1]), 8080),
                        peer_type: PeerType::Node,
                    },
                ]
                .iter()
                .cloned()
                .collect(),
            )),
        };
        let mut buffer = Cursor::new(Vec::new());

        serialize(&mut message, &mut buffer).unwrap();
        let deserialized = deserialize(&buffer.get_ref()).unwrap();
        assert_eq!(deserialized.payload, message.payload);
    }

    #[test]
    fn fbs_serde_response_handshake() {
        let mut message = NetworkMessage {
            timestamp1: None,
            timestamp2: None,
            payload:    NetworkMessagePayload::NetworkResponse(NetworkResponse::Handshake(
                P2PNodeId(77),
                1234,
                [100u16, 1000, 1234, 9999]
                    .iter()
                    .copied()
                    .map(NetworkId::from)
                    .collect(),
                Vec::new(),
            )),
        };
        let mut buffer = Cursor::new(Vec::new());

        serialize(&mut message, &mut buffer).unwrap();
        let deserialized = deserialize(&buffer.get_ref()).unwrap();
        assert_eq!(deserialized.payload, message.payload);
    }

    #[test]
    fn fbs_serde_packet() {
        let mut message = create_random_packet(8);
        let mut buffer = Cursor::new(Vec::new());

        serialize(&mut message, &mut buffer).unwrap();
        let deserialized = deserialize(&buffer.get_ref()).unwrap();
        assert_eq!(deserialized.payload, message.payload);
    }
}
