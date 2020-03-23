//! Flatbuffers serialization.

use failure::{Error, Fallible};
use flatbuffers::FlatBufferBuilder;
use semver::Version;

use crate::flatbuffers_shim::network;

use crate::{
    common::{
        get_current_stamp,
        p2p_peer::{P2PPeer, PeerType},
        P2PNodeId,
    },
    network::{
        Handshake, NetworkId, NetworkMessage, NetworkPacket, NetworkPayload, NetworkRequest,
        NetworkResponse, PacketDestination,
    },
    p2p::bans::BanId,
};

use std::{
    io::{self, Read, Write},
    net::{IpAddr, SocketAddr},
    panic,
};

impl NetworkMessage {
    // FIXME: remove the unwind once the verifier is available
    pub fn deserialize(buffer: &[u8]) -> Fallible<Self> {
        match panic::catch_unwind(|| _deserialize(buffer)) {
            Ok(nm) => nm,
            Err(e) => bail!("can't deserialize a network message: {:?}", e),
        }
    }

    pub fn serialize<T: Write>(&self, target: &mut T) -> Fallible<()> {
        let capacity = if let NetworkPayload::NetworkPacket(ref packet) = self.payload {
            packet.message.len() + 64 // FIXME: fine-tune the overhead
        } else {
            256
        };
        let mut builder = FlatBufferBuilder::new_with_capacity(capacity);

        let (payload_type, payload_offset) = match self.payload {
            NetworkPayload::NetworkPacket(ref packet) => {
                (network::NetworkPayload::NetworkPacket, serialize_packet(&mut builder, packet)?)
            }
            NetworkPayload::NetworkRequest(ref request) => {
                (network::NetworkPayload::NetworkRequest, serialize_request(&mut builder, request)?)
            }
            NetworkPayload::NetworkResponse(ref response) => (
                network::NetworkPayload::NetworkResponse,
                serialize_response(&mut builder, response)?,
            ),
        };

        let message_offset =
            network::NetworkMessage::create(&mut builder, &network::NetworkMessageArgs {
                timestamp: get_current_stamp(),
                payload_type,
                payload: Some(payload_offset),
            });

        network::finish_size_prefixed_network_message_buffer(&mut builder, message_offset);

        target.write_all(builder.finished_data()).map_err(Error::from)?;

        Ok(())
    }
}

// deserialization

fn _deserialize(buffer: &[u8]) -> Fallible<NetworkMessage> {
    if buffer.len() < 12 {
        bail!("the buffer is too small")
    }

    if !network::network_message_size_prefixed_buffer_has_identifier(buffer) {
        bail!("unrecognized protocol name")
    }

    let root = network::get_size_prefixed_root_as_network_message(buffer);

    let created = root.timestamp();

    let payload = match root.payload_type() {
        network::NetworkPayload::NetworkPacket => deserialize_packet(&root)?,
        network::NetworkPayload::NetworkRequest => deserialize_request(&root)?,
        network::NetworkPayload::NetworkResponse => deserialize_response(&root)?,
        _ => bail!("invalid network message payload type"),
    };

    Ok(NetworkMessage {
        created,
        received: Some(get_current_stamp()),
        payload,
    })
}

fn deserialize_packet(root: &network::NetworkMessage) -> Fallible<NetworkPayload> {
    let packet = if let Some(payload) = root.payload() {
        network::NetworkPacket::init_from_table(payload)
    } else {
        bail!("missing network message payload (expected a packet)")
    };

    let destination = if let Some(direct) = packet.destination_as_direct() {
        PacketDestination::Direct(P2PNodeId(direct.target_id()))
    } else if let Some(broadcast) = packet.destination_as_broadcast() {
        let ids_to_exclude = if let Some(ids) = broadcast.ids_to_exclude() {
            ids.safe_slice().iter().copied().map(P2PNodeId).collect()
        } else {
            Vec::new()
        };
        PacketDestination::Broadcast(ids_to_exclude)
    } else {
        bail!("invalid network packet type")
    };

    let network_id = NetworkId::from(packet.networkId());

    let payload = if let Some(payload) = packet.payload() {
        payload.to_vec()
    } else {
        bail!("missing packet payload")
    };

    Ok(NetworkPayload::NetworkPacket(NetworkPacket {
        destination,
        network_id,
        message: payload,
    }))
}

fn deserialize_request(root: &network::NetworkMessage) -> Fallible<NetworkPayload> {
    let request = if let Some(payload) = root.payload() {
        network::NetworkRequest::init_from_table(payload)
    } else {
        bail!("missing network message payload (expected a request)")
    };

    match request.variant() {
        network::RequestVariant::Ping => Ok(NetworkPayload::NetworkRequest(NetworkRequest::Ping)),
        network::RequestVariant::GetPeers => {
            if let Some(network_ids) = request
                .payload()
                .map(network::NetworkIds::init_from_table)
                .and_then(|payload| payload.ids())
            {
                let network_ids =
                    network_ids.safe_slice().iter().copied().map(NetworkId::from).collect();
                Ok(NetworkPayload::NetworkRequest(NetworkRequest::GetPeers(network_ids)))
            } else {
                bail!("missing network ids in a GetPeers request")
            }
        }
        network::RequestVariant::Handshake => {
            if let Some(handshake) = request.payload().map(network::Handshake::init_from_table) {
                let remote_id = P2PNodeId(handshake.nodeId());
                let remote_port = handshake.port();
                let networks = if let Some(networks) = handshake.networkIds() {
                    networks.safe_slice().iter().copied().map(NetworkId::from).collect()
                } else {
                    bail!("missing network ids in a Handshake")
                };

                let version = if let Some(version) = handshake.version() {
                    Version::parse(std::str::from_utf8(version)?)?
                } else {
                    bail!("missing network ids in a Handshake")
                };

                Ok(NetworkPayload::NetworkRequest(NetworkRequest::Handshake(Handshake {
                    remote_id,
                    remote_port,
                    networks,
                    version,
                    proof: Vec::new(),
                })))
            } else {
                bail!("missing handshake payload")
            }
        }
        network::RequestVariant::BanNode | network::RequestVariant::UnbanNode => {
            if let Some(id) = request.payload().map(network::BanUnban::init_from_table) {
                let tgt = if let Some(id) = id.id_as_node_id().map(|id| id.id()) {
                    BanId::NodeId(P2PNodeId(id))
                } else if let Some(ip) = id.id_as_ip_addr() {
                    if let Some(mut octets) = ip.octets() {
                        match ip.variant() {
                            network::IpVariant::V4 => {
                                let mut addr = [0u8; 4];
                                octets.read_exact(&mut addr)?;
                                BanId::Ip(IpAddr::from(addr))
                            }
                            network::IpVariant::V6 => {
                                let mut addr = [0u8; 16];
                                octets.read_exact(&mut addr)?;
                                BanId::Ip(IpAddr::from(addr))
                            }
                        }
                    } else {
                        bail!("missing ban ip in a ban/unban request")
                    }
                } else {
                    bail!("missing ban id/ip in a ban/unban request")
                };

                Ok(NetworkPayload::NetworkRequest(match request.variant() {
                    network::RequestVariant::BanNode => NetworkRequest::BanNode(tgt),
                    network::RequestVariant::UnbanNode => NetworkRequest::UnbanNode(tgt),
                    _ => unreachable!(),
                }))
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
                Ok(NetworkPayload::NetworkRequest(match request.variant() {
                    network::RequestVariant::JoinNetwork => NetworkRequest::JoinNetwork(id),
                    network::RequestVariant::LeaveNetwork => NetworkRequest::LeaveNetwork(id),
                    _ => unreachable!(),
                }))
            } else {
                bail!("missing network id in a join/leave network request")
            }
        }
    }
}

fn deserialize_response(root: &network::NetworkMessage) -> Fallible<NetworkPayload> {
    let response = if let Some(payload) = root.payload() {
        network::NetworkResponse::init_from_table(payload)
    } else {
        bail!("missing network message payload (expected a request)")
    };

    match response.variant() {
        network::ResponseVariant::Pong => {
            Ok(NetworkPayload::NetworkResponse(NetworkResponse::Pong))
        }
        network::ResponseVariant::PeerList => {
            if let Some(peers) = response.payload_as_peer_list().and_then(|peers| peers.peers()) {
                let mut list = Vec::with_capacity(peers.len());
                for i in 0..peers.len() {
                    let peer = peers.get(i);

                    let addr = if let Some(addr) = peer.addr() {
                        if let Some(mut ip) = addr.octets() {
                            match addr.variant() {
                                network::IpVariant::V4 => {
                                    let mut octets = [0u8; 4];
                                    ip.read_exact(&mut octets)?;
                                    SocketAddr::new(IpAddr::from(octets), peer.port())
                                }
                                network::IpVariant::V6 => {
                                    let mut octets = [0u8; 16];
                                    ip.read_exact(&mut octets)?;
                                    SocketAddr::new(IpAddr::from(octets), peer.port())
                                }
                            }
                        } else {
                            bail!("missing peer ip in a PeerList response")
                        }
                    } else {
                        bail!("missing peer address in a PeerList response")
                    };

                    let peer_type = match peer.variant() {
                        network::PeerVariant::Node => PeerType::Node,
                        network::PeerVariant::Bootstrapper => PeerType::Bootstrapper,
                    };

                    let peer = P2PPeer {
                        id: P2PNodeId(peer.id()),
                        addr,
                        peer_type,
                    };

                    list.push(peer);
                }

                Ok(NetworkPayload::NetworkResponse(NetworkResponse::PeerList(list)))
            } else {
                bail!("missing peers in a PeerList response")
            }
        }
    }
}

// serialization

fn serialize_packet(
    builder: &mut FlatBufferBuilder,
    packet: &NetworkPacket,
) -> io::Result<flatbuffers::WIPOffset<flatbuffers::UnionWIPOffset>> {
    let (destination_offset, destination_type) = match packet.destination {
        PacketDestination::Direct(target_id) => {
            let offset = network::Direct::create(builder, &network::DirectArgs {
                target_id: target_id.as_raw(),
            });

            (offset.as_union_value(), network::Destination::Direct)
        }
        PacketDestination::Broadcast(ref ids_to_exclude) => {
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

    let payload_offset = builder.create_vector_direct::<u8>(&packet.message);

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
        NetworkRequest::Ping => {
            (network::RequestVariant::Ping, network::RequestPayload::NONE, None)
        }
        NetworkRequest::GetPeers(nets) => {
            builder.start_vector::<u16>(nets.len());
            for net in nets {
                builder.push(net.id);
            }
            let nets_offset = Some(builder.end_vector(nets.len()));
            let offset = network::NetworkIds::create(builder, &network::NetworkIdsArgs {
                ids: nets_offset,
            });
            (
                network::RequestVariant::GetPeers,
                network::RequestPayload::NetworkIds,
                Some(offset.as_union_value()),
            )
        }
        NetworkRequest::Handshake(handshake) => {
            builder.start_vector::<u16>(handshake.networks.len());
            for net in &handshake.networks {
                builder.push(net.id);
            }
            let nets_offset = Some(builder.end_vector(handshake.networks.len()));

            let version = handshake.version.to_string().into_bytes();
            builder.start_vector::<u8>(version.len());
            for byte in version.iter().rev() {
                builder.push(*byte);
            }
            let version_offset = Some(builder.end_vector(version.len()));

            let offset = network::Handshake::create(builder, &network::HandshakeArgs {
                nodeId:     handshake.remote_id.as_raw(),
                port:       handshake.remote_port,
                networkIds: nets_offset,
                version:    version_offset,
                zk:         None,
            });
            (
                network::RequestVariant::Handshake,
                network::RequestPayload::Handshake,
                Some(offset.as_union_value()),
            )
        }
        NetworkRequest::BanNode(id) | NetworkRequest::UnbanNode(id) => {
            let ban_unban = match request {
                NetworkRequest::BanNode(_) => network::RequestVariant::BanNode,
                NetworkRequest::UnbanNode(_) => network::RequestVariant::UnbanNode,
                _ => unreachable!(),
            };

            let (id_type, id_offset) = match id {
                BanId::NodeId(id) => {
                    let offset = network::NodeId::create(builder, &network::NodeIdArgs {
                        id: id.as_raw(),
                    })
                    .as_union_value();

                    (network::BanId::NodeId, offset)
                }
                BanId::Ip(ip) => {
                    let (variant, octets) = match ip {
                        IpAddr::V4(ip) => (network::IpVariant::V4, ip.octets().to_vec()),
                        IpAddr::V6(ip) => (network::IpVariant::V6, ip.octets().to_vec()),
                    };

                    let octets_len = octets.len();
                    builder.start_vector::<u8>(octets_len);
                    for byte in octets.into_iter().rev() {
                        builder.push(byte);
                    }
                    let octets = Some(builder.end_vector(octets_len));

                    let ip_offset = network::IpAddr::create(builder, &network::IpAddrArgs {
                        variant,
                        octets,
                    });

                    (network::BanId::IpAddr, ip_offset.as_union_value())
                }
                _ => unimplemented!("Socket address bans don't propagate"),
            };
            let offset = network::BanUnban::create(builder, &network::BanUnbanArgs {
                id_type,
                id: Some(id_offset),
            });

            (ban_unban, network::RequestPayload::BanUnban, Some(offset.as_union_value()))
        }
        NetworkRequest::JoinNetwork(id) => {
            let offset = network::NetworkId::create(builder, &network::NetworkIdArgs {
                id: id.id,
            });
            (
                network::RequestVariant::JoinNetwork,
                network::RequestPayload::NetworkId,
                Some(offset.as_union_value()),
            )
        }
        NetworkRequest::LeaveNetwork(id) => {
            let offset = network::NetworkId::create(builder, &network::NetworkIdArgs {
                id: id.id,
            });
            (
                network::RequestVariant::LeaveNetwork,
                network::RequestPayload::NetworkId,
                Some(offset.as_union_value()),
            )
        }
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
        NetworkResponse::Pong => {
            (network::ResponseVariant::Pong, network::ResponsePayload::NONE, None)
        }
        NetworkResponse::PeerList(peerlist) => {
            let mut peers = Vec::with_capacity(peerlist.len());
            for peer in peerlist.iter() {
                let (variant, octets) = match peer.addr.ip() {
                    IpAddr::V4(ip) => (network::IpVariant::V4, ip.octets().to_vec()),
                    IpAddr::V6(ip) => (network::IpVariant::V6, ip.octets().to_vec()),
                };
                let octets_len = octets.len();
                builder.start_vector::<u8>(octets_len);
                for byte in octets.into_iter().rev() {
                    builder.push(byte);
                }
                let octets = Some(builder.end_vector(octets_len));

                let ip_offset = network::IpAddr::create(builder, &network::IpAddrArgs {
                    variant,
                    octets,
                });

                let peer_type = match peer.peer_type {
                    PeerType::Node => network::PeerVariant::Node,
                    PeerType::Bootstrapper => network::PeerVariant::Bootstrapper,
                };

                let peer = network::P2PPeer::create(builder, &network::P2PPeerArgs {
                    id:      peer.id.as_raw(),
                    addr:    Some(ip_offset),
                    port:    peer.addr.port(),
                    variant: peer_type,
                });
                peers.push(peer);
            }
            let peers_offset = Some(builder.create_vector(&peers));
            let offset = Some(
                network::PeerList::create(builder, &network::PeerListArgs {
                    peers: peers_offset,
                })
                .as_union_value(),
            );

            (network::ResponseVariant::PeerList, network::ResponsePayload::PeerList, offset)
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

#[cfg(test)]
mod tests {
    #[test]
    fn s11n_size_fbs() {
        use crate::test_utils::create_random_packet;

        let payload_size = 1000;
        let msg = create_random_packet(payload_size);
        let mut buffer = std::io::Cursor::new(Vec::with_capacity(payload_size));

        msg.serialize(&mut buffer).unwrap();
        println!("flatbuffers s11n ratio: {}", buffer.get_ref().len() as f64 / payload_size as f64);
    }
}
