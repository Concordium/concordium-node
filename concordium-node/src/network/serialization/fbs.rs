//! Flatbuffers serialization.

use failure::{Error, Fallible};
use flatbuffers::FlatBufferBuilder;
use semver::Version;

use crate::{consensus_ffi::blockchain_types::BlockHash, flatbuffers_shim::network};

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
};

use std::{
    io::{self, Read, Write},
    net::{IpAddr, SocketAddr},
    panic,
};

/// The HANDSHAKE message version. In order to make the handshake robust, we
/// need to version the message itself. Higher versions are assumed to append
/// new fields at the end of the message so it should be still deserializable
/// even if the new fields are not understood, but a warning will be emitted.
pub const HANDSHAKE_MESSAGE_VERSION: u8 = 0;

impl NetworkMessage {
    // FIXME: remove the unwind once the verifier is available
    pub fn deserialize(buffer: &[u8]) -> Fallible<Self> {
        match panic::catch_unwind(|| _deserialize(buffer)) {
            Ok(msg) => msg,
            Err(_) => bail!("caught a panic: received a mangled buffer"),
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

    let destination = if let Some(destination) = packet.destination() {
        match destination.variant() {
            network::Direction::Direct => {
                PacketDestination::Direct(P2PNodeId(destination.target()))
            }
            network::Direction::Broadcast => PacketDestination::Broadcast(Vec::new()),
        }
    } else {
        bail!("missing direction on network packet");
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
                if handshake.version() != HANDSHAKE_MESSAGE_VERSION {
                    warn!(
                        "Received handshake version ({}) is higher than our version ({}). \
                         Attempting to parse.",
                        handshake.version(),
                        HANDSHAKE_MESSAGE_VERSION
                    );
                }
                let remote_id = P2PNodeId(handshake.nodeId());
                let remote_port = handshake.port();
                let networks = if let Some(networks) = handshake.networkIds() {
                    networks.safe_slice().iter().copied().map(NetworkId::from).collect()
                } else {
                    bail!("missing network ids in a Handshake")
                };

                let node_version =
                    if let Some(node_version) = handshake.nodeVersion().and_then(|x| x.version()) {
                        Version::parse(std::str::from_utf8(node_version)?)?
                    } else {
                        bail!("missing node version in a Handshake")
                    };

                let wire_versions = if let Some(wire_versions) = handshake.wireVersions() {
                    wire_versions.to_vec()
                } else {
                    bail!("missing wire version in a Handshake")
                };

                let genesis_blocks = if let Some(genesis_blocks) = handshake.genesisBlocks() {
                    genesis_blocks
                        .iter()
                        .map(|wv| {
                            wv.genesisBlock().map_or_else(
                                || bail!("Missing block hash"),
                                |w| Ok(BlockHash::new(w)),
                            )
                        })
                        .collect::<Result<Vec<BlockHash>, failure::Error>>()?
                } else {
                    bail!("missing genesis blocks in a Handshake")
                };

                Ok(NetworkPayload::NetworkRequest(NetworkRequest::Handshake(Handshake {
                    remote_id,
                    remote_port,
                    networks,
                    node_version,
                    wire_versions,
                    genesis_blocks,
                    proof: Vec::new(),
                })))
            } else {
                bail!("missing handshake payload")
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
    let destination_offset = match packet.destination {
        PacketDestination::Direct(target_id) => {
            network::Destination::create(builder, &network::DestinationArgs {
                variant: network::Direction::Direct,
                target:  target_id.as_raw(),
            })
        }
        PacketDestination::Broadcast(..) => {
            network::Destination::create(builder, &network::DestinationArgs {
                variant: network::Direction::Broadcast,
                target:  Default::default(),
            })
        }
    };

    let payload_offset = builder.create_vector_direct::<u8>(&packet.message);

    let packet_offset = network::NetworkPacket::create(builder, &network::NetworkPacketArgs {
        destination: Some(destination_offset),
        networkId:   packet.network_id.id,
        payload:     Some(payload_offset),
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

            let node_version = handshake.node_version.to_string().into_bytes();
            builder.start_vector::<u8>(node_version.len());
            for byte in node_version.iter().rev() {
                builder.push(*byte);
            }
            let node_version = Some(builder.end_vector(node_version.len()));
            let node_version_offset = network::Version::create(builder, &network::VersionArgs {
                version: node_version,
            });

            builder.start_vector::<u8>(handshake.wire_versions.len());
            for byte in handshake.wire_versions.iter().rev() {
                builder.push(*byte);
            }
            let wire_version_offset = Some(builder.end_vector(handshake.wire_versions.len()));

            let genesis_blocks = handshake
                .genesis_blocks
                .iter()
                .map(|x| {
                    builder.start_vector::<u8>(32);
                    for byte in x.iter().rev() {
                        builder.push(*byte);
                    }
                    let block_offset = Some(builder.end_vector(32));
                    network::BlockHash::create(builder, &network::BlockHashArgs {
                        genesisBlock: block_offset,
                    })
                })
                .collect::<Vec<flatbuffers::WIPOffset<network::BlockHash>>>();
            builder
                .start_vector::<flatbuffers::WIPOffset<network::BlockHash>>(genesis_blocks.len());
            for offset in genesis_blocks.iter() {
                builder.push(*offset);
            }
            let genesis_blocks_offset = Some(builder.end_vector(genesis_blocks.len()));

            let offset = network::Handshake::create(builder, &network::HandshakeArgs {
                version:       0,
                nodeId:        handshake.remote_id.as_raw(),
                port:          handshake.remote_port,
                networkIds:    nets_offset,
                nodeVersion:   Some(node_version_offset),
                wireVersions:  wire_version_offset,
                genesisBlocks: genesis_blocks_offset,
                zk:            None,
            });
            (
                network::RequestVariant::Handshake,
                network::RequestPayload::Handshake,
                Some(offset.as_union_value()),
            )
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
