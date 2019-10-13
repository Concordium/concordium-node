use failure::Fallible;
use flatbuffers::FlatBufferBuilder;

use crate::flatbuffers_shim::network;

use crate::{
    common::{get_current_stamp, P2PNodeId},
    network::{NetworkId, NetworkMessage, NetworkMessagePayload, NetworkPacket, NetworkPacketType},
};
use concordium_common::hybrid_buf::HybridBuf;

use std::{
    convert::TryFrom,
    io::{self, Write},
};

pub fn deserialize(buffer: &[u8]) -> Fallible<NetworkMessage> {
    let root = network::get_size_prefixed_root_as_network_message(buffer);

    let timestamp1 = Some(root.timestamp());

    let payload = match root.payload_type() {
        network::NetworkMessagePayload::NetworkPacket => {
            let packet = if let Some(payload) = root.payload() {
                network::NetworkPacket::init_from_table(payload)
            } else {
                bail!("missing network message payload")
            };

            let packet_type = if let Some(direct) = packet.packetType_as_direct_message() {
                NetworkPacketType::DirectMessage(P2PNodeId(direct.target()))
            } else if let Some(broadcast) = packet.packetType_as_broadcast_message() {
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

            let message = if let Some(message) = packet.message() {
                HybridBuf::try_from(message)?
            } else {
                bail!("missing packet payload")
            };

            NetworkMessagePayload::NetworkPacket(NetworkPacket {
                packet_type,
                network_id,
                message,
            })
        }
        _ => bail!("Invalid network message payload type"),
    };

    Ok(NetworkMessage {
        timestamp1,
        timestamp2: Some(get_current_stamp()),
        payload,
    })
}

pub fn serialize<T: Write>(source: &mut NetworkMessage, target: &mut T) -> io::Result<()> {
    let capacity = if let NetworkMessagePayload::NetworkPacket(ref packet) = source.payload {
        packet.message.len()? as usize + 64 // FIXME: fine-tune the overhead
    } else {
        256
    };
    let mut builder = FlatBufferBuilder::new_with_capacity(capacity);

    let (payload_offset, payload_type) = match source.payload {
        NetworkMessagePayload::NetworkPacket(ref mut packet) => {
            let (packet_type_offset, packet_type) = match packet.packet_type {
                NetworkPacketType::DirectMessage(target) => {
                    let offset =
                        network::DirectMessage::create(&mut builder, &network::DirectMessageArgs {
                            target: target.as_raw(),
                        });

                    (offset.as_union_value(), network::PacketType::DirectMessage)
                }
                NetworkPacketType::BroadcastedMessage(ref ids_to_exclude) => {
                    builder.start_vector::<u64>(ids_to_exclude.len());
                    for id in ids_to_exclude {
                        builder.push(id.as_raw());
                    }
                    let ids_vec_offset = builder.end_vector(ids_to_exclude.len());
                    let offset = network::BroadcastMessage::create(
                        &mut builder,
                        &network::BroadcastMessageArgs {
                            ids_to_exclude: Some(ids_vec_offset),
                        },
                    );

                    (
                        offset.as_union_value(),
                        network::PacketType::BroadcastMessage,
                    )
                }
            };

            let message_offset =
                builder.create_vector_direct::<u8>(&packet.message.remaining_bytes()?);

            let packet_offset =
                network::NetworkPacket::create(&mut builder, &network::NetworkPacketArgs {
                    packetType_type: packet_type,
                    packetType:      Some(packet_type_offset),
                    networkId:       packet.network_id.id,
                    message:         Some(message_offset),
                })
                .as_union_value();

            (packet_offset, network::NetworkMessagePayload::NetworkPacket)
        }
        _ => unimplemented!(),
    };

    let message_offset =
        network::NetworkMessage::create(&mut builder, &network::NetworkMessageArgs {
            timestamp: get_current_stamp(),
            payload_type,
            payload: Some(payload_offset),
        });

    network::finish_size_prefixed_network_message_buffer(&mut builder, message_offset);

    target.write_all(builder.finished_data())
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::test_utils::create_random_packet;

    #[test]
    fn fbs_serde() {
        let mut network_message = create_random_packet(4096);
        let mut buffer = Vec::new();

        serialize(&mut network_message, &mut buffer).unwrap();
        deserialize(&buffer).unwrap();
    }

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
}
