use failure::Fallible;
use flatbuffers::FlatBufferBuilder;

#[path = "../../schema_generated.rs"]
mod schema_generated;
use schema_generated::network;

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
    let root = network::get_root_as_network_message(buffer);

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
    let mut builder = FlatBufferBuilder::new_with_capacity(4096);

    let payload_offset = match source.payload {
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

            let message_offset = builder.create_vector::<u8>(&packet.message.remaining_bytes()?);

            let mut packet_builder = network::NetworkPacketBuilder::new(&mut builder);
            packet_builder.add_packetType_type(packet_type);
            packet_builder.add_packetType(packet_type_offset);
            packet_builder.add_networkId(packet.network_id.id);
            packet_builder.add_message(message_offset);
            packet_builder.finish().as_union_value()
        }
        _ => unimplemented!(),
    };

    let payload_type = match source.payload {
        NetworkMessagePayload::NetworkPacket(_) => network::NetworkMessagePayload::NetworkPacket,
        _ => unimplemented!(),
    };

    let mut message_builder = network::NetworkMessageBuilder::new(&mut builder);
    message_builder.add_timestamp(get_current_stamp());
    message_builder.add_payload_type(payload_type);
    message_builder.add_payload(payload_offset);
    let offset = message_builder.finish();

    network::finish_network_message_buffer(&mut builder, offset);

    target.write(builder.finished_data()).map(|_| ())
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
}
