use std::{
    collections::{HashSet, VecDeque},
    sync::{mpsc::Sender, Arc, RwLock},
};

use crate::{
    common::functor::FunctorResult,
    connection::SeenMessagesList,
    network::{
        NetworkId, NetworkMessage, NetworkPacket, NetworkPacketType, NetworkRequest,
        NetworkResponse,
    },
    stats_export_service::StatsExportService,
};

/// It forwards network response message into `queue`.
pub fn forward_network_response(
    res: &NetworkResponse,
    queue: &Sender<Arc<NetworkMessage>>,
) -> FunctorResult {
    let outer = Arc::new(NetworkMessage::NetworkResponse(res.to_owned(), None, None));

    if let Err(queue_error) = queue.send(outer) {
        warn!("Message cannot be forwarded: {:?}", queue_error);
    };

    Ok(())
}

/// It forwards network request message into `packet_queue`
pub fn forward_network_request(
    req: &NetworkRequest,
    packet_queue: &Sender<Arc<NetworkMessage>>,
) -> FunctorResult {
    let outer = Arc::new(NetworkMessage::NetworkRequest(req.to_owned(), None, None));

    if let Err(e) = packet_queue.send(outer) {
        warn!(
            "Network request cannot be forward by packet queue: {}",
            e.to_string()
        )
    }

    Ok(())
}

/// It forwards network packet message into `packet_queue` if message id has not
/// been already seen and its `network id` belong to `own_networks`.
pub fn forward_network_packet_message<S: ::std::hash::BuildHasher>(
    seen_messages: &SeenMessagesList,
    stats_export_service: &Option<Arc<RwLock<StatsExportService>>>,
    own_networks: &Arc<RwLock<HashSet<NetworkId, S>>>,
    send_queue: &RwLock<VecDeque<Arc<NetworkMessage>>>,
    packet_queue: &Sender<Arc<NetworkMessage>>,
    pac: &NetworkPacket,
    blind_trust_broadcast: bool,
) -> FunctorResult {
    match pac.packet_type {
        NetworkPacketType::DirectMessage(..) => forward_network_packet_message_common(
            seen_messages,
            stats_export_service,
            own_networks,
            send_queue,
            packet_queue,
            pac,
            "Dropping duplicate direct packet",
            blind_trust_broadcast,
        ),
        NetworkPacketType::BroadcastedMessage => forward_network_packet_message_common(
            seen_messages,
            stats_export_service,
            own_networks,
            send_queue,
            packet_queue,
            pac,
            "Dropping duplicate broadcast packet",
            blind_trust_broadcast,
        ),
    }
}

/// # TODO
/// Avoid to create a new packet instead of reusing it.
fn forward_network_packet_message_common<S: ::std::hash::BuildHasher>(
    seen_messages: &SeenMessagesList,
    stats_export_service: &Option<Arc<RwLock<StatsExportService>>>,
    own_networks: &Arc<RwLock<HashSet<NetworkId, S>>>,
    send_queue: &RwLock<VecDeque<Arc<NetworkMessage>>>,
    packet_queue: &Sender<Arc<NetworkMessage>>,
    pac: &NetworkPacket,
    drop_message: &str,
    blind_trust_broadcast: bool,
) -> FunctorResult {
    debug!("### Forward Broadcast Message: msgid: {}", pac.message_id);
    if !seen_messages.contains(&pac.message_id) {
        if safe_read!(own_networks)?.contains(&pac.network_id) {
            debug!("Received direct message of size {}", pac.message.len());
            let outer = Arc::new(NetworkMessage::NetworkPacket(pac.to_owned(), None, None));

            seen_messages.append(&pac.message_id);
            if blind_trust_broadcast {
                if let NetworkPacketType::BroadcastedMessage = pac.packet_type {
                    safe_write!(send_queue)?.push_back(Arc::clone(&outer));
                    if let Some(ref service) = stats_export_service {
                        safe_write!(service)?.queue_size_inc();
                    };
                }
            }

            if let Err(e) = packet_queue.send(outer) {
                warn!(
                    "Forward network packet cannot be sent by incoming_pkts: {}",
                    e.to_string()
                );
            }
        } else if let Some(ref service) = stats_export_service {
            safe_write!(service)?.invalid_network_pkts_received_inc();
        }
    } else {
        info!(
            "{} {}/{}/{}",
            drop_message,
            pac.peer.id().to_string(),
            pac.network_id,
            pac.message_id
        );
    }

    Ok(())
}
