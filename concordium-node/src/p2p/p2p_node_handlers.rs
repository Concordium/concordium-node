use crate::{
    common::P2PNodeId,
    connection::SeenMessagesList,
    network::{
        NetworkId, NetworkMessage, NetworkPacket, NetworkPacketType, NetworkRequest,
        NetworkResponse,
    },
    stats_export_service::StatsExportService,
};
use concordium_common::{functor::FuncResult, RelayOrStopSender, RelayOrStopSenderHelper};
use std::{
    collections::HashSet,
    ops::Deref,
    sync::{mpsc::Sender, Arc, Mutex, RwLock},
};

/// It forwards network response message into `queue`.
pub fn forward_network_response(
    res: &NetworkResponse,
    queue: &RelayOrStopSender<Arc<NetworkMessage>>,
) -> FuncResult<()> {
    let outer = Arc::new(NetworkMessage::NetworkResponse(res.to_owned(), None, None));

    if let Err(queue_error) = queue.send_msg(outer) {
        warn!("Message cannot be forwarded: {:?}", queue_error);
    };

    Ok(())
}

/// It forwards network request message into `packet_queue`
pub fn forward_network_request(
    req: &NetworkRequest,
    packet_queue: &RelayOrStopSender<Arc<NetworkMessage>>,
) -> FuncResult<()> {
    let outer = Arc::new(NetworkMessage::NetworkRequest(req.to_owned(), None, None));

    if let Err(e) = packet_queue.send_msg(outer) {
        warn!(
            "Network request cannot be forward by packet queue: {}",
            e.to_string()
        )
    }

    Ok(())
}

pub struct OutgoingQueues<'a> {
    /// Send_queue to other nodes
    pub send_queue: &'a Sender<Arc<NetworkMessage>>,
    /// Queue to super process (to bakers or db)
    pub queue_to_super: &'a RelayOrStopSender<Arc<NetworkMessage>>,
    /// Queue to the RPC subscription
    pub rpc_queue: &'a Mutex<Option<Sender<Arc<NetworkMessage>>>>,
}

/// It forwards network packet message into `packet_queue` if message id has not
/// been already seen and its `network id` belong to `own_networks`.
pub fn forward_network_packet_message<S: ::std::hash::BuildHasher>(
    own_id: P2PNodeId,
    seen_messages: &SeenMessagesList,
    stats_export_service: &Option<Arc<RwLock<StatsExportService>>>,
    own_networks: &Arc<RwLock<HashSet<NetworkId, S>>>,
    outgoing_queues: &OutgoingQueues,
    pac: &NetworkPacket,
    blind_trust_broadcast: bool,
) -> FuncResult<()> {
    let drop_msg = match pac.packet_type {
        NetworkPacketType::DirectMessage(..) => "Dropping duplicate direct packet",
        NetworkPacketType::BroadcastedMessage => "Dropping duplicate broadcast packet",
    };
    if !is_message_already_seen(seen_messages, pac, drop_msg) {
        forward_network_packet_message_common(
            own_id,
            seen_messages,
            stats_export_service,
            own_networks,
            outgoing_queues,
            pac,
            blind_trust_broadcast,
        )
    } else {
        Ok(())
    }
}

fn is_message_already_seen(
    seen_messages: &SeenMessagesList,
    pac: &NetworkPacket,
    drop_message: &str,
) -> bool {
    if seen_messages.contains(&pac.message_id) {
        trace!(
            "{} {}/{}/{}",
            drop_message,
            pac.peer.id().to_string(),
            pac.network_id,
            pac.message_id
        );
        true
    } else {
        false
    }
}

/// # TODO
/// Avoid to create a new packet instead of reusing it.
fn forward_network_packet_message_common<S: ::std::hash::BuildHasher>(
    own_id: P2PNodeId,
    seen_messages: &SeenMessagesList,
    stats_export_service: &Option<Arc<RwLock<StatsExportService>>>,
    own_networks: &Arc<RwLock<HashSet<NetworkId, S>>>,
    outgoing_queues: &OutgoingQueues,
    pac: &NetworkPacket,
    blind_trust_broadcast: bool,
) -> FuncResult<()> {
    trace!("Processing message for relaying");
    if safe_read!(own_networks)?.contains(&pac.network_id) {
        trace!(
            "Received message of size {} from {}",
            pac.message.len(),
            pac.peer.id()
        );
        let outer = Arc::new(NetworkMessage::NetworkPacket(pac.to_owned(), None, None));

        if seen_messages.append(&pac.message_id) {
            if blind_trust_broadcast {
                if let NetworkPacketType::BroadcastedMessage = pac.packet_type {
                    debug!(
                        "Peer {} rebroadcasting message {} from {}",
                        own_id,
                        pac.message_id,
                        pac.peer.id()
                    );
                    send_or_die!(outgoing_queues.send_queue, Arc::clone(&outer));
                    if let Some(ref service) = stats_export_service {
                        safe_write!(service)?.queue_size_inc();
                    };
                }
            }

            if let Ok(locked) = outgoing_queues.rpc_queue.lock() {
                if let Some(queue) = locked.deref() {
                    if let Err(e) = queue.send(outer.clone()) {
                        warn!(
                            "Can't send message on to the RPC outbound queue: {}",
                            e.to_string()
                        );
                    }
                }
            }

            if let Err(e) = outgoing_queues.queue_to_super.send_msg(outer.clone()) {
                warn!(
                    "Can't send message to the outer super queue: {}",
                    e.to_string()
                );
            }
        }
    } else if let Some(ref service) = stats_export_service {
        safe_write!(service)?.invalid_network_pkts_received_inc();
    }
    Ok(())
}
