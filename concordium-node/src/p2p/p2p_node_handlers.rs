use crate::network::{NetworkId, NetworkMessage, NetworkPacket, NetworkRequest, NetworkResponse};
use concordium_common::{
    functor::FuncResult, stats_export_service::StatsExportService, RelayOrStopSenderHelper,
    RelayOrStopSyncSender,
};
use std::{
    collections::HashSet,
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::SyncSender,
        Arc, RwLock,
    },
};

/// It forwards network response message into `queue`.
pub fn forward_network_response(
    res: &NetworkResponse,
    queue: RelayOrStopSyncSender<NetworkMessage>,
) -> FuncResult<()> {
    let outer = NetworkMessage::NetworkResponse(res.to_owned(), None, None);

    if let Err(queue_error) = queue.send_msg(outer) {
        warn!("Message cannot be forwarded: {:?}", queue_error);
    };

    Ok(())
}

/// It forwards network request message into `packet_queue`
pub fn forward_network_request(
    req: &NetworkRequest,
    packet_queue: &RelayOrStopSyncSender<NetworkMessage>,
) -> FuncResult<()> {
    let outer = NetworkMessage::NetworkRequest(req.to_owned(), None, None);

    if let Err(e) = packet_queue.send_msg(outer) {
        warn!(
            "Network request cannot be forward by packet queue: {}",
            e.to_string()
        )
    }

    Ok(())
}

#[derive(Clone)]
pub struct OutgoingQueues {
    /// Send_queue to other nodes
    pub send_queue: SyncSender<NetworkMessage>,
    /// Queue to super process (to bakers or db)
    pub queue_to_super: RelayOrStopSyncSender<NetworkMessage>,
    /// Queue to the RPC subscription
    pub rpc_queue: SyncSender<NetworkMessage>,
}

/// It forwards network packet message into `packet_queue` if message id has not
/// been already seen and its `network id` belong to `own_networks`.
/// # TODO
/// Avoid to create a new packet instead of reusing it.
pub fn forward_network_packet_message<S: ::std::hash::BuildHasher>(
    stats_export_service: Option<StatsExportService>,
    own_networks: Arc<RwLock<HashSet<NetworkId, S>>>,
    outgoing_queues: OutgoingQueues,
    pac: Arc<NetworkPacket>,
    is_rpc_online: Arc<AtomicBool>,
) -> FuncResult<()> {
    trace!("Processing message for relaying");
    if safe_read!(own_networks)?.contains(&pac.network_id) {
        trace!(
            "Received message of size {} from {}",
            pac.message.len()?,
            pac.peer.id()
        );
        let outer = NetworkMessage::NetworkPacket(pac, None, None);

        if is_rpc_online.load(Ordering::Relaxed) {
            if let Err(e) = outgoing_queues.rpc_queue.send(outer.clone()) {
                warn!(
                    "Can't relay a message to the RPC outbound queue: {}",
                    e.to_string()
                );
            }
        }

        if let Err(e) = outgoing_queues.queue_to_super.send_msg(outer.clone()) {
            warn!(
                "Can't relay a message on to the outer super queue: {}",
                e.to_string()
            );
        }
    } else if let Some(ref service) = stats_export_service {
        service.invalid_network_pkts_received_inc();
    }
    Ok(())
}
