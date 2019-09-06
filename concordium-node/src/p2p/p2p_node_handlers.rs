use crate::network::{NetworkId, NetworkMessage};
use concordium_common::{
    functor::FuncResult, stats_export_service::StatsExportService, RelayOrStopSenderHelper,
    RelayOrStopSyncSender,
};
use std::{
    collections::HashSet,
    hash::BuildHasher,
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::SyncSender,
        Arc, RwLock,
    },
};

#[derive(Clone)]
pub struct OutgoingQueues {
    /// The packet queue
    pub packet_queue: RelayOrStopSyncSender<NetworkMessage>,
    /// Queue to super process (to bakers or db)
    pub queue_to_super: RelayOrStopSyncSender<NetworkMessage>,
    /// Queue to the RPC subscription
    pub rpc_queue: SyncSender<NetworkMessage>,
}

/// It forwards network message into `queue`.
pub fn forward_network_message<S: BuildHasher>(
    msg: &NetworkMessage,
    stats_export_service: Option<StatsExportService>,
    own_networks: Arc<RwLock<HashSet<NetworkId, S>>>,
    outgoing_queues: OutgoingQueues,
    is_rpc_online: Arc<AtomicBool>,
) -> FuncResult<()> {
    match msg {
        NetworkMessage::NetworkResponse(msg, ..) => {
            let msg = NetworkMessage::NetworkResponse(msg.to_owned(), None, None);

            if let Err(queue_error) = outgoing_queues.packet_queue.send_msg(msg) {
                warn!("Message cannot be forwarded: {:?}", queue_error);
            };
        }
        NetworkMessage::NetworkRequest(msg, ..) => {
            let msg = NetworkMessage::NetworkRequest(msg.to_owned(), None, None);

            if let Err(queue_error) = outgoing_queues.packet_queue.send_msg(msg) {
                warn!("Message cannot be forwarded: {:?}", queue_error);
            };
        }
        NetworkMessage::NetworkPacket(pac, ..) => {
            trace!("Processing message for relaying");
            if safe_read!(own_networks)?.contains(&pac.network_id) {
                trace!(
                    "Received message of size {} from {}",
                    pac.message.len()?,
                    pac.peer.id()
                );
                let outer = NetworkMessage::NetworkPacket(Arc::clone(&pac), None, None);

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
        }
        _ => {}
    };

    Ok(())
}
