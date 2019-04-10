use std::{
    collections::{HashSet, VecDeque},
    sync::{mpsc::Sender, Arc, RwLock},
};

use crate::{
    common::functor::{FunctorError, FunctorResult},
    connection::SeenMessagesList,
    network::{NetworkMessage, NetworkPacket, NetworkPacketType, NetworkRequest, NetworkResponse},
    prometheus_exporter::PrometheusServer,
};
use failure::err_msg;

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
pub fn forward_network_packet_message(
    seen_messages: &SeenMessagesList,
    prometheus_exporter: &Option<Arc<RwLock<PrometheusServer>>>,
    own_networks: HashSet<u16>,
    send_queue: &RwLock<VecDeque<Arc<NetworkMessage>>>,
    packet_queue: &Sender<Arc<NetworkMessage>>,
    pac: &NetworkPacket,
    blind_trust_broadcast: bool,
) -> FunctorResult {
    match pac.packet_type {
        NetworkPacketType::DirectMessage(..) => forward_network_packet_message_common(
            seen_messages,
            prometheus_exporter,
            own_networks,
            send_queue,
            packet_queue,
            pac,
            "Dropping duplicate direct packet",
            blind_trust_broadcast,
        ),
        NetworkPacketType::BroadcastedMessage => forward_network_packet_message_common(
            seen_messages,
            prometheus_exporter,
            own_networks,
            send_queue,
            packet_queue,
            pac,
            "Dropping duplicate broadcast packet",
            blind_trust_broadcast,
        ),
    }
}

/// It returns a `FunctorRunningError` with the specific message.
fn make_fn_err(e: &'static str) -> FunctorError { FunctorError::new(vec![err_msg(e)]) }

fn make_fn_error_prometheus() -> FunctorError { make_fn_err("Prometheus has failed") }

/// # TODO
/// Avoid to create a new packet instead of reusing it.
fn forward_network_packet_message_common(
    seen_messages: &SeenMessagesList,
    prometheus_exporter: &Option<Arc<RwLock<PrometheusServer>>>,
    own_networks: HashSet<u16>,
    send_queue: &RwLock<VecDeque<Arc<NetworkMessage>>>,
    packet_queue: &Sender<Arc<NetworkMessage>>,
    pac: &NetworkPacket,
    drop_message: &str,
    blind_trust_broadcast: bool,
) -> FunctorResult {
    debug!("### Forward Broadcast Message: msgid: {}", pac.message_id);
    if !seen_messages.contains(&pac.message_id) {
        if own_networks.contains(&pac.network_id) {
            debug!("Received direct message of size {}", pac.message.len());
            let outer = Arc::new(NetworkMessage::NetworkPacket(pac.to_owned(), None, None));

            seen_messages.append(&pac.message_id);
            if blind_trust_broadcast {
                if let NetworkPacketType::BroadcastedMessage = pac.packet_type {
                    safe_write!(send_queue)?.push_back(Arc::clone(&outer));
                    if let Some(ref prom) = prometheus_exporter {
                        safe_write!(prom)?
                            .queue_size_inc()
                            .map_err(|_| make_fn_error_prometheus())?;
                    };
                }
            }

            if let Err(e) = packet_queue.send(outer) {
                warn!(
                    "Forward network packet cannot be sent by incoming_pkts: {}",
                    e.to_string()
                );
            }
        } else {
            if let Some(ref prom) = prometheus_exporter {
                safe_write!(prom)?
                    .invalid_network_pkts_received_inc()
                    .map_err(|e| error!("{}", e))
                    .ok();
            }
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
