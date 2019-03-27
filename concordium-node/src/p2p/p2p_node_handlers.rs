use std::sync::{ Arc, RwLock };
use std::sync::mpsc::{ Sender };
use std::collections::{ VecDeque };

use crate::common::{ P2PPeer };
use crate::common::functor::{ FunctorResult, fails as functor_fails };
use crate::network::{ NetworkRequest, NetworkMessage, NetworkPacket, NetworkResponse };
use crate::prometheus_exporter::PrometheusServer;
use crate::connection::{ SeenMessagesList };
use failure::{err_msg};


/// It forwards network response message into `queue`.
pub fn forward_network_response(
        res: &NetworkResponse,
        queue: &Sender<Arc<NetworkMessage>> ) -> FunctorResult {
    let outer = Arc::new(NetworkMessage::NetworkResponse( res.clone(), None, None));

    if let Err(queue_error) = queue.send(outer) {
        warn!( "Message cannot be forwarded: {:?}", queue_error);
    };

    Ok(())
}

/// It forwards network request message into `packet_queue`
pub fn forward_network_request(
        req: &NetworkRequest,
        packet_queue: &Sender<Arc<NetworkMessage>> ) -> FunctorResult {
    let cloned_req = req.clone();
    let outer = Arc::new(NetworkMessage::NetworkRequest( cloned_req, None, None));

    if let Err(e) = packet_queue.send(outer) {
        warn!("Network request cannot be forward by packet queue: {}", e.to_string())
    }

    Ok(())
}

/// It forwards network packet message into `packet_queue` if message id has not been already
/// seen and its `network id` belong to `own_networks`.
pub fn forward_network_packet_message(
        seen_messages: &SeenMessagesList,
        prometheus_exporter: &Option<Arc<RwLock<PrometheusServer>>>,
        own_networks: &Arc<RwLock<Vec<u16>>>,
        send_queue: &Arc<RwLock<VecDeque<Arc<NetworkMessage>>>>,
        packet_queue: &Sender<Arc<NetworkMessage>>,
        pac: &NetworkPacket,
        blind_trust_broadcast: bool,) -> FunctorResult {

    match pac {
        NetworkPacket::DirectMessage( ref sender, ref msg_id, _, ref network_id, ref msg) =>{
            forward_network_packet_message_common(
                seen_messages, prometheus_exporter, own_networks, send_queue, packet_queue,
                pac, sender, msg_id, network_id, msg,
                "Dropping duplicate direct packet", blind_trust_broadcast)
        },
        NetworkPacket::BroadcastedMessage(ref sender, ref msg_id, ref network_id, ref msg) => {
            forward_network_packet_message_common(
                seen_messages, prometheus_exporter, own_networks, send_queue, packet_queue,
                pac, sender, msg_id, network_id, msg,
                "Dropping duplicate broadcast packet", blind_trust_broadcast)
        }
    }
}

/// It returns a `FunctorRunningError` with the specific message.
fn make_fn_err( e: &'static str) -> functor_fails::FunctorError {
    functor_fails::FunctorError::new(vec![err_msg(e)])
}

fn make_fn_error_prometheus() -> functor_fails::FunctorError {
    make_fn_err( "Prometheus has failed")
}

/// # TODO
/// Avoid to create a new packet instead of reusing it.
fn forward_network_packet_message_common(
        seen_messages: &SeenMessagesList,
        prometheus_exporter: &Option<Arc<RwLock<PrometheusServer>>>,
        own_networks: &Arc<RwLock<Vec<u16>>>,
        send_queue: &Arc<RwLock<VecDeque<Arc<NetworkMessage>>>>,
        packet_queue: &Sender<Arc<NetworkMessage>>,
        pac: &NetworkPacket,
        sender: &P2PPeer,
        msg_id: &String,
        network_id: &u16,
        msg: &[u8],
        drop_message: &str,
        blind_trust_broadcast: bool,
        ) -> FunctorResult {

    debug!("### Forward Broadcast Message: msgid: {}", msg_id);
    if !seen_messages.contains(msg_id) {
        if safe_read!(own_networks)?
            .contains(network_id) {
            debug!("Received direct message of size {}", msg.len());
            let outer = Arc::new(NetworkMessage::NetworkPacket( pac.clone(), None, None));

            seen_messages.append(&msg_id);
            if blind_trust_broadcast {
                if let NetworkPacket::BroadcastedMessage(..) = pac {

                    safe_write!(send_queue)?
                        .push_back( outer.clone());
                    if let Some(ref prom) = prometheus_exporter {
                        safe_write!(prom)?
                            .queue_size_inc()
                            .map_err(|_| make_fn_error_prometheus())?;
                    };
                }
            }

            if let Err(e) = packet_queue.send(outer) {
                warn!( "Forward network packet cannot be sent by incoming_pkts: {}", e.to_string());
            }
        } else {
            if let Some(ref prom) = prometheus_exporter {
                safe_write!(prom)?
                    .invalid_network_pkts_received_inc()
                    .map_err(|e| error!("{}", e)).ok();
            }
        }
    } else {
        info!( "{} {}/{}/{}", drop_message, sender.id().to_string(), network_id, msg_id);
    }

    Ok(())
}
