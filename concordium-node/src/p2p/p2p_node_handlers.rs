use std::sync::{ Arc, Mutex };
use std::sync::mpsc::{ Sender };
use std::collections::{ VecDeque };

use common::{ P2PPeer };
use common::functor::{ FunctorResult };
use network::{ NetworkRequest, NetworkMessage, NetworkPacket, NetworkResponse };
use prometheus_exporter::PrometheusServer;
use connection::{ SeenMessagesList };
use errors::{ ErrorWrapper, ErrorKindWrapper };

macro_rules! wrap_error {
    ($src_error:ident) => {
        ErrorWrapper::with_chain(
            $src_error,
            ErrorKindWrapper::FunctorRunningError( "Couldn't send to packet queue"))
    }
}

/// It forwards network response message into `queue`.
pub fn forward_network_response(
        res: &NetworkResponse,
        queue: &Sender<Arc<Box<NetworkMessage>>> ) -> FunctorResult {
    let outer = Arc::new( box NetworkMessage::NetworkResponse( res.clone(), None, None));

    queue.send(outer).map_err( |e| { wrap_error!(e) })
}

/// It forwards network request message into `packet_queue`
pub fn forward_network_request(
        req: &NetworkRequest,
        packet_queue: &Sender<Arc<Box<NetworkMessage>>> ) -> FunctorResult {
    let cloned_req = req.clone();
    let outer = Arc::new( box NetworkMessage::NetworkRequest( cloned_req, None, None));

    match packet_queue.send( outer) {
        Err(e) => warn!( "Network request cannot be forward by packet queue: {}", e.to_string()),
        _ => {}
    };

    Ok(())
}

/// It forwards network packet message into `packet_queue` if message id has not been already
/// seen and its `network id` belong to `own_networks`.
pub fn forward_network_packet_message(
        seen_messages: &SeenMessagesList,
        prometheus_exporter: &Option<Arc<Mutex<PrometheusServer>>>,
        own_networks: &Arc<Mutex<Vec<u16>>>,
        send_queue: &Arc<Mutex<VecDeque<Arc<Box<NetworkMessage>>>>>,
        packet_queue: &Sender<Arc<Box<NetworkMessage>>>,
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
fn make_fn_err( e: &'static str) -> ErrorWrapper {
    ErrorWrapper::from_kind(
        ErrorKindWrapper::FunctorRunningError( e))
}

fn make_fn_error_prometheus() -> ErrorWrapper {
    make_fn_err( "Prometheus has faild")
}

/// # TODO
/// Avoid to create a new packet instead of reusing it.
fn forward_network_packet_message_common(
        seen_messages: &SeenMessagesList,
        prometheus_exporter: &Option<Arc<Mutex<PrometheusServer>>>,
        own_networks: &Arc<Mutex<Vec<u16>>>,
        send_queue: &Arc<Mutex<VecDeque<Arc<Box<NetworkMessage>>>>>,
        packet_queue: &Sender<Arc<Box<NetworkMessage>>>,
        pac: &NetworkPacket,
        sender: &P2PPeer,
        msg_id: &String,
        network_id: &u16,
        msg: &Vec<u8>,
        drop_message: &str,
        blind_trust_broadcast: bool,
        ) -> FunctorResult {

    debug!("### Forward Broadcast Message: msgid: {}", msg_id);
    if !seen_messages.contains(msg_id) {
        if own_networks.lock()?.contains(network_id) {
            debug!("Received direct message of size {}", msg.len());
            let outer = Arc::new( box NetworkMessage::NetworkPacket( pac.clone(), None, None));

            seen_messages.append(&msg_id);
            if blind_trust_broadcast {
                if let NetworkPacket::BroadcastedMessage(_,_,_,_) = pac {
                    
                    send_queue.lock()?.push_back( outer.clone());
                    if let Some(ref prom) = prometheus_exporter {
                        prom.lock()?.queue_size_inc()
                            .map_err(|_| make_fn_error_prometheus())?;
                    };
                }
            }

            if let Err(e) = packet_queue.send(outer) {
                warn!( "Forward network packet cannot be sent by incoming_pkts: {}", e.to_string());
            }
        } else {
            if let Some(ref prom) = prometheus_exporter {
                prom.lock()?.invalid_network_pkts_received_inc()
                    .map_err(|e| error!("{}", e)).ok();
            }
        }
    } else {
        info!( "{} {}/{}/{}", drop_message, sender.id().to_string(), network_id, msg_id);
    }

    Ok(())
}
