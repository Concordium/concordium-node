use std::rc::{ Rc };
use std::sync::{ Arc, Mutex, RwLock };
use std::sync::atomic::{ AtomicU64, Ordering };
use byteorder::{ NetworkEndian,  WriteBytesExt };
use atomic_counter::AtomicCounter;

use rustls::{ Session };
use prometheus_exporter::{ PrometheusServer };

use common::{ P2PPeer, P2PNodeId, get_current_stamp };
use common::counter::{ TOTAL_MESSAGES_SENT_COUNTER };
use network::{  NetworkRequest, NetworkResponse, Buckets };
use connection::{ ConnSession,  P2PNodeMode };
use connection::parse_handler::{ ParseCallbackResult };
use errors::*;

fn serialize_bytes( session: &Arc< RwLock<dyn Session>>, pkt: &[u8]) -> ParseCallbackResult {
    // Write size of pkt into 4 bytes vector.
    let mut size_vec = Vec::with_capacity(4);
    size_vec.write_u32::<NetworkEndian>(pkt.len() as u32)?;

    // Write 4bytes size + pkt into session.
    let mut locked_session = session.write()?;
    locked_session.write_all( &size_vec[..])?; 
    locked_session.write_all( pkt)?;

    Ok(())
}


/// Default `NetworkRequest::Ping` handler. 
/// It responds with a pong packet. 
pub fn default_network_request_ping_handle( 
        session_opt: &ConnSession,
        peer: &P2PPeer,
        mode: P2PNodeMode, 
        last_seen: & Rc< AtomicU64 >,
        prometheus_exporter: & Option<Arc< Mutex< PrometheusServer>>>) -> ParseCallbackResult {

    update_atomic_stamp!( mode, last_seen);
    TOTAL_MESSAGES_SENT_COUNTER.inc();

    if let Some(ref prom) = prometheus_exporter {
        prom.lock()?.pkt_sent_inc()?
    };

    // Make `Pong` response and send
    if let Some(ref session) = session_opt {
        let pong_data = NetworkResponse::Pong(peer.clone()).serialize();

        Ok( serialize_bytes( &session, &pong_data)?)
    } else {
        Err( ErrorWrapper::from_kind( 
                ErrorKindWrapper::MessageProcessError( 
                    "Session is not found".to_string())))
    }
}

/// It sends the list of nodes.
pub fn default_network_request_find_node_handle(
        session_opt: &ConnSession, 
        self_peer: &P2PPeer,
        mode: P2PNodeMode,
        last_seen: & Rc< AtomicU64 >,
        buckets: & Arc< RwLock< Buckets> >,
        req: &NetworkRequest
    ) -> ParseCallbackResult {

    match req {
        NetworkRequest::FindNode(_, node_id) => {
            update_atomic_stamp!( mode, last_seen);
   
            //Return list of nodes
            if let Some(ref session) = session_opt {
                let nodes = buckets.read()?.closest_nodes(node_id);
                let response_data = NetworkResponse::FindNode(self_peer.clone(), nodes)
                    .serialize();

                Ok( serialize_bytes( &session, &response_data)?)
            } else {
                Err( ErrorWrapper::from_kind( 
                        ErrorKindWrapper::MessageProcessError( 
                            "Session is not found".to_string())))
            }
        }
        _ => {
            Err( ErrorWrapper::from_kind( 
                        ErrorKindWrapper::MessageProcessError( 
                            "Find node handler cannot handler this packet".to_string())))
        }
    }
}

pub fn default_network_request_get_peers(
    session_opt: &ConnSession,
    self_peer: &P2PPeer,
    mode: P2PNodeMode,
    last_seen: & Rc< AtomicU64 >,
    buckets: & Arc< RwLock< Buckets> >,
    prometheus_exporter: & Option<Arc< Mutex< PrometheusServer>>>,
    req: &NetworkRequest
    ) -> ParseCallbackResult {

    match req {
        NetworkRequest::GetPeers(ref sender, ref networks) => {
            debug!("Got request for GetPeers");

            update_atomic_stamp!( mode, last_seen);
            TOTAL_MESSAGES_SENT_COUNTER.inc();

            let nodes = buckets.read()?
                .get_all_nodes(Some(&sender), networks);

            if let Some(ref prom) = prometheus_exporter {
                prom.lock()?.pkt_sent_inc()?;
            };

            if let Some(ref session) = session_opt {
                let peer_list_packet = &NetworkResponse::PeerList(self_peer.clone(), nodes)
                        .serialize();
                Ok( serialize_bytes( &session, peer_list_packet)?)
            } else {
                Err( ErrorWrapper::from_kind( 
                        ErrorKindWrapper::MessageProcessError( 
                            "Session is not found".to_string())))
            }
        },
        _ => {
            Err( ErrorWrapper::from_kind( 
                ErrorKindWrapper::MessageProcessError( 
                    "Get peers handler cannot handler this packet".to_string())))
        }
    }
}

/// TODO log_event and update target_network is pending
/*
fn default_network_request_join_network(
    self_peer: &P2PPeer,
    mode: P2PNodeMode,
    last_seen: & Rc< AtomicU64 >,
    buckets: & Arc< RwLock< Buckets> >,
    target_network: &mut Vec<u16>,
    req: &NetworkRequest
    ) -> ParseCallbackResult {

    match req {
        NetworkRequest::JoinNetwork(ref _sender, ref network) => {
            update_atomic_stamp!( mode, last_seen);

            if !target_network.contains(network) {
                target_network.push(*network);
            }
            
            buckets.write()?.update_network_ids( self_peer, target_network.clone());
            // self.log_event(P2PEvent::JoinedNetwork(sender.clone(), *network));
        },
        _ => { }
    };

    Ok(())
}*/

pub fn default_network_response_find_node (
    own_id: &P2PNodeId,
    mode: P2PNodeMode,
    last_seen: & Rc< AtomicU64 >,
    buckets: & Arc< RwLock< Buckets> >,
    res: &NetworkResponse) -> ParseCallbackResult {

    match res {
        NetworkResponse::FindNode(_, ref peers) => {
            debug!("Got response to FindNode");
            update_atomic_stamp!( mode, last_seen);

            //Process the received node list
            let mut ref_buckets = buckets.write()?;
            for peer in peers.iter() {
                ref_buckets.insert_into_bucket(peer, own_id, vec![]);
            }

            Ok(())
        },
        _ => {
            Err( ErrorWrapper::from_kind( 
                ErrorKindWrapper::MessageProcessError( 
                    "Response find node handler cannot handler this packet".to_string())))
        }
    }
}

/*
pub fn default_network_response_pong (
    mode: P2PNodeMode,
    last_seen: & Rc< AtomicU64 >,
    ) -> ParseCallbackResult {
    debug!("Got response for ping");
    self.set_measured_ping();

    update_atomic_stamp!( mode, last_seen); 
}*/
