use std::rc::{ Rc };
use std::sync::{ Arc, Mutex, RwLock };
use std::sync::atomic::{ AtomicU64, Ordering };
use byteorder::{ NetworkEndian,  WriteBytesExt };
use atomic_counter::AtomicCounter;

use rustls::{ Session };
use prometheus_exporter::{ PrometheusServer };

use common::{ P2PPeer, get_current_stamp };
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

