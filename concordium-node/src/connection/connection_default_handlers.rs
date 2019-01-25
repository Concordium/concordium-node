use std::rc::{ Rc };
use std::cell::{ RefCell };
use std::sync::{ Arc, RwLock };
use std::sync::mpsc::{ Sender };
use byteorder::{ NetworkEndian,  WriteBytesExt };
use atomic_counter::AtomicCounter;

use rustls::{ Session };

use common::{ P2PPeer, get_current_stamp };
use common::counter::{ TOTAL_MESSAGES_SENT_COUNTER };
use network::{ NetworkRequest, NetworkResponse };
use connection::{ P2PEvent };
use connection::parse_handler::{ ParseCallbackResult };
use connection::connection_private::{ ConnectionPrivate };

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

fn make_msg_error( e:&'static str) -> ErrorWrapper {
    ErrorWrapper::from_kind(
            ErrorKindWrapper::MessageProcessError(
                    e.to_string()))
}

/// It returns a `FunctorRunningError` with the specific message.
fn make_fn_err( e: &'static str) -> ErrorWrapper {
    ErrorWrapper::from_kind(
        ErrorKindWrapper::FunctorRunningError( e))
}

fn make_fn_error_session() -> ErrorWrapper {
    make_fn_err( "Session not found")
}

fn make_fn_error_peer() -> ErrorWrapper {
    make_fn_err( "Peer not found")
}


/// Default `NetworkRequest::Ping` handler.
/// It responds with a pong packet.
pub fn default_network_request_ping_handle(
        priv_conn: &Rc< RefCell< ConnectionPrivate >>,
        _req: &NetworkRequest) -> ParseCallbackResult {

    priv_conn.borrow_mut().update_last_seen();
    TOTAL_MESSAGES_SENT_COUNTER.inc();

    let priv_conn_borrow = priv_conn.borrow();
    if let Some(ref prom) = priv_conn_borrow.prometheus_exporter {
        prom.lock()?.pkt_sent_inc()?
    };

    // Make `Pong` response and send
    let ref session = priv_conn_borrow.session()
        .ok_or_else( || make_fn_error_session())?;
    let peer = priv_conn_borrow.peer.clone()
        .ok_or_else( || make_fn_error_peer())?;
    let pong_data = NetworkResponse::Pong(peer.clone()).serialize();

    Ok( serialize_bytes( session, &pong_data)?)
}

/// It sends the list of nodes.
pub fn default_network_request_find_node_handle(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        req: &NetworkRequest
    ) -> ParseCallbackResult {

    match req {
        NetworkRequest::FindNode(_, node_id) => {
            priv_conn.borrow_mut().update_last_seen();

            //Return list of nodes
            let priv_conn_borrow = priv_conn.borrow();
            let ref session = priv_conn_borrow.session()
                .ok_or_else( || make_fn_error_session())?;
            let peer = priv_conn_borrow.peer.clone()
                .ok_or_else( || make_fn_error_peer())?;
            let nodes = priv_conn_borrow.buckets.read()?.closest_nodes(node_id);
            let response_data = NetworkResponse::FindNode( peer, nodes)
                    .serialize();

            Ok( serialize_bytes( &session, &response_data)?)
        }
        _ => {
            Err( make_msg_error( "Find node handler cannot handler this packet"))
        }
    }
}

pub fn default_network_request_get_peers(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        req: &NetworkRequest) -> ParseCallbackResult {

    match req {
        NetworkRequest::GetPeers(ref sender, ref networks) => {
            debug!("Got request for GetPeers");

            priv_conn.borrow_mut().update_last_seen();
            TOTAL_MESSAGES_SENT_COUNTER.inc();

            let priv_conn_borrow = priv_conn.borrow();
            let nodes = priv_conn_borrow.buckets.read()?
                .get_all_nodes(Some(&sender), networks);

            if let Some(ref prom) = priv_conn_borrow.prometheus_exporter {
                prom.lock()?.pkt_sent_inc()?;
            };

            let ref session = priv_conn_borrow.session()
                    .ok_or_else( || make_fn_error_session())?;
            let peer = priv_conn_borrow.peer.clone()
                    .ok_or_else( || make_fn_error_peer())?;
            let peer_list_packet = &NetworkResponse::PeerList(peer, nodes)
                    .serialize();

            Ok( serialize_bytes( &session, peer_list_packet)?)
        },
        _ => {
            Err( make_msg_error( "Get peers handler cannot handler this packet"))
        }
    }
}

pub fn default_network_response_find_node (
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        res: &NetworkResponse) -> ParseCallbackResult {

    match res {
        NetworkResponse::FindNode(_, ref peers) => {
            debug!("Got response to FindNode");

            let priv_conn_borrow = priv_conn.borrow();
            //Process the received node list
            let mut ref_buckets = priv_conn_borrow.buckets.write()?;
            for peer in peers.iter() {
                ref_buckets.insert_into_bucket(peer, &priv_conn_borrow.own_id, vec![]);
            }

            Ok(())
        },
        _ => {
            Err( make_msg_error( "Response find node handler cannot handler this packet"))
        }
    }
}

pub fn default_network_response_pong(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        _res: &NetworkResponse) -> ParseCallbackResult {

    let ping: u64 = priv_conn.borrow().sent_ping.clone();
    if ping != u64::max_value() {
        priv_conn.borrow_mut().last_latency_measured = get_current_stamp() - ping;
    }

    Ok(())
}

pub fn default_network_response_peer_list(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        res: &NetworkResponse) -> ParseCallbackResult {
    match res {
        NetworkResponse::PeerList( _, ref peers) => {
            let priv_conn_borrow = priv_conn.borrow();
            let mut locked_buckets = priv_conn_borrow.buckets.write()?;
            for peer in peers.iter() {
                locked_buckets.insert_into_bucket( peer, &priv_conn_borrow.own_id, vec![]);
            }
        },
        _ => {}
    };
    Ok(())
}

fn log_as_joined_network(
        event_log: &Option<Sender<P2PEvent>>,
        peer: &P2PPeer,
        networks: &Vec<u16>) -> ResultExtWrapper<()> {
    if let Some(ref log) = event_log {
        for ele in networks.iter() {
            log.send( P2PEvent::JoinedNetwork(peer.clone(), *ele))
                .map_err(|e| ErrorWrapper::with_chain( e, ErrorKindWrapper::FunctorRunningError(
                            "Join Network Event cannot be sent to log")))?;
        }
    }
    Ok(())
}

fn log_as_leave_network(
        event_log: &Option<Sender<P2PEvent>>,
        sender: &P2PPeer,
        network: u16) -> ResultExtWrapper<()> {
    if let Some(ref log) = event_log {
        log.send( P2PEvent::LeftNetwork( sender.clone(), network))
            .map_err( |e| ErrorWrapper::with_chain( e, ErrorKindWrapper::FunctorRunningError( "Left Network Event cannot be sent to log")))
    } else {
        Ok(())
    }
}

pub fn default_network_response_handshake(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        res: &NetworkResponse) -> ParseCallbackResult {

    match res {
        NetworkResponse::Handshake(ref rpeer, ref nets, _) => {
            {
                let mut priv_conn_mut = priv_conn.borrow_mut();
                priv_conn_mut.sent_handshake = get_current_stamp();
                priv_conn_mut.add_networks( nets);
                priv_conn_mut.peer = Some(rpeer.clone());
            }

            let priv_conn_borrow = priv_conn.borrow();
            let conn_type = priv_conn_borrow.connection_type;
            let own_id = priv_conn_borrow.own_id.clone();
            let bucket_sender = P2PPeer::from( conn_type,
                                              rpeer.id().clone(),
                                              rpeer.ip().clone(),
                                              rpeer.port());
            priv_conn_borrow.buckets.write()?
                .insert_into_bucket(&bucket_sender, &own_id, nets.clone());

            if let Some(ref prom) = priv_conn_borrow.prometheus_exporter {
                prom.lock()?.peers_inc()?;
            };

            log_as_joined_network( &priv_conn_borrow.event_log, &rpeer, &nets)?;
        },
        _ => {}
    };
    Ok(())
}

pub fn default_network_request_join_network(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        res: &NetworkRequest) -> ParseCallbackResult {

    match res {
        NetworkRequest::JoinNetwork( ref _sender, ref network) => {
            let networks = vec![*network];
            priv_conn.borrow_mut().add_networks(&networks);

            let priv_conn_borrow = priv_conn.borrow();
            let peer = priv_conn_borrow.peer.clone()
                .ok_or_else( || make_fn_error_peer())?;
            let priv_conn_networks = priv_conn_borrow.networks.clone();

            priv_conn_borrow.buckets.write()?
                .update_network_ids(&peer, priv_conn_networks);

            log_as_joined_network( &priv_conn_borrow.event_log, &peer,  &networks)?;
        },
        _ => {}
    };

    Ok(())
}

pub fn default_network_request_leave_network(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        req: &NetworkRequest) -> ParseCallbackResult {

    match req {
        NetworkRequest::LeaveNetwork( sender, network) => {
            priv_conn.borrow_mut().remove_network( network);

            let priv_conn_borrow = priv_conn.borrow();
            let peer = priv_conn_borrow.peer.clone()
                .ok_or_else( || make_fn_error_peer())?;

            priv_conn_borrow.buckets.write()?
                .update_network_ids( &peer, priv_conn_borrow.networks.clone());

            log_as_leave_network( &priv_conn_borrow.event_log, &sender, *network)?;
        },
        _ => {}
    };
    Ok(())
}

