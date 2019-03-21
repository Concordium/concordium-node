use std::rc::{ Rc };
use std::cell::{ RefCell };
use std::sync::mpsc::{ Sender };
use byteorder::{ NetworkEndian,  WriteBytesExt };
use atomic_counter::AtomicCounter;

use crate::common::{ P2PPeer, get_current_stamp };
use crate::common::counter::{ TOTAL_MESSAGES_SENT_COUNTER };
use crate::common::functor::{ FunctorResult, FunctorError };
use crate::network::{ NetworkRequest, NetworkResponse };
use crate::connection::{ P2PEvent, P2PNodeMode, CommonSession };
use crate::connection::connection_private::{ ConnectionPrivate };
use crate::fails as global_fails;

use super::fails::ConnectionError;
use failure::{Backtrace, Error };

const BOOTSTRAP_PEER_COUNT: usize = 100;

fn serialize_bytes( session: &mut Box<dyn CommonSession>, pkt: &[u8]) -> FunctorResult {
    // Write size of pkt into 4 bytes vector.
    let mut size_vec = Vec::with_capacity(4);
    size_vec.write_u32::<NetworkEndian>(pkt.len() as u32)?;

    session.write_all( &size_vec[..])?;
    session.write_all( pkt)?;

    Ok(())
}

fn make_msg_error(e: &'static str) -> FunctorError  {
    FunctorError::new(vec![Error::from(ConnectionError::MessageProcessError {
        message: e,
        backtrace: Backtrace::new()
    })])
}

fn make_fn_error_peer(e: &'static str) -> FunctorError {
    FunctorError::new(vec![Error::from(ConnectionError::PeerError {
        message: e
    })])
}

fn make_log_error(e: &'static str) -> FunctorError {
    FunctorError::new(vec![Error::from(ConnectionError::LogError {
        message: e
    })])
}

fn make_fn_error_prometheus() -> FunctorError {
    FunctorError::new(vec![Error::from(ConnectionError::PrometheusError {
        message: "Prometheus failed",
    })])
}

/// Default `NetworkRequest::Ping` handler.
/// It responds with a pong packet.
pub fn default_network_request_ping_handle(
        priv_conn: &Rc< RefCell< ConnectionPrivate >>,
        _req: &NetworkRequest) -> FunctorResult {

    priv_conn.borrow_mut().update_last_seen();
    TOTAL_MESSAGES_SENT_COUNTER.inc();

    let pong_data = {
        let priv_conn_borrow = priv_conn.borrow();
        if let Some(ref prom) = priv_conn_borrow.prometheus_exporter {
            prom.lock().map_err(global_fails::PoisonError::from)?.pkt_sent_inc()?
        };

        // Make `Pong` response and send
        let peer = priv_conn_borrow.peer().clone()
            .ok_or_else( || make_fn_error_peer("Couldn't get peer"))?;

        NetworkResponse::Pong(peer.clone()).serialize()
    };


    Ok( serialize_bytes( &mut priv_conn.borrow_mut().tls_session, &pong_data)?)
}

/// It sends the list of nodes.
pub fn default_network_request_find_node_handle(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        req: &NetworkRequest
    ) -> FunctorResult {

    if let NetworkRequest::FindNode(_, node_id) = req {
        priv_conn.borrow_mut().update_last_seen();

        //Return list of nodes
        let response_data = {
            let priv_conn_borrow = priv_conn.borrow();
            let peer = priv_conn_borrow.peer().clone()
                .ok_or_else( || make_fn_error_peer("Couldn't borrow peer"))?;
            let nodes = priv_conn_borrow.buckets.read()
                .map_err(global_fails::PoisonError::from)?
                .closest_nodes(node_id);
            NetworkResponse::FindNode( peer, nodes).serialize()
        };

        Ok( serialize_bytes( &mut priv_conn.borrow_mut().tls_session, &response_data)?)
     } else {
        Err(make_msg_error( "Find node handler cannot handler this packet"))?
    }
}

pub fn default_network_request_get_peers(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        req: &NetworkRequest) -> FunctorResult {

    if let NetworkRequest::GetPeers(ref sender, ref networks) = req {

        debug!("Got request for GetPeers");

        priv_conn.borrow_mut().update_last_seen();
        TOTAL_MESSAGES_SENT_COUNTER.inc();

        let peer_list_packet = {
            let priv_conn_borrow = priv_conn.borrow();
            let nodes = priv_conn_borrow.buckets.read()
                .map_err(global_fails::PoisonError::from)?
                .get_all_nodes(Some(&sender), networks);

            if let Some(ref prom) = priv_conn_borrow.prometheus_exporter {
                prom.lock()
                    .map_err(global_fails::PoisonError::from)?.pkt_sent_inc()?;
            };

            let peer = priv_conn_borrow.peer().clone().ok_or_else( || make_fn_error_peer("Couldn't borrow peer"))?;
            NetworkResponse::PeerList(peer, nodes)
                .serialize()
        };

        Ok(serialize_bytes( &mut priv_conn.borrow_mut().tls_session, &peer_list_packet)?)
    } else {
            Err(make_msg_error( "Get peers handler cannot handler this packet"))?
    }
}


pub fn default_network_response_find_node (
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        res: &NetworkResponse) -> FunctorResult {

    if let NetworkResponse::FindNode(_, ref peers) = res {
        debug!("Got response to FindNode");

        let priv_conn_borrow = priv_conn.borrow();
        //Process the received node list
        let mut ref_buckets = priv_conn_borrow.buckets.write().map_err(global_fails::PoisonError::from)?;
        for peer in peers.iter() {
            ref_buckets.insert_into_bucket(peer, &priv_conn_borrow.own_id, vec![]);
        }

        Ok(())
    } else {
        Err(make_msg_error( "Response find node handler cannot handler this packet"))?
    }
}

/// It measures network latency.
pub fn default_network_response_pong(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        _res: &NetworkResponse) -> FunctorResult {

    let ping: u64 = priv_conn.borrow().sent_ping.clone();
    let curr: u64 = get_current_stamp();

    if curr >= ping {
        priv_conn.borrow_mut().last_latency_measured = curr - ping;
    }

    Ok(())
}

/// It inserts new peers into buckets.
pub fn default_network_response_peer_list(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        res: &NetworkResponse) -> FunctorResult {
    if let NetworkResponse::PeerList( _, ref peers) = res {
        let priv_conn_borrow = priv_conn.borrow();
        let mut locked_buckets = priv_conn_borrow.buckets.write().map_err(global_fails::PoisonError::from)?;
        for peer in peers.iter() {
            locked_buckets.insert_into_bucket( peer, &priv_conn_borrow.own_id, vec![]);
        }
    };
    Ok(())
}

/// Log when it has been joined to a network.
fn log_as_joined_network(
        event_log: &Option<Sender<P2PEvent>>,
        peer: &P2PPeer,
        networks: &Vec<u16>) -> FunctorResult {
    if let Some(ref log) = event_log {
        for ele in networks.iter() {
            log.send( P2PEvent::JoinedNetwork(peer.clone(), *ele))
                .map_err(|_| make_log_error("Join Network Event cannot be sent to log"))?;
        }
    }
    Ok(())
}

/// Log when it has been removed from a network.
fn log_as_leave_network(
        event_log: &Option<Sender<P2PEvent>>,
        sender: &P2PPeer,
        network: u16) -> FunctorResult {
    if let Some(ref log) = event_log {
        log.send( P2PEvent::LeftNetwork( sender.clone(), network))
            .map_err(|_| make_log_error("Left Network Event cannot be sent to log"))?;
    };
    Ok(())
}

/// In handshake:
///     - Add network
///     - Store target peer info and allocates buckets for this connection.
///     - Statistics: Export to Prometheus
///     - Log: Join to network
pub fn default_network_response_handshake(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        res: &NetworkResponse) -> FunctorResult {

    if let NetworkResponse::Handshake(ref rpeer, ref nets, _) = res {
        {
            let mut priv_conn_mut = priv_conn.borrow_mut();
            priv_conn_mut.sent_handshake = get_current_stamp();
            priv_conn_mut.add_networks( nets);
            priv_conn_mut.set_peer( rpeer.clone());
        }

        let priv_conn_borrow = priv_conn.borrow();
        let conn_type = priv_conn_borrow.connection_type;
        let own_id = priv_conn_borrow.own_id.clone();
        let bucket_sender = P2PPeer::from( conn_type,
                                           rpeer.id().clone(),
                                           rpeer.ip().clone(),
                                           rpeer.port());
        priv_conn_borrow.buckets.write().map_err(global_fails::PoisonError::from)?
            .insert_into_bucket(&bucket_sender, &own_id, nets.clone());

        if let Some(ref prom) = priv_conn_borrow.prometheus_exporter {
            prom.lock().map_err(global_fails::PoisonError::from)?.peers_inc()?;
        };
    }

    Ok(())
}

/// It adds new network and update its buckets.
pub fn default_network_request_join_network(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        res: &NetworkRequest) -> FunctorResult {

    if let NetworkRequest::JoinNetwork(ref _sender, ref network) = res {
        let networks = vec![*network];
        priv_conn.borrow_mut().add_networks(&networks);

        let priv_conn_borrow = priv_conn.borrow();
        let peer = priv_conn_borrow.peer().clone()
            .ok_or_else( || make_fn_error_peer("Couldn't borrow peer"))?;
        let priv_conn_networks = priv_conn_borrow.networks.clone();

        priv_conn_borrow.buckets.write().map_err(global_fails::PoisonError::from)?
            .update_network_ids(&peer, priv_conn_networks);

        log_as_joined_network( &priv_conn_borrow.event_log, &peer,  &networks)?;
    }

    Ok(())
}

/// It removes that network from its owns and update buckets.
pub fn default_network_request_leave_network(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        req: &NetworkRequest) -> FunctorResult {

    if let NetworkRequest::LeaveNetwork(sender, network) = req {
        priv_conn.borrow_mut().remove_network( network);
        let priv_conn_borrow = priv_conn.borrow();
        let peer = priv_conn_borrow.peer().clone()
            .ok_or_else( || make_fn_error_peer("Couldn't borrow peer"))?;

        priv_conn_borrow.buckets.write().map_err(global_fails::PoisonError::from)?
            .update_network_ids( &peer, priv_conn_borrow.networks.clone());

        log_as_leave_network( &priv_conn_borrow.event_log, &sender, *network)?;
    }

    Ok(())
}

/// It sends handshake message and a ping message.
fn send_handshake_and_ping(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>
    ) -> FunctorResult {

    let (my_nets, self_peer) = {
        let priv_conn_borrow = priv_conn.borrow();
        let my_nets = priv_conn_borrow.own_networks.lock().map_err(global_fails::PoisonError::from)?.clone();
        let self_peer = priv_conn_borrow.self_peer.clone();
        (my_nets, self_peer)
    };

    let session = &mut priv_conn.borrow_mut().tls_session;
    serialize_bytes(
        session,
        &NetworkResponse::Handshake(
            self_peer.clone(),
            my_nets,
            vec![]).serialize())?;

    serialize_bytes(
        session,
        &NetworkRequest::Ping(
            self_peer).serialize())?;

    TOTAL_MESSAGES_SENT_COUNTER.add(2);
    Ok(())
}

/// It sends its peer list.
fn send_peer_list(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        sender: &P2PPeer,
        nets: &[u16]
    ) -> FunctorResult {

    debug!(
        "Running in bootstrapper mode, so instantly sending peers {} random peers",
        BOOTSTRAP_PEER_COUNT);

    let data = {
        let priv_conn_borrow = priv_conn.borrow();
        let random_nodes = priv_conn_borrow.buckets.read().map_err(global_fails::PoisonError::from)?
            .get_random_nodes(&sender, BOOTSTRAP_PEER_COUNT, &nets);

        let self_peer = & priv_conn_borrow.self_peer;
        NetworkResponse::PeerList( self_peer.clone(), random_nodes).serialize()
    };

    serialize_bytes( &mut priv_conn.borrow_mut().tls_session, &data)?;

    if let Some(ref prom) = priv_conn.borrow().prometheus_exporter {
        prom.lock().map_err(global_fails::PoisonError::from)?.pkt_sent_inc()
            .map_err(|_| make_fn_error_prometheus())?;
    };

    TOTAL_MESSAGES_SENT_COUNTER.inc();

    Ok(())
}

fn update_buckets(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        sender: &P2PPeer,
        nets: &[u16],
        valid_mode: bool
    ) -> FunctorResult {

    let priv_conn_borrow = priv_conn.borrow();
    let own_id = & priv_conn_borrow.own_id;
    let buckets = & priv_conn_borrow.buckets;
    let sender_ip = sender.ip();

    if valid_mode {
        buckets.write().map_err(global_fails::PoisonError::from)?
            .insert_into_bucket( sender, &own_id, nets.to_owned());
    } else if sender_ip.is_global()
            && !sender_ip.is_multicast()
            && !sender_ip.is_documentation() {
                buckets.write().map_err(global_fails::PoisonError::from)?
                    .insert_into_bucket( sender, &own_id, nets.to_owned());
    }

    let prometheus_exporter = & priv_conn_borrow.prometheus_exporter;
    if let Some(ref prom) = prometheus_exporter {
        let mut locked_prom = prom.lock().map_err(global_fails::PoisonError::from)?;
        locked_prom.peers_inc()
            .map_err(|_| make_fn_error_prometheus())?;
        locked_prom.pkt_sent_inc_by(2)
            .map_err(|_| make_fn_error_prometheus())?;
    };

    Ok(())
}

/// Node is valid if its mode is `NormalPrivateMode` or `BootstrapperPrivateMode`.
fn is_valid_mode(
    priv_conn: &Rc< RefCell< ConnectionPrivate>> ) -> bool {
    let mode = priv_conn.borrow().mode;

    mode == P2PNodeMode::BootstrapperPrivateMode
    || mode == P2PNodeMode::NormalPrivateMode
}

/// On a handshake request:
///     - It replies with a handshake response and a ping.
///     - It adds the new network, and updates its buckets.
///     - Finally, it sends its peer list.
pub fn default_network_request_handshake(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        req: &NetworkRequest) -> FunctorResult {
    if let NetworkRequest::Handshake(sender, nets, _) = req {
        debug!("Got request for Handshake");

        // Setup peer and networks before send Handshake.
        {
            let mut priv_conn_mut = priv_conn.borrow_mut();
            priv_conn_mut.add_networks(nets);
            priv_conn_mut.set_peer( sender.clone());

        }
        send_handshake_and_ping( priv_conn)?;
        {
            let mut priv_conn_mut = priv_conn.borrow_mut();
            priv_conn_mut.update_last_seen();
            priv_conn_mut.set_measured_ping_sent();
        }

        let valid_mode: bool = is_valid_mode( priv_conn);
        update_buckets( priv_conn, sender, nets, valid_mode)?;

        if valid_mode {
            send_peer_list( priv_conn, sender, nets)?;
        }
    }

    Ok(())
}

/// Unknown messages only updates statistic information.
pub fn default_unknown_message(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        _: &()) -> FunctorResult {

    debug!("Unknown message received!");

    {
        let mut priv_conn_mut = priv_conn.borrow_mut();

        priv_conn_mut.failed_pkts += 1;
        priv_conn_mut.update_last_seen();
    }

    // TODO It will need access to buf.
    // trace!("Contents were: {:?}",
    //        String::from_utf8(buf.to_vec()).unwrap());

    if let Some(ref prom) = priv_conn.borrow().prometheus_exporter {
        prom.lock().map_err(global_fails::PoisonError::from)?
            .unknown_pkts_received_inc()?;
    }
    Ok(())
}

/// Invalid messages only updates statistic information.
pub fn default_invalid_message(
         priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        _: &()) -> FunctorResult {
    {
        let mut priv_conn_mut = priv_conn.borrow_mut();

        priv_conn_mut.failed_pkts += 1;
        priv_conn_mut.update_last_seen();
    }

    // trace!("Contents were: {:?}",
    //        String::from_utf8(buf.to_vec()).unwrap());

    if let Some(ref prom) = priv_conn.borrow().prometheus_exporter {
       prom.lock().map_err(global_fails::PoisonError::from)?
            .invalid_pkts_received_inc()?;
    }

    Ok(())
}
