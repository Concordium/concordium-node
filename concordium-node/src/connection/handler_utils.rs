use byteorder::{NetworkEndian, WriteBytesExt};
use std::{cell::RefCell, collections::HashSet, sync::mpsc::Sender};

use crate::{
    common::{
        counter::TOTAL_MESSAGES_SENT_COUNTER,
        functor::{FunctorError, FunctorResult},
        get_current_stamp,
        serialization::Serializable,
        P2PPeer,
    },
    connection::{connection_private::ConnectionPrivate, CommonSession, P2PEvent},
    network::{NetworkId, NetworkMessage, NetworkRequest, NetworkResponse},
};
use std::sync::atomic::Ordering;

use super::fails;
use failure::{Backtrace, Error, Fallible};

const BOOTSTRAP_PEER_COUNT: usize = 100;

pub fn make_msg_error(e: &'static str) -> FunctorError {
    FunctorError::new(vec![Error::from(fails::MessageProcessError {
        message:   e,
        backtrace: Backtrace::new(),
    })])
}
pub fn make_fn_error_peer(e: &'static str) -> FunctorError {
    FunctorError::new(vec![Error::from(fails::PeerError { message: e })])
}

pub fn make_log_error(e: &'static str) -> FunctorError {
    FunctorError::new(vec![Error::from(fails::LogError { message: e })])
}

pub fn serialize_bytes(session: &mut dyn CommonSession, pkt: &[u8]) -> FunctorResult {
    // Write size of pkt into 4 bytes vector.
    let mut size_vec = Vec::with_capacity(4);
    size_vec.write_u32::<NetworkEndian>(pkt.len() as u32)?;

    session.write_all(&size_vec[..])?;
    session.write_all(pkt)?;

    Ok(())
}

/// Log when it has been joined to a network.
pub fn log_as_joined_network(
    event_log: &Option<Sender<P2PEvent>>,
    peer: &P2PPeer,
    networks: &HashSet<NetworkId>,
) -> FunctorResult {
    if let Some(ref log) = event_log {
        for net_id in networks.iter() {
            log.send(P2PEvent::JoinedNetwork(peer.to_owned(), *net_id))
                .map_err(|_| make_log_error("Join Network Event cannot be sent to log"))?;
        }
    }
    Ok(())
}

/// Log when it has been removed from a network.
pub fn log_as_leave_network(
    event_log: &Option<Sender<P2PEvent>>,
    sender: &P2PPeer,
    network: NetworkId,
) -> FunctorResult {
    if let Some(ref log) = event_log {
        log.send(P2PEvent::LeftNetwork(sender.to_owned(), network))
            .map_err(|_| make_log_error("Left Network Event cannot be sent to log"))?;
    };
    Ok(())
}

/// It sends handshake message and a ping message.
pub fn send_handshake_and_ping(priv_conn: &RefCell<ConnectionPrivate>) -> FunctorResult {
    let (my_nets, local_peer) = {
        let priv_conn_borrow = priv_conn.borrow();
        let remote_end_networks = priv_conn_borrow.remote_end_networks.clone();
        let local_peer = priv_conn_borrow.local_peer.to_owned();
        (remote_end_networks, local_peer)
    };

    let session = &mut *priv_conn.borrow_mut().tls_session;

    // Send handshake
    let handshake_msg = NetworkMessage::NetworkResponse(
        NetworkResponse::Handshake(local_peer.clone(), my_nets, vec![]),
        Some(get_current_stamp()),
        None,
    );
    let handshake_data = serialize_into_memory!(handshake_msg, 128)?;
    serialize_bytes(session, &handshake_data)?;

    // Send ping
    let ping_msg = NetworkMessage::NetworkRequest(
        NetworkRequest::Ping(local_peer),
        Some(get_current_stamp()),
        None,
    );
    let ping_data = serialize_into_memory!(ping_msg, 64)?;
    serialize_bytes(session, &ping_data)?;

    TOTAL_MESSAGES_SENT_COUNTER.fetch_add(2, Ordering::Relaxed);
    Ok(())
}

/// It sends its peer list.
pub fn send_peer_list(
    priv_conn: &RefCell<ConnectionPrivate>,
    sender: &P2PPeer,
    nets: &HashSet<NetworkId>,
) -> FunctorResult {
    debug!(
        "Running in bootstrapper mode, so instantly sending peers {} random peers",
        BOOTSTRAP_PEER_COUNT
    );

    let data = {
        let priv_conn_borrow = priv_conn.borrow();
        let random_nodes = safe_read!(priv_conn_borrow.buckets)?.get_random_nodes(
            &sender,
            BOOTSTRAP_PEER_COUNT,
            nets,
        );

        let local_peer = &priv_conn_borrow.local_peer;
        let peer_list_msg = NetworkMessage::NetworkResponse(
            NetworkResponse::PeerList(local_peer.to_owned(), random_nodes),
            Some(get_current_stamp()),
            None,
        );
        serialize_into_memory!(peer_list_msg, 256)?
    };

    serialize_bytes(&mut *priv_conn.borrow_mut().tls_session, &data)?;

    if let Some(ref service) = priv_conn.borrow().stats_export_service {
        let mut writable_service = safe_write!(service)?;
        writable_service.pkt_sent_inc();
    };

    TOTAL_MESSAGES_SENT_COUNTER.fetch_add(1, Ordering::Relaxed);

    Ok(())
}

/// It sends a retransmit request.
pub fn send_retransmit_request(
    priv_conn: &RefCell<ConnectionPrivate>,
    since_stamp: u64,
    network_id: NetworkId,
) -> FunctorResult {
    let retransmit = NetworkMessage::NetworkRequest(
        NetworkRequest::Retransmit(
            priv_conn.borrow().local_peer.to_owned(),
            since_stamp,
            network_id,
        ),
        Some(get_current_stamp()),
        None,
    );
    let retransmit_data = serialize_into_memory!(retransmit, 256)?;

    serialize_bytes(&mut *priv_conn.borrow_mut().tls_session, &retransmit_data)?;

    if let Some(ref service) = priv_conn.borrow().stats_export_service {
        let mut writable_service = safe_write!(service)?;
        writable_service.pkt_sent_inc();
    };
    TOTAL_MESSAGES_SENT_COUNTER.fetch_add(1, Ordering::Relaxed);
    Ok(())
}

pub fn update_buckets(
    priv_conn: &RefCell<ConnectionPrivate>,
    sender: &P2PPeer,
    nets: HashSet<NetworkId>,
) -> FunctorResult {
    let priv_conn_borrow = priv_conn.borrow();
    let buckets = &priv_conn_borrow.buckets;

    safe_write!(buckets)?.insert_into_bucket(sender, nets);

    let stats_export_service = &priv_conn_borrow.stats_export_service;
    if let Some(ref service) = stats_export_service {
        let mut writable_service = safe_write!(service)?;
        writable_service.peers_inc();
        writable_service.pkt_sent_inc_by(2);
    };

    Ok(())
}
