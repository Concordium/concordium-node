use super::fails;
use crate::{
    common::{
        counter::TOTAL_MESSAGES_SENT_COUNTER, get_current_stamp,
        serialization::serialize_into_memory, P2PPeer,
    },
    connection::{connection_private::ConnectionPrivate, MessageSendingPriority, P2PEvent},
    network::{NetworkId, NetworkMessage, NetworkRequest, NetworkResponse},
};
use concordium_common::{fails::FunctorError, functor::FuncResult, hybrid_buf::HybridBuf};

use failure::{Backtrace, Error};

use std::{
    collections::HashSet,
    convert::TryFrom,
    sync::{atomic::Ordering, mpsc::SyncSender, RwLock},
};

const BOOTSTRAP_PEER_COUNT: usize = 100;

pub fn make_msg_error(e: &'static str) -> FunctorError {
    FunctorError::from(vec![Error::from(fails::MessageProcessError {
        message:   e,
        backtrace: Backtrace::new(),
    })])
}
pub fn make_fn_error_peer(e: &'static str) -> FunctorError {
    FunctorError::from(vec![Error::from(fails::PeerError { message: e })])
}

pub fn make_log_error(e: &'static str) -> FunctorError {
    FunctorError::from(vec![Error::from(fails::LogError { message: e })])
}

/// Log when it has been joined to a network.
pub fn log_as_joined_network(
    event_log: &Option<SyncSender<P2PEvent>>,
    peer: &P2PPeer,
    networks: &HashSet<NetworkId>,
) -> FuncResult<()> {
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
    event_log: &Option<SyncSender<P2PEvent>>,
    sender: &P2PPeer,
    network: NetworkId,
) -> FuncResult<()> {
    if let Some(ref log) = event_log {
        log.send(P2PEvent::LeftNetwork(sender.to_owned(), network))
            .map_err(|_| make_log_error("Left Network Event cannot be sent to log"))?;
    };
    Ok(())
}

/// It sends handshake message and a ping message.
pub fn send_handshake_and_ping(priv_conn: &RwLock<ConnectionPrivate>) -> FuncResult<()> {
    let (my_nets, local_peer) = {
        let priv_conn_reader = read_or_die!(priv_conn);
        let remote_end_networks = priv_conn_reader.remote_end_networks.clone();
        let local_peer = priv_conn_reader.conn().local_peer();
        (remote_end_networks, local_peer)
    };

    // Send handshake
    let handshake_msg = NetworkMessage::NetworkResponse(
        NetworkResponse::Handshake(local_peer, my_nets, vec![]),
        Some(get_current_stamp()),
        None,
    );
    let handshake_data = serialize_into_memory(&handshake_msg, 128)?;

    // Send ping
    let ping_msg = NetworkMessage::NetworkRequest(
        NetworkRequest::Ping(local_peer),
        Some(get_current_stamp()),
        None,
    );
    let ping_data = serialize_into_memory(&ping_msg, 64)?;

    {
        let mut priv_conn_writer = write_or_die!(priv_conn);

        // Ignore returned value because it is an asynchronous operation.
        let _ = priv_conn_writer.async_send(
            HybridBuf::try_from(handshake_data)?,
            MessageSendingPriority::High,
        )?;

        // Ignore returned value because it is an asynchronous operation, and ship out
        // as normal priority to ensure proper queueing here.
        let _ = priv_conn_writer.async_send(
            HybridBuf::try_from(ping_data)?,
            MessageSendingPriority::Normal,
        )?;
    }

    TOTAL_MESSAGES_SENT_COUNTER.fetch_add(2, Ordering::Relaxed);
    Ok(())
}

/// It sends its peer list.
pub fn send_peer_list(
    priv_conn: &RwLock<ConnectionPrivate>,
    sender: &P2PPeer,
    nets: &HashSet<NetworkId>,
) -> FuncResult<()> {
    let mut priv_conn_writer = write_or_die!(priv_conn);
    let random_nodes = safe_read!(priv_conn_writer.conn().handler().connection_handler.buckets)?
        .get_random_nodes(&sender, BOOTSTRAP_PEER_COUNT, nets);
    if random_nodes.len()
        >= usize::from(
            priv_conn_writer
                .conn()
                .handler()
                .config
                .bootstrapper_wait_minimum_peers,
        )
    {
        debug!(
            "Running in bootstrapper mode, so instantly sending {} random peers to a peer",
            BOOTSTRAP_PEER_COUNT
        );
        let peer_list_msg = {
            if let Some(ref service) = priv_conn_writer.conn().handler().stats_export_service() {
                service.pkt_sent_inc();
            };
            NetworkMessage::NetworkResponse(
                NetworkResponse::PeerList(priv_conn_writer.conn().local_peer(), random_nodes),
                Some(get_current_stamp()),
                None,
            )
        };
        // Ignore returned value because it is an asynchronous operation.
        let _ = priv_conn_writer.async_send(
            serialize_into_memory(&peer_list_msg, 256)?,
            MessageSendingPriority::Normal,
        )?;
        TOTAL_MESSAGES_SENT_COUNTER.fetch_add(1, Ordering::Relaxed);
    }
    Ok(())
}

pub fn update_buckets(
    priv_conn: &RwLock<ConnectionPrivate>,
    sender: &P2PPeer,
    nets: HashSet<NetworkId>,
) -> FuncResult<()> {
    let priv_conn_borrow = read_or_die!(priv_conn);

    safe_write!(priv_conn_borrow.conn().handler().connection_handler.buckets)?
        .insert_into_bucket(sender, nets);

    if let Some(ref service) = priv_conn_borrow.conn().handler().stats_export_service() {
        service.peers_inc();
        service.pkt_sent_inc_by(2);
    };

    Ok(())
}
