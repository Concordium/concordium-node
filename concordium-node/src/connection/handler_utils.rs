use byteorder::{NetworkEndian, WriteBytesExt};
use std::{cell::RefCell, collections::HashSet, sync::mpsc::Sender};

use crate::{
    common::{
        counter::TOTAL_MESSAGES_SENT_COUNTER,
        functor::{FunctorError, FunctorResult},
        P2PPeer,
    },
    connection::{connection_private::ConnectionPrivate, CommonSession, P2PEvent},
    network::{NetworkId, NetworkRequest, NetworkResponse},
};
use std::sync::atomic::Ordering;

use super::fails;
use failure::{Backtrace, Error};

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

pub fn make_fn_error_prometheus() -> FunctorError {
    FunctorError::new(vec![Error::from(fails::PrometheusError {
        message: "Prometheus failed",
    })])
}

pub fn serialize_bytes(session: &mut Box<dyn CommonSession>, pkt: &[u8]) -> FunctorResult {
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
    let (my_nets, self_peer) = {
        let priv_conn_borrow = priv_conn.borrow();
        let my_nets = priv_conn_borrow.networks.clone();
        let self_peer = priv_conn_borrow.self_peer.to_owned();
        (my_nets, self_peer)
    };

    let session = &mut priv_conn.borrow_mut().tls_session;
    serialize_bytes(
        session,
        &NetworkResponse::Handshake(self_peer.clone(), my_nets, vec![]).serialize(),
    )?;

    serialize_bytes(session, &NetworkRequest::Ping(self_peer).serialize())?;

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

        let self_peer = &priv_conn_borrow.self_peer;
        NetworkResponse::PeerList(self_peer.to_owned(), random_nodes).serialize()
    };

    serialize_bytes(&mut priv_conn.borrow_mut().tls_session, &data)?;

    if let Some(ref prom) = priv_conn.borrow().prometheus_exporter {
        let mut writable_prom = safe_write!(prom)?;
        writable_prom
            .pkt_sent_inc()
            .map_err(|_| make_fn_error_prometheus())?;
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

    let prometheus_exporter = &priv_conn_borrow.prometheus_exporter;
    if let Some(ref prom) = prometheus_exporter {
        let mut writable_prom = safe_write!(prom)?;
        writable_prom
            .peers_inc()
            .map_err(|_| make_fn_error_prometheus())?;
        writable_prom
            .pkt_sent_inc_by(2)
            .map_err(|_| make_fn_error_prometheus())?;
    };

    Ok(())
}
