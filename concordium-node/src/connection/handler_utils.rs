use std::rc::{ Rc };
use std::cell::{ RefCell };
use std::sync::mpsc::{ Sender };
use byteorder::{ NetworkEndian,  WriteBytesExt };
use atomic_counter::AtomicCounter;

use crate::common::{ P2PPeer };
use crate::common::counter::{ TOTAL_MESSAGES_SENT_COUNTER };
use crate::common::functor::{ FunctorResult, FunctorError };
use crate::network::{ NetworkRequest, NetworkResponse };
use crate::connection::{ P2PEvent, P2PNodeMode, CommonSession };
use crate::connection::connection_private::{ ConnectionPrivate };

use super::fails;
use failure::{Backtrace, Error };

const BOOTSTRAP_PEER_COUNT: usize = 100;

pub fn make_msg_error(e: &'static str) -> FunctorError  {
    FunctorError::new(vec![Error::from(fails::MessageProcessError {
        message: e,
        backtrace: Backtrace::new()
    })])
}
pub fn make_fn_error_peer(e: &'static str) -> FunctorError {
    FunctorError::new(vec![Error::from(fails::PeerError {
        message: e
    })])
}

pub fn make_log_error(e: &'static str) -> FunctorError {
    FunctorError::new(vec![Error::from(fails::LogError {
        message: e
    })])
}

pub fn make_fn_error_prometheus() -> FunctorError {
    FunctorError::new(vec![Error::from(fails::PrometheusError {
        message: "Prometheus failed",
    })])
}

pub fn serialize_bytes( session: &mut Box<dyn CommonSession>, pkt: &[u8]) -> FunctorResult {
    // Write size of pkt into 4 bytes vector.
    let mut size_vec = Vec::with_capacity(4);
    size_vec.write_u32::<NetworkEndian>(pkt.len() as u32)?;

    session.write_all( &size_vec[..])?;
    session.write_all( pkt)?;

    Ok(())
}

/// Log when it has been joined to a network.
pub fn log_as_joined_network(
        event_log: &Option<Sender<P2PEvent>>,
        peer: &P2PPeer,
        networks: &[u16]) -> FunctorResult {
    if let Some(ref log) = event_log {
        for ele in networks.iter() {
            log.send( P2PEvent::JoinedNetwork(peer.clone(), *ele))
                .map_err(|_| make_log_error("Join Network Event cannot be sent to log"))?;
        }
    }
    Ok(())
}

/// Log when it has been removed from a network.
pub fn log_as_leave_network(
        event_log: &Option<Sender<P2PEvent>>,
        sender: &P2PPeer,
        network: u16) -> FunctorResult {
    if let Some(ref log) = event_log {
        log.send( P2PEvent::LeftNetwork( sender.clone(), network))
            .map_err(|_| make_log_error("Left Network Event cannot be sent to log"))?;
    };
    Ok(())
}

/// It sends handshake message and a ping message.
pub fn send_handshake_and_ping(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>
    ) -> FunctorResult {

    let (my_nets, self_peer) = {
        let priv_conn_borrow = priv_conn.borrow();
        let my_nets = safe_read!(priv_conn_borrow.own_networks.clone())?.clone();
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
pub fn send_peer_list(
        priv_conn: &Rc< RefCell< ConnectionPrivate>>,
        sender: &P2PPeer,
        nets: &[u16]
    ) -> FunctorResult {

    debug!(
        "Running in bootstrapper mode, so instantly sending peers {} random peers",
        BOOTSTRAP_PEER_COUNT);

    let data = {
        let priv_conn_borrow = priv_conn.borrow();
        let random_nodes = safe_read!(priv_conn_borrow.buckets)?
            .get_random_nodes(&sender, BOOTSTRAP_PEER_COUNT, &nets);

        let self_peer = & priv_conn_borrow.self_peer;
        NetworkResponse::PeerList( self_peer.clone(), random_nodes).serialize()
    };

    serialize_bytes( &mut priv_conn.borrow_mut().tls_session, &data)?;

    if let Some(ref prom) = priv_conn.borrow().prometheus_exporter {
        let mut writable_prom = safe_write!(prom)?;
        writable_prom.pkt_sent_inc()
            .map_err(|_| make_fn_error_prometheus())?;
    };

    TOTAL_MESSAGES_SENT_COUNTER.inc();

    Ok(())
}

pub fn update_buckets(
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
        safe_write!(buckets)?
            .insert_into_bucket( sender, &own_id, nets.to_owned());
    } else if sender_ip.is_global()
            && !sender_ip.is_multicast()
            && !sender_ip.is_documentation() {
                safe_write!(buckets)?
                    .insert_into_bucket( sender, &own_id, nets.to_owned());
    }

    let prometheus_exporter = & priv_conn_borrow.prometheus_exporter;
    if let Some(ref prom) = prometheus_exporter {
        let mut writable_prom = safe_write!(prom)?;
        writable_prom.peers_inc()
            .map_err(|_| make_fn_error_prometheus())?;
        writable_prom.pkt_sent_inc_by(2)
            .map_err(|_| make_fn_error_prometheus())?;
    };

    Ok(())
}

/// Node is valid if its mode is `NormalPrivateMode` or `BootstrapperPrivateMode`.
pub fn is_valid_mode(
    priv_conn: &Rc< RefCell< ConnectionPrivate>> ) -> bool {
    let mode = priv_conn.borrow().mode;

    mode == P2PNodeMode::BootstrapperPrivateMode
    || mode == P2PNodeMode::NormalPrivateMode
}
