use rustls::{ClientSession, ServerSession};
use std::{
    collections::HashSet,
    net::IpAddr,
    sync::{
        atomic::{AtomicU64, Ordering},
        mpsc::Sender,
        Arc, RwLock,
    },
};

use failure::Fallible;

use crate::{
    common::{get_current_stamp, P2PNodeId, P2PPeer, PeerType, RemotePeer},
    connection::{CommonSession, P2PEvent},
    network::{Buckets, NetworkId},
    prometheus_exporter::PrometheusServer,
};

/// It is just a helper struct to facilitate sharing information with
/// message handlers, which are set up from _inside_ `Connection`.
/// In this way, all closures only need two arguments:
///     - This structure as a shared object, like `Rc< RefCell<...>>`
///     - The input message.
pub struct ConnectionPrivate {
    pub local_peer:          P2PPeer,
    pub remote_peer:         RemotePeer,
    pub remote_end_networks: HashSet<NetworkId>,
    pub local_end_networks:  Arc<RwLock<HashSet<NetworkId>>>,
    pub buckets:             Arc<RwLock<Buckets>>,

    // Session
    pub tls_session: Box<dyn CommonSession>,

    // Stats
    last_seen:               AtomicU64,
    pub failed_pkts:         u32,
    pub prometheus_exporter: Option<Arc<RwLock<PrometheusServer>>>,
    pub event_log:           Option<Sender<P2PEvent>>,

    // Time
    pub sent_handshake:        u64,
    pub sent_ping:             u64,
    pub last_latency_measured: u64,

    blind_trusted_broadcast: bool,
}

impl ConnectionPrivate {
    pub fn new(
        local_peer: P2PPeer,
        remote_peer: RemotePeer,
        local_end_networks: Arc<RwLock<HashSet<NetworkId>>>,
        buckets: Arc<RwLock<Buckets>>,
        tls_server_session: Option<ServerSession>,
        tls_client_session: Option<ClientSession>,
        prometheus_exporter: Option<Arc<RwLock<PrometheusServer>>>,
        event_log: Option<Sender<P2PEvent>>,
        blind_trusted_broadcast: bool,
    ) -> Self {
        let u64_max_value: u64 = u64::max_value();
        let tls_session = if let Some(s) = tls_server_session {
            Box::new(s) as Box<dyn CommonSession>
        } else if let Some(c) = tls_client_session {
            Box::new(c) as Box<dyn CommonSession>
        } else {
            panic!("Connection needs one session");
        };

        ConnectionPrivate {
            local_peer,
            remote_peer,
            remote_end_networks: HashSet::new(),
            local_end_networks,
            buckets,
            tls_session,
            last_seen: AtomicU64::new(get_current_stamp()),
            failed_pkts: 0,
            prometheus_exporter,
            event_log,
            sent_handshake: u64_max_value,
            sent_ping: u64_max_value,
            last_latency_measured: u64_max_value,
            blind_trusted_broadcast,
        }
    }

    pub fn update_last_seen(&mut self) {
        if self.local_peer.peer_type() != PeerType::Bootstrapper {
            self.last_seen.store(get_current_stamp(), Ordering::Relaxed);
        }
    }

    pub fn last_seen(&self) -> u64 { self.last_seen.load(Ordering::Relaxed) }

    #[inline]
    pub fn add_remote_end_network(&mut self, network: NetworkId) {
        self.remote_end_networks.insert(network);
    }

    #[inline]
    pub fn add_remote_end_networks(&mut self, networks: &HashSet<NetworkId>) {
        self.remote_end_networks.extend(networks.iter())
    }

    pub fn remove_remote_end_network(&mut self, network: NetworkId) {
        self.remote_end_networks.remove(&network);
    }

    pub fn set_measured_ping_sent(&mut self) { self.sent_ping = get_current_stamp() }

    pub fn remote_peer(&self) -> RemotePeer { self.remote_peer.clone() }

    pub fn remote_ip(&self) -> IpAddr { self.remote_peer().ip() }

    pub fn remote_port(&self) -> u16 { self.remote_peer().port() }

    pub fn promote_to_post_handshake(
        &mut self,
        id: P2PNodeId,
        ip: IpAddr,
        port: u16,
    ) -> Fallible<()> {
        self.remote_peer = self.remote_peer.promote_to_post_handshake(id, ip, port)?;
        Ok(())
    }

    #[allow(unused)]
    pub fn blind_trusted_broadcast(&self) -> bool { self.blind_trusted_broadcast }
}
