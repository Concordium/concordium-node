use rustls::{ClientSession, ServerSession};
use std::{
    collections::HashSet,
    net::SocketAddr,
    sync::{
        atomic::{AtomicU64, Ordering},
        mpsc::Sender,
        Arc, RwLock,
    },
};

use failure::Fallible;

use crate::{
    common::{get_current_stamp, P2PNodeId, P2PPeer, PeerType, RemotePeer},
    connection::{fails, CommonSession, P2PEvent},
    network::{Buckets, NetworkId},
    stats_export_service::StatsExportService,
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
    pub last_seen:            AtomicU64,
    pub failed_pkts:          u32,
    pub stats_export_service: Option<Arc<RwLock<StatsExportService>>>,
    pub event_log:            Option<Sender<P2PEvent>>,

    // Time
    pub sent_handshake:        u64,
    pub sent_ping:             u64,
    pub last_latency_measured: u64,

    pub blind_trusted_broadcast: bool,
}

impl ConnectionPrivate {
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

    pub fn promote_to_post_handshake(&mut self, id: P2PNodeId, addr: SocketAddr) -> Fallible<()> {
        self.remote_peer = self.remote_peer.promote_to_post_handshake(id, addr)?;
        Ok(())
    }

    #[allow(unused)]
    pub fn blind_trusted_broadcast(&self) -> bool { self.blind_trusted_broadcast }
}

pub struct ConnectionPrivateBuilder {
    pub local_peer:         Option<P2PPeer>,
    pub remote_peer:        Option<RemotePeer>,
    pub local_end_networks: Option<Arc<RwLock<HashSet<NetworkId>>>>,
    pub buckets:            Option<Arc<RwLock<Buckets>>>,

    // Sessions
    pub server_session: Option<ServerSession>,
    pub client_session: Option<ClientSession>,

    // Stats
    pub stats_export_service: Option<Arc<RwLock<StatsExportService>>>,
    pub event_log:            Option<Sender<P2PEvent>>,

    pub blind_trusted_broadcast: Option<bool>,
}

impl Default for ConnectionPrivateBuilder {
    fn default() -> Self { ConnectionPrivateBuilder::new() }
}

impl ConnectionPrivateBuilder {
    pub fn new() -> ConnectionPrivateBuilder {
        ConnectionPrivateBuilder {
            local_peer:              None,
            remote_peer:             None,
            local_end_networks:      None,
            buckets:                 None,
            server_session:          None,
            client_session:          None,
            stats_export_service:    None,
            event_log:               None,
            blind_trusted_broadcast: None,
        }
    }

    pub fn build(self) -> Fallible<ConnectionPrivate> {
        let u64_max_value: u64 = u64::max_value();
        let tls_session = if let Some(s) = self.server_session {
            Box::new(s) as Box<dyn CommonSession>
        } else if let Some(c) = self.client_session {
            Box::new(c) as Box<dyn CommonSession>
        } else {
            bail!(fails::MissingFieldsConnectionBuilder);
        };
        if let (
            Some(local_peer),
            Some(remote_peer),
            Some(local_end_networks),
            Some(buckets),
            Some(blind_trusted_broadcast),
        ) = (
            self.local_peer,
            self.remote_peer,
            self.local_end_networks,
            self.buckets,
            self.blind_trusted_broadcast,
        ) {
            Ok(ConnectionPrivate {
                local_peer,
                remote_peer,
                remote_end_networks: HashSet::new(),
                local_end_networks,
                buckets,
                tls_session,
                last_seen: AtomicU64::new(get_current_stamp()),
                failed_pkts: 0,
                stats_export_service: self.stats_export_service,
                event_log: self.event_log,
                sent_handshake: u64_max_value,
                sent_ping: u64_max_value,
                last_latency_measured: u64_max_value,
                blind_trusted_broadcast,
            })
        } else {
            bail!(fails::MissingFieldsConnectionBuilder)
        }
    }

    pub fn set_local_peer(mut self, p: P2PPeer) -> ConnectionPrivateBuilder {
        self.local_peer = Some(p);
        self
    }

    pub fn set_remote_peer(mut self, p: RemotePeer) -> ConnectionPrivateBuilder {
        self.remote_peer = Some(p);
        self
    }

    pub fn set_local_end_networks(
        mut self,
        local_end_nets: Arc<RwLock<HashSet<NetworkId>>>,
    ) -> ConnectionPrivateBuilder {
        self.local_end_networks = Some(local_end_nets);
        self
    }

    pub fn set_buckets(mut self, buckets: Arc<RwLock<Buckets>>) -> ConnectionPrivateBuilder {
        self.buckets = Some(buckets);
        self
    }

    pub fn set_server_session(mut self, ss: Option<ServerSession>) -> ConnectionPrivateBuilder {
        self.server_session = ss;
        self
    }

    pub fn set_client_session(mut self, cs: Option<ClientSession>) -> ConnectionPrivateBuilder {
        self.client_session = cs;
        self
    }

    pub fn set_stats_export_service(
        mut self,
        se: Option<Arc<RwLock<StatsExportService>>>,
    ) -> ConnectionPrivateBuilder {
        self.stats_export_service = se;
        self
    }

    pub fn set_event_log(mut self, el: Option<Sender<P2PEvent>>) -> ConnectionPrivateBuilder {
        self.event_log = el;
        self
    }

    pub fn set_blind_trusted_broadcast(mut self, btb: bool) -> ConnectionPrivateBuilder {
        self.blind_trusted_broadcast = Some(btb);
        self
    }
}
