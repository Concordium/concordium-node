use std::sync::atomic::{ AtomicU64, Ordering };
use std::sync::mpsc::{ Sender };
use std::sync::{ Arc, Mutex, RwLock };
use rustls::{ ServerSession, ClientSession };

use crate::common::{ P2PNodeId, P2PPeer, ConnectionType, get_current_stamp };
use crate::connection::{ P2PNodeMode, P2PEvent, CommonSession };
use crate::network::{ Buckets };
use crate::prometheus_exporter::{ PrometheusServer };


/// It is just a helper struct to facilitate sharing information with
/// message handlers, which are set up from _inside_ `Connection`.
/// In this way, all closures only need two arguments:
///     - This structure as a shared object, like `Rc< RefCell<...>>`
///     - The input message.
pub struct ConnectionPrivate {
    pub connection_type: ConnectionType,
    pub own_id: P2PNodeId,
    pub mode: P2PNodeMode,
    pub self_peer: P2PPeer,
    peer: Option<P2PPeer>,
    pub networks: Vec<u16>,
    pub own_networks: Arc<Mutex<Vec<u16>>>,
    pub buckets: Arc< RwLock< Buckets > >,

    // Session
    pub tls_session: Box<dyn CommonSession>,

    // Stats
    last_seen: AtomicU64,
    pub failed_pkts: u32,
    pub prometheus_exporter: Option<Arc<Mutex<PrometheusServer>>>,
    pub event_log: Option<Sender<P2PEvent>>,

    // Time
    pub sent_handshake: u64,
    pub sent_ping: u64,
    pub last_latency_measured: u64,

    blind_trusted_broadcast: bool,
}

impl ConnectionPrivate {
    pub fn new(
            connection_type: ConnectionType,
            mode: P2PNodeMode,
            own_id: P2PNodeId,
            self_peer: P2PPeer,
            own_networks: Arc< Mutex< Vec<u16>>>,
            buckets: Arc< RwLock< Buckets > >,
            tls_server_session: Option< ServerSession>,
            tls_client_session: Option< ClientSession>,
            prometheus_exporter: Option<Arc<Mutex<PrometheusServer>>>,
            event_log: Option<Sender<P2PEvent>>,
            blind_trusted_broadcast: bool,
            ) -> Self {

        let u64_max_value: u64 = u64::max_value();
        let tls_session = if let Some(s) = tls_server_session {
                box s as Box<dyn CommonSession>
            } else if let Some(c) = tls_client_session {
                box c as Box<dyn CommonSession>
            } else {
                panic!( "Connection needs one session");
        };

        ConnectionPrivate {
            connection_type: connection_type,
            mode: mode,
            own_id: own_id,
            self_peer: self_peer,
            peer: None,
            networks: vec![],
            own_networks: own_networks,
            buckets: buckets,

            tls_session: tls_session,

            last_seen: AtomicU64::new( get_current_stamp()),
            failed_pkts: 0,
            prometheus_exporter: prometheus_exporter,
            event_log: event_log,

            sent_handshake: u64_max_value,
            sent_ping: u64_max_value,
            last_latency_measured: u64_max_value,
            blind_trusted_broadcast,
        }
    }

    pub fn update_last_seen(&mut self) {
        if self.mode != P2PNodeMode::BootstrapperMode
            && self.mode != P2PNodeMode::BootstrapperPrivateMode {
            self.last_seen.store( get_current_stamp(), Ordering::Relaxed);
        }
    }

    pub fn last_seen(&self) -> u64 {
        self.last_seen.load( Ordering::Relaxed)
    }

    pub fn add_networks(&mut self, networks: &[u16]) {
        for ele in networks {
            if !self.networks.contains(ele) {
                self.networks.push(*ele);
            }
        }
    }

    pub fn remove_network(&mut self, network: &u16) {
        self.networks.retain(|x| x != network);
    }

    pub fn set_measured_ping_sent(&mut self) {
        self.sent_ping = get_current_stamp()
    }

    pub fn peer(&self) -> Option<P2PPeer> {
        self.peer.clone()
    }

    pub fn set_peer(&mut self, p: P2PPeer) {
        self.peer = Some(p);
    }

    #[allow(unused)]
    pub fn blind_trusted_broadcast(&self) -> bool {
        self.blind_trusted_broadcast
    }
}

