use std::sync::{ Arc, Mutex, mpsc::Sender };
use std::collections::{ HashMap, HashSet };
use mio::{ Token, Poll, Event };

use errors::{ ErrorKindWrapper, ResultExtWrapper };
use common::{ P2PNodeId, P2PPeer, ConnectionType, get_current_stamp };
use connection::{ Connection, P2PNodeMode };
use network::{ NetworkMessage, NetworkRequest };
use prometheus_exporter::{ PrometheusServer };

use p2p::peer_statistics::{ PeerStatistic };
use p2p::unreachable_nodes::{ UnreachableNodes };

const MAX_FAILED_PACKETS_ALLOWED: u32 = 50;
const MAX_UNREACHABLE_MARK_TIME: u64 = 1000 * 60 * 60 * 24;

/// This class allows to share some information between `TlsServer` and its handler.
/// This concept is similar to `d-Pointer` of C++ but it is used just to facilitate information
/// sharing.
pub struct TlsServerPrivate {
    pub connections: HashMap<Token, Connection>,
    pub unreachable_nodes: UnreachableNodes,
    pub banned_peers: HashSet<P2PPeer>,
    pub networks: Arc<Mutex<Vec<u16>>>,
    pub prometheus_exporter: Option<Arc<Mutex<PrometheusServer>>>,
}

impl TlsServerPrivate {
    pub fn new(
            networks: Vec<u16>,
            prometheus_exporter: Option<Arc<Mutex<PrometheusServer>>>) -> Self {
        TlsServerPrivate {
            connections: HashMap::new(),
            unreachable_nodes: UnreachableNodes::new(),
            banned_peers: HashSet::new(),
            networks: Arc::new(Mutex::new(networks)),
            prometheus_exporter: prometheus_exporter
        }
    }

    /// It adds new node to the banned peer list.
    pub fn ban_node(&mut self, peer: P2PPeer) -> bool {
        self.banned_peers.insert(peer)
    }

    /// It remove a node from the banned peer list.
    pub fn unban_node(&mut self, peer: &P2PPeer) -> bool {
        self.banned_peers.remove(peer)
    }

    /// It removes this server from `network_id` network.
    /// *Note:* Network list is shared, and this will updated all other instances.
    pub fn remove_network(&mut self, network_id: &u16) -> ResultExtWrapper<()> {
        self.networks.lock()?.retain(|x| x == network_id);
        Ok(())
    }

    /// It adds this server to `network_id` network.
    pub fn add_network(&mut self, network_id: &u16) -> ResultExtWrapper<()> {
        {
            let mut networks = self.networks.lock()?;
            if !networks.contains(network_id) {
                networks.push(*network_id)
            }
        }
        Ok(())
    }

    /// It generates a peer statistic list for each connected peer which belongs to
    /// any of networks in `nids`.
    pub fn get_peer_stats(&self, nids: &Vec<u16>) -> Vec<PeerStatistic> {
        let mut ret = vec![];
        for (_, ref conn) in &self.connections {
            match conn.peer() {
                Some(ref x) => {
                    if nids.len() == 0 || conn.networks().iter().any(|nid| nids.contains(nid)) {
                        ret.push(
                            PeerStatistic::new(
                                x.id().to_string(),
                                x.ip().clone(),
                                x.port(),
                                conn.get_messages_sent(),
                                conn.get_messages_received(),
                                conn.get_last_latency_measured()));
                    }
                }
                None => {}
            }
        }

        ret
    }

    /// It find a connection by its `P2PNodeId`.
    pub fn find_connection(&mut self, id: P2PNodeId) -> Option<&mut Connection> {
        let mut tok = Token(0);
        for (token, mut connection) in &self.connections {
            match connection.peer() {
                Some(ref x) => {
                    if x.id() == id {
                        tok = *token;
                    } else {
                        break;
                    }
                }
                _ => {
                    break;
                }
            }
        }

        if tok == Token(0) {
            None
        } else {
            match self.connections.get_mut(&tok) {
                Some(x) => Some(x),
                None => {
                    error!("Couldn't get connections mutable");
                    None
                }
            }
        }
    }

    pub fn conn_event(&mut self,
                  poll: &mut Poll,
                  event: &Event,
                  packet_queue: &Sender<Arc<Box<NetworkMessage>>>)
                  -> ResultExtWrapper<()> {
        let token = event.token();
        if self.connections.contains_key(&token) {
            match self.connections.get_mut(&token) {
                Some(x) => x.ready(poll, event, &packet_queue)
                            .map_err(|e| error!("Error while performing ready() check on connection '{}'", e))
                            .ok(),
                None => {
                    return Err(ErrorKindWrapper::LockingError("Couldn't get lock for connection".to_string()).into())
                }
            };

            if self.connections[&token].is_closed() {
                self.connections.remove(&token);
            }
        }
        Ok(())
    }

    pub fn cleanup_connections(&mut self, mode: P2PNodeMode, mut poll: &mut Poll) -> ResultExtWrapper<()> {
        let curr_stamp = get_current_stamp();

        if mode == P2PNodeMode::BootstrapperMode || mode == P2PNodeMode::BootstrapperPrivateMode {
            for conn in self.connections.values_mut() {
                if conn.last_seen() + 300000 < curr_stamp {
                    conn.close(&mut poll).map_err(|e| error!("{}", e)).ok();
                }
            }
        } else {
            for conn in self.connections.values_mut() {
                if conn.last_seen() + 1200000 < curr_stamp
                    && conn.connection_type() == ConnectionType::Node
                    {
                        conn.close(&mut poll).map_err(|e| error!("{}", e)).ok();
                    }
                if conn.failed_pkts() >= MAX_FAILED_PACKETS_ALLOWED {
                    conn.close(&mut poll).map_err(|e| error!("{}", e)).ok();
                }
            }
            self.unreachable_nodes
                .cleanup(curr_stamp - MAX_UNREACHABLE_MARK_TIME);
        }

        let closed_ones: Vec<_> = self.connections
            .iter()
            .filter(|&(_, &ref v)| v.closing)
            .map(|(k, _)| k.clone())
            .collect();
        for closed in closed_ones {
            if let Some(ref prom) = &self.prometheus_exporter {
                if let Some(ref peer) = self.connections.get(&closed) {
                    if let Some(_) = peer.peer() {
                        prom.lock()?.peers_dec().map_err(|e| error!("{}", e)).ok();
                    };
                };
            };

            self.connections.remove(&closed);
        }

        //Kill banned connections
        for peer in &self.banned_peers {
            for conn in self.connections.values_mut() {
                match conn.peer().clone() {
                    Some(ref p) => {
                        if p == peer {
                            conn.close(&mut poll).map_err(|e| error!("{}", e)).ok();
                        }
                    }
                    None => {}
                }
            }
        }
        Ok(())

    }

    pub fn liveness_check(&mut self) -> ResultExtWrapper<()> {
        let curr_stamp = get_current_stamp();

        for conn in self.connections.values_mut() {
            if conn.last_seen() + 120000 < curr_stamp
               || conn.get_last_ping_sent() + 300000 < curr_stamp
            {
                let self_peer = conn.get_self_peer().clone();
                conn.serialize_bytes( &NetworkRequest::Ping(self_peer).serialize())
                    .map_err(|e| error!("{}", e))
                    .ok();
                conn.set_measured_ping_sent();
                conn.set_last_ping_sent();
            }
        }
        Ok(())
    }

    /// It sends `data` message over all filtered connections.
    ///
    /// # Arguments
    /// * `data` - Raw message.
    /// * `filter_conn` - It will send using all connection, where this function returns `true`.
    /// * `send_status` - It will called after each sent, to notify the result of the operation.
    pub fn send_over_all_connections( &mut self,
            data: &Vec<u8>,
            filter_conn: &Fn( &Connection) -> bool,
            send_status: &Fn( &Connection, ResultExtWrapper<()>))
    {
        for conn in self.connections.values_mut()
        {
            if filter_conn( conn) {
                let status = conn.serialize_bytes( data);
                send_status( conn, status)
            }
        }
    }

}
