use failure::{bail, err_msg, Fallible};
use mio::{Event, Poll, Token};
use std::{
    cell::RefCell,
    collections::{HashSet, VecDeque},
    net::{IpAddr, SocketAddr},
    rc::Rc,
    sync::{mpsc::Sender, Arc, RwLock},
};

use crate::{
    common::{get_current_stamp, P2PNodeId, PeerType, RemotePeer},
    connection::Connection,
    network::{NetworkId, NetworkMessage, NetworkRequest},
    prometheus_exporter::PrometheusServer,
};

use crate::p2p::{
    banned_nodes::{BannedNode, BannedNodes},
    peer_statistics::PeerStatistic,
    unreachable_nodes::UnreachableNodes,
};

use super::fails;

const MAX_FAILED_PACKETS_ALLOWED: u32 = 50;
const MAX_UNREACHABLE_MARK_TIME: u64 = 1000 * 60 * 60 * 24;
const MAX_BOOTSTRAPPER_KEEP_ALIVE: u64 = 300_000;
const MAX_NORMAL_KEEP_ALIVE: u64 = 1_200_000;

/// This class allows to share some information between `TlsServer` and its
/// handler. This concept is similar to `d-Pointer` of C++ but it is used just
/// to facilitate information sharing.
///
/// # Connections
/// Connections are stored in RC in two `hashmap`s in order to improve
/// performance access for specific look-ups.
pub struct TlsServerPrivate {
    connections:             Vec<Rc<RefCell<Connection>>>,
    pub to_disconnect:       Rc<RefCell<VecDeque<P2PNodeId>>>,
    pub unreachable_nodes:   UnreachableNodes,
    pub banned_peers:        Rc<RefCell<BannedNodes>>,
    pub networks:            Arc<RwLock<HashSet<NetworkId>>>,
    pub prometheus_exporter: Option<Arc<RwLock<PrometheusServer>>>,
}

impl TlsServerPrivate {
    pub fn new(
        networks: HashSet<NetworkId>,
        prometheus_exporter: Option<Arc<RwLock<PrometheusServer>>>,
    ) -> Self {
        TlsServerPrivate {
            connections: Vec::new(),
            to_disconnect: Rc::new(RefCell::new(VecDeque::<P2PNodeId>::new())),
            unreachable_nodes: UnreachableNodes::new(),
            banned_peers: Rc::new(RefCell::new(BannedNodes::new())),
            networks: Arc::new(RwLock::new(networks)),
            prometheus_exporter,
        }
    }

    /// Adds a new node to the banned list and marks its connection for closure
    pub fn ban_node(&mut self, peer: BannedNode) -> bool {
        if self.banned_peers.borrow_mut().insert(peer) {
            match peer {
                BannedNode::ById(id) => {
                    if let Some(c) = self.find_connection_by_id(id) {
                        c.borrow_mut().close();
                    }
                }
                BannedNode::ByAddr(addr) => {
                    self.find_connections_by_ip(addr).for_each(|c| {
                        c.borrow_mut().close();
                    });
                }
            };
            true
        } else {
            false
        }
    }

    /// It removes a node from the banned peer list.
    pub fn unban_node(&mut self, peer: BannedNode) -> bool {
        self.banned_peers.borrow_mut().remove(&peer)
    }

    pub fn id_is_banned(&self, id: P2PNodeId) -> bool {
        self.banned_peers.borrow().is_id_banned(id)
    }

    pub fn addr_is_banned(&self, sockaddr: SocketAddr) -> bool {
        self.banned_peers.borrow().is_addr_banned(sockaddr.ip())
    }

    pub fn get_banlist(&self) -> Vec<BannedNode> {
        self.banned_peers
            .borrow()
            .by_id
            .iter()
            .map(|id| BannedNode::ById(*id))
            .chain(
                self.banned_peers
                    .borrow()
                    .by_addr
                    .iter()
                    .map(|addr| BannedNode::ByAddr(*addr)),
            )
            .collect()
    }

    /// It removes this server from `network_id` network.
    /// *Note:* Network list is shared, and this will updated all other
    /// instances.
    pub fn remove_network(&mut self, network_id: NetworkId) -> Fallible<()> {
        safe_write!(self.networks)?.retain(|x| *x == network_id);
        Ok(())
    }

    /// It adds this server to `network_id` network.
    pub fn add_network(&mut self, network_id: NetworkId) -> Fallible<()> {
        safe_write!(self.networks)?.insert(network_id);
        Ok(())
    }

    /// It generates a peer statistic list for each connected peer which belongs
    /// to any of networks in `nids`.
    pub fn get_peer_stats(&self, nids: &[NetworkId]) -> Vec<PeerStatistic> {
        let mut ret = vec![];
        for rc_conn in &self.connections {
            let conn = rc_conn.borrow();
            if let RemotePeer::PostHandshake(remote_peer) = conn.remote_peer() {
                if nids.is_empty()
                    || conn
                        .remote_end_networks()
                        .iter()
                        .any(|nid| nids.contains(nid))
                {
                    ret.push(PeerStatistic::new(
                        remote_peer.id().to_string(),
                        remote_peer.ip(),
                        remote_peer.port(),
                        remote_peer.peer_type(),
                        conn.get_messages_sent(),
                        conn.get_messages_received(),
                        conn.get_last_latency_measured(),
                    ));
                }
            }
        }

        ret
    }

    pub fn find_connection_by_id(&self, id: P2PNodeId) -> Option<&Rc<RefCell<Connection>>> {
        self.connections
            .iter()
            .find(|&conn| conn.borrow().remote_id() == Some(id))
    }

    pub fn find_connections_by_id(&self, id: P2PNodeId) -> Vec<&Rc<RefCell<Connection>>> {
        self.connections
            .iter()
            .filter(|&conn| conn.borrow().remote_id() == Some(id))
            .collect()
    }

    pub fn find_connection_by_token(&self, token: Token) -> Option<&Rc<RefCell<Connection>>> {
        self.connections
            .iter()
            .find(|&conn| conn.borrow().token() == token)
    }

    pub fn find_connection_by_ip_addr(
        &self,
        ip: IpAddr,
        port: u16,
    ) -> Option<&Rc<RefCell<Connection>>> {
        self.connections.iter().find(|&conn| {
            let borrowed = conn.borrow();
            borrowed.remote_ip() == ip && borrowed.remote_port() == port
        })
    }

    pub fn find_connections_by_ip(
        &self,
        ip: IpAddr,
    ) -> impl Iterator<Item = &Rc<RefCell<Connection>>> {
        self.connections.iter().filter(move |&conn| {
            let borrowed = conn.borrow();
            borrowed.remote_ip() == ip
        })
    }

    fn remove_connection(&mut self, to_remove: Token) {
        self.connections
            .retain(|conn| conn.borrow().token() != to_remove);
    }

    pub fn add_connection(&mut self, conn: Connection) {
        self.connections.push(Rc::new(RefCell::new(conn)));
    }

    pub fn conn_event(
        &mut self,
        poll: &mut Poll,
        event: &Event,
        packet_queue: &Sender<Arc<NetworkMessage>>,
    ) -> Fallible<()> {
        let token = event.token();

        if let Some(rc_conn) = self.find_connection_by_token(token) {
            let mut conn = rc_conn.borrow_mut();
            conn.ready(poll, event, packet_queue)?;
        } else {
            bail!(fails::PeerNotFoundError)
        }

        Ok(())
    }

    pub fn cleanup_connections(&mut self, peer_type: PeerType, poll: &mut Poll) -> Fallible<()> {
        let curr_stamp = get_current_stamp();

        self.to_disconnect.borrow_mut().drain(..).for_each(|x| {
            if let Some(conn) = self.find_connection_by_id(x) {
                conn.borrow_mut().close();
            }
        });

        // Kill nodes which are no longer seen and also closing connections
        let (closing_conns, err_conns): (Vec<Fallible<Token>>, Vec<Fallible<Token>>) = self.connections
            .iter()
            // Get only connections that have been inactive for more time than allowed or closing connections
            .filter(|rc_conn| {
                let conn = rc_conn.borrow();
                (peer_type == PeerType::Bootstrapper
                    && conn.last_seen() + MAX_BOOTSTRAPPER_KEEP_ALIVE < curr_stamp)
                    || ((conn.last_seen() + MAX_NORMAL_KEEP_ALIVE < curr_stamp
                        && conn.local_peer().peer_type() == PeerType::Node)
                        || conn.failed_pkts() >= MAX_FAILED_PACKETS_ALLOWED)
                    || conn.closing
            })
            .map(|rc_conn| {
                // Deregister connection from the poll and shut down the socket
                let mut conn = rc_conn.borrow_mut();
                into_err!(poll.deregister(&conn.socket))?;
                let _ = match conn.shutdown() {
                    Err(e) => {
                        let downcasted_error = e.downcast::<std::io::Error>().unwrap();
                        match downcasted_error.kind() {
                            std::io::ErrorKind::NotFound => Ok(()),
                            _ => Err(failure::Error::from_boxed_compat(Box::new(downcasted_error)))
                        }
                    }
                    _ => Ok(())
                }?;
                // Report number of peers to prometheus
                if let Some(ref prom) = &self.prometheus_exporter {
                    if conn.is_post_handshake() {
                        if let Ok(mut p) = safe_write!(prom) {
                            p.peers_dec_by(1)?;
                        }
                    }
                }
                Ok(conn.token())
            }).partition(Result::is_ok);

        // Clean duplicates only if it's a regular node we're running
        if peer_type != PeerType::Bootstrapper {
            let mut connection_map: Vec<_> = self
                .connections
                .iter()
                .filter_map(|rc_conn| {
                    let rc_conn_borrowed = rc_conn.borrow();
                    if rc_conn_borrowed.remote_id().is_some() {
                        Some((
                            rc_conn_borrowed.remote_id().unwrap(),
                            rc_conn_borrowed.token(),
                            rc_conn_borrowed.last_seen(),
                        ))
                    } else {
                        None
                    }
                })
                .collect();
            connection_map.sort_by_key(|p| std::cmp::Reverse((p.0, p.2)));
            connection_map.dedup_by_key(|p| p.0);
            self.connections.iter().for_each(|rc_conn| {
                let mut rc_conn_borrowed = rc_conn.borrow_mut();
                if rc_conn_borrowed.remote_id().is_some()
                    && !connection_map
                        .iter()
                        .any(|(_, token, _)| token == &rc_conn_borrowed.token())
                {
                    rc_conn_borrowed.close();
                }
            });
        }

        // Remove the connection from the list of connections
        for conn in closing_conns {
            // safe unwrapping since we are iterating over the list that only contains `Ok`s
            self.remove_connection(conn.unwrap());
        }

        if peer_type != PeerType::Bootstrapper {
            self.unreachable_nodes
                .cleanup(curr_stamp - MAX_UNREACHABLE_MARK_TIME);
        }

        if !err_conns.is_empty() {
            bail!(err_msg(format!(
                "Some connections couldn't be cleaned: {:?}",
                err_conns
            )));
        } else {
            Ok(())
        }
    }

    pub fn liveness_check(&mut self) -> Fallible<()> {
        let curr_stamp = get_current_stamp();

        self.connections
            .iter()
            .filter(|ref rc_conn| {
                let conn = rc_conn.borrow();
                conn.last_seen() + 120_000 < curr_stamp
                    || conn.get_last_ping_sent() + 300_000 < curr_stamp
            })
            .for_each(|ref rc_conn| {
                let mut conn = rc_conn.borrow_mut();

                let local_peer = conn.local_peer();
                conn.serialize_bytes(&NetworkRequest::Ping(local_peer).serialize())
                    .map_err(|e| error!("{}", e))
                    .ok();
                conn.set_measured_ping_sent();
                conn.set_last_ping_sent();
            });

        Ok(())
    }

    /// It sends `data` message over all filtered connections.
    ///
    /// # Arguments
    /// * `data` - Raw message.
    /// * `filter_conn` - It will send using all connection, where this function
    ///   returns `true`.
    /// * `send_status` - It will called after each sent, to notify the result
    ///   of the operation.
    pub fn send_over_all_connections(
        &mut self,
        data: &[u8],
        filter_conn: &dyn Fn(&Connection) -> bool,
        send_status: &dyn Fn(&Connection, Fallible<usize>),
    ) {
        for rc_conn in self.connections.iter() {
            let mut conn = rc_conn.borrow_mut();
            if filter_conn(&conn) {
                let status = conn.serialize_bytes(data);
                send_status(&conn, status)
            }
        }
    }
}
