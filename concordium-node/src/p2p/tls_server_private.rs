use failure::{bail, err_msg, Fallible};
use mio::{Event, Poll, Token};
use std::{
    cell::RefCell,
    collections::{HashSet, VecDeque},
    net::{IpAddr, SocketAddr},
    rc::Rc,
    sync::{Arc, RwLock},
};

use crate::{
    common::{get_current_stamp, serialization::Serializable, P2PNodeId, PeerType, RemotePeer},
    connection::Connection,
    network::{NetworkId, NetworkMessage, NetworkRequest},
    p2p::{
        banned_nodes::{BannedNode, BannedNodes},
        peer_statistics::PeerStatistic,
        unreachable_nodes::UnreachableNodes,
    },
    stats_export_service::StatsExportService,
};

use super::fails;

const MAX_FAILED_PACKETS_ALLOWED: u32 = 50;
const MAX_UNREACHABLE_MARK_TIME: u64 = 86_400_000;
const MAX_BOOTSTRAPPER_KEEP_ALIVE: u64 = 300_000;
const MAX_NORMAL_KEEP_ALIVE: u64 = 1_200_000;
const MAX_PREHANDSHAKE_KEEP_ALIVE: u64 = 120_000;

/// This class allows to share some information between `TlsServer` and its
/// handler. This concept is similar to `d-Pointer` of C++ but it is used just
/// to facilitate information sharing.
///
/// # Connections
/// Connections are stored in RC in two `hashmap`s in order to improve
/// performance access for specific look-ups.
pub struct TlsServerPrivate {
    connections:              Vec<Rc<RefCell<Connection>>>,
    pub to_disconnect:        Rc<RefCell<VecDeque<P2PNodeId>>>,
    pub unreachable_nodes:    UnreachableNodes,
    pub banned_peers:         Rc<RefCell<BannedNodes>>,
    pub networks:             Arc<RwLock<HashSet<NetworkId>>>,
    pub stats_export_service: Option<Arc<RwLock<StatsExportService>>>,
}

impl TlsServerPrivate {
    pub fn new(
        networks: HashSet<NetworkId>,
        stats_export_service: Option<Arc<RwLock<StatsExportService>>>,
    ) -> Self {
        TlsServerPrivate {
            connections: Vec::new(),
            to_disconnect: Rc::new(RefCell::new(VecDeque::<P2PNodeId>::new())),
            unreachable_nodes: UnreachableNodes::new(),
            banned_peers: Rc::new(RefCell::new(BannedNodes::new())),
            networks: Arc::new(RwLock::new(networks)),
            stats_export_service,
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
    pub fn remove_network(&mut self, network_id: NetworkId) {
        write_or_die!(self.networks).retain(|x| *x != network_id);
    }

    /// It adds this server to `network_id` network.
    pub fn add_network(&mut self, network_id: NetworkId) {
        write_or_die!(self.networks).insert(network_id);
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
                        remote_peer.addr,
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

    pub fn find_connection_by_ip_addr(&self, addr: SocketAddr) -> Option<&Rc<RefCell<Connection>>> {
        self.connections
            .iter()
            .find(|conn| conn.borrow().remote_addr() == addr)
    }

    pub fn find_connections_by_ip(
        &self,
        ip: IpAddr,
    ) -> impl Iterator<Item = &Rc<RefCell<Connection>>> {
        self.connections
            .iter()
            .filter(move |&conn| conn.borrow().remote_peer().addr().ip() == ip)
    }

    fn remove_connection(&mut self, to_remove: Token) {
        self.connections
            .retain(|conn| conn.borrow().token() != to_remove);
    }

    pub fn add_connection(&mut self, conn: Connection) {
        self.connections.push(Rc::new(RefCell::new(conn)));
    }

    pub fn conn_event(&mut self, event: &Event) -> Fallible<()> {
        let token = event.token();

        if let Some(rc_conn) = self.find_connection_by_token(token) {
            let mut conn = rc_conn.borrow_mut();
            conn.ready(event)?;
        } else {
            bail!(fails::PeerNotFoundError)
        }

        Ok(())
    }

    pub fn cleanup_connections(&mut self, peer_type: PeerType, poll: &mut Poll) -> Fallible<()> {
        trace!("Cleaning up connections");
        let curr_stamp = get_current_stamp();

        trace!("Disconnecting already marked as going down");
        self.to_disconnect.borrow_mut().drain(..).for_each(|x| {
            if let Some(conn) = self.find_connection_by_id(x) {
                conn.borrow_mut().close();
            }
        });

        // Clean duplicates only if it's a regular node we're running
        if peer_type != PeerType::Bootstrapper {
            let mut connection_map: Vec<_> = self
                .connections
                .iter()
                .filter_map(|rc_conn| {
                    let rc_conn_borrowed = rc_conn.borrow();
                    rc_conn_borrowed.remote_id().and_then(|remote_id| {
                        Some((
                            remote_id,
                            rc_conn_borrowed.token(),
                            rc_conn_borrowed.last_seen(),
                        ))
                    })
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

        trace!("Filtering by non-fatal gone connections");
        let wrap_connection_already_gone_as_non_fatal =
            |token, res: Fallible<()>| -> Fallible<Token> {
                use std::io::ErrorKind;
                match res {
                    Err(e) => {
                        let downcasted_error = e.downcast::<std::io::Error>()?;
                        match downcasted_error.kind() {
                            ErrorKind::NotFound | ErrorKind::NotConnected => Ok(token),
                            _ => into_err!(Err(downcasted_error)),
                        }
                    }
                    _ => Ok(token),
                }
            };

        let filter_predicate_bootstrapper_no_activity_allowed_period = |conn: &Connection| -> bool {
            peer_type == PeerType::Bootstrapper
                && conn.is_post_handshake()
                && conn.last_seen() + MAX_BOOTSTRAPPER_KEEP_ALIVE < curr_stamp
        };

        let filter_predicate_node_no_activity_allowed_period = |conn: &Connection| -> bool {
            peer_type == PeerType::Node
                && conn.is_post_handshake()
                && conn.last_seen() + MAX_NORMAL_KEEP_ALIVE < curr_stamp
        };

        let filter_predicate_stable_conn_and_no_handshake = |conn: &Connection| -> bool {
            conn.failed_pkts() >= MAX_FAILED_PACKETS_ALLOWED
                || !conn.is_post_handshake()
                    && conn.last_seen() + MAX_PREHANDSHAKE_KEEP_ALIVE < curr_stamp
        };

        trace!(
            "Killing nodes who are marked for closing, didn't complete handshake fast enough, or \
             lingered without sending us packets for too long"
        );
        // Kill nodes which are no longer seen and also closing connections
        let (closing_conns, err_conns): (Vec<Fallible<Token>>, Vec<Fallible<Token>>) = self.connections
            .iter()
            // Get only connections that have been inactive for more time than allowed or closing connections
            .filter(|rc_conn| {
                let rc_conn_borrowed = rc_conn.borrow();
                rc_conn_borrowed.closing ||
                    filter_predicate_stable_conn_and_no_handshake(&rc_conn_borrowed) ||
                    filter_predicate_bootstrapper_no_activity_allowed_period(&rc_conn_borrowed) ||
                    filter_predicate_node_no_activity_allowed_period(&rc_conn_borrowed)
            })
            .map(|rc_conn| {
                // Deregister connection from the poll and shut down the socket
                let mut conn = rc_conn.borrow_mut();
                trace!("Going to kill {}:{}", conn.remote_addr().ip(), conn.remote_addr().port());
                wrap_connection_already_gone_as_non_fatal(conn.token(),into_err!(poll.deregister(&conn.socket)))?;
                wrap_connection_already_gone_as_non_fatal(conn.token(), conn.shutdown())?;
                // Report number of peers to stats export engine
                if let Some(ref service) = &self.stats_export_service {
                    if conn.is_post_handshake() {
                        if let Ok(mut p) = safe_write!(service) {
                            p.peers_dec();
                        }
                    }
                }
                Ok(conn.token())
            }).partition(Result::is_ok);

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
        trace!("Completing liveness check of peers");
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

                let request_ping = NetworkMessage::NetworkRequest(
                    NetworkRequest::Ping(local_peer),
                    Some(get_current_stamp()),
                    None,
                );
                if let Ok(request_ping_data) = serialize_into_memory!(request_ping, 128) {
                    conn.serialize_bytes(&request_ping_data)
                        .map_err(|e| error!("{}", e))
                        .ok();
                    conn.set_measured_ping_sent();
                    conn.set_last_ping_sent();
                }
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
