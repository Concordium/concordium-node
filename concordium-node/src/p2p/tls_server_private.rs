use failure::{bail, Error, Fallible};
use mio::{Event, Poll, Token};
use rand::seq::IteratorRandom;
use std::{
    collections::{HashSet, VecDeque},
    net::{IpAddr, SocketAddr},
    sync::{
        mpsc::{self, SyncSender},
        Arc, RwLock,
    },
};

use super::fails;
use crate::{
    common::{
        get_current_stamp, serialization::serialize_into_memory, NetworkRawRequest, P2PNodeId,
        PeerType, RemotePeer,
    },
    connection::{
        network_handler::message_processor::ProcessResult, Connection, MessageSendingPriority,
    },
    dumper::DumpItem,
    network::{NetworkId, NetworkMessage, NetworkRequest},
    p2p::{
        banned_nodes::{BannedNode, BannedNodes},
        peer_statistics::PeerStatistic,
        unreachable_nodes::UnreachableNodes,
    },
};
use concordium_common::{functor::UnitFunction, stats_export_service::StatsExportService, UCursor};

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
    pub network_request_sender: SyncSender<NetworkRawRequest>,
    connections:                Vec<Arc<RwLock<Connection>>>,
    pub to_disconnect:          Arc<RwLock<VecDeque<P2PNodeId>>>,
    pub unreachable_nodes:      UnreachableNodes,
    pub banned_peers:           Arc<RwLock<BannedNodes>>,
    pub networks:               Arc<RwLock<HashSet<NetworkId>>>,
    pub stats_export_service:   Option<Arc<RwLock<StatsExportService>>>,
}

impl TlsServerPrivate {
    pub fn new(
        networks: HashSet<NetworkId>,
        stats_export_service: Option<Arc<RwLock<StatsExportService>>>,
    ) -> Self {
        let (network_request_sender, _) = mpsc::sync_channel(64);

        TlsServerPrivate {
            network_request_sender,
            connections: Vec::new(),
            to_disconnect: Arc::new(RwLock::new(VecDeque::<P2PNodeId>::new())),
            unreachable_nodes: UnreachableNodes::new(),
            banned_peers: Arc::new(RwLock::new(BannedNodes::new())),
            networks: Arc::new(RwLock::new(networks)),
            stats_export_service,
        }
    }

    pub fn set_network_request_sender(&mut self, sender: SyncSender<NetworkRawRequest>) {
        for conn in &self.connections {
            write_or_die!(conn).set_network_request_sender(sender.clone());
        }
        self.network_request_sender = sender;
    }

    pub fn connections_posthandshake_count(&self, exclude_type: Option<PeerType>) -> u16 {
        // We will never have more than 2^16 connections per node, so this conversion is
        // safe.
        self.connections
            .iter()
            .filter(|&rc_conn| {
                let rc_conn_borrowed = read_or_die!(rc_conn);
                if !rc_conn_borrowed.is_post_handshake() {
                    return false;
                }
                if let Some(exclude_type) = exclude_type {
                    return rc_conn_borrowed.remote_peer().peer_type() != exclude_type;
                }
                true
            })
            .count() as u16
    }

    /// Adds a new node to the banned list and marks its connection for closure
    pub fn ban_node(&mut self, peer: BannedNode) -> bool {
        if write_or_die!(self.banned_peers).insert(peer) {
            match peer {
                BannedNode::ById(id) => {
                    if let Some(c) = self.find_connection_by_id(id) {
                        write_or_die!(c).close();
                    }
                }
                BannedNode::ByAddr(addr) => {
                    for conn in self.find_connections_by_ip(addr) {
                        write_or_die!(conn).close();
                    }
                }
            };
            true
        } else {
            false
        }
    }

    /// It removes a node from the banned peer list.
    pub fn unban_node(&mut self, peer: BannedNode) -> bool {
        write_or_die!(self.banned_peers).remove(&peer)
    }

    pub fn id_is_banned(&self, id: P2PNodeId) -> bool {
        read_or_die!(self.banned_peers).is_id_banned(id)
    }

    pub fn addr_is_banned(&self, sockaddr: SocketAddr) -> bool {
        read_or_die!(self.banned_peers).is_addr_banned(sockaddr.ip())
    }

    pub fn get_banlist(&self) -> Vec<BannedNode> {
        read_or_die!(self.banned_peers)
            .by_id
            .iter()
            .map(|id| BannedNode::ById(*id))
            .chain(
                read_or_die!(self.banned_peers)
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
            let conn = read_or_die!(rc_conn);
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

    pub fn find_connection_by_id(&self, id: P2PNodeId) -> Option<&Arc<RwLock<Connection>>> {
        self.connections
            .iter()
            .find(|&conn| read_or_die!(conn).remote_id() == Some(id))
    }

    pub fn find_connections_by_id(&self, id: P2PNodeId) -> Vec<&Arc<RwLock<Connection>>> {
        self.connections
            .iter()
            .filter(|&conn| read_or_die!(conn).remote_id() == Some(id))
            .collect()
    }

    pub fn find_connection_by_token(&self, token: Token) -> Option<&Arc<RwLock<Connection>>> {
        self.connections
            .iter()
            .find(|&conn| read_or_die!(conn).token() == token)
    }

    pub fn find_connection_by_ip_addr(&self, addr: SocketAddr) -> Option<&Arc<RwLock<Connection>>> {
        self.connections
            .iter()
            .find(|conn| read_or_die!(conn).remote_addr() == addr)
    }

    pub fn find_connections_by_ip(&self, ip: IpAddr) -> Vec<&Arc<RwLock<Connection>>> {
        self.connections
            .iter()
            .filter(|&conn| read_or_die!(conn).remote_peer().addr().ip() == ip)
            .collect::<Vec<_>>()
    }

    fn remove_connection(&mut self, to_remove: Token) {
        self.connections
            .retain(|conn| read_or_die!(conn).token() != to_remove);
    }

    pub fn add_connection(&mut self, conn: Connection) {
        self.connections.push(Arc::new(RwLock::new(conn)));
    }

    pub fn conn_event(&mut self, event: &Event) -> Fallible<ProcessResult> {
        let token = event.token();

        if let Some(rc_conn) = self.find_connection_by_token(token) {
            let mut conn = write_or_die!(rc_conn);
            conn.ready(event).map_err(|x| {
                let x: Vec<failure::Error> = x.into_iter().map(Result::unwrap_err).collect();
                failure::Error::from(concordium_common::fails::FunctorError::from(x))
            })
        } else {
            Err(Error::from(fails::PeerNotFoundError))
        }
    }

    pub fn cleanup_connections(
        &mut self,
        peer_type: PeerType,
        max_peers_number: u16,
        poll: &RwLock<Poll>,
    ) -> Fallible<()> {
        let curr_stamp = get_current_stamp();

        write_or_die!(self.to_disconnect).drain(..).for_each(|x| {
            if let Some(conn) = self.find_connection_by_id(x) {
                trace!(
                    "Disconnecting connection {} already marked as going down",
                    usize::from(read_or_die!(conn).token())
                );
                write_or_die!(conn).close();
            }
        });

        // Clean duplicates only if it's a regular node we're running
        if peer_type != PeerType::Bootstrapper {
            let mut connection_map: Vec<_> = self
                .connections
                .iter()
                .filter_map(|rc_conn| {
                    let rc_conn_borrowed = read_or_die!(rc_conn);
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
                let mut rc_conn_borrowed = write_or_die!(rc_conn);
                if rc_conn_borrowed.remote_id().is_some()
                    && !connection_map
                        .iter()
                        .any(|(_, token, _)| token == &rc_conn_borrowed.token())
                {
                    rc_conn_borrowed.close();
                }
            });
        }

        let wrap_connection_already_gone_as_non_fatal =
            |token, res: Fallible<()>| -> Fallible<Token> {
                use crate::connection::fails::PeerTerminatedConnection;
                match res {
                    Err(err) => {
                        if err.downcast_ref::<PeerTerminatedConnection>().is_some() {
                            Ok(token)
                        } else {
                            Err(err)
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
                || (!conn.is_post_handshake()
                    && conn.last_seen() + MAX_PREHANDSHAKE_KEEP_ALIVE < curr_stamp)
        };

        // Kill nodes which are no longer seen and also closing connections
        let (closing_conns, err_conns): (Vec<Fallible<Token>>, Vec<Fallible<Token>>) = self.connections
            .iter()
            // Get only connections that have been inactive for more time than allowed or closing connections
            .filter(|rc_conn| {
                let rc_conn_borrowed = read_or_die!(rc_conn);
                rc_conn_borrowed.is_closed() ||
                    filter_predicate_stable_conn_and_no_handshake(&rc_conn_borrowed) ||
                    filter_predicate_bootstrapper_no_activity_allowed_period(&rc_conn_borrowed) ||
                    filter_predicate_node_no_activity_allowed_period(&rc_conn_borrowed)
            })
            .map(|rc_conn| {
                // Deregister connection from the poll and shut down the socket
                let conn_token = read_or_die!(rc_conn).token();
                {
                    let mut conn = write_or_die!(rc_conn);
                    trace!("Kill connection {} {}:{}", usize::from(conn_token), conn.remote_addr().ip(), conn.remote_addr().port());
                    wrap_connection_already_gone_as_non_fatal(conn_token, conn.deregister(poll))?;
                    wrap_connection_already_gone_as_non_fatal(conn_token, conn.shutdown())?;
                }
                // Report number of peers to stats export engine
                if let Some(ref service) = &self.stats_export_service {
                    if read_or_die!(rc_conn).is_post_handshake() {
                        if let Ok(mut p) = safe_write!(service) {
                            p.peers_dec();
                        }
                    }
                }
                Ok(conn_token)
            }).partition(Result::is_ok);

        // Remove the connection from the list of connections
        for conn in closing_conns.into_iter().map(Result::unwrap) {
            // safe unwrapping since we are iterating over the list that only contains `Ok`s
            trace!("Remove connection {} from TLS Server", usize::from(conn));
            self.remove_connection(conn);
        }

        if peer_type != PeerType::Bootstrapper {
            self.unreachable_nodes
                .cleanup(curr_stamp - MAX_UNREACHABLE_MARK_TIME);
        }

        if !err_conns.is_empty() {
            bail!(format!(
                "Some connections couldn't be cleaned: {:?}",
                err_conns
            ));
        }

        // Toss out a random selection, for now, of connections that's post-handshake,
        // to lower the node count to an appropriate level.
        if peer_type == PeerType::Node {
            let peer_count = self.connections_posthandshake_count(Some(PeerType::Bootstrapper));
            if peer_count > max_peers_number {
                let mut rng = rand::thread_rng();
                self.connections
                    .iter()
                    .choose_multiple(&mut rng, (peer_count - max_peers_number) as usize)
                    .iter()
                    .for_each(|rc_conn| {
                        write_or_die!(rc_conn).close();
                    });
            }
        }

        Ok(())
    }

    pub fn liveness_check(&mut self) -> Fallible<()> {
        let curr_stamp = get_current_stamp();

        self.connections
            .iter()
            .filter(|ref rc_conn| {
                let conn = read_or_die!(rc_conn);
                (conn.last_seen() + 120_000 < curr_stamp
                    || conn.get_last_ping_sent() + 300_000 < curr_stamp)
                    && conn.is_post_handshake()
                    && !conn.is_closed()
            })
            .for_each(|ref rc_conn| {
                let request_ping = {
                    let conn = read_or_die!(rc_conn);
                    let local_peer = conn.local_peer();

                    NetworkMessage::NetworkRequest(
                        NetworkRequest::Ping(local_peer),
                        Some(get_current_stamp()),
                        None,
                    )
                };
                if let Ok(request_ping_data) = serialize_into_memory(&request_ping, 128) {
                    let mut conn = write_or_die!(rc_conn);
                    if let Err(e) = conn.async_send(
                        UCursor::from(request_ping_data),
                        MessageSendingPriority::High,
                    ) {
                        error!("{}", e);
                    }
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
    /// # Returns
    /// * amount of packets written to connections
    pub fn send_over_all_connections(
        &mut self,
        data: UCursor,
        filter_conn: &dyn Fn(&Connection) -> bool,
        send_status: &dyn Fn(&Connection, Fallible<()>),
    ) -> usize {
        self.connections
            .iter_mut()
            .filter(|rc_conn| {
                let rc_conn_borrowed = &read_or_die!(rc_conn);
                !rc_conn_borrowed.is_closed() && filter_conn(rc_conn_borrowed)
            })
            .map(|rc_conn| {
                let conn = read_or_die!(rc_conn);
                let status = conn.async_send(data.clone(), MessageSendingPriority::Normal);
                send_status(&conn, status)
            })
            .count()
    }

    pub fn dump_all_connections(&mut self, log_dumper: Option<SyncSender<DumpItem>>) {
        self.connections.iter_mut().for_each(|conn| {
            write_or_die!(conn).set_log_dumper(log_dumper.clone());
        });
    }

    pub fn add_notification(&mut self, func: UnitFunction<NetworkMessage>) {
        self.connections
            .iter_mut()
            .for_each(|conn| write_or_die!(conn).add_notification(func.clone()))
    }

    pub fn get_all_current_peers(&self, peer_type: Option<PeerType>) -> Box<[P2PNodeId]> {
        self.connections
        .iter()
        .filter(|rc_conn| {
            let rc_conn_borrowed = read_or_die!(rc_conn);
            rc_conn_borrowed.is_post_handshake() && (
                peer_type.is_none() || peer_type == Some(rc_conn_borrowed.remote_peer_type()) )
        })
        // we can safely unwrap here, because we've filetered away any
        // non-post-handshake peers already
        .map(|conn| read_or_die!(conn).remote_peer().peer().unwrap().id() )
        .collect::<Vec<_>>()
        .into_boxed_slice()
    }
}
