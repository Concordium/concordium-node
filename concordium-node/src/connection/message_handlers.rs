use crate::{
    client::plugins::consensus::*,
    common::{
        get_current_stamp, serialization::serialize_into_memory, P2PNodeId, P2PPeer, PeerType,
    },
    connection::{Connection, ConnectionPrivate, MessageSendingPriority, P2PEvent},
    network::{
        request::RequestedElementType, NetworkId, NetworkMessage, NetworkPacket, NetworkPacketType,
        NetworkRequest, NetworkResponse,
    },
    p2p::*,
    stats_engine::StatsEngine,
    utils::{self, GlobalStateSenders},
};
use concordium_common::{cache::Cache, read_or_die, write_or_die, PacketType};
use failure::Fallible;

use std::{
    collections::HashSet,
    sync::{atomic::Ordering, Arc},
};

const BOOTSTRAP_PEER_COUNT: usize = 100;

pub fn handle_incoming_message(node_ref: &P2PNode, conn: &Connection, full_msg: &NetworkMessage) {
    match full_msg {
        NetworkMessage::NetworkRequest(NetworkRequest::Handshake(source, ref networks, _), ..) => {
            if let Err(e) = handle_handshake_req(conn, *source, networks) {
                error!(
                    "Couldn't handle a Handshake request from peer {}: {}",
                    source.id, e
                );
            }
        }
        NetworkMessage::NetworkResponse(NetworkResponse::Handshake(source, ref nets, _), ..) => {
            if let Err(e) = handle_handshake_resp(conn, *source, nets) {
                error!(
                    "Couldn't handle a Handshake response from peer {}: {}",
                    source.id, e
                );
            }
        }
        NetworkMessage::NetworkRequest(NetworkRequest::Ping(source), ..) => {
            if let Err(e) = handle_ping(conn) {
                error!("Couldn't handle a Ping from peer {}: {}", source.id, e);
            }
        }
        NetworkMessage::NetworkResponse(NetworkResponse::Pong(source), ..) => {
            if let Err(e) = handle_pong(conn) {
                error!("Couldn't handle a Pong from peer {}: {}", source.id, e);
            }
        }
        NetworkMessage::NetworkRequest(NetworkRequest::FindNode(source, node), ..) => {
            if let Err(e) = handle_find_node_req(node_ref, *source, *node) {
                error!(
                    "Couldn't handle a FindNode request from peer {}: {}",
                    source.id, e
                );
            }
        }
        NetworkMessage::NetworkResponse(NetworkResponse::FindNode(source, ref peers), ..) => {
            if let Err(e) = handle_find_node_resp(node_ref, *source, peers) {
                error!(
                    "Couldn't handle a FindNode response from peer {}: {}",
                    source.id, e
                );
            }
        }
        NetworkMessage::NetworkRequest(NetworkRequest::GetPeers(source, ref networks), ..) => {
            if let Err(e) = handle_get_peers_req(node_ref, *source, networks) {
                error!(
                    "Couldn't handle a GetPeers request from peer {}: {}",
                    source.id, e
                );
            }
        }
        NetworkMessage::NetworkResponse(NetworkResponse::PeerList(source, ref peers), ..) => {
            if let Err(e) = handle_peer_list_resp(node_ref, *source, peers) {
                error!(
                    "Couldn't handle a PeerList response from peer {}: {}",
                    source.id, e
                );
            }
        }
        NetworkMessage::NetworkRequest(NetworkRequest::JoinNetwork(source, network), ..) => {
            if let Err(e) = handle_join_network_req(node_ref, *source, *network) {
                error!(
                    "Couldn't handle a JoinNetwork request from peer {}: {}",
                    source.id, e
                );
            }
        }
        NetworkMessage::NetworkRequest(NetworkRequest::LeaveNetwork(source, network), ..) => {
            if let Err(e) = handle_leave_network_req(node_ref, *source, *network) {
                error!(
                    "Couldn't handle a LeaveNetwork request from peer {}: {}",
                    source.id, e
                );
            }
        }
        NetworkMessage::NetworkRequest(NetworkRequest::BanNode(ref peer, peer_to_ban), ..) => {
            utils::ban_node(node_ref, peer, *peer_to_ban);
        }
        NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(ref peer, peer_to_ban), ..) => {
            utils::unban_node(node_ref, peer, *peer_to_ban);
        }
        NetworkMessage::NetworkPacket(..) => {
            // handled by handle_incoming_network_packet
        }
        NetworkMessage::NetworkRequest(NetworkRequest::Retransmit(..), ..) => {
            // handled by handle_retransmit_req
        }
        NetworkMessage::InvalidMessage => {
            // handle_invalid_network_msg()
        }
    }
}

fn get_peers_connection(node: &P2PNode, peer_id: P2PNodeId) -> Fallible<Connection> {
    if let Some(conn) = node.find_connection_by_id(peer_id) {
        Ok(conn)
    } else {
        Err(format_err!(
            "Can't find the connection for peer id {}!",
            peer_id
        ))
    }
}

fn send_handshake_and_ping(conn: &Connection, priv_conn: &mut ConnectionPrivate) -> Fallible<()> {
    let (my_nets, local_peer) = {
        let remote_end_networks = priv_conn.remote_end_networks.clone();
        let local_peer = conn.handler().self_peer;
        (remote_end_networks, local_peer)
    };

    let handshake_msg = NetworkMessage::NetworkResponse(
        NetworkResponse::Handshake(local_peer, my_nets, vec![]),
        Some(get_current_stamp()),
        None,
    );

    // Ignore returned value because it is an asynchronous operation.
    let _ = priv_conn.async_send(
        serialize_into_memory(&handshake_msg, 128)?,
        MessageSendingPriority::High,
    )?;

    let ping_msg = NetworkMessage::NetworkRequest(
        NetworkRequest::Ping(local_peer),
        Some(get_current_stamp()),
        None,
    );

    // Ignore returned value because it is an asynchronous operation, and ship out
    // as normal priority to ensure proper queueing here.
    let _ = priv_conn.async_send(
        serialize_into_memory(&ping_msg, 64)?,
        MessageSendingPriority::Normal,
    )?;

    Ok(())
}

fn send_peer_list(conn: &Connection, sender: P2PPeer, nets: &HashSet<NetworkId>) -> Fallible<()> {
    let random_nodes = safe_read!(conn.handler().connection_handler.buckets)?.get_random_nodes(
        &sender,
        BOOTSTRAP_PEER_COUNT,
        nets,
    );
    if random_nodes.len() >= usize::from(conn.handler().config.bootstrapper_wait_minimum_peers) {
        debug!(
            "Running in bootstrapper mode, so instantly sending {} random peers to a peer",
            BOOTSTRAP_PEER_COUNT
        );
        let peer_list_msg = NetworkMessage::NetworkResponse(
            NetworkResponse::PeerList(conn.handler().self_peer, random_nodes),
            Some(get_current_stamp()),
            None,
        );

        let mut conn_writer = write_or_die!(conn.dptr);
        let _ = conn_writer.async_send(
            serialize_into_memory(&peer_list_msg, 256)?,
            MessageSendingPriority::Normal,
        )?;
    }
    Ok(())
}

fn handle_handshake_req(
    conn: &Connection,
    source: P2PPeer,
    networks: &HashSet<NetworkId>,
) -> Fallible<()> {
    debug!("Got a Handshake request");

    if read_or_die!(conn.handler().connection_handler.banned_peers).is_id_banned(source.id()) {
        write_or_die!(conn.handler().connection_handler.to_disconnect).push_back(source.id());
    }

    {
        let mut priv_conn_mut = write_or_die!(conn.dptr);

        priv_conn_mut.add_remote_end_networks(networks);
        priv_conn_mut.promote_to_post_handshake(source.id(), source.addr)?;

        send_handshake_and_ping(&conn, &mut priv_conn_mut)?;

        priv_conn_mut.update_last_seen();
        priv_conn_mut.set_measured_ping_sent();
    }

    safe_write!(conn.handler().connection_handler.buckets)?
        .insert_into_bucket(&source, networks.to_owned());

    if conn.handler().peer_type() == PeerType::Bootstrapper {
        send_peer_list(&conn, source, networks)?;
    }

    Ok(())
}

fn handle_handshake_resp(
    conn: &Connection,
    source: P2PPeer,
    networks: &HashSet<NetworkId>,
) -> Fallible<()> {
    debug!("Got a Handshake response");

    {
        let mut priv_conn_mut = write_or_die!(conn.dptr);
        priv_conn_mut.add_remote_end_networks(networks);
        priv_conn_mut.promote_to_post_handshake(source.id(), source.addr)?;
    }
    {
        let priv_conn_ref = read_or_die!(conn.dptr);
        priv_conn_ref
            .sent_handshake
            .store(get_current_stamp(), Ordering::SeqCst);

        let bucket_sender = P2PPeer::from(source.peer_type(), source.id(), source.addr);
        if source.peer_type() != PeerType::Bootstrapper {
            safe_write!(conn.handler().connection_handler.buckets)?
                .insert_into_bucket(&bucket_sender, networks.clone());
        }

        if let Some(ref service) = conn.handler().stats_export_service {
            service.peers_inc();
        };
    }

    Ok(())
}

fn handle_ping(conn: &Connection) -> Fallible<()> {
    let pong_msg = {
        let priv_conn_reader = read_or_die!(conn.dptr);
        priv_conn_reader.update_last_seen();
        // Make `Pong` response and send it
        let remote_peer = priv_conn_reader
            .remote_peer()
            .post_handshake_peer_or_else(|| {
                format_err!("Can't send a pong before the handshake")
            })?;

        NetworkMessage::NetworkResponse(
            NetworkResponse::Pong(remote_peer),
            Some(get_current_stamp()),
            None,
        )
    };

    let mut conn_writer = write_or_die!(conn.dptr);
    conn_writer
        .async_send(
            serialize_into_memory(&pong_msg, 64)?,
            MessageSendingPriority::High,
        )
        .map(|_bytes| ())
}

fn handle_pong(conn: &Connection) -> Fallible<()> {
    let ping: u64 = read_or_die!(conn.dptr).sent_ping.load(Ordering::SeqCst);
    let curr: u64 = get_current_stamp();

    if curr >= ping {
        write_or_die!(conn.dptr)
            .last_latency_measured
            .store(curr - ping, Ordering::SeqCst);
    }

    Ok(())
}

fn handle_find_node_req(node: &P2PNode, source: P2PPeer, _target_node: P2PNodeId) -> Fallible<()> {
    trace!("Got a FindNode request");

    let conn = get_peers_connection(node, source.id)?;
    let find_node_msg = {
        let priv_conn_reader = read_or_die!(conn.dptr);
        priv_conn_reader.update_last_seen();

        let remote_peer = priv_conn_reader
            .remote_peer()
            .post_handshake_peer_or_else(|| {
                format_err!("Can't handle FindNode requests before the handshake")
            })?;
        let nodes = safe_read!(priv_conn_reader.conn().handler().connection_handler.buckets)?
            .buckets[0] // The Buckets object is never empty
            .clone()
            .into_iter()
            .map(|node| node.peer)
            .collect::<Vec<_>>();

        NetworkMessage::NetworkResponse(
            NetworkResponse::FindNode(remote_peer, nodes),
            Some(get_current_stamp()),
            None,
        )
    };

    let mut conn_writer = write_or_die!(conn.dptr);
    conn_writer
        .async_send(
            serialize_into_memory(&find_node_msg, 256)?,
            MessageSendingPriority::Normal,
        )
        .map(|_bytes| ())
}

fn handle_find_node_resp(node: &P2PNode, source: P2PPeer, nodes: &[P2PPeer]) -> Fallible<()> {
    trace!("Got a FindNode reponse");

    let conn = get_peers_connection(node, source.id)?;

    let mut ref_buckets = safe_write!(conn.handler().connection_handler.buckets)?;
    for peer in nodes.iter() {
        ref_buckets.insert_into_bucket(peer, HashSet::new());
    }

    Ok(())
}

fn handle_get_peers_req(
    node: &P2PNode,
    source: P2PPeer,
    networks: &HashSet<NetworkId>,
) -> Fallible<()> {
    trace!("Got a GetPeers request");

    let conn = get_peers_connection(node, source.id)?;
    let peer_list_msg = {
        let priv_conn_reader = read_or_die!(conn.dptr);
        priv_conn_reader.update_last_seen();

        let remote_peer = priv_conn_reader
            .remote_peer()
            .post_handshake_peer_or_else(|| {
                format_err!("Can't handle GetPeers requests before the handshake")
            })?;

        let nodes = if priv_conn_reader.conn().handler().peer_type() == PeerType::Bootstrapper {
            safe_read!(priv_conn_reader.conn().handler().connection_handler.buckets)?
                .get_all_nodes(Some(&source), networks)
        } else {
            priv_conn_reader
                .conn()
                .handler()
                .get_peer_stats()
                .iter()
                .filter(|element| element.peer_type == PeerType::Node)
                .map(|element| {
                    P2PPeer::from(element.peer_type, P2PNodeId(element.id), element.addr)
                })
                .collect()
        };

        NetworkMessage::NetworkResponse(
            NetworkResponse::PeerList(remote_peer, nodes),
            Some(get_current_stamp()),
            None,
        )
    };

    let mut conn_writer = write_or_die!(conn.dptr);
    conn_writer
        .async_send(
            serialize_into_memory(&peer_list_msg, 256)?,
            MessageSendingPriority::Normal,
        )
        .map(|_bytes| ())
}

fn handle_peer_list_resp(node: &P2PNode, source: P2PPeer, peers: &[P2PPeer]) -> Fallible<()> {
    trace!("Received a PeerList response");

    let conn = get_peers_connection(node, source.id)?;

    let mut locked_buckets = safe_write!(conn.handler().connection_handler.buckets)?;

    let mut new_peers = 0;
    let curr_peer_count = node
        .get_peer_stats()
        .iter()
        .filter(|x| x.peer_type == PeerType::Node)
        .count();

    for peer in peers.iter() {
        locked_buckets.insert_into_bucket(peer, HashSet::new());

        trace!(
            "Peer {}/{}/{} sent us peer info for {}/{}/{}",
            source.id(),
            source.ip(),
            source.port(),
            peer.id(),
            peer.ip(),
            peer.port()
        );
        if node
            .connect(PeerType::Node, peer.addr, Some(peer.id()))
            .map_err(|e| trace!("{}", e))
            .is_ok()
        {
            new_peers += 1;
        }

        if new_peers + curr_peer_count as u8 >= node.config.desired_nodes_count {
            break;
        }

        // The block below is only used to inspect for leaking P2PNodeIds
        // of bootstrappers in debug builds, for the test-net.
        #[cfg(debug_assertions)]
        {
            if peer.id().as_raw() >= 1_000_000 {
                error!(
                    "I got a bootstrapper in a PeerList from the node {}",
                    source
                );
            }
        }
    }

    Ok(())
}

fn handle_join_network_req(node: &P2PNode, source: P2PPeer, network: NetworkId) -> Fallible<()> {
    trace!("Received a JoinNetwork request");

    let conn = get_peers_connection(node, source.id)?;
    write_or_die!(conn.dptr).add_remote_end_network(network);

    let (remote_peer, event_log) = {
        let priv_conn_reader = read_or_die!(conn.dptr);
        let remote_peer = priv_conn_reader
            .remote_peer()
            .post_handshake_peer_or_else(|| {
                format_err!("Can't handle JoinNetwork requests before the handshake")
            })?;

        safe_write!(conn.handler().connection_handler.buckets)?
            .update_network_ids(&remote_peer, priv_conn_reader.remote_end_networks.clone());

        (
            remote_peer,
            conn.handler().connection_handler.event_log.clone(),
        )
    };
    let networks: HashSet<NetworkId> = vec![network].into_iter().collect();

    if let Some(ref log) = event_log {
        for net_id in networks.iter() {
            if log
                .send(P2PEvent::JoinedNetwork(remote_peer, *net_id))
                .is_err()
            {
                error!("A JoinNetwork Event cannot be sent to the P2PEvent log");
            }
        }
    }

    Ok(())
}

fn handle_leave_network_req(node: &P2PNode, source: P2PPeer, network: NetworkId) -> Fallible<()> {
    trace!("Received a LeaveNetwork request");

    let conn = get_peers_connection(node, source.id)?;
    write_or_die!(conn.dptr).remove_remote_end_network(network);

    let event_log = {
        let priv_conn_reader = read_or_die!(conn.dptr);
        let remote_peer = priv_conn_reader
            .remote_peer()
            .post_handshake_peer_or_else(|| {
                format_err!("Can't handle LeaveNetwork requests before the handshake")
            })?;

        safe_write!(conn.handler().connection_handler.buckets)?
            .update_network_ids(&remote_peer, priv_conn_reader.remote_end_networks.clone());

        conn.handler().connection_handler.event_log.clone()
    };

    if let Some(ref log) = event_log {
        if log.send(P2PEvent::LeftNetwork(source, network)).is_err() {
            error!("Left Network Event cannot be sent to log");
        }
    };

    Ok(())
}

pub fn handle_retransmit_req(
    node: &P2PNode,
    requester: P2PPeer,
    element_type: RequestedElementType,
    since: u64,
    nid: NetworkId,
    transactions_cache: &mut Cache<Arc<[u8]>>,
) {
    if let RequestedElementType::Transaction = element_type {
        let transactions = transactions_cache.get_since(since);
        transactions.iter().for_each(|transaction| {
            if let Err(e) = send_consensus_msg_to_net(
                node,
                vec![],
                Some(requester.id()),
                nid,
                PacketType::Transaction,
                Some(format!("{:?}", transaction)),
                &transaction,
            ) {
                error!("Couldn't retransmit a trancation! ({:?})", e);
            }
        })
    } else {
        error!("Received request for unknown element type in a Retransmit request")
    }
}

pub fn handle_incoming_packet(
    pac: &NetworkPacket,
    global_state_senders: &GlobalStateSenders,
    transactions_cache: &mut Cache<Arc<[u8]>>,
    _stats_engine: &mut StatsEngine,
    _msg_count: &mut u64,
    _tps_test_enabled: bool,
    _tps_message_count: u64,
) {
    if let NetworkPacketType::DirectMessage(..) = pac.packet_type {
        if _tps_test_enabled {
            if let Ok(len) = pac.message.len() {
                _stats_engine.add_stat(len);
                *_msg_count += 1;

                if *_msg_count == _tps_message_count {
                    info!(
                        "TPS over {} messages is {}",
                        _tps_message_count,
                        _stats_engine.calculate_total_tps_average()
                    );
                    *_msg_count = 0;
                    _stats_engine.clear();
                }
            }
        }
    }

    let is_broadcast = match pac.packet_type {
        NetworkPacketType::BroadcastedMessage(..) => true,
        _ => false,
    };

    let dont_relay_to = if let NetworkPacketType::BroadcastedMessage(ref peers) = pac.packet_type {
        let mut list = peers.clone().to_owned();
        list.push(pac.peer.id());
        list
    } else {
        vec![]
    };

    if let Err(e) = handle_pkt_out(
        dont_relay_to,
        pac.peer.id(),
        pac.message.clone(),
        &global_state_senders,
        transactions_cache,
        is_broadcast,
    ) {
        error!(
            "Couldn't handle a NetworkPacket from peer {}: {}",
            pac.peer.id, e
        );
    }
}

// TODO: add source peer to the InvalidMessage
// fn handle_invalid_network_msg(node: &P2PNode, source: P2PPeer) ->
// Fallible<()> { debug!("Received an invalid network message!");
//
// {
// let mut priv_conn_mut = write_or_die!(priv_conn);
//
// priv_conn_mut.failed_pkts += 1;
// priv_conn_mut.update_last_seen();
// }
//
// if let Some(ref service) = read_or_die!(priv_conn)
// .conn()
// .handler()
// .stats_export_service
// {
// service.invalid_pkts_received_inc();
// }
//
// Ok(())
// }
