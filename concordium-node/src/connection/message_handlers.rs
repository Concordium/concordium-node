use crate::{
    client::plugins::consensus::*,
    common::{get_current_stamp, P2PNodeId, P2PPeer, PeerType},
    connection::{Connection, MessageSendingPriority, P2PEvent},
    network::{
        request::RequestedElementType, NetworkId, NetworkMessage, NetworkPacket, NetworkPacketType,
        NetworkRequest, NetworkResponse,
    },
    p2p::{banned_nodes::BannedNode, p2p_node::P2PNode},
    stats_engine::StatsEngine,
    utils::GlobalStateSenders,
};
use concordium_common::{read_or_die, serial::serialize_into_buffer, write_or_die, PacketType};

use failure::Fallible;

use std::{collections::HashSet, net::SocketAddr, sync::atomic::Ordering};

pub fn handle_incoming_message(conn: &Connection, full_msg: &NetworkMessage) {
    if let Err(e) = match full_msg {
        NetworkMessage::NetworkRequest(
            NetworkRequest::Handshake(remote_node_id, remote_port, ref networks, _),
            ..
        ) => handle_handshake_req(conn, *remote_node_id, *remote_port, networks),
        NetworkMessage::NetworkResponse(
            NetworkResponse::Handshake(remote_node_id, remote_port, ref nets, _),
            ..
        ) => handle_handshake_resp(conn, *remote_node_id, *remote_port, nets),
        NetworkMessage::NetworkRequest(NetworkRequest::Ping, ..) => handle_ping(conn),
        NetworkMessage::NetworkResponse(NetworkResponse::Pong, ..) => handle_pong(conn),
        NetworkMessage::NetworkRequest(NetworkRequest::FindNode(node), ..) => {
            handle_find_node_req(conn, *node)
        }
        NetworkMessage::NetworkResponse(NetworkResponse::FindNode(ref peers), ..) => {
            handle_find_node_resp(conn, peers)
        }
        NetworkMessage::NetworkRequest(NetworkRequest::GetPeers(ref networks), ..) => {
            handle_get_peers_req(conn, networks)
        }
        NetworkMessage::NetworkResponse(NetworkResponse::PeerList(ref peers), ..) => {
            handle_peer_list_resp(conn, peers)
        }
        NetworkMessage::NetworkRequest(NetworkRequest::JoinNetwork(network), ..) => {
            handle_join_network_req(conn, *network)
        }
        NetworkMessage::NetworkRequest(NetworkRequest::LeaveNetwork(network), ..) => {
            handle_leave_network_req(conn, *network)
        }
        NetworkMessage::NetworkRequest(NetworkRequest::BanNode(peer_to_ban), ..) => {
            handle_ban(conn, *peer_to_ban)
        }
        NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(peer_to_unban), ..) => {
            handle_unban(conn, *peer_to_unban)
        }
        NetworkMessage::NetworkPacket(..) => {
            // handled by handle_incoming_packet
            Ok(())
        }
        NetworkMessage::NetworkRequest(NetworkRequest::Retransmit(elem_type, since, nid), ..) => {
            handle_retransmit_req(conn, *elem_type, *since, *nid)
        }
        NetworkMessage::InvalidMessage => {
            handle_invalid_network_msg(conn);
            Ok(())
        }
    } {
        error!("Couldn't handle the network message {:?}: {}", full_msg, e);
    }
}

fn handle_handshake_req(
    conn: &Connection,
    remote_node_id: P2PNodeId,
    remote_port: u16,
    networks: &HashSet<NetworkId>,
) -> Fallible<()> {
    debug!("Got a Handshake request from peer {}", remote_node_id);

    if conn.handler().is_banned(BannedNode::ById(remote_node_id))? {
        conn.handler().remove_connection(conn.token);
        bail!("Rejected a handshake request from a banned node");
    }

    conn.promote_to_post_handshake(remote_node_id)?;
    conn.add_remote_end_networks(networks);

    let remote_peer = P2PPeer::from(
        conn.remote_peer.peer_type(),
        remote_node_id,
        SocketAddr::new(conn.remote_peer.addr().ip(), remote_port),
    );

    conn.send_handshake_response(remote_node_id)?;
    conn.send_ping()?;

    if remote_peer.peer_type() != PeerType::Bootstrapper {
        write_or_die!(conn.handler().connection_handler.buckets)
            .insert_into_bucket(&remote_peer, networks.clone());
    }

    if conn.handler().peer_type() == PeerType::Bootstrapper {
        debug!("Running in bootstrapper mode; attempting to send a PeerList upon handshake");
        conn.send_peer_list_resp(networks)?;
    }

    Ok(())
}

fn handle_handshake_resp(
    conn: &Connection,
    remote_node_id: P2PNodeId,
    remote_port: u16,
    networks: &HashSet<NetworkId>,
) -> Fallible<()> {
    debug!("Got a Handshake response from peer {}", remote_node_id);

    conn.promote_to_post_handshake(remote_node_id)?;
    conn.add_remote_end_networks(networks);

    conn.stats
        .sent_handshake
        .store(get_current_stamp(), Ordering::SeqCst);

    let remote_peer = P2PPeer::from(
        conn.remote_peer.peer_type(),
        remote_node_id,
        SocketAddr::new(conn.remote_peer.addr().ip(), remote_port),
    );

    if remote_peer.peer_type() != PeerType::Bootstrapper {
        write_or_die!(conn.handler().connection_handler.buckets)
            .insert_into_bucket(&remote_peer, networks.clone());
    }

    if let Some(ref service) = conn.handler().stats_export_service {
        service.peers_inc();
    };

    Ok(())
}

fn handle_ping(conn: &Connection) -> Fallible<()> {
    if !conn.is_post_handshake() {
        bail!("Can't reply to pings before the handshake is complete!")
    } else {
        conn.send_pong()
    }
}

fn handle_pong(conn: &Connection) -> Fallible<()> {
    let ping_time: u64 = conn.stats.last_ping_sent.load(Ordering::SeqCst);
    let curr_time: u64 = get_current_stamp();

    if curr_time >= ping_time {
        let new_latency = curr_time - ping_time;
        let old_latency = conn.get_last_latency();
        conn.set_last_latency((new_latency + old_latency) / 2);
    }

    Ok(())
}

fn handle_find_node_req(conn: &Connection, _target_node: P2PNodeId) -> Fallible<()> {
    trace!("Got a FindNode request");

    if !conn.is_post_handshake() {
        bail!("handle_find_node_req was called before the handshake!")
    }

    let find_node_msg = {
        let nodes = safe_read!(conn.handler().connection_handler.buckets)?.buckets[0] // The Buckets object is never empty
            .clone()
            .into_iter()
            .map(|node| node.peer)
            .collect::<Vec<_>>();

        NetworkMessage::NetworkResponse(
            NetworkResponse::FindNode(nodes),
            Some(get_current_stamp()),
            None,
        )
    };

    conn.async_send(
        serialize_into_buffer(&find_node_msg, 256)?,
        MessageSendingPriority::Normal,
    )
    .map(|_bytes| ())
}

fn handle_find_node_resp(conn: &Connection, nodes: &[P2PPeer]) -> Fallible<()> {
    trace!("Got a FindNode reponse");

    let mut ref_buckets = safe_write!(conn.handler().connection_handler.buckets)?;
    for peer in nodes.iter() {
        ref_buckets.insert_into_bucket(peer, HashSet::new());
    }

    Ok(())
}

fn handle_get_peers_req(conn: &Connection, networks: &HashSet<NetworkId>) -> Fallible<()> {
    if !conn.is_post_handshake() {
        bail!("handle_get_peers_req was called before the handshake!")
    }

    let remote_peer = P2PPeer::from(
        conn.remote_peer.peer_type(),
        conn.remote_id().unwrap(), // safe, post-handshake
        conn.remote_addr(),
    );

    debug!("Got a GetPeers request from peer {}", remote_peer.id);

    conn.send_peer_list_resp(networks)
}

fn handle_peer_list_resp(conn: &Connection, peers: &[P2PPeer]) -> Fallible<()> {
    if !conn.is_post_handshake() {
        bail!("handle_get_peers_req was called before the handshake!")
    }
    let peer_id = conn.remote_id().unwrap(); // safe, post-handshake

    debug!("Received a PeerList response from peer {}", peer_id);

    let mut new_peers = 0;
    let curr_peer_count = conn
        .handler()
        .get_peer_stats()
        .iter()
        .filter(|peer| peer.peer_type == PeerType::Node)
        .count();

    let current_peers = conn.handler().get_all_current_peers(Some(PeerType::Node));
    let applicable_candidates = peers
        .iter()
        .filter(|candidate| !current_peers.contains(&candidate.id));

    let mut locked_buckets = safe_write!(conn.handler().connection_handler.buckets)?;
    for peer in applicable_candidates {
        trace!(
            "Got info for peer {}/{}/{}",
            peer.id(),
            peer.ip(),
            peer.port()
        );
        if conn
            .handler()
            .connect(PeerType::Node, peer.addr, Some(peer.id()))
            .map_err(|e| trace!("{}", e))
            .is_ok()
        {
            new_peers += 1;
            locked_buckets.insert_into_bucket(peer, HashSet::new());
        }

        if new_peers + curr_peer_count >= conn.handler().config.desired_nodes_count as usize {
            break;
        }

        // The block below is only used to inspect for leaking P2PNodeIds
        // of bootstrappers in debug builds, for the test-net.
        #[cfg(debug_assertions)]
        {
            if peer.id().as_raw() >= 1_000_000 {
                error!("I got a bootstrapper in a PeerList from peer {}", peer_id);
            }
        }
    }

    Ok(())
}

fn handle_join_network_req(conn: &Connection, network: NetworkId) -> Fallible<()> {
    trace!("Received a JoinNetwork request");

    if !conn.is_post_handshake() {
        bail!("handle_join_network_req was called before the handshake!")
    }

    conn.add_remote_end_network(network);

    let (remote_peer, event_log) = {
        let remote_peer = conn.remote_peer().peer().unwrap();

        safe_write!(conn.handler().connection_handler.buckets)?.update_network_ids(
            &remote_peer,
            read_or_die!(conn.remote_end_networks).to_owned(),
        );

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

fn handle_leave_network_req(conn: &Connection, network: NetworkId) -> Fallible<()> {
    trace!("Received a LeaveNetwork request");

    if !conn.is_post_handshake() {
        bail!("handle_leave_network_req was called before the handshake!")
    }
    let remote_peer = conn.remote_peer().peer().unwrap();

    conn.remove_remote_end_network(network);

    safe_write!(conn.handler().connection_handler.buckets)?.update_network_ids(
        &remote_peer,
        read_or_die!(conn.remote_end_networks).to_owned(),
    );

    if let Some(ref log) = conn.handler().connection_handler.event_log {
        if log
            .send(P2PEvent::LeftNetwork(remote_peer, network))
            .is_err()
        {
            error!("Left Network Event cannot be sent to log");
        }
    };

    Ok(())
}

pub fn handle_retransmit_req(
    conn: &Connection,
    element_type: RequestedElementType,
    since: u64,
    nid: NetworkId,
) -> Fallible<()> {
    let remote_peer = if let Some(id) = conn.remote_peer().peer() {
        id
    } else {
        bail!("handle_retransmit_req was called before the handshake!");
    };

    debug!("Received a Retransmit request from peer {}", remote_peer);

    if let RequestedElementType::Transaction = element_type {
        read_or_die!(conn.handler().transactions_cache)
            .get_since(since)
            .iter()
            .for_each(|transaction| {
                if let Err(e) = send_consensus_msg_to_net(
                    conn.handler(),
                    vec![],
                    Some(remote_peer.id),
                    nid,
                    PacketType::Transaction,
                    Some(format!("{:?}", transaction)),
                    &transaction,
                ) {
                    error!("Couldn't retransmit a transaction! ({:?})", e);
                }
            })
    } else {
        bail!("Received request for unknown element type in a Retransmit request");
    }

    Ok(())
}

fn handle_ban(conn: &Connection, peer: BannedNode) -> Fallible<()> {
    conn.handler().ban_node(peer)?;

    Ok(())
}

fn handle_unban(conn: &Connection, peer: BannedNode) -> Fallible<()> {
    let is_self_unban = match peer {
        BannedNode::ById(id) => Some(id) == conn.remote_id(),
        BannedNode::ByAddr(addr) => addr == conn.remote_addr().ip(),
    };

    if is_self_unban {
        bail!("Rejecting a self-unban attempt");
    }

    conn.handler().unban_node(peer)?;

    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub fn handle_incoming_packet(
    node: &P2PNode,
    pac: &NetworkPacket,
    global_state_senders: &GlobalStateSenders,
    dedup_queues: &mut DeduplicationQueues,
    _stats_engine: &mut StatsEngine,
    _msg_count: &mut u64,
    _tps_test_enabled: bool,
    _tps_message_count: u64,
) {
    let is_broadcast = match pac.packet_type {
        NetworkPacketType::BroadcastedMessage(..) => true,
        _ => false,
    };

    if !is_broadcast && _tps_test_enabled {
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

    let dont_relay_to = if let NetworkPacketType::BroadcastedMessage(ref peers) = pac.packet_type {
        let mut list = peers.clone().to_owned();
        list.push(pac.peer.id());
        list
    } else {
        vec![]
    };

    if let Err(e) = handle_pkt_out(
        node,
        dont_relay_to,
        pac.peer.id(),
        pac.message.clone(),
        &global_state_senders,
        dedup_queues,
        is_broadcast,
    ) {
        error!(
            "Couldn't handle a NetworkPacket from peer {}: {}",
            pac.peer.id, e
        );
    }
}

fn handle_invalid_network_msg(conn: &Connection) {
    debug!("Received an invalid network message!");

    conn.stats.failed_pkts.fetch_add(1, Ordering::Relaxed);

    if let Some(ref service) = conn.handler().stats_export_service {
        service.invalid_pkts_received_inc();
    }
}
