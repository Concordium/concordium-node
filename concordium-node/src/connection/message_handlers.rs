use crate::{
    client::plugins::consensus::*,
    common::{
        get_current_stamp, serialization::serialize_into_memory, P2PNodeId, P2PPeer, PeerType,
    },
    connection::{Connection, MessageSendingPriority, P2PEvent},
    network::{
        request::RequestedElementType, NetworkId, NetworkMessage, NetworkPacket, NetworkPacketType,
        NetworkRequest, NetworkResponse,
    },
    p2p::{banned_nodes::BannedNode, *},
    stats_engine::StatsEngine,
    utils::GlobalStateSenders,
};
use concordium_common::{cache::Cache, read_or_die, write_or_die, PacketType};

use circular_queue::CircularQueue;
use failure::Fallible;

use std::{
    collections::HashSet,
    sync::{atomic::Ordering, Arc},
};

const BOOTSTRAP_PEER_COUNT: usize = 100;

pub fn handle_incoming_message(node_ref: &P2PNode, conn: &Connection, full_msg: &NetworkMessage) {
    if let Err(e) = match full_msg {
        NetworkMessage::NetworkRequest(NetworkRequest::Handshake(source, ref networks, _), ..) => {
            handle_handshake_req(conn, *source, networks)
        }
        NetworkMessage::NetworkResponse(NetworkResponse::Handshake(source, ref nets, _), ..) => {
            handle_handshake_resp(conn, *source, nets)
        }
        NetworkMessage::NetworkRequest(NetworkRequest::Ping(_source), ..) => handle_ping(conn),
        NetworkMessage::NetworkResponse(NetworkResponse::Pong(_source), ..) => handle_pong(conn),
        NetworkMessage::NetworkRequest(NetworkRequest::FindNode(_source, node), ..) => {
            handle_find_node_req(conn, *node)
        }
        NetworkMessage::NetworkResponse(NetworkResponse::FindNode(_source, ref peers), ..) => {
            handle_find_node_resp(conn, peers)
        }
        NetworkMessage::NetworkRequest(NetworkRequest::GetPeers(source, ref networks), ..) => {
            handle_get_peers_req(conn, *source, networks)
        }
        NetworkMessage::NetworkResponse(NetworkResponse::PeerList(source, ref peers), ..) => {
            handle_peer_list_resp(node_ref, conn, *source, peers)
        }
        NetworkMessage::NetworkRequest(NetworkRequest::JoinNetwork(_source, network), ..) => {
            handle_join_network_req(conn, *network)
        }
        NetworkMessage::NetworkRequest(NetworkRequest::LeaveNetwork(source, network), ..) => {
            handle_leave_network_req(conn, *source, *network)
        }
        NetworkMessage::NetworkRequest(NetworkRequest::BanNode(_source, peer_to_ban), ..) => {
            node_ref.ban_node(*peer_to_ban)
        }
        NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(_source, peer_to_unban), ..) => {
            node_ref.unban_node(*peer_to_unban)
        }
        NetworkMessage::NetworkPacket(..) => {
            // handled by handle_incoming_network_packet
            Ok(())
        }
        NetworkMessage::NetworkRequest(NetworkRequest::Retransmit(..), ..) => {
            // handled by handle_retransmit_req
            Ok(())
        }
        NetworkMessage::InvalidMessage => {
            handle_invalid_network_msg(conn);
            Ok(())
        }
    } {
        error!("Couldn't handle the network message {:?}: {}", full_msg, e);
    }
}

fn send_handshake_and_ping(conn: &Connection) -> Fallible<()> {
    let local_peer = conn.handler().self_peer;

    let handshake_msg = NetworkMessage::NetworkResponse(
        NetworkResponse::Handshake(
            local_peer,
            read_or_die!(conn.remote_end_networks).to_owned(),
            vec![],
        ),
        Some(get_current_stamp()),
        None,
    );

    // Ignore returned value because it is an asynchronous operation.
    conn.async_send(
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
    conn.async_send(
        serialize_into_memory(&ping_msg, 64)?,
        MessageSendingPriority::Normal,
    )?;

    conn.set_last_ping_sent();

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

        conn.async_send(
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
    debug!("Got a Handshake request from peer {}", source.id());

    if conn.handler().is_banned(BannedNode::ById(source.id()))? {
        conn.handler().remove_connection(conn.token);
        bail!("Rejected a handshake request from a banned node");
    }

    conn.promote_to_post_handshake(source.id())?;
    conn.add_remote_end_networks(networks);
    send_handshake_and_ping(&conn)?;

    write_or_die!(conn.handler().connection_handler.buckets)
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
    debug!("Got a Handshake response from peer {}", source.id());

    conn.promote_to_post_handshake(source.id())?;
    conn.add_remote_end_networks(networks);

    conn.sent_handshake
        .store(get_current_stamp(), Ordering::SeqCst);

    let bucket_sender = P2PPeer::from(source.peer_type(), source.id(), source.addr);
    if source.peer_type() != PeerType::Bootstrapper {
        write_or_die!(conn.handler().connection_handler.buckets)
            .insert_into_bucket(&bucket_sender, networks.clone());
    }

    if let Some(ref service) = conn.handler().stats_export_service {
        service.peers_inc();
    };

    Ok(())
}

fn handle_ping(conn: &Connection) -> Fallible<()> {
    if !conn.is_post_handshake() {
        bail!("handle_ping was called before the handshake!")
    }

    let pong_msg = {
        let remote_peer = conn.remote_peer().peer().unwrap();

        NetworkMessage::NetworkResponse(
            NetworkResponse::Pong(remote_peer),
            Some(get_current_stamp()),
            None,
        )
    };

    conn.async_send(
        serialize_into_memory(&pong_msg, 64)?,
        MessageSendingPriority::High,
    )
    .map(|_bytes| ())
}

fn handle_pong(conn: &Connection) -> Fallible<()> {
    let ping_time: u64 = conn.last_ping_sent.load(Ordering::SeqCst);
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
        let remote_peer = conn.remote_peer().peer().unwrap();
        let nodes = safe_read!(conn.handler().connection_handler.buckets)?.buckets[0] // The Buckets object is never empty
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

    conn.async_send(
        serialize_into_memory(&find_node_msg, 256)?,
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

fn handle_get_peers_req(
    conn: &Connection,
    source: P2PPeer,
    networks: &HashSet<NetworkId>,
) -> Fallible<()> {
    debug!("Got a GetPeers request from peer {}", source.id());

    if !conn.is_post_handshake() {
        bail!("handle_get_peers_req was called before the handshake!")
    }

    let peer_list_msg = {
        let nodes = if conn.handler().peer_type() == PeerType::Bootstrapper {
            safe_read!(conn.handler().connection_handler.buckets)?
                .get_all_nodes(Some(&source), networks)
        } else {
            conn.handler()
                .get_peer_stats()
                .iter()
                .filter(|stat| stat.peer_type == PeerType::Node)
                .filter(|stat| P2PNodeId(stat.id) != source.id)
                .map(|stat| P2PPeer::from(stat.peer_type, P2PNodeId(stat.id), stat.addr))
                .collect()
        };

        NetworkMessage::NetworkResponse(
            NetworkResponse::PeerList(conn.handler().self_peer, nodes),
            Some(get_current_stamp()),
            None,
        )
    };

    conn.async_send(
        serialize_into_memory(&peer_list_msg, 256)?,
        MessageSendingPriority::Normal,
    )
    .map(|_bytes| ())
}

fn handle_peer_list_resp(
    node: &P2PNode,
    conn: &Connection,
    source: P2PPeer,
    peers: &[P2PPeer],
) -> Fallible<()> {
    debug!("Received a PeerList response from peer {}", source.id());

    let mut new_peers = 0;
    let curr_peer_count = node
        .get_peer_stats()
        .iter()
        .filter(|peer| peer.peer_type == PeerType::Node)
        .count();

    let mut locked_buckets = safe_write!(conn.handler().connection_handler.buckets)?;
    for peer in peers.iter() {
        trace!(
            "Got info for peer {}/{}/{}",
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
            locked_buckets.insert_into_bucket(peer, HashSet::new());
        }

        if new_peers + curr_peer_count >= node.config.desired_nodes_count as usize {
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

fn handle_leave_network_req(
    conn: &Connection,
    source: P2PPeer,
    network: NetworkId,
) -> Fallible<()> {
    trace!("Received a LeaveNetwork request");

    if !conn.is_post_handshake() {
        bail!("handle_leave_network_req was called before the handshake!")
    }

    conn.remove_remote_end_network(network);

    let event_log = {
        let remote_peer = conn.remote_peer().peer().unwrap();

        safe_write!(conn.handler().connection_handler.buckets)?.update_network_ids(
            &remote_peer,
            read_or_die!(conn.remote_end_networks).to_owned(),
        );

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
    debug!("Received a Retransmit request from peer {}", requester.id());

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
                error!("Couldn't retransmit a transaction! ({:?})", e);
            }
        })
    } else {
        error!("Received request for unknown element type in a Retransmit request")
    }
}

#[allow(clippy::too_many_arguments)]
pub fn handle_incoming_packet(
    pac: &NetworkPacket,
    global_state_senders: &GlobalStateSenders,
    transactions_cache: &mut Cache<Arc<[u8]>>,
    dedup_queue: &mut CircularQueue<[u8; 8]>,
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
        dedup_queue,
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

    conn.failed_pkts.fetch_add(1, Ordering::Relaxed);

    if let Some(ref service) = conn.handler().stats_export_service {
        service.invalid_pkts_received_inc();
    }
}
