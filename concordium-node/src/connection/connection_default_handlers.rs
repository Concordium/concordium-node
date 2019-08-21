use concordium_common::{functor::FuncResult, hybrid_buf::HybridBuf};

use std::{
    collections::HashSet,
    sync::{atomic::Ordering, RwLock},
};

use crate::{
    common::{
        counter::TOTAL_MESSAGES_SENT_COUNTER, get_current_stamp,
        serialization::serialize_into_memory,
    },
    connection::{connection_private::ConnectionPrivate, MessageSendingPriority},
    network::{NetworkId, NetworkMessage, NetworkRequest, NetworkResponse},
};

use super::{fails, handler_utils::*};
use failure::Error;

macro_rules! reject_handshake {
    ($direction:ident, $message:ident) => {{
        if let $direction::Handshake(..) = $message {
            Err(failure::Error::from(fails::UnwantedMessageError {
                message: "Unwanted handshake message".to_owned(),
            }))
        } else {
            Ok(())
        }
    }};
}

/// Default `NetworkRequest::Ping` handler.
/// It responds with a pong packet.
pub fn default_network_request_ping_handle(
    priv_conn: &RwLock<ConnectionPrivate>,
    _req: &NetworkRequest,
) -> FuncResult<()> {
    read_or_die!(priv_conn).update_last_seen();
    TOTAL_MESSAGES_SENT_COUNTER.fetch_add(1, Ordering::Relaxed);

    let pong_msg = {
        let priv_conn_reader = read_or_die!(priv_conn);
        // Make `Pong` response and send
        let remote_peer = priv_conn_reader
            .remote_peer()
            .post_handshake_peer_or_else(|| {
                make_fn_error_peer("Can't perform this action pre-handshake")
            })?;

        if let Some(ref service) = priv_conn_reader.stats_export_service {
            service.pkt_sent_inc();
        }

        NetworkMessage::NetworkResponse(
            NetworkResponse::Pong(remote_peer),
            Some(get_current_stamp()),
            None,
        )
    };
    let pong_data = serialize_into_memory(&pong_msg, 64)?;

    // Ignore the return value because it is an asynchronous operation.
    write_or_die!(priv_conn)
        .async_send(HybridBuf::from(pong_data), MessageSendingPriority::High)
        .map(|_bytes| ())
}

/// It sends the list of nodes.
pub fn default_network_request_find_node_handle(
    priv_conn: &RwLock<ConnectionPrivate>,
    req: &NetworkRequest,
) -> FuncResult<()> {
    if let NetworkRequest::FindNode(..) = req {
        // Return list of nodes
        let find_node_msg = {
            let priv_conn_reader = read_or_die!(priv_conn);
            priv_conn_reader.update_last_seen();

            let remote_peer = priv_conn_reader
                .remote_peer()
                .post_handshake_peer_or_else(|| {
                    make_fn_error_peer("Can't perform this action pre-handshake")
                })?;
            let nodes = safe_read!(priv_conn_reader.buckets)?.buckets[0] // The Buckets object is never empty
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
        let response_data = serialize_into_memory(&find_node_msg, 256)?;

        // Ignore returned because it is an asynchronous operation.
        write_or_die!(priv_conn)
            .async_send(
                HybridBuf::from(response_data),
                MessageSendingPriority::Normal,
            )
            .map(|_bytes| ())
    } else {
        Err(Error::from(make_msg_error(
            "Find node handler cannot handle this packet",
        )))
    }
}

pub fn default_network_request_get_peers(
    priv_conn: &RwLock<ConnectionPrivate>,
    req: &NetworkRequest,
) -> FuncResult<()> {
    if let NetworkRequest::GetPeers(ref sender, ref networks) = req {
        debug!("Got request for GetPeers");
        TOTAL_MESSAGES_SENT_COUNTER.fetch_add(1, Ordering::Relaxed);

        let peer_list_msg = {
            let priv_conn_reader = read_or_die!(priv_conn);
            priv_conn_reader.update_last_seen();
            let nodes =
                safe_read!(priv_conn_reader.buckets)?.get_all_nodes(Some(&sender), networks);

            let remote_peer = priv_conn_reader
                .remote_peer()
                .post_handshake_peer_or_else(|| {
                    make_fn_error_peer("Can't perform this action pre-handshake")
                })?;

            if let Some(ref service) = priv_conn_reader.stats_export_service {
                service.pkt_sent_inc();
            };

            NetworkMessage::NetworkResponse(
                NetworkResponse::PeerList(remote_peer, nodes),
                Some(get_current_stamp()),
                None,
            )
        };
        let peer_list_packet = serialize_into_memory(&peer_list_msg, 256)?;

        // Ignore returned because it is an asynchronous operation.
        write_or_die!(priv_conn)
            .async_send(
                HybridBuf::from(peer_list_packet),
                MessageSendingPriority::Normal,
            )
            .map(|_bytes| ())
    } else {
        Err(Error::from(make_msg_error(
            "Get peers handler cannot handler this packet",
        )))
    }
}

pub fn default_network_response_find_node(
    priv_conn: &RwLock<ConnectionPrivate>,
    res: &NetworkResponse,
) -> FuncResult<()> {
    if let NetworkResponse::FindNode(_, ref peers) = res {
        debug!("Got response to FindNode");

        let priv_conn_reader = read_or_die!(priv_conn);
        // Process the received node list
        let mut ref_buckets = safe_write!(priv_conn_reader.buckets)?;
        for peer in peers.iter() {
            ref_buckets.insert_into_bucket(peer, HashSet::new());
        }

        Ok(())
    } else {
        Err(Error::from(make_msg_error(
            "Response find node handler cannot handler this packet",
        )))
    }
}

/// It measures network latency.
pub fn default_network_response_pong(
    priv_conn: &RwLock<ConnectionPrivate>,
    _res: &NetworkResponse,
) -> FuncResult<()> {
    let ping: u64 = read_or_die!(priv_conn).sent_ping.load(Ordering::SeqCst);
    let curr: u64 = get_current_stamp();

    if curr >= ping {
        write_or_die!(priv_conn).last_latency_measured = curr - ping;
    }

    Ok(())
}

/// It inserts new peers into buckets.
pub fn default_network_response_peer_list(
    priv_conn: &RwLock<ConnectionPrivate>,
    res: &NetworkResponse,
) -> FuncResult<()> {
    if let NetworkResponse::PeerList(_, ref peers) = res {
        let priv_conn_reader = read_or_die!(priv_conn);
        let mut locked_buckets = safe_write!(priv_conn_reader.buckets)?;
        for peer in peers.iter() {
            locked_buckets.insert_into_bucket(peer, HashSet::new());
        }
    };
    Ok(())
}

/// In handshake:
///     - Add network
///     - Store target peer info and allocates buckets for this connection.
///     - Statistics: Export to Stats Exporter Service
///     - Log: Join to network
pub fn default_network_response_handshake(res: &NetworkResponse) -> FuncResult<()> {
    reject_handshake!(NetworkResponse, res)
}

/// It adds new network and update its buckets.
pub fn default_network_request_join_network(
    priv_conn: &RwLock<ConnectionPrivate>,
    res: &NetworkRequest,
) -> FuncResult<()> {
    if let NetworkRequest::JoinNetwork(_, network) = res {
        write_or_die!(priv_conn).add_remote_end_network(*network);

        let (remote_peer, event_log) = {
            let priv_conn_reader = read_or_die!(priv_conn);
            let remote_peer = priv_conn_reader
                .remote_peer()
                .post_handshake_peer_or_else(|| {
                    make_fn_error_peer("Can't perform this action pre-handshake")
                })?;

            safe_write!(priv_conn_reader.buckets)?
                .update_network_ids(&remote_peer, priv_conn_reader.remote_end_networks.clone());

            (remote_peer, priv_conn_reader.event_log.clone())
        };
        let networks: HashSet<NetworkId> = vec![*network].into_iter().collect();
        log_as_joined_network(&event_log, &remote_peer, &networks)?;
    }

    Ok(())
}

/// It removes that network from its owns and update buckets.
pub fn default_network_request_leave_network(
    priv_conn: &RwLock<ConnectionPrivate>,
    req: &NetworkRequest,
) -> FuncResult<()> {
    if let NetworkRequest::LeaveNetwork(sender, network) = req {
        write_or_die!(priv_conn).remove_remote_end_network(*network);

        let event_log = {
            let priv_conn_reader = read_or_die!(priv_conn);
            let remote_peer = priv_conn_reader
                .remote_peer()
                .post_handshake_peer_or_else(|| {
                    make_fn_error_peer("Can't perform this action pre-handshake")
                })?;

            safe_write!(priv_conn_reader.buckets)?
                .update_network_ids(&remote_peer, priv_conn_reader.remote_end_networks.clone());

            priv_conn_reader.event_log.clone()
        };

        log_as_leave_network(&event_log, &sender, *network)?;
    }

    Ok(())
}

/// On a handshake request:
///     - It replies with a handshake response and a ping.
///     - It adds the new network, and updates its buckets.
///     - Finally, it sends its peer list.
pub fn default_network_request_handshake(req: &NetworkRequest) -> FuncResult<()> {
    reject_handshake!(NetworkRequest, req)
}

/// Unknown messages only updates statistic information.
pub fn default_unknown_message(priv_conn: &RwLock<ConnectionPrivate>) -> FuncResult<()> {
    debug!("Unknown message received!");

    {
        let mut priv_conn_mut = write_or_die!(priv_conn);

        priv_conn_mut.failed_pkts += 1;
        priv_conn_mut.update_last_seen();
    }

    if let Some(ref service) = read_or_die!(priv_conn).stats_export_service {
        service.unknown_pkts_received_inc();
    }
    Ok(())
}

/// Invalid messages only updates statistic information.
pub fn default_invalid_message(priv_conn: &RwLock<ConnectionPrivate>) -> FuncResult<()> {
    debug!("Invalid message received!");

    {
        let mut priv_conn_mut = write_or_die!(priv_conn);

        priv_conn_mut.failed_pkts += 1;
        priv_conn_mut.update_last_seen();
    }

    if let Some(ref service) = read_or_die!(priv_conn).stats_export_service {
        service.invalid_pkts_received_inc();
    }

    Ok(())
}
