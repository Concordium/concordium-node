use std::{
    collections::HashSet,
    sync::{atomic::Ordering, RwLock},
};

use crate::{
    common::{
        counter::TOTAL_MESSAGES_SENT_COUNTER, get_current_stamp,
        serialization::serialize_into_memory, P2PNodeId, P2PPeer, PeerType,
    },
    connection::{connection_private::ConnectionPrivate, MessageSendingPriority, P2PEvent},
    network::{NetworkId, NetworkMessage, NetworkRequest, NetworkResponse},
};

use super::fails;
use concordium_common::{fails::FunctorError, functor::FuncResult};
use failure::Error;

fn make_fn_error_peer(e: &'static str) -> FunctorError {
    FunctorError::from(vec![Error::from(fails::PeerError { message: e })])
}

fn make_log_error(e: &'static str) -> FunctorError {
    FunctorError::from(vec![Error::from(fails::LogError { message: e })])
}

pub fn network_message_handle(
    priv_conn: &RwLock<ConnectionPrivate>,
    msg: &NetworkMessage,
) -> FuncResult<()> {
    match msg {
        NetworkMessage::NetworkRequest(NetworkRequest::Ping(..), ..) => {
            read_or_die!(priv_conn).update_last_seen();
            TOTAL_MESSAGES_SENT_COUNTER.fetch_add(1, Ordering::Relaxed);

            let pong_msg = {
                let priv_conn_reader = read_or_die!(priv_conn);
                // Make `Pong` response and send
                let remote_peer =
                    priv_conn_reader
                        .remote_peer()
                        .post_handshake_peer_or_else(|| {
                            make_fn_error_peer("Can't perform this action pre-handshake")
                        })?;

                if let Some(ref service) = priv_conn_reader.conn().handler().stats_export_service()
                {
                    service.pkt_sent_inc();
                }

                NetworkMessage::NetworkResponse(
                    NetworkResponse::Pong(remote_peer),
                    Some(get_current_stamp()),
                    None,
                )
            };

            // Ignore the return value because it is an asynchronous operation.
            write_or_die!(priv_conn)
                .async_send(
                    serialize_into_memory(&pong_msg, 64)?,
                    MessageSendingPriority::High,
                )
                .map(|_bytes| ())
        }
        NetworkMessage::NetworkResponse(NetworkResponse::Pong(..), ..) => {
            let ping: u64 = read_or_die!(priv_conn).sent_ping.load(Ordering::SeqCst);
            let curr: u64 = get_current_stamp();

            if curr >= ping {
                write_or_die!(priv_conn)
                    .last_latency_measured
                    .store(curr - ping, Ordering::SeqCst);
            }

            Ok(())
        }
        NetworkMessage::NetworkRequest(NetworkRequest::FindNode(..), ..) => {
            trace!("Got a FindNode request");
            let find_node_msg = {
                let priv_conn_reader = read_or_die!(priv_conn);
                priv_conn_reader.update_last_seen();

                let remote_peer =
                    priv_conn_reader
                        .remote_peer()
                        .post_handshake_peer_or_else(|| {
                            make_fn_error_peer("Can't perform this action pre-handshake")
                        })?;
                let nodes =
                    safe_read!(priv_conn_reader.conn().handler().connection_handler.buckets)?
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

            // Ignore returned because it is an asynchronous operation.
            write_or_die!(priv_conn)
                .async_send(
                    serialize_into_memory(&find_node_msg, 256)?,
                    MessageSendingPriority::Normal,
                )
                .map(|_bytes| ())
        }
        NetworkMessage::NetworkResponse(NetworkResponse::FindNode(_, ref peers), ..) => {
            trace!("Got a FindNode reponse");
            let priv_conn_reader = read_or_die!(priv_conn);
            // Process the received node list
            let mut ref_buckets =
                safe_write!(priv_conn_reader.conn().handler().connection_handler.buckets)?;
            for peer in peers.iter() {
                ref_buckets.insert_into_bucket(peer, HashSet::new());
            }

            Ok(())
        }
        NetworkMessage::NetworkRequest(NetworkRequest::GetPeers(ref sender, ref networks), ..) => {
            trace!("Got a GetPeers request");
            let peer_list_msg = {
                let priv_conn_reader = read_or_die!(priv_conn);
                priv_conn_reader.update_last_seen();

                let remote_peer =
                    priv_conn_reader
                        .remote_peer()
                        .post_handshake_peer_or_else(|| {
                            make_fn_error_peer("Can't perform this action pre-handshake")
                        })?;

                let nodes = if priv_conn_reader.conn().local_peer().peer_type()
                    == PeerType::Bootstrapper
                {
                    safe_read!(priv_conn_reader.conn().handler().connection_handler.buckets)?
                        .get_all_nodes(Some(&sender), networks)
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

                if let Some(ref service) = priv_conn_reader.conn().handler().stats_export_service()
                {
                    service.pkt_sent_inc();
                };

                NetworkMessage::NetworkResponse(
                    NetworkResponse::PeerList(remote_peer, nodes),
                    Some(get_current_stamp()),
                    None,
                )
            };

            TOTAL_MESSAGES_SENT_COUNTER.fetch_add(1, Ordering::Relaxed);

            // Ignore returned because it is an asynchronous operation.
            write_or_die!(priv_conn)
                .async_send(
                    serialize_into_memory(&peer_list_msg, 256)?,
                    MessageSendingPriority::Normal,
                )
                .map(|_bytes| ())
        }
        NetworkMessage::NetworkResponse(NetworkResponse::PeerList(sender, ref peers), ..) => {
            let priv_conn_reader = read_or_die!(priv_conn);
            let mut locked_buckets =
                safe_write!(priv_conn_reader.conn().handler().connection_handler.buckets)?;
            for peer in peers.iter() {
                // The block below is only used to inspect for leaking P2PNodeIds
                // of bootstrappers in debug builds, for the test-net.
                #[cfg(debug_assertions)]
                {
                    if peer.id().as_raw() >= 1_000_000 {
                        error!(
                            "I got a bootstrapper in a PeerList from the node {}",
                            sender
                        );
                    }
                }
                trace!("Received PeerList response from {}", sender);
                locked_buckets.insert_into_bucket(peer, HashSet::new());
            }
            Ok(())
        }
        NetworkMessage::NetworkRequest(NetworkRequest::JoinNetwork(_, network), ..) => {
            write_or_die!(priv_conn).add_remote_end_network(*network);

            let (remote_peer, event_log) = {
                let priv_conn_reader = read_or_die!(priv_conn);
                let remote_peer =
                    priv_conn_reader
                        .remote_peer()
                        .post_handshake_peer_or_else(|| {
                            make_fn_error_peer("Can't perform this action pre-handshake")
                        })?;

                safe_write!(priv_conn_reader.conn().handler().connection_handler.buckets)?
                    .update_network_ids(&remote_peer, priv_conn_reader.remote_end_networks.clone());

                (
                    remote_peer,
                    priv_conn_reader
                        .conn()
                        .handler()
                        .connection_handler
                        .event_log
                        .clone(),
                )
            };
            let networks: HashSet<NetworkId> = vec![*network].into_iter().collect();

            if let Some(ref log) = event_log {
                for net_id in networks.iter() {
                    log.send(P2PEvent::JoinedNetwork(remote_peer, *net_id))
                        .map_err(|_| make_log_error("Join Network Event cannot be sent to log"))?;
                }
            }

            Ok(())
        }
        NetworkMessage::NetworkRequest(NetworkRequest::LeaveNetwork(sender, network), ..) => {
            write_or_die!(priv_conn).remove_remote_end_network(*network);

            let event_log = {
                let priv_conn_reader = read_or_die!(priv_conn);
                let remote_peer =
                    priv_conn_reader
                        .remote_peer()
                        .post_handshake_peer_or_else(|| {
                            make_fn_error_peer("Can't perform this action pre-handshake")
                        })?;

                safe_write!(priv_conn_reader.conn().handler().connection_handler.buckets)?
                    .update_network_ids(&remote_peer, priv_conn_reader.remote_end_networks.clone());

                priv_conn_reader
                    .conn()
                    .handler()
                    .connection_handler
                    .event_log
                    .clone()
            };

            if let Some(ref log) = event_log {
                log.send(P2PEvent::LeftNetwork(*sender, *network))
                    .map_err(|_| make_log_error("Left Network Event cannot be sent to log"))?;
            };

            Ok(())
        }
        NetworkMessage::InvalidMessage => {
            debug!("Invalid message received!");

            {
                let mut priv_conn_mut = write_or_die!(priv_conn);

                priv_conn_mut.failed_pkts += 1;
                priv_conn_mut.update_last_seen();
            }

            if let Some(ref service) = read_or_die!(priv_conn)
                .conn()
                .handler()
                .stats_export_service()
            {
                service.invalid_pkts_received_inc();
            }

            Ok(())
        }
        _ => Ok(()),
    }
}
