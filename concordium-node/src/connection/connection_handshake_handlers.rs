use crate::{
    common::{
        counter::TOTAL_MESSAGES_SENT_COUNTER, get_current_stamp,
        serialization::serialize_into_memory, P2PPeer, PeerType,
    },
    connection::{connection_private::ConnectionPrivate, ConnectionStatus, MessageSendingPriority},
    network::{NetworkId, NetworkMessage, NetworkRequest, NetworkResponse},
};
use concordium_common::functor::FuncResult;
use std::{
    collections::HashSet,
    sync::{atomic::Ordering, RwLock},
};

const BOOTSTRAP_PEER_COUNT: usize = 100;

pub fn handshake_handle(
    priv_conn: &RwLock<ConnectionPrivate>,
    msg: &NetworkMessage,
) -> FuncResult<()> {
    match msg {
        NetworkMessage::NetworkResponse(
            NetworkResponse::Handshake(ref remote_peer, ref nets, _),
            ..
        ) => {
            debug!("Got a Handshake response");
            {
                let mut priv_conn_mut = write_or_die!(priv_conn);
                priv_conn_mut.add_remote_end_networks(nets);
                priv_conn_mut.promote_to_post_handshake(remote_peer.id(), remote_peer.addr)?;
            }
            {
                let priv_conn_ref = read_or_die!(priv_conn);
                priv_conn_ref
                    .sent_handshake
                    .store(get_current_stamp(), Ordering::SeqCst);

                let bucket_sender =
                    P2PPeer::from(remote_peer.peer_type(), remote_peer.id(), remote_peer.addr);
                if remote_peer.peer_type() != PeerType::Bootstrapper {
                    safe_write!(
                        priv_conn_ref
                            .conn()
                            .handler()
                            .connection_handler
                            .buckets
                    )?
                    .insert_into_bucket(&bucket_sender, nets.clone());
                }

                if let Some(ref service) = priv_conn_ref.conn().handler().stats_export_service() {
                    service.peers_inc();
                };
            }
        }
        NetworkMessage::NetworkRequest(NetworkRequest::Handshake(sender, nets, _), ..) => {
            debug!("Got a Handshake request");
            let mut priv_conn_mut = write_or_die!(priv_conn);

            priv_conn_mut.add_remote_end_networks(nets);
            priv_conn_mut.promote_to_post_handshake(sender.id(), sender.addr)?;

            send_handshake_and_ping(&mut priv_conn_mut)?;

            priv_conn_mut.update_last_seen();
            priv_conn_mut.set_measured_ping_sent();

            update_buckets(&priv_conn_mut, sender, nets.clone())?;

            if priv_conn_mut.conn().local_peer().peer_type() == PeerType::Bootstrapper {
                send_peer_list(&mut priv_conn_mut, sender, nets)?;
            }
        }
        _ => {
            safe_write!(priv_conn)?.status = ConnectionStatus::Closing;
            error!("Peer tried to send packets before handshake was completed!");
        }
    }

    Ok(())
}

fn send_handshake_and_ping(priv_conn: &mut ConnectionPrivate) -> FuncResult<()> {
    let (my_nets, local_peer) = {
        let remote_end_networks = priv_conn.remote_end_networks.clone();
        let local_peer = priv_conn.conn().local_peer();
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

    TOTAL_MESSAGES_SENT_COUNTER.fetch_add(2, Ordering::Relaxed);
    Ok(())
}

fn send_peer_list(
    priv_conn: &mut ConnectionPrivate,
    sender: &P2PPeer,
    nets: &HashSet<NetworkId>,
) -> FuncResult<()> {
    let random_nodes = safe_read!(priv_conn.conn().handler().connection_handler.buckets)?
        .get_random_nodes(&sender, BOOTSTRAP_PEER_COUNT, nets);
    if random_nodes.len()
        >= usize::from(
            priv_conn
                .conn()
                .handler()
                .config
                .bootstrapper_wait_minimum_peers,
        )
    {
        debug!(
            "Running in bootstrapper mode, so instantly sending {} random peers to a peer",
            BOOTSTRAP_PEER_COUNT
        );
        let peer_list_msg = {
            if let Some(ref service) = priv_conn.conn().handler().stats_export_service() {
                service.pkt_sent_inc();
            };
            NetworkMessage::NetworkResponse(
                NetworkResponse::PeerList(priv_conn.conn().local_peer(), random_nodes),
                Some(get_current_stamp()),
                None,
            )
        };
        // Ignore returned value because it is an asynchronous operation.
        let _ = priv_conn.async_send(
            serialize_into_memory(&peer_list_msg, 256)?,
            MessageSendingPriority::Normal,
        )?;
        TOTAL_MESSAGES_SENT_COUNTER.fetch_add(1, Ordering::Relaxed);
    }
    Ok(())
}

fn update_buckets(
    priv_conn: &ConnectionPrivate,
    sender: &P2PPeer,
    nets: HashSet<NetworkId>,
) -> FuncResult<()> {
    safe_write!(priv_conn.conn().handler().connection_handler.buckets)?
        .insert_into_bucket(sender, nets);

    if let Some(ref service) = priv_conn.conn().handler().stats_export_service() {
        service.peers_inc();
        service.pkt_sent_inc_by(2);
    };

    Ok(())
}
