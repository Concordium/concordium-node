use super::handler_utils::*;
use crate::{
    common::{get_current_stamp, P2PPeer, PeerType},
    connection::{connection_private::ConnectionPrivate, ConnectionStatus},
    network::{NetworkMessage, NetworkRequest, NetworkResponse},
};
use concordium_common::functor::FuncResult;
use std::sync::{atomic::Ordering, RwLock};

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
                        read_or_die!(priv_conn)
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
            {
                let mut priv_conn_mut = write_or_die!(priv_conn);
                priv_conn_mut.add_remote_end_networks(nets);
                priv_conn_mut.promote_to_post_handshake(sender.id(), sender.addr)?;
            }
            send_handshake_and_ping(priv_conn)?;
            {
                let priv_conn_ref = read_or_die!(priv_conn);
                priv_conn_ref.update_last_seen();
                priv_conn_ref.set_measured_ping_sent();
            }

            update_buckets(priv_conn, sender, nets.clone())?;

            if read_or_die!(priv_conn).conn().local_peer().peer_type() == PeerType::Bootstrapper {
                send_peer_list(priv_conn, sender, nets)?;
            }
        }
        _ => {
            safe_write!(priv_conn)?.status = ConnectionStatus::Closing;
            error!("Peer tried to send packets before handshake was completed!");
        }
    }

    Ok(())
}
