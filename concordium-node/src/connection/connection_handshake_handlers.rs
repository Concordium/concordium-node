use super::handler_utils::*;
use crate::{
    common::{get_current_stamp, P2PPeer, PeerType},
    connection::{connection_private::ConnectionPrivate, ConnectionStatus},
    network::{NetworkRequest, NetworkResponse},
};
use concordium_common::functor::FuncResult;
use std::sync::RwLock;

pub fn handshake_response_handle(
    priv_conn: &RwLock<ConnectionPrivate>,
    req: &NetworkResponse,
) -> FuncResult<()> {
    if let NetworkResponse::Handshake(ref remote_peer, ref nets, _) = req {
        {
            let mut priv_conn_mut = write_or_die!(priv_conn);
            priv_conn_mut.sent_handshake = get_current_stamp();
            priv_conn_mut.add_remote_end_networks(nets);
            priv_conn_mut.promote_to_post_handshake(remote_peer.id(), remote_peer.addr)?;
        }

        let bucket_sender =
            P2PPeer::from(remote_peer.peer_type(), remote_peer.id(), remote_peer.addr);
        if remote_peer.peer_type() != PeerType::Bootstrapper {
            safe_write!(read_or_die!(priv_conn).buckets)?
                .insert_into_bucket(&bucket_sender, nets.clone());
        }
        if let Some(ref service) = read_or_die!(priv_conn).stats_export_service {
            service.peers_inc();
        };
    } else {
        safe_write!(priv_conn)?.status = ConnectionStatus::Closing;
        error!(
            "Peer tried to send packets before handshake was completed (still waiting on \
             HandshakeResponse)!"
        );
    }
    Ok(())
}

pub fn handshake_request_handle(
    priv_conn: &RwLock<ConnectionPrivate>,
    req: &NetworkRequest,
) -> FuncResult<()> {
    if let NetworkRequest::Handshake(sender, nets, _) = req {
        debug!("Got request for Handshake");

        // Setup peer and networks before sending handshake.
        {
            let mut priv_conn_mut = write_or_die!(priv_conn);
            priv_conn_mut.add_remote_end_networks(nets);
            priv_conn_mut.promote_to_post_handshake(sender.id(), sender.addr)?;
        }
        send_handshake_and_ping(priv_conn)?;
        {
            let mut priv_conn_mut = write_or_die!(priv_conn);
            priv_conn_mut.update_last_seen();
            priv_conn_mut.set_measured_ping_sent();
        }

        update_buckets(priv_conn, sender, nets.clone())?;

        if read_or_die!(priv_conn).local_peer.peer_type() == PeerType::Bootstrapper {
            send_peer_list(priv_conn, sender, nets)?;
        }
    } else {
        safe_write!(priv_conn)?.status = ConnectionStatus::Closing;
        error!(
            "Peer tried to send packets before handshake was completed (still waiting on \
             HandshakeRequest)!"
        );
    }
    Ok(())
}
