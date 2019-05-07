use super::{fails, handler_utils::*};
use crate::{
    common::{get_current_stamp, P2PPeer, PeerType},
    connection::connection_private::ConnectionPrivate,
    network::{NetworkRequest, NetworkResponse},
};
use concordium_common::functor::FuncResult;
use failure::bail;
use std::cell::RefCell;

pub fn handshake_response_handle(
    priv_conn: &RefCell<ConnectionPrivate>,
    req: &NetworkResponse,
) -> FuncResult<()> {
    if let NetworkResponse::Handshake(ref remote_peer, ref nets, _) = req {
        {
            let mut priv_conn_mut = priv_conn.borrow_mut();
            priv_conn_mut.sent_handshake = get_current_stamp();
            priv_conn_mut.add_remote_end_networks(nets);
            priv_conn_mut.promote_to_post_handshake(remote_peer.id(), remote_peer.addr)?;
        }

        let bucket_sender =
            P2PPeer::from(remote_peer.peer_type(), remote_peer.id(), remote_peer.addr);
        if remote_peer.peer_type() != PeerType::Bootstrapper {
            safe_write!(priv_conn.borrow().buckets)?
                .insert_into_bucket(&bucket_sender, nets.clone());
        }
        if let Some(ref service) = priv_conn.borrow().stats_export_service {
            safe_write!(service)?.peers_inc();
        };

        // TODO - only issue when needed externally, after global state knows it needs
        // to do this
        if priv_conn.borrow().remote_peer.peer_type() != PeerType::Bootstrapper
            && priv_conn.borrow().local_peer.peer_type() != PeerType::Bootstrapper
        {
            let one_net = priv_conn.borrow().remote_end_networks.clone();
            if let Some(one_net) = one_net.iter().next() {
                send_retransmit_request(priv_conn, 0, *one_net)?
            };
        }
        Ok(())
    } else {
        bail!(fails::UnwantedMessageError {
            message: format!("Was expecting handshake, received {:?}", req),
        });
    }
}

pub fn handshake_request_handle(
    priv_conn: &RefCell<ConnectionPrivate>,
    req: &NetworkRequest,
) -> FuncResult<()> {
    if let NetworkRequest::Handshake(sender, nets, _) = req {
        debug!("Got request for Handshake");

        // Setup peer and networks before send Handshake.
        {
            let mut priv_conn_mut = priv_conn.borrow_mut();
            priv_conn_mut.add_remote_end_networks(nets);
            priv_conn_mut.promote_to_post_handshake(sender.id(), sender.addr)?;
        }
        send_handshake_and_ping(priv_conn)?;
        {
            let mut priv_conn_mut = priv_conn.borrow_mut();
            priv_conn_mut.update_last_seen();
            priv_conn_mut.set_measured_ping_sent();
        }

        update_buckets(priv_conn, sender, nets.clone())?;

        if priv_conn.borrow().local_peer.peer_type() == PeerType::Bootstrapper {
            send_peer_list(priv_conn, sender, nets)?;
        }
        Ok(())
    } else {
        bail!(fails::UnwantedMessageError {
            message: format!("Was expecting handshake, received {:?}", req),
        });
    }
}
