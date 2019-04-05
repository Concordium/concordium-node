use std::cell::RefCell;

use super::{fails, handler_utils::*};
use crate::{
    common::{functor::FunctorResult, get_current_stamp, ConnectionType, P2PPeer},
    connection::connection_private::ConnectionPrivate,
    network::{NetworkRequest, NetworkResponse},
};
use failure::bail;

pub fn handshake_response_handle(
    priv_conn: &RefCell<ConnectionPrivate>,
    req: &NetworkResponse,
) -> FunctorResult {
    if let NetworkResponse::Handshake(ref rpeer, ref nets, _) = req {
        {
            let mut priv_conn_mut = priv_conn.borrow_mut();
            priv_conn_mut.sent_handshake = get_current_stamp();
            priv_conn_mut.add_networks(nets);
            priv_conn_mut.set_peer(rpeer.clone());
        }

        let priv_conn_borrow = priv_conn.borrow();
        let conn_type = priv_conn_borrow.connection_type;
        let own_id = priv_conn_borrow.own_id.clone();
        let bucket_sender = P2PPeer::from(conn_type, rpeer.id().clone(), rpeer.ip(), rpeer.port());
        safe_write!(priv_conn_borrow.buckets)?.insert_into_bucket(
            &bucket_sender,
            &own_id,
            nets.clone(),
        );

        if let Some(ref prom) = priv_conn_borrow.prometheus_exporter {
            safe_write!(prom)?.peers_inc()?;
        };
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
) -> FunctorResult {
    if let NetworkRequest::Handshake(sender, nets, _) = req {
        debug!("Got request for Handshake");

        // Setup peer and networks before send Handshake.
        {
            let mut priv_conn_mut = priv_conn.borrow_mut();
            priv_conn_mut.add_networks(nets);
            priv_conn_mut.set_peer(sender.clone());
        }
        send_handshake_and_ping(priv_conn)?;
        {
            let mut priv_conn_mut = priv_conn.borrow_mut();
            priv_conn_mut.update_last_seen();
            priv_conn_mut.set_measured_ping_sent();
        }

        update_buckets(priv_conn, sender, nets)?;

        if priv_conn.borrow().connection_type == ConnectionType::Bootstrapper {
            send_peer_list(priv_conn, sender, nets)?;
        }
        Ok(())
    } else {
        bail!(fails::UnwantedMessageError {
            message: format!("Was expecting handshake, received {:?}", req),
        });
    }
}
