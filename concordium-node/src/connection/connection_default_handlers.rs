use std::cell::{ RefCell };

use crate::common::{ get_current_stamp };
use crate::common::counter::{ TOTAL_MESSAGES_SENT_COUNTER };
use crate::common::functor::{ FunctorResult };
use crate::network::{ NetworkRequest, NetworkResponse };
use crate::connection::connection_private::{ ConnectionPrivate };

use std::sync::atomic::Ordering;
use super::fails;
use super::handler_utils::*;
use failure::{ bail };

macro_rules! reject_handshake {
    ($direction:ident, $message:ident) => {{
        if let $direction::Handshake(..) = $message {
            bail!(fails::UnwantedMessageError{message: "Unwanted handshake message".to_owned()})
        }

        Ok(())
    }}
}

/// Default `NetworkRequest::Ping` handler.
/// It responds with a pong packet.
pub fn default_network_request_ping_handle(
        priv_conn: &RefCell<ConnectionPrivate>,
        _req: &NetworkRequest) -> FunctorResult {

    priv_conn.borrow_mut().update_last_seen();
    TOTAL_MESSAGES_SENT_COUNTER.fetch_add( 1, Ordering::Relaxed );

    let pong_data = {
        let priv_conn_borrow = priv_conn.borrow();
        if let Some(ref prom) = priv_conn_borrow.prometheus_exporter {
            safe_write!(prom)?.pkt_sent_inc()?
        };

        // Make `Pong` response and send
        let peer = priv_conn_borrow.peer().clone()
            .ok_or_else( || make_fn_error_peer("Couldn't get peer"))?;

        NetworkResponse::Pong(peer.clone()).serialize()
    };


    Ok( serialize_bytes( &mut priv_conn.borrow_mut().tls_session, &pong_data)?)
}

/// It sends the list of nodes.
pub fn default_network_request_find_node_handle(
        priv_conn: &RefCell<ConnectionPrivate>,
        req: &NetworkRequest
    ) -> FunctorResult {

    if let NetworkRequest::FindNode(_, node_id) = req {
        priv_conn.borrow_mut().update_last_seen();

        //Return list of nodes
        let response_data = {
            let priv_conn_borrow = priv_conn.borrow();
            let peer = priv_conn_borrow.peer().clone()
                .ok_or_else( || make_fn_error_peer("Couldn't borrow peer"))?;
            let nodes = safe_read!(priv_conn_borrow.buckets)?
                .closest_nodes(node_id);
            NetworkResponse::FindNode( peer, nodes).serialize()
        };

        Ok( serialize_bytes( &mut priv_conn.borrow_mut().tls_session, &response_data)?)
     } else {
        bail!(make_msg_error( "Find node handler cannot handler this packet"))
    }
}

pub fn default_network_request_get_peers(
        priv_conn: &RefCell<ConnectionPrivate>,
        req: &NetworkRequest) -> FunctorResult {

    if let NetworkRequest::GetPeers(ref sender, ref networks) = req {

        debug!("Got request for GetPeers");

        priv_conn.borrow_mut().update_last_seen();
        TOTAL_MESSAGES_SENT_COUNTER.fetch_add( 1, Ordering::Relaxed);

        let peer_list_packet = {
            let priv_conn_borrow = priv_conn.borrow();
            let nodes = safe_read!(priv_conn_borrow.buckets)?
                .get_all_nodes(Some(&sender), networks);

            if let Some(ref prom) = priv_conn_borrow.prometheus_exporter {
                safe_write!(prom)?.pkt_sent_inc()?;
            };

            let peer = priv_conn_borrow.peer().clone().ok_or_else( || make_fn_error_peer("Couldn't borrow peer"))?;
            NetworkResponse::PeerList(peer, nodes)
                .serialize()
        };

        Ok(serialize_bytes( &mut priv_conn.borrow_mut().tls_session, &peer_list_packet)?)
    } else {
        bail!(make_msg_error( "Get peers handler cannot handler this packet"))
    }
}


pub fn default_network_response_find_node (
        priv_conn: &RefCell<ConnectionPrivate>,
        res: &NetworkResponse) -> FunctorResult {

    if let NetworkResponse::FindNode(_, ref peers) = res {
        debug!("Got response to FindNode");

        let priv_conn_borrow = priv_conn.borrow();
        //Process the received node list
        let mut ref_buckets = safe_write!(priv_conn_borrow.buckets)?;
        for peer in peers.iter() {
            ref_buckets.insert_into_bucket(peer, &priv_conn_borrow.own_id, vec![]);
        }

        Ok(())
    } else {
        bail!(make_msg_error( "Response find node handler cannot handler this packet"))
    }
}

/// It measures network latency.
pub fn default_network_response_pong(
        priv_conn: &RefCell<ConnectionPrivate>,
        _res: &NetworkResponse) -> FunctorResult {

    let ping: u64 = priv_conn.borrow().sent_ping.clone();
    let curr: u64 = get_current_stamp();

    if curr >= ping {
        priv_conn.borrow_mut().last_latency_measured = curr - ping;
    }

    Ok(())
}

/// It inserts new peers into buckets.
pub fn default_network_response_peer_list(
        priv_conn: &RefCell< ConnectionPrivate>,
        res: &NetworkResponse) -> FunctorResult {
    if let NetworkResponse::PeerList( _, ref peers) = res {
        let priv_conn_borrow = priv_conn.borrow();
        let mut locked_buckets = safe_write!(priv_conn_borrow.buckets)?;
        for peer in peers.iter() {
            locked_buckets.insert_into_bucket( peer, &priv_conn_borrow.own_id, vec![]);
        }
    };
    Ok(())
}

/// In handshake:
///     - Add network
///     - Store target peer info and allocates buckets for this connection.
///     - Statistics: Export to Prometheus
///     - Log: Join to network
pub fn default_network_response_handshake(
        res: &NetworkResponse) -> FunctorResult {
    reject_handshake!(NetworkResponse, res)
}

/// It adds new network and update its buckets.
pub fn default_network_request_join_network(
        priv_conn: &RefCell< ConnectionPrivate>,
        res: &NetworkRequest) -> FunctorResult {

    if let NetworkRequest::JoinNetwork(ref _sender, ref network) = res {
        let networks = vec![*network];
        priv_conn.borrow_mut().add_networks(&networks);

        let priv_conn_borrow = priv_conn.borrow();
        let peer = priv_conn_borrow.peer().clone()
            .ok_or_else( || make_fn_error_peer("Couldn't borrow peer"))?;
        let priv_conn_networks = priv_conn_borrow.networks.clone();

        safe_write!(priv_conn_borrow.buckets)?
            .update_network_ids(&peer, priv_conn_networks);

        log_as_joined_network( &priv_conn_borrow.event_log, &peer,  &networks)?;
    }

    Ok(())
}

/// It removes that network from its owns and update buckets.
pub fn default_network_request_leave_network(
        priv_conn: &RefCell<ConnectionPrivate>,
        req: &NetworkRequest) -> FunctorResult {

    if let NetworkRequest::LeaveNetwork(sender, network) = req {
        priv_conn.borrow_mut().remove_network( network);
        let priv_conn_borrow = priv_conn.borrow();
        let peer = priv_conn_borrow.peer().clone()
            .ok_or_else( || make_fn_error_peer("Couldn't borrow peer"))?;

        safe_write!(priv_conn_borrow.buckets)?
            .update_network_ids( &peer, priv_conn_borrow.networks.clone());

        log_as_leave_network( &priv_conn_borrow.event_log, &sender, *network)?;
    }

    Ok(())
}


/// On a handshake request:
///     - It replies with a handshake response and a ping.
///     - It adds the new network, and updates its buckets.
///     - Finally, it sends its peer list.
pub fn default_network_request_handshake(
        req: &NetworkRequest) -> FunctorResult {
    reject_handshake!(NetworkRequest, req)
}

/// Unknown messages only updates statistic information.
pub fn default_unknown_message(
        priv_conn: &RefCell<ConnectionPrivate>,
        _: &()) -> FunctorResult {

    debug!("Unknown message received!");

    {
        let mut priv_conn_mut = priv_conn.borrow_mut();

        priv_conn_mut.failed_pkts += 1;
        priv_conn_mut.update_last_seen();
    }

    // TODO It will need access to buf.
    // trace!("Contents were: {:?}",
    //        String::from_utf8(buf.to_vec()).unwrap());

    if let Some(ref prom) = priv_conn.borrow().prometheus_exporter {
        safe_write!(prom)?.unknown_pkts_received_inc()?;
    }
    Ok(())
}

/// Invalid messages only updates statistic information.
pub fn default_invalid_message(
         priv_conn: &RefCell<ConnectionPrivate>,
        _: &()) -> FunctorResult {
    {
        let mut priv_conn_mut = priv_conn.borrow_mut();

        priv_conn_mut.failed_pkts += 1;
        priv_conn_mut.update_last_seen();
    }

    // trace!("Contents were: {:?}",
    //        String::from_utf8(buf.to_vec()).unwrap());

    if let Some(ref prom) = priv_conn.borrow().prometheus_exporter {
       safe_write!(prom)?.invalid_pkts_received_inc()?;
    }

    Ok(())
}
