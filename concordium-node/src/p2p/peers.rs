use crate::{
    common::{get_current_stamp, P2PNodeId, PeerStats, PeerType},
    connection::Connection,
    netmsg,
    network::{NetworkMessage, NetworkMessagePayload, NetworkRequest},
    p2p::{maintenance::attempt_bootstrap, P2PNode},
};

use std::sync::{atomic::Ordering, Arc};

impl P2PNode {
    pub fn get_peer_stats(&self, peer_type: Option<PeerType>) -> Vec<PeerStats> {
        read_or_die!(self.connections())
            .values()
            .filter(|conn| conn.is_post_handshake())
            .filter(|conn| peer_type.is_none() || peer_type == Some(conn.remote_peer_type()))
            .filter_map(|conn| conn.remote_peer_stats().ok())
            .collect()
    }

    /// This function is called periodically to print information about current
    /// nodes.
    pub fn print_stats(&self, peer_stat_list: &[PeerStats]) {
        trace!("Printing out stats");
        debug!("I currently have {}/{} peers", peer_stat_list.len(), self.config.max_allowed_nodes);

        // Print nodes
        if self.config.print_peers {
            for (i, peer) in peer_stat_list.iter().enumerate() {
                trace!("Peer {}: {}/{}/{}", i, P2PNodeId(peer.id), peer.addr, peer.peer_type);
            }
        }
    }

    pub fn get_node_peer_ids(&self) -> Vec<u64> {
        self.get_peer_stats(Some(PeerType::Node)).into_iter().map(|stats| stats.id).collect()
    }

    pub fn measure_throughput(&self, peer_stats: &[PeerStats]) -> (u64, u64) {
        let prev_bytes_received = self.stats.get_bytes_received();
        let prev_bytes_sent = self.stats.get_bytes_sent();

        let (bytes_received, bytes_sent) = peer_stats
            .iter()
            .filter(|ps| ps.peer_type == PeerType::Node)
            .map(|ps| (ps.bytes_received, ps.bytes_sent))
            .fold((0, 0), |(acc_i, acc_o), (i, o)| (acc_i + i, acc_o + o));

        self.stats.set_bytes_received(bytes_received);
        self.stats.set_bytes_sent(bytes_sent);

        let time_diff = self.config.housekeeping_interval as f64;

        let avg_bps_in =
            ((bytes_received.saturating_sub(prev_bytes_received)) as f64 / time_diff) as u64;
        let avg_bps_out = ((bytes_sent.saturating_sub(prev_bytes_sent)) as f64 / time_diff) as u64;

        self.stats.set_avg_bps_in(avg_bps_in);
        self.stats.set_avg_bps_out(avg_bps_out);

        (avg_bps_in, avg_bps_out)
    }

    fn send_get_peers(&self) {
        if let Ok(nids) = safe_read!(self.networks()) {
            let request = NetworkRequest::GetPeers(nids.iter().copied().collect());
            let message = netmsg!(NetworkRequest, request);
            let filter = |_: &Connection| true;

            if let Err(e) = {
                let mut buf = Vec::with_capacity(256);
                message
                    .serialize(&mut buf)
                    .map(|_| buf)
                    .map(|buf| self.send_over_all_connections(&buf, &filter))
            } {
                error!("A network message couldn't be forwarded: {}", e);
            }
        }
    }

    pub fn bump_last_peer_update(&self) {
        self.connection_handler.last_peer_update.store(get_current_stamp(), Ordering::SeqCst)
    }

    pub fn last_peer_update(&self) -> u64 {
        self.connection_handler.last_peer_update.load(Ordering::SeqCst)
    }
}

pub fn check_peers(node: &Arc<P2PNode>, peer_stat_list: &[PeerStats]) {
    trace!("Checking for needed peers");
    if node.peer_type() != PeerType::Bootstrapper
        && !node.config.no_net
        && node.config.desired_nodes_count
            > peer_stat_list.iter().filter(|peer| peer.peer_type != PeerType::Bootstrapper).count()
                as u16
    {
        if peer_stat_list.is_empty() {
            info!("Sending out GetPeers to any bootstrappers we may still be connected to");
            {
                node.send_get_peers();
            }
            if !node.config.no_bootstrap_dns {
                info!("No peers at all - retrying bootstrapping");
                attempt_bootstrap(node);
            } else {
                info!(
                    "No nodes at all - Not retrying bootstrapping using DNS since --no-bootstrap \
                     is specified"
                );
            }
        } else {
            info!("Not enough peers, sending GetPeers requests");
            node.send_get_peers();
        }
    }
}
