//! Peer handling.

use crate::{
    common::{get_current_stamp, p2p_peer::RemotePeerId, PeerStats, PeerType},
    connection::Connection,
    netmsg,
    network::NetworkRequest,
    only_fbs,
    p2p::{maintenance::attempt_bootstrap, P2PNode},
    read_or_die,
};
use std::{
    sync::{atomic::Ordering, Arc},
    time::{Duration, SystemTime},
};

impl P2PNode {
    /// Obtain the list of statistics from all the peers, optionally of a
    /// specific peer type.
    pub fn get_peer_stats(&self, peer_type: Option<PeerType>) -> Vec<PeerStats> {
        read_or_die!(self.connections())
            .values()
            .filter(|conn| peer_type.is_none() || peer_type == Some(conn.remote_peer_type()))
            .map(|conn| {
                PeerStats::new(
                    conn.remote_peer.local_id,
                    conn.remote_peer.self_id.unwrap(), // safe - always available post-handshake
                    conn.remote_addr(),
                    conn.remote_peer_external_port(),
                    conn.remote_peer_type(),
                    &conn.stats,
                )
            })
            .collect()
    }

    /// Prints information about all the peers.
    pub fn print_stats(&self, peer_stat_list: &[PeerStats]) {
        for (i, peer) in peer_stat_list.iter().enumerate() {
            trace!(
                "Peer {}({}): {}/{}/{}",
                i,
                peer.self_id,
                peer.local_id,
                peer.addr,
                peer.peer_type
            );
        }
    }

    /// Obtain the node ids of all the node peers.
    pub fn get_node_peer_tokens(&self) -> Vec<RemotePeerId> {
        self.get_peer_stats(Some(PeerType::Node)).into_iter().map(|stats| stats.local_id).collect()
    }

    /// Measures the node's average byte throughput.
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

        let now = SystemTime::now();
        let last_measurement =
            SystemTime::UNIX_EPOCH + Duration::from_secs(self.stats.get_throughput_timestamp());
        match now.duration_since(last_measurement) {
            Ok(delta) => {
                let delta_secs = delta.as_secs();
                let avg_bps_in = bytes_received.saturating_sub(prev_bytes_received / delta_secs);
                let avg_bps_out = bytes_sent.saturating_sub(prev_bytes_sent) / delta_secs;
                self.stats.set_avg_bps_in(avg_bps_in);
                self.stats.set_avg_bps_out(avg_bps_out);
                match now.duration_since(SystemTime::UNIX_EPOCH) {
                    Ok(timestamp) => {
                        self.stats.set_throughput_timestamp(timestamp.as_secs());
                    }
                    Err(_err) => (), // This only happens if suddenly `now` was before `1/1/1970`
                }
                (avg_bps_in, avg_bps_out)
            }
            Err(_err) => (0, 0), /* this only happens if `prev_throughput_measurement_timestamp`
                                  * is later than `now`. */
        }
    }

    fn send_get_peers(&self) {
        let request =
            NetworkRequest::GetPeers(read_or_die!(self.networks()).iter().copied().collect());
        let message = netmsg!(NetworkRequest, request);
        let filter = |_: &Connection| true;

        only_fbs!({
            let mut buf = Vec::with_capacity(256);

            if let Err(e) = message
                .serialize(&mut buf)
                .map(|_| buf)
                .map(|buf| self.send_over_all_connections(&buf, &filter))
            {
                error!("Can't send a GetPeers request: {}", e);
            }
        });
    }

    /// Update the timestamp of the last peer update.
    pub fn bump_last_peer_update(&self) {
        self.connection_handler.last_peer_update.store(get_current_stamp(), Ordering::SeqCst)
    }

    /// Obtain the timestamp of the last peer update.
    pub fn last_peer_update(&self) -> u64 {
        self.connection_handler.last_peer_update.load(Ordering::SeqCst)
    }
}

/// Checks whether we need any more peers, based on the `desired_nodes_count`
/// config.
/// If bootstrapping was just attempted then this function will not attempt it
/// again. Otherwise if there are no peers bootstrapping will be attempted.
/// FIXME: This is not a good solution, but I do not want to change the
/// bootstrapping logic at the last minute. This addresses the duplication of
/// work and the strange looking messages "already connected to ..." in the
/// logs.
pub fn check_peers(node: &Arc<P2PNode>, peer_stats: &[PeerStats], attempted_bootstrap: bool) {
    debug!("I currently have {}/{} peers", peer_stats.len(), node.config.max_allowed_nodes);

    if node.config.print_peers {
        node.print_stats(&peer_stats);
    }

    if node.self_peer.peer_type == PeerType::Node {
        let node_count = peer_stats.iter().filter(|peer| peer.peer_type == PeerType::Node).count();

        if !node.config.no_net && node_count < node.config.desired_nodes_count as usize {
            if peer_stats.is_empty() {
                if !attempted_bootstrap {
                    if !node.config.no_bootstrap_dns {
                        info!("No peers at all - retrying bootstrapping");
                        attempt_bootstrap(node);
                    } else {
                        info!(
                            "No nodes at all - Not retrying bootstrapping using DNS since \
                             --no-bootstrap is specified"
                        );
                    }
                }
            } else {
                info!("Not enough peers - sending GetPeers requests");
                node.send_get_peers();
            }
        }
    }
}
