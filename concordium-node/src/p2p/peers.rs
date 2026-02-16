//! Peer handling.

use crate::connection::MessageSendingPriority;
use crate::{
    common::{get_current_stamp, p2p_peer::RemotePeerId, PeerStats, PeerType},
    netmsg,
    network::NetworkRequest,
    p2p::{connectivity::connect, maintenance::attempt_bootstrap, P2PNode},
    read_or_die, write_or_die,
};
use anyhow::ensure;
use chrono::Utc;
use prometheus::core::Atomic;
use rkv::{StoreOptions, Value};
use std::{
    net::SocketAddr,
    sync::{atomic::Ordering, Arc},
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
        self.get_peer_stats(Some(PeerType::Node))
            .into_iter()
            .map(|stats| stats.local_id)
            .collect()
    }

    /// Measures the node's average byte throughput as bps i.e., bytes per
    /// second.
    pub fn measure_throughput(&self) -> anyhow::Result<()> {
        let prev_bytes_received = self.stats.last_throughput_measurement_received_bytes.get();
        let prev_bytes_sent = self.stats.last_throughput_measurement_sent_bytes.get();

        let bytes_received = self.stats.received_bytes.get();
        let bytes_sent = self.stats.sent_bytes.get();

        self.stats
            .last_throughput_measurement_received_bytes
            .set(bytes_received);
        self.stats
            .last_throughput_measurement_sent_bytes
            .set(bytes_sent);

        let now = Utc::now().timestamp_millis();
        let (avg_bps_in, avg_bps_out) = calculate_average_throughput(
            self.stats.last_throughput_measurement_timestamp.get(),
            now,
            prev_bytes_received,
            bytes_received,
            prev_bytes_sent,
            bytes_sent,
        )?;
        self.stats.avg_bps_in.set(avg_bps_in);
        self.stats.avg_bps_out.set(avg_bps_out);
        self.stats.last_throughput_measurement_timestamp.set(now);
        Ok(())
    }

    fn send_get_peers(&self) {
        let request =
            NetworkRequest::GetPeers(read_or_die!(self.networks()).iter().copied().collect());
        let message = netmsg!(NetworkRequest, request);
        let mut buf = Vec::with_capacity(256);

        if let Err(e) = message
            .serialize(&mut buf)            
        {
            error!("Can't send a GetPeers request: {}", e);
            return;
        }
        
        self.send_get_peers_to_all_connections(&buf);
    }

    pub fn send_get_peers_to_all_connections(&self, data: &[u8]) -> usize {
        let mut sent_messages = 0usize;
        let data = Arc::from(data);

        for conn in write_or_die!(self.connections()).values_mut() {
            conn.get_peers_list_semaphore.add_permits(1);
            println!(
                "**** Armed semaphore for Peer {}. Permits: {} ****",
                conn.remote_peer.local_id,
                conn.get_peers_list_semaphore.available_permits()
            );

            conn.async_send(Arc::clone(&data), MessageSendingPriority::Normal);
            sent_messages += 1;
        }
        println!("**** number of sent messages: {} ****", sent_messages);
        sent_messages
    }

    /// Update the timestamp of the last peer update.
    pub fn bump_last_peer_update(&self) {
        self.connection_handler
            .last_peer_update
            .store(get_current_stamp(), Ordering::SeqCst)
    }

    /// Obtain the timestamp of the last peer update.
    pub fn last_peer_update(&self) -> u64 {
        self.connection_handler
            .last_peer_update
            .load(Ordering::SeqCst)
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
    debug!(
        "I currently have {}/{} peers",
        peer_stats.len(),
        node.config.max_allowed_nodes
    );

    if node.config.print_peers {
        node.print_stats(peer_stats);
    }

    if node.self_peer.peer_type == PeerType::Node {
        let node_count = peer_stats
            .iter()
            .filter(|peer| peer.peer_type == PeerType::Node)
            .count();

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

/// Calculate the average bytes bps (Bytes per second) received and sent during
/// the time `delta` (specified in milliseconds).
fn calculate_average_throughput(
    before_millis: i64,   // timestamp of the last measurement
    now_millis: i64,      // timestamp of the current measurement
    prev_bytes_recv: u64, // number of bytes received at the time of previous measurement
    bytes_recv: u64,      // number of bytes received at the time of current measurement
    prev_bytes_sent: u64, // number of bytes sent at the time of previous measurement
    bytes_sent: u64,      // number of bytes sent at the the time of current measurement
) -> anyhow::Result<(u64, u64)> {
    let milliseconds_to_second = 1000u64;
    ensure!(
        now_millis > before_millis,
        "Time went backwards or did not change. Refusing to calculate average throughput."
    );
    let delta: u64 = (now_millis - before_millis) as u64; // as is safe since we checked the difference is positive.

    let avg_bps_in = (milliseconds_to_second * (bytes_recv - prev_bytes_recv)) / delta;
    let avg_bps_out = (milliseconds_to_second * (bytes_sent - prev_bytes_sent)) / delta;

    Ok((avg_bps_in, avg_bps_out))
}

/// The lmdb store name of the persisted peers.
const PEERS_STORE_NAME: &str = "peers";

/// Try connect to previously connected peers if any.
/// Note that as opposed to [`connect_to_config_nodes`] this function respects
/// the maximum peers configured for the node.
pub fn connect_to_stored_nodes(node: &Arc<P2PNode>) -> anyhow::Result<()> {
    let Ok(kvs_env) = node.kvs.read() else {
        anyhow::bail!("could not read previously connected peers: cannot obtain lock for the kvs");
    };
    let peers_store = kvs_env.open_single(PEERS_STORE_NAME, StoreOptions::create())?;
    let peers_reader = kvs_env.read()?;
    let peers_iter = peers_store.iter_start(&peers_reader)?;
    for entry in peers_iter {
        let (peer_bytes, _expiry) = entry?;
        let stored_peer: SocketAddr = serde_json::from_slice(peer_bytes)?;
        if let Err(e) = connect(node, PeerType::Node, stored_peer, None, true) {
            warn!("could not connect to previously connected peer {}", e);
        }
    }
    Ok(())
}

impl P2PNode {
    /// Persist the [`SocketAddr`] of a peer.
    pub fn persist_peer(&self, peer_addr: SocketAddr) -> anyhow::Result<()> {
        let Ok(kv) = self.kvs.read() else {
            anyhow::bail!("Could not acquire lock over lmdb");
        };
        let peers_store = kv.open_single(PEERS_STORE_NAME, StoreOptions::create())?;
        let buf = serde_json::to_vec::<SocketAddr>(&peer_addr)?;
        let mut writer = kv.write()?;
        peers_store.put(&mut writer, buf, &Value::U64(0))?;
        writer.commit()?;
        Ok(())
    }

    /// Remove a peer from the persisted peer database.
    pub fn remove_persisted_peer(&self, peer_addr: SocketAddr) -> anyhow::Result<()> {
        let Ok(kv) = self.kvs.read() else {
            anyhow::bail!("Could not acqure lock over lmdb");
        };
        let peers_store = kv.open_single(PEERS_STORE_NAME, StoreOptions::create())?;
        let key = serde_json::to_vec::<SocketAddr>(&peer_addr)?;
        let mut writer = kv.write()?;
        peers_store.delete(&mut writer, key)?;
        writer.commit()?;
        Ok(())
    }

    /// Clear all peers in the lmdb peers store.
    pub fn clear_persisted_peers(&self) -> anyhow::Result<()> {
        let Ok(kvs_env) = self.kvs.read() else {
            anyhow::bail!("Couldn't clear the bans: couldn't obtain a lock over the kvs");
        };
        let peers_store = kvs_env.open_single(PEERS_STORE_NAME, StoreOptions::create())?;
        let mut writer = kvs_env.write()?;
        peers_store.clear(&mut writer)?;
        writer.commit().map_err(|e| e.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_average_throughput() {
        // Test with a sound delta
        // Send and receive 1kb in 1000 milliseconds is 1000bps or 1kbps
        if let Ok((recv, send)) = calculate_average_throughput(1, 1001, 1, 1001, 1, 1001) {
            assert_eq!(1000, recv);
            assert_eq!(1000, send);
        }

        // Test with a zero delta
        assert!(
            calculate_average_throughput(1, 1, 1, 2, 1, 2).is_err(),
            "Calculation should fail since time difference is 0."
        );

        assert!(
            calculate_average_throughput(2, 1, 1, 2, 1, 2).is_err(),
            "Calculation should fail since time difference is negative."
        );
    }
}
