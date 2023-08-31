//! Peer handling.

use crate::{
    common::{get_current_stamp, p2p_peer::RemotePeerId, PeerStats, PeerType},
    connection::Connection,
    netmsg,
    network::NetworkRequest,
    p2p::{connectivity::connect, maintenance::attempt_bootstrap, P2PNode},
    read_or_die,
};
use anyhow::ensure;
use byteorder::{BigEndian, ByteOrder, ReadBytesExt, WriteBytesExt};
use chrono::Utc;
use concordium_base::common::{Buffer, Deserial, Serial};
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
        self.get_peer_stats(Some(PeerType::Node)).into_iter().map(|stats| stats.local_id).collect()
    }

    /// Measures the node's average byte throughput as bps i.e., bytes per
    /// second.
    pub fn measure_throughput(&self) -> anyhow::Result<()> {
        let prev_bytes_received = self.stats.last_throughput_measurement_received_bytes.get();
        let prev_bytes_sent = self.stats.last_throughput_measurement_sent_bytes.get();

        let bytes_received = self.stats.received_bytes.get();
        let bytes_sent = self.stats.sent_bytes.get();

        self.stats.last_throughput_measurement_received_bytes.set(bytes_received);
        self.stats.last_throughput_measurement_sent_bytes.set(bytes_sent);

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
        let filter = |_: &Connection| true;

        let mut buf = Vec::with_capacity(256);

        if let Err(e) = message
            .serialize(&mut buf)
            .map(|_| buf)
            .map(|buf| self.send_over_all_connections(&buf, &filter))
        {
            error!("Can't send a GetPeers request: {}", e);
        }
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
        node.print_stats(peer_stats);
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

const PEERS_STORE_NAME: &str = "peers";

/// A peer that is stored in the lmdb database.
/// This is just a newtype over a [`SocketAddr`] so
/// it is possible to implement the necessary serializing/deserializing traits.
#[derive(Debug, PartialEq, Eq)]
pub struct StoredPeer(SocketAddr);

impl From<SocketAddr> for StoredPeer {
    fn from(addr: SocketAddr) -> Self { StoredPeer(addr) }
}

impl Serial for StoredPeer {
    fn serial<W: Buffer + WriteBytesExt>(&self, target: &mut W) {
        match self.0 {
            SocketAddr::V4(addr) => {
                target.write_u8(0).expect("surely we can write to memory");
                target.write_u16::<BigEndian>(addr.port()).expect("surely we can write to memory");
                target
                    .write_u32::<BigEndian>(BigEndian::read_u32(&addr.ip().octets()))
                    .expect("surely we can write to memory");
            }
            SocketAddr::V6(addr) => {
                target.write_u8(1).expect("surely we can write to memory");
                target.write_u16::<BigEndian>(addr.port()).expect("surely we can write to memory");
                target
                    .write_u128::<BigEndian>(BigEndian::read_u128(&addr.ip().octets()))
                    .expect("surely we can write to memory");
            }
        }
    }
}

impl Deserial for StoredPeer {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> anyhow::Result<Self> {
        let peer = match source.read_u8()? {
            0 => {
                let port = source.read_u16::<BigEndian>()?;
                let ip = source.read_u32::<BigEndian>()?;
                let mut buf = [0; 4];
                BigEndian::write_u32(&mut buf, ip);
                Self(SocketAddr::new(std::net::IpAddr::V4(buf.into()), port))
            }
            1 => {
                let port = source.read_u16::<BigEndian>()?;
                let ip = source.read_u128::<BigEndian>()?;
                let mut buf = [0; 16];
                BigEndian::write_u128(&mut buf, ip);
                Self(SocketAddr::new(std::net::IpAddr::V6(buf.into()), port))
            }
            _ => anyhow::bail!("Unsupported type of `StoredPeer`"),
        };
        Ok(peer)
    }
}

/// Persist the [`SocketAddr`] of a peer.
pub fn persist_peer(node: &Arc<P2PNode>, peer_addr: SocketAddr) {
    if let Ok(kv) = node.kvs.read() {
        let peers_store = kv.open_single(PEERS_STORE_NAME, StoreOptions::create()).expect("foo");
        let mut buf = Vec::new();
        let stored_peer: StoredPeer = peer_addr.into();
        stored_peer.serial(&mut buf);
        let mut writer = kv.write().expect("foo");
        peers_store.put(&mut writer, buf, &Value::U64(0)).expect("foo");
        writer.commit().expect("foo");
    } else {
        warn!("Could not acqure lock over lmdb");
    };
}

/// Remove a peer from the persisted peer database.
pub fn remove_persisted_peer(node: &Arc<P2PNode>, peer_addr: SocketAddr) {
    if let Ok(kv) = node.kvs.read() {
        let peers_store = kv.open_single(PEERS_STORE_NAME, StoreOptions::create()).expect("foo");
        let mut key = Vec::new();
        let stored_peer: StoredPeer = peer_addr.into();
        stored_peer.serial(&mut key);
        let mut writer = kv.write().expect("foo");
        peers_store.delete(&mut writer, key).expect("foo");
        writer.commit().expect("foo");
    } else {
        warn!("Could not acqure lock over lmdb");
    };
}

/// Try connect to previosly connected peers if
/// anyone is stored.
/// Note that as opposed to [`connect_to_config_nodes`] this function respects
/// the maximum peers configured for the node.
pub fn connect_to_stored_nodes(node: &Arc<P2PNode>) -> anyhow::Result<()> {
    if let Ok(kvs_env) = node.kvs.read() {
        let peers_store = kvs_env.open_single(PEERS_STORE_NAME, StoreOptions::create())?;
        let peers_reader = kvs_env.read()?;
        let peers_iter = peers_store.iter_start(&peers_reader)?;
        for entry in peers_iter {
            let (mut peer_bytes, _expiry) = entry?;
            if let Err(e) =
                connect(node, PeerType::Node, StoredPeer::deserial(&mut peer_bytes)?.0, None, true)
            {
                warn!("could not connect to previosly connected peer {}", e);
            }
        }
        Ok(())
    } else {
        anyhow::bail!("could not read previosly connected peers: cannot obtain lock for the kvs");
    }
}

impl P2PNode {
    pub fn clear_persisted_peers(&self) -> anyhow::Result<()> {
        if let Ok(kvs_env) = self.kvs.read() {
            let peers_store = kvs_env.open_single(PEERS_STORE_NAME, StoreOptions::create())?;
            let mut writer = kvs_env.write()?;
            peers_store.clear(&mut writer)?;
            writer.commit().map_err(|e| e.into())
        } else {
            anyhow::bail!("Couldn't clear the bans: couldn't obtain a lock over the kvs");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{
        io::Cursor,
        net::{IpAddr, Ipv4Addr, Ipv6Addr},
    };

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

    #[test]
    fn test_serial_deserial_stored_peer() -> anyhow::Result<()> {
        let stored_peer_v4: StoredPeer =
            SocketAddr::new(IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)), 8080).into();
        let mut buf = Vec::new();
        stored_peer_v4.serial(&mut buf);
        let deserialized_peer_v4 = StoredPeer::deserial(&mut Cursor::new(&buf))?;
        assert_eq!(stored_peer_v4, deserialized_peer_v4);

        let stored_peer_v6: StoredPeer =
            SocketAddr::new(IpAddr::V6(Ipv6Addr::new(0, 0, 0, 0, 0, 0, 0, 1)), 8080).into();
        let mut buf = Vec::new();
        stored_peer_v6.serial(&mut buf);
        let deserialized_peer_v6 = StoredPeer::deserial(&mut Cursor::new(&buf))?;
        assert_eq!(stored_peer_v6, deserialized_peer_v6);

        Ok(())
    }
}
