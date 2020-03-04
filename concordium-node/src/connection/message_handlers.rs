use crate::{
    common::{get_current_stamp, P2PPeer, PeerType},
    configuration::COMPATIBLE_CLIENT_VERSIONS,
    connection::Connection,
    network::{
        Handshake, NetworkId, NetworkMessage, NetworkPacket, NetworkPayload, NetworkRequest,
        NetworkResponse, PacketDestination,
    },
    p2p::{bans::BanId, connectivity::connect},
    plugins::consensus::*,
};
use concordium_common::{read_or_die, write_or_die};

use failure::Fallible;

use std::{collections::HashSet, net::SocketAddr, sync::atomic::Ordering};

impl Connection {
    /// Processes a network message based on its type.
    pub fn handle_incoming_message(&self, full_msg: NetworkMessage) -> Fallible<()> {
        match full_msg.payload {
            NetworkPayload::NetworkRequest(NetworkRequest::Handshake(handshake), ..) => {
                self.handle_handshake_req(handshake)
            }
            NetworkPayload::NetworkRequest(NetworkRequest::Ping, ..) => self.send_pong(),
            NetworkPayload::NetworkResponse(NetworkResponse::Pong, ..) => self.handle_pong(),
            NetworkPayload::NetworkRequest(NetworkRequest::GetPeers(ref networks), ..) => {
                self.handle_get_peers_req(networks)
            }
            NetworkPayload::NetworkResponse(NetworkResponse::PeerList(ref peers), ..) => {
                self.handle_peer_list_resp(peers)
            }
            NetworkPayload::NetworkRequest(NetworkRequest::JoinNetwork(network), ..) => {
                self.handle_join_network_req(network)
            }
            NetworkPayload::NetworkRequest(NetworkRequest::LeaveNetwork(network), ..) => {
                self.handle_leave_network_req(network)
            }
            NetworkPayload::NetworkRequest(NetworkRequest::BanNode(peer_to_ban), ..) => {
                self.handler.ban_node(peer_to_ban)
            }
            NetworkPayload::NetworkRequest(NetworkRequest::UnbanNode(peer_to_unban), ..) => {
                self.handle_unban(peer_to_unban)
            }
            NetworkPayload::NetworkPacket(pac, ..) => self.handle_incoming_packet(pac),
        }
    }

    fn handle_handshake_req(&self, handshake: Handshake) -> Fallible<()> {
        debug!("Got a Handshake request from peer {}", handshake.remote_id);

        if self.handler.is_banned(BanId::NodeId(handshake.remote_id))? {
            bail!("Rejecting a handshake request from a banned node");
        }

        if !COMPATIBLE_CLIENT_VERSIONS.contains(&handshake.version.to_string().as_str()) {
            bail!("Rejecting an incompatible client");
        }

        self.promote_to_post_handshake(handshake.remote_id, handshake.remote_port);
        self.add_remote_end_networks(&handshake.networks);

        let remote_peer = P2PPeer::from((
            self.remote_peer.peer_type,
            handshake.remote_id,
            SocketAddr::new(self.remote_peer.addr.ip(), handshake.remote_port),
        ));

        if remote_peer.peer_type != PeerType::Bootstrapper {
            write_or_die!(self.handler.connection_handler.buckets)
                .insert_into_bucket(&remote_peer, handshake.networks.clone());
        }

        if self.handler.peer_type() == PeerType::Bootstrapper {
            debug!("Running in bootstrapper mode; attempting to send a PeerList upon handshake");
            self.send_peer_list_resp(&handshake.networks)?;
        }

        Ok(())
    }

    fn handle_pong(&self) -> Fallible<()> {
        self.stats.valid_latency.store(true, Ordering::Relaxed);

        let ping_time: u64 = self.stats.last_ping_sent.load(Ordering::SeqCst);
        let curr_time: u64 = get_current_stamp();

        if curr_time >= ping_time {
            self.set_last_latency(curr_time - ping_time);
        }

        Ok(())
    }

    fn handle_get_peers_req(&self, networks: &HashSet<NetworkId>) -> Fallible<()> {
        let peer_id = self.remote_id().ok_or_else(|| format_err!("handshake not concluded yet"))?;

        debug!("Got a GetPeers request from peer {}", peer_id);

        self.send_peer_list_resp(networks)
    }

    fn handle_peer_list_resp(&self, peers: &[P2PPeer]) -> Fallible<()> {
        let peer_id = self.remote_id().ok_or_else(|| format_err!("handshake not concluded yet"))?;

        debug!("Received a PeerList response from peer {}", peer_id);

        let mut new_peers = 0;
        let current_peers = self.handler.get_peer_stats(Some(PeerType::Node));

        let curr_peer_count = current_peers.len();

        let applicable_candidates = peers.iter().filter(|candidate| {
            !current_peers
                .iter()
                .any(|peer| peer.id == candidate.id.as_raw() || peer.addr == candidate.addr)
        });

        for peer in applicable_candidates {
            trace!("Got info for peer {}/{}/{}", peer.id, peer.ip(), peer.port());
            if connect(&self.handler, PeerType::Node, peer.addr, Some(peer.id)).is_ok() {
                new_peers += 1;
                safe_write!(self.handler.connection_handler.buckets)?
                    .insert_into_bucket(peer, HashSet::new());
            }

            if new_peers + curr_peer_count >= self.handler.config.desired_nodes_count as usize {
                break;
            }
        }

        Ok(())
    }

    fn handle_join_network_req(&self, network: NetworkId) -> Fallible<()> {
        let remote_peer =
            self.remote_peer.peer().ok_or_else(|| format_err!("handshake not concluded yet"))?;

        debug!("Received a JoinNetwork request from peer {}", remote_peer.id);

        self.add_remote_end_network(network);
        safe_write!(self.handler.connection_handler.buckets)?
            .update_network_ids(&remote_peer, read_or_die!(self.remote_end_networks).to_owned());

        Ok(())
    }

    fn handle_leave_network_req(&self, network: NetworkId) -> Fallible<()> {
        let remote_peer =
            self.remote_peer.peer().ok_or_else(|| format_err!("handshake not concluded yet"))?;

        debug!("Received a LeaveNetwork request from peer {}", remote_peer.id);

        self.remove_remote_end_network(network);
        safe_write!(self.handler.connection_handler.buckets)?
            .update_network_ids(&remote_peer, read_or_die!(self.remote_end_networks).to_owned());

        Ok(())
    }

    fn handle_unban(&self, peer: BanId) -> Fallible<()> {
        let is_self_unban = match peer {
            BanId::NodeId(id) => Some(id) == self.remote_id(),
            BanId::Ip(addr) => addr == self.remote_addr().ip(),
            _ => unimplemented!("Socket address bans don't propagate"),
        };
        if is_self_unban {
            bail!("Rejecting a self-unban attempt");
        }

        self.handler.unban_node(peer)
    }

    fn handle_incoming_packet(&self, pac: NetworkPacket) -> Fallible<()> {
        let peer_id = self.remote_id().ok_or_else(|| format_err!("handshake not concluded yet"))?;

        trace!("Received a Packet from peer {}", peer_id);

        let is_broadcast = match pac.destination {
            PacketDestination::Broadcast(..) => true,
            _ => false,
        };

        let dont_relay_to = if let PacketDestination::Broadcast(ref peers) = pac.destination {
            let mut list = Vec::with_capacity(peers.len() + 1);
            list.extend_from_slice(peers);
            list.push(peer_id);
            list
        } else {
            vec![]
        };

        handle_pkt_out(&self.handler, dont_relay_to, peer_id, pac.message, is_broadcast)
    }
}
