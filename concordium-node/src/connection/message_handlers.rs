//! Incoming network message handing.

use crate::{
    common::{p2p_peer::PeerStats, P2PNodeId, PeerType},
    configuration::{is_compatible_version, MAX_PEER_NETWORKS},
    connection::{ConnChange, Connection},
    network::{
        Handshake, NetworkMessage, NetworkPacket, NetworkPayload, NetworkRequest, NetworkResponse,
        PacketDestination,
    },
    p2p::bans::BanId,
    plugins::consensus::*,
};

use failure::Fallible;

impl Connection {
    /// Processes a network message based on its type.
    pub fn handle_incoming_message(
        &mut self,
        msg: NetworkMessage,
        conn_stats: &[PeerStats],
    ) -> Fallible<()> {
        // the handshake should be the first incoming network message
        let peer_id = match msg.payload {
            NetworkPayload::NetworkRequest(NetworkRequest::Handshake(handshake), ..) => {
                return self.handle_handshake_req(handshake, conn_stats);
            }
            _ => self.remote_id().ok_or_else(|| format_err!("handshake not concluded yet"))?,
        };

        match msg.payload {
            NetworkPayload::NetworkRequest(NetworkRequest::Handshake(_), ..) => {
                // already handled at the beginning
                Ok(())
            }
            NetworkPayload::NetworkRequest(NetworkRequest::Ping, ..) => {
                trace!("Got a Ping from peer {}", peer_id);
                self.send_pong()
            }
            NetworkPayload::NetworkResponse(NetworkResponse::Pong, ..) => {
                trace!("Got a Pong from peer {}", peer_id);
                self.handle_pong()
            }
            NetworkPayload::NetworkRequest(NetworkRequest::GetPeers(networks), ..) => {
                debug!("Got a GetPeers request from peer {}", peer_id);
                self.send_peer_list_resp(networks, conn_stats)
            }
            NetworkPayload::NetworkResponse(NetworkResponse::PeerList(peers), ..) => {
                debug!("Got a PeerList ({} peers) from peer {}", peers.len(), peer_id);
                self.handler.register_conn_change(ConnChange::NewPeers(peers));
                Ok(())
            }
            NetworkPayload::NetworkRequest(NetworkRequest::JoinNetwork(network), ..) => {
                debug!("Got a JoinNetwork request from peer {}", peer_id);
                self.add_remote_end_network(network)
            }
            NetworkPayload::NetworkRequest(NetworkRequest::LeaveNetwork(network), ..) => {
                debug!("Got a LeaveNetwork request from peer {}", peer_id);
                self.remove_remote_end_network(network)
            }
            NetworkPayload::NetworkRequest(NetworkRequest::BanNode(peer_to_ban), ..) => {
                debug!("Got a Ban request from peer {}", peer_id);
                if self.handler.config.trust_bans {
                    self.handle_ban(peer_to_ban)
                } else {
                    warn!("Ignoring a ban request from peer since `--trust-bans` is not given.");
                    Ok(())
                }
            }
            NetworkPayload::NetworkRequest(NetworkRequest::UnbanNode(peer_to_unban), ..) => {
                debug!("Got an Unban request from peer {}", peer_id);
                if self.handler.config.trust_bans {
                    self.handle_unban(peer_to_unban)
                } else {
                    warn!("Ignoring an unban request from peer since `--trust-bans` is not given.");
                    Ok(())
                }
            }
            NetworkPayload::NetworkPacket(pac, ..) => {
                // packet receipt is logged later, along with its contents
                self.handle_incoming_packet(pac, peer_id)
            }
        }
    }

    fn handle_handshake_req(
        &mut self,
        handshake: Handshake,
        conn_stats: &[PeerStats],
    ) -> Fallible<()> {
        debug!("Got a Handshake request from peer {}", handshake.remote_id);

        if self.handler.is_banned(BanId::NodeId(handshake.remote_id))? {
            bail!("Rejecting handshake: banned node");
        }
        if !is_compatible_version(&handshake.version) {
            bail!("Rejecting handshake: incompatible client");
        }
        if handshake.networks.len() > MAX_PEER_NETWORKS {
            bail!("Rejecting handshake: too many networks");
        }

        self.promote_to_post_handshake(
            handshake.remote_id,
            handshake.remote_port,
            &handshake.networks,
        );

        if self.handler.peer_type() == PeerType::Bootstrapper {
            debug!("Running in bootstrapper mode; attempting to send a PeerList upon handshake");
            self.send_peer_list_resp(handshake.networks, conn_stats)?;
        }

        Ok(())
    }

    fn handle_pong(&self) -> Fallible<()> { self.stats.notify_pong() }

    fn handle_ban(&self, peer_to_ban: BanId) -> Fallible<()> { self.handler.ban_node(peer_to_ban) }

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

    fn handle_incoming_packet(&self, pac: NetworkPacket, peer_id: P2PNodeId) -> Fallible<()> {
        let is_broadcast = match pac.destination {
            PacketDestination::Broadcast(..) => true,
            _ => false,
        };

        // Ignore the deserialized p2p node ids to be excluded from the wire.
        handle_pkt_out(&self.handler, vec![peer_id], peer_id, pac.message, is_broadcast)
    }
}
