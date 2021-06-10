//! Incoming network message handing.

use crate::{
    common::{
        p2p_peer::{PeerStats, RemotePeerId},
        PeerType,
    },
    configuration::{is_compatible_version, is_compatible_wire_version, MAX_PEER_NETWORKS},
    connection::{ConnChange, Connection},
    network::{
        Handshake, NetworkMessage, NetworkPacket, NetworkPayload, NetworkRequest, NetworkResponse,
        PacketDestination,
    },
    plugins::consensus::*,
    read_or_die,
};
use anyhow::{bail, ensure};

impl Connection {
    /// Processes a network message based on its type.
    pub fn handle_incoming_message(
        &mut self,
        msg: NetworkMessage,
        conn_stats: &[PeerStats],
    ) -> anyhow::Result<()> {
        // the handshake should be the first incoming network message
        let peer_id = match msg.payload {
            NetworkPayload::NetworkRequest(NetworkRequest::Handshake(handshake), ..) => {
                return self.handle_handshake_req(handshake, conn_stats);
            }
            _ => {
                ensure!(
                    self.is_post_handshake(),
                    "Connection to {} has not yet completed the handshake.",
                    self.remote_peer.local_id
                );
                self.remote_peer.local_id
            }
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
    ) -> anyhow::Result<()> {
        debug!("Got a Handshake request from peer {}", handshake.remote_id);

        if !is_compatible_version(&handshake.node_version) {
            bail!("Rejecting handshake: incompatible client ({}).", handshake.node_version);
        }
        if handshake.wire_versions.is_empty() {
            bail!("Rejecting handshake: Handshake message lacked wire versions.");
        }
        if is_compatible_wire_version(&handshake.wire_versions).is_none() {
            if handshake.wire_versions.len() > 10 {
                bail!("Rejecting handshake: incompatible wire protocol versions received.",);
            } else {
                bail!(
                    "Rejecting handshake: incompatible wire protocol versions ({:?}).",
                    handshake.wire_versions
                );
            }
        }
        if handshake.networks.len() > MAX_PEER_NETWORKS {
            bail!("Rejecting handshake: too many networks.");
        }

        {
            let our_blocks = read_or_die!(self.handler.config.regenesis_arc);
            // we will consider that the list of regenesis blocks is sorted
            // by height, so we check sequentially.
            let common_blocks = our_blocks
                .iter()
                .zip(handshake.genesis_blocks.iter())
                .enumerate()
                .find(|(_, (a, b))| a != b);
            if let Some((i, (ours, theirs))) = common_blocks {
                bail!(
                    "Rejecting handshake: Didn't find a common prefix on the genesis block \
                     hashes. Difference: our block: {}, their block {} at position {}.",
                    ours,
                    theirs,
                    i
                );
            }
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

    /// Check whether the connection has completed the handshake.
    pub(crate) fn is_post_handshake(&self) -> bool { self.remote_peer.self_id.is_some() }

    fn handle_pong(&self) -> anyhow::Result<()> { self.stats.notify_pong() }

    fn handle_incoming_packet(
        &self,
        pac: NetworkPacket,
        peer_id: RemotePeerId,
    ) -> anyhow::Result<()> {
        let is_broadcast = match pac.destination {
            PacketDestination::Broadcast(..) => true,
            _ => false,
        };

        // Ignore the deserialized p2p node ids to be excluded from the wire.
        handle_pkt_out(&self.handler, vec![peer_id], peer_id, pac.message, is_broadcast)
    }
}
