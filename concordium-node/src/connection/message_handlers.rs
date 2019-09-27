use crate::{
    client::plugins::consensus::*,
    common::{get_current_stamp, P2PNodeId, P2PPeer, PeerType},
    connection::{Connection, MessageSendingPriority, P2PEvent},
    network::{
        request::RequestedElementType, NetworkId, NetworkMessage, NetworkPacket, NetworkPacketType,
        NetworkRequest, NetworkResponse,
    },
    p2p::banned_nodes::BannedNode,
};
use concordium_common::{read_or_die, serial::serialize_into_buffer, write_or_die, PacketType};

use failure::Fallible;

use std::{collections::HashSet, net::SocketAddr, sync::atomic::Ordering};

impl Connection {
    pub fn handle_incoming_message(&self, full_msg: &NetworkMessage) {
        if let Err(e) = match full_msg {
            NetworkMessage::NetworkRequest(
                NetworkRequest::Handshake(remote_node_id, remote_port, ref networks, _),
                ..
            ) => self.handle_handshake_req(*remote_node_id, *remote_port, networks),
            NetworkMessage::NetworkResponse(
                NetworkResponse::Handshake(remote_node_id, remote_port, ref nets, _),
                ..
            ) => self.handle_handshake_resp(*remote_node_id, *remote_port, nets),
            NetworkMessage::NetworkRequest(NetworkRequest::Ping, ..) => self.send_pong(),
            NetworkMessage::NetworkResponse(NetworkResponse::Pong, ..) => self.handle_pong(),
            NetworkMessage::NetworkRequest(NetworkRequest::FindNode(node), ..) => {
                self.handle_find_node_req(*node)
            }
            NetworkMessage::NetworkRequest(NetworkRequest::GetPeers(ref networks), ..) => {
                self.handle_get_peers_req(networks)
            }
            NetworkMessage::NetworkResponse(NetworkResponse::PeerList(ref peers), ..) => {
                self.handle_peer_list_resp(peers)
            }
            NetworkMessage::NetworkRequest(NetworkRequest::JoinNetwork(network), ..) => {
                self.handle_join_network_req(*network)
            }
            NetworkMessage::NetworkRequest(NetworkRequest::LeaveNetwork(network), ..) => {
                self.handle_leave_network_req(*network)
            }
            NetworkMessage::NetworkRequest(NetworkRequest::BanNode(peer_to_ban), ..) => {
                self.handler().ban_node(*peer_to_ban)
            }
            NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(peer_to_unban), ..) => {
                self.handle_unban(*peer_to_unban)
            }
            NetworkMessage::NetworkPacket(pac, ..) => self.handle_incoming_packet(pac),
            NetworkMessage::NetworkRequest(
                NetworkRequest::Retransmit(elem_type, since, nid),
                ..
            ) => self.handle_retransmit_req(*elem_type, *since, *nid),
            NetworkMessage::InvalidMessage => {
                self.handle_invalid_network_msg();
                Ok(())
            }
        } {
            error!("Couldn't handle the network message {:?}: {}", full_msg, e);
        }
    }

    fn handle_handshake_req(
        &self,
        remote_node_id: P2PNodeId,
        remote_port: u16,
        networks: &HashSet<NetworkId>,
    ) -> Fallible<()> {
        debug!("Got a Handshake request from peer {}", remote_node_id);

        if self.handler().is_banned(BannedNode::ById(remote_node_id))? {
            self.handler().remove_connection(self.token);
            bail!("Rejected a handshake request from a banned node");
        }

        self.promote_to_post_handshake(remote_node_id, remote_port)?;
        self.add_remote_end_networks(networks);

        let remote_peer = P2PPeer::from(
            self.remote_peer.peer_type(),
            remote_node_id,
            SocketAddr::new(self.remote_peer.addr().ip(), remote_port),
        );

        self.send_handshake_response(remote_node_id)?;
        self.send_ping()?;

        if remote_peer.peer_type() != PeerType::Bootstrapper {
            write_or_die!(self.handler().connection_handler.buckets)
                .insert_into_bucket(&remote_peer, networks.clone());
        }

        if self.handler().peer_type() == PeerType::Bootstrapper {
            debug!("Running in bootstrapper mode; attempting to send a PeerList upon handshake");
            self.send_peer_list_resp(networks)?;
        }

        Ok(())
    }

    fn handle_handshake_resp(
        &self,
        remote_node_id: P2PNodeId,
        remote_port: u16,
        networks: &HashSet<NetworkId>,
    ) -> Fallible<()> {
        debug!("Got a Handshake response from peer {}", remote_node_id);

        self.promote_to_post_handshake(remote_node_id, remote_port)?;
        self.add_remote_end_networks(networks);

        self.stats
            .sent_handshake
            .store(get_current_stamp(), Ordering::SeqCst);

        let remote_peer = P2PPeer::from(
            self.remote_peer.peer_type(),
            remote_node_id,
            SocketAddr::new(self.remote_peer.addr().ip(), remote_port),
        );

        if remote_peer.peer_type() != PeerType::Bootstrapper {
            write_or_die!(self.handler().connection_handler.buckets)
                .insert_into_bucket(&remote_peer, networks.clone());
        }

        if let Some(ref service) = self.handler().stats_export_service {
            service.peers_inc();
        };

        Ok(())
    }

    fn handle_pong(&self) -> Fallible<()> {
        let ping_time: u64 = self.stats.last_ping_sent.load(Ordering::SeqCst);
        let curr_time: u64 = get_current_stamp();

        if curr_time >= ping_time {
            let new_latency = curr_time - ping_time;
            let old_latency = self.get_last_latency();
            self.set_last_latency((new_latency + old_latency) / 2);
        }

        Ok(())
    }

    fn handle_find_node_req(&self, _target_node: P2PNodeId) -> Fallible<()> {
        debug!("Got a FindNode request");

        let find_node_msg = {
            let nodes = safe_read!(self.handler().connection_handler.buckets)?.buckets[0] // The Buckets object is never empty
                .clone()
                .into_iter()
                .map(|node| node.peer)
                .collect::<Vec<_>>();

            NetworkMessage::NetworkResponse(
                NetworkResponse::PeerList(nodes),
                Some(get_current_stamp()),
                None,
            )
        };

        self.async_send(
            serialize_into_buffer(&find_node_msg, 256)?,
            MessageSendingPriority::Normal,
        )
    }

    fn handle_get_peers_req(&self, networks: &HashSet<NetworkId>) -> Fallible<()> {
        let remote_peer = P2PPeer::from(
            self.remote_peer.peer_type(),
            self.remote_id().unwrap(), // safe, post-handshake
            self.remote_addr(),
        );

        debug!("Got a GetPeers request from peer {}", remote_peer.id);

        self.send_peer_list_resp(networks)
    }

    fn handle_peer_list_resp(&self, peers: &[P2PPeer]) -> Fallible<()> {
        let peer_id = self.remote_id().unwrap(); // safe, post-handshake

        debug!("Received a PeerList response from peer {}", peer_id);

        let mut new_peers = 0;
        let curr_peer_count = self
            .handler()
            .get_peer_stats()
            .iter()
            .filter(|peer| peer.peer_type == PeerType::Node)
            .count();

        let current_peers = self.handler().get_all_current_peers(Some(PeerType::Node));
        let applicable_candidates = peers
            .iter()
            .filter(|candidate| !current_peers.contains(&candidate.id));

        let mut locked_buckets = safe_write!(self.handler().connection_handler.buckets)?;
        for peer in applicable_candidates {
            trace!(
                "Got info for peer {}/{}/{}",
                peer.id(),
                peer.ip(),
                peer.port()
            );
            if self
                .handler()
                .connect(PeerType::Node, peer.addr, Some(peer.id()))
                .map_err(|e| trace!("{}", e))
                .is_ok()
            {
                new_peers += 1;
                locked_buckets.insert_into_bucket(peer, HashSet::new());
            }

            if new_peers + curr_peer_count >= self.handler().config.desired_nodes_count as usize {
                break;
            }
        }

        Ok(())
    }

    fn handle_join_network_req(&self, network: NetworkId) -> Fallible<()> {
        debug!("Received a JoinNetwork request");

        let remote_peer = self.remote_peer().peer().unwrap(); // safe, post-handshake

        self.add_remote_end_network(network);
        safe_write!(self.handler().connection_handler.buckets)?.update_network_ids(
            &remote_peer,
            read_or_die!(self.remote_end_networks).to_owned(),
        );

        if let Some(ref log) = self.handler().connection_handler.event_log {
            if log
                .send(P2PEvent::JoinedNetwork(remote_peer, network))
                .is_err()
            {
                error!("A JoinNetwork Event cannot be sent to the P2PEvent log");
            }
        }

        Ok(())
    }

    fn handle_leave_network_req(&self, network: NetworkId) -> Fallible<()> {
        debug!("Received a LeaveNetwork request");

        let remote_peer = self.remote_peer().peer().unwrap(); // safe, post-handshake

        self.remove_remote_end_network(network);
        safe_write!(self.handler().connection_handler.buckets)?.update_network_ids(
            &remote_peer,
            read_or_die!(self.remote_end_networks).to_owned(),
        );

        if let Some(ref log) = self.handler().connection_handler.event_log {
            if log
                .send(P2PEvent::LeftNetwork(remote_peer, network))
                .is_err()
            {
                error!("Left Network Event cannot be sent to the P2PEvent log");
            }
        };

        Ok(())
    }

    pub fn handle_retransmit_req(
        &self,
        element_type: RequestedElementType,
        since: u64,
        nid: NetworkId,
    ) -> Fallible<()> {
        debug!("Received a Retransmit request");

        let remote_peer = self.remote_peer().peer().unwrap(); // safe, post-handshake

        if let RequestedElementType::Transaction = element_type {
            read_or_die!(self.handler().transactions_cache)
                .get_since(since)
                .iter()
                .for_each(|transaction| {
                    if let Err(e) = send_consensus_msg_to_net(
                        self.handler(),
                        vec![],
                        remote_peer.id,
                        Some(remote_peer.id),
                        nid,
                        PacketType::Transaction,
                        Some(format!("{:?}", transaction)),
                        &transaction,
                    ) {
                        error!("Couldn't retransmit a transaction! ({:?})", e);
                    }
                })
        } else {
            bail!("Received a retransmit request for an unknown element type");
        }

        Ok(())
    }

    fn handle_unban(&self, peer: BannedNode) -> Fallible<()> {
        let is_self_unban = match peer {
            BannedNode::ById(id) => Some(id) == self.remote_id(),
            BannedNode::ByAddr(addr) => addr == self.remote_addr().ip(),
        };
        if is_self_unban {
            bail!("Rejecting a self-unban attempt");
        }

        self.handler().unban_node(peer)
    }

    pub fn handle_incoming_packet(&self, pac: &NetworkPacket) -> Fallible<()> {
        trace!("Received a Packet");

        let remote_peer = self.remote_peer().peer().unwrap(); // safe, post-handshake

        let is_broadcast = match pac.packet_type {
            NetworkPacketType::BroadcastedMessage(..) => true,
            _ => false,
        };

        #[cfg(feature = "benchmark")]
        {
            if !is_broadcast && self.handler().config.enable_tps_test {
                let mut stats_engine = write_or_die!(self.handler().stats_engine);
                if let Ok(len) = pac.message.len() {
                    stats_engine.add_stat(len);

                    if stats_engine.msg_count == self.handler().config.tps_message_count {
                        info!(
                            "TPS over {} messages is {}",
                            self.handler().config.tps_message_count,
                            stats_engine.calculate_total_tps_average()
                        );
                        stats_engine.clear();
                    }
                }
            }
        }

        let dont_relay_to =
            if let NetworkPacketType::BroadcastedMessage(ref peers) = pac.packet_type {
                let mut list = peers.clone().to_owned();
                list.push(remote_peer.id);
                list
            } else {
                vec![]
            };

        handle_pkt_out(
            self.handler(),
            dont_relay_to,
            remote_peer.id,
            pac.message.clone(),
            is_broadcast,
        )
    }

    fn handle_invalid_network_msg(&self) {
        debug!("Received an invalid network message!");

        self.stats.failed_pkts.fetch_add(1, Ordering::Relaxed);

        if let Some(ref service) = self.handler().stats_export_service {
            service.invalid_pkts_received_inc();
        }
    }
}
