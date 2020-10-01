//! Consensus layer handling.
use crossbeam_channel::TrySendError;
use failure::Fallible;

use crate::{
    common::{get_current_stamp, P2PNodeId},
    configuration::{self, MAX_CATCH_UP_TIME},
    connection::ConnChange,
    find_conn_by_id,
    p2p::{
        connectivity::{send_broadcast_message, send_direct_message},
        P2PNode,
    },
};
use concordium_common::{
    ConsensusFfiResponse,
    PacketType::{self, *},
    QueueMsg,
};
use consensus_rust::{
    catch_up::PeerStatus,
    consensus::{self, ConsensusContainer, CALLBACK_QUEUE},
    ffi,
    messaging::{ConsensusMessage, DistributionMode, MessageType},
};
use crypto_common::Deserial;

use std::{
    collections::hash_map::Entry::*,
    convert::TryFrom,
    fs::OpenOptions,
    io::{Cursor, Read},
    path::PathBuf,
    sync::Arc,
};

const FILE_NAME_GENESIS_DATA: &str = "genesis.dat";
const FILE_NAME_PREFIX_BAKER_PRIVATE: &str = "baker-";
const FILE_NAME_SUFFIX_BAKER_PRIVATE: &str = "-credentials.json";

/// Initializes the consensus layer with the given setup.
pub fn start_consensus_layer(
    conf: &configuration::BakerConfig,
    genesis_data: Vec<u8>,
    private_data: Option<Vec<u8>>,
    max_logging_level: consensus::ConsensusLogLevel,
    appdata_dir: &PathBuf,
    database_connection_url: &str,
) -> Fallible<ConsensusContainer> {
    info!("Starting up the consensus thread");

    #[cfg(feature = "profiling")]
    ffi::start_haskell(
        &conf.heap_profiling,
        conf.stack_profiling,
        conf.time_profiling,
        conf.backtraces_profiling,
        conf.gc_logging.clone(),
        &conf.profiling_sampling_interval,
        &conf.rts_flags,
    );
    #[cfg(not(feature = "profiling"))]
    ffi::start_haskell(&conf.rts_flags);

    ConsensusContainer::new(
        u64::from(conf.maximum_block_size),
        u64::from(conf.transaction_insertions_before_purge),
        u64::from(conf.transaction_keep_alive),
        u64::from(conf.transactions_purging_delay),
        genesis_data,
        private_data,
        conf.baker_id,
        max_logging_level,
        appdata_dir,
        database_connection_url,
    )
}

/// Stop consensus container
pub fn stop_consensus_layer(container: ConsensusContainer) {
    container.stop();
    consensus_rust::ffi::stop_haskell();
}

/// Obtains the genesis data and baker's private data.
pub fn get_baker_data(
    app_prefs: &configuration::AppPreferences,
    conf: &configuration::BakerConfig,
    needs_private: bool,
) -> Fallible<(Vec<u8>, Option<Vec<u8>>)> {
    let mut genesis_loc = app_prefs.get_user_app_dir();
    genesis_loc.push(FILE_NAME_GENESIS_DATA);

    let credentials_loc = if let Some(path) = &conf.baker_credentials_file {
        std::path::PathBuf::from(path)
    } else {
        let mut private_loc = app_prefs.get_user_app_dir();
        if let Some(baker_id) = conf.baker_id {
            private_loc.push(format!(
                "{}{}{}",
                FILE_NAME_PREFIX_BAKER_PRIVATE, baker_id, FILE_NAME_SUFFIX_BAKER_PRIVATE
            ));
        }
        private_loc
    };

    let genesis_data = match OpenOptions::new().read(true).open(&genesis_loc) {
        Ok(mut file) => {
            let mut read_data = vec![];
            match file.read_to_end(&mut read_data) {
                Ok(_) => read_data,
                Err(_) => bail!("Couldn't read genesis file properly"),
            }
        }
        Err(e) => bail!("Can't open the genesis file ({})!", e),
    };

    let private_data = if needs_private {
        match OpenOptions::new().read(true).open(&credentials_loc) {
            Ok(mut file) => {
                let mut read_data = vec![];
                match file.read_to_end(&mut read_data) {
                    Ok(_) => Some(read_data),
                    Err(_) => bail!("Couldn't open up private baker file for reading"),
                }
            }
            Err(e) => bail!("Can't open the private data file ({})!", e),
        }
    } else {
        None
    };

    Ok((genesis_data, private_data))
}

/// Handles packets coming from other peers.
pub fn handle_pkt_out(
    node: &P2PNode,
    dont_relay_to: Vec<P2PNodeId>,
    peer_id: P2PNodeId,
    msg: Vec<u8>,
    is_broadcast: bool,
) -> Fallible<()> {
    ensure!(!msg.is_empty(), "Packet payload can't be empty");
    let consensus_type = u8::deserial(&mut Cursor::new(&msg[..1]))?;
    let packet_type = PacketType::try_from(consensus_type)?;

    let distribution_mode = if is_broadcast {
        DistributionMode::Broadcast
    } else {
        DistributionMode::Direct
    };

    let request = ConsensusMessage::new(
        MessageType::Inbound(peer_id.0, distribution_mode),
        packet_type,
        Arc::from(msg),
        dont_relay_to.into_iter().map(P2PNodeId::as_raw).collect(),
        None,
    );

    match if packet_type == PacketType::Transaction {
        CALLBACK_QUEUE.send_in_low_priority_message(request)
    } else {
        CALLBACK_QUEUE.send_in_high_priority_message(request)
    } {
        Ok(_) => {
            if packet_type == PacketType::Transaction {
                node.stats.inbound_low_priority_consensus_inc();
            } else {
                node.stats.inbound_high_priority_consensus_inc();
            }
        }
        Err(e) => match e.downcast::<TrySendError<QueueMsg<ConsensusMessage>>>()? {
            TrySendError::Full(_) => {
                if packet_type == PacketType::Transaction {
                    node.stats.inbound_low_priority_consensus_drops_inc();
                    warn!("The low priority inbound consensus queue is full!")
                } else {
                    node.stats.inbound_high_priority_consensus_drops_inc();
                    warn!("The high priority inbound consensus queue is full!")
                }
            }
            TrySendError::Disconnected(_) => {
                panic!("One of the inbound consensus queues has been shutdown!")
            }
        },
    }

    Ok(())
}

/// Routes a self-made consensus message to the right peers.
pub fn handle_consensus_outbound_msg(node: &P2PNode, message: ConsensusMessage) -> Fallible<()> {
    if let Some(status) = message.omit_status {
        for peer in read_or_die!(node.peers)
            .peer_states
            .iter()
            .filter(|(_, &state)| state != status)
            .map(|(&id, _)| id)
        {
            send_consensus_msg_to_net(
                node,
                Vec::new(),
                Some(P2PNodeId(peer)),
                (message.payload.clone(), message.variant),
            );
        }
    } else {
        send_consensus_msg_to_net(
            node,
            message.dont_relay_to(),
            message.target_peer().map(P2PNodeId),
            (message.payload, message.variant),
        );
    }
    Ok(())
}

/// Processes a consensus message from the network.
pub fn handle_consensus_inbound_msg(
    node: &P2PNode,
    consensus: &ConsensusContainer,
    request: ConsensusMessage,
) -> Fallible<()> {
    // If the drop_rebroadcast_probability parameter is set, do not
    // rebroadcast the packet to the network with the given chance.
    let drop_message = match node.config.drop_rebroadcast_probability {
        Some(probability) => {
            use rand::distributions::{Bernoulli, Distribution};
            if Bernoulli::new(probability)?.sample(&mut rand::thread_rng()) {
                trace!("Will not rebroadcast this packet");
                true
            } else {
                false
            }
        }
        _ => false,
    };

    let source = P2PNodeId(request.source_peer());

    if node.config.no_rebroadcast_consensus_validation {
        if !drop_message && request.distribution_mode() == DistributionMode::Broadcast {
            send_consensus_msg_to_net(
                &node,
                request.dont_relay_to(),
                None,
                (request.payload.clone(), request.variant),
            );
        }

        // relay external messages to Consensus
        let consensus_result = send_msg_to_consensus(node, source, consensus, &request)?;

        // early blocks should be removed from the deduplication queue
        if consensus_result == ConsensusFfiResponse::BlockTooEarly {
            write_or_die!(&node.connection_handler.deduplication_queues.blocks)
                .invalidate_if_exists(&request.payload);
        }

        // adjust the peer state(s) based on the feedback from Consensus
        update_peer_states(node, &request, consensus_result);
    } else {
        // relay external messages to Consensus
        let consensus_result = send_msg_to_consensus(node, source, consensus, &request)?;

        // adjust the peer state(s) based on the feedback from Consensus
        update_peer_states(node, &request, consensus_result);

        // rebroadcast incoming broadcasts if applicable
        if !drop_message
            && request.distribution_mode() == DistributionMode::Broadcast
            && consensus_result.is_rebroadcastable()
        {
            send_consensus_msg_to_net(
                &node,
                request.dont_relay_to(),
                None,
                (request.payload, request.variant),
            );
        }
    }

    Ok(())
}

fn send_msg_to_consensus(
    node: &P2PNode,
    source_id: P2PNodeId,
    consensus: &ConsensusContainer,
    message: &ConsensusMessage,
) -> Fallible<ConsensusFfiResponse> {
    let payload = &message.payload[1..]; // non-empty, already checked
    let raw_id = source_id.as_raw();

    let consensus_response = match message.variant {
        Block => consensus.send_block(payload),
        Transaction => consensus.send_transaction(payload),
        FinalizationMessage => consensus.send_finalization(payload),
        FinalizationRecord => consensus.send_finalization_record(payload),
        CatchUpStatus => {
            consensus.receive_catch_up_status(payload, raw_id, node.config.catch_up_batch_limit)
        }
    };

    if consensus_response.is_acceptable() {
        info!("Processed a {} from {}", message.variant, source_id);
    } else {
        debug!("Couldn't process a {} due to error code {:?}", message, consensus_response,);
    }

    Ok(consensus_response)
}

fn send_consensus_msg_to_net(
    node: &P2PNode,
    dont_relay_to: Vec<u64>,
    target_id: Option<P2PNodeId>,
    (payload, msg_desc): (Arc<[u8]>, PacketType),
) {
    let sent = if let Some(target_id) = target_id {
        send_direct_message(node, target_id, node.config.default_network, payload)
    } else {
        send_broadcast_message(
            node,
            dont_relay_to.into_iter().map(P2PNodeId).collect(),
            node.config.default_network,
            payload,
        )
    };

    if sent > 0 {
        let target_desc = if let Some(id) = target_id {
            format!("direct message to peer {}", id)
        } else {
            "broadcast".to_string()
        };
        info!("Sent a {} containing a {}", target_desc, msg_desc);
    }
}

/// Updates the peer list upon changes to the list of peer nodes.
pub fn update_peer_list(node: &P2PNode) {
    trace!("The peers have changed; updating the catch-up peer list");

    let peer_ids = node.get_node_peer_ids();

    let mut peers = write_or_die!(node.peers);
    // remove global state peers whose connections were dropped
    peers.peer_states.retain(|id, _| peer_ids.contains(&id));
    peers.pending_queue.retain(|id| peer_ids.contains(&id));
    if let Some(in_progress) = peers.catch_up_peer {
        if !peers.peer_states.contains_key(&in_progress) {
            peers.catch_up_peer = None;
        }
    }

    // include newly added peers
    let new_peers = peer_ids.len() - peers.peer_states.len();
    peers.peer_states.reserve(new_peers);
    peers.pending_queue.reserve(new_peers);
    for id in peer_ids {
        if let Vacant(v) = peers.peer_states.entry(id) {
            v.insert(PeerStatus::Pending);
            peers.pending_queue.push_back(id);
        }
    }
}

/// Check whether the peers require catching up.
pub fn check_peer_states(node: &P2PNode, consensus: &ConsensusContainer) {
    // If we are catching-up with a peer, check if the peer has timed-out.
    if let Some(id) = read_or_die!(node.peers).catch_up_peer {
        if let Some(token) = find_conn_by_id!(node, P2PNodeId(id)).map(|conn| conn.token) {
            if get_current_stamp() > read_or_die!(node.peers).catch_up_stamp + MAX_CATCH_UP_TIME {
                // Try to remove the peer, since it timed-out.
                debug!("Peer {:016x} took too long to catch up; dropping", id);
                // This function may not actually remove the peer, so we do not assume
                // that it will be removed.
                node.register_conn_change(ConnChange::Removal(token));
            }
            return;
        } else {
            // Connection no longer exists
            debug!("Catch-up-in-progress peer {:016x} no longer exists", id);
            let peers = &mut write_or_die!(node.peers);
            peers.catch_up_peer = None;
            peers.peer_states.remove(&id);
        }
    }
    // We are not currently catching-up with a peer, so try to.
    let peers = &mut write_or_die!(node.peers);
    if let Some(id) = peers.next_pending() {
        debug!("Attempting to catch up with peer {:016x}", id);
        peers.catch_up_stamp = get_current_stamp();
        let sent = send_direct_message(
            node,
            P2PNodeId(id),
            node.config.default_network,
            consensus.get_catch_up_status(),
        );
        if sent > 0 {
            info!(
                "Sent a direct message to peer {} containing a {}",
                P2PNodeId(id),
                PacketType::CatchUpStatus
            );
        } else {
            // If no packets were sent, then this must not be a valid peer,
            // so remove it from the peers.
            debug!("Could not send catch-up message to peer {}", P2PNodeId(id));
            peers.catch_up_peer = None;
            peers.peer_states.remove(&id);
        }
    }
}

fn update_peer_states(
    node: &P2PNode,
    request: &ConsensusMessage,
    consensus_result: ConsensusFfiResponse,
) {
    use ConsensusFfiResponse::*;
    use PeerStatus::*;

    let source_peer = request.source_peer();
    let mut peers = write_or_die!(node.peers);
    if request.variant == CatchUpStatus {
        match consensus_result {
            Success => {
                // We are up-to-date with the peer.
                peers.peer_states.insert(source_peer, UpToDate);
                if peers.catch_up_peer == Some(source_peer) {
                    peers.catch_up_peer = None;
                }
            }
            PendingBlock | PendingFinalization => {
                // We are behind the peer.
                match peers.peer_states.insert(source_peer, Pending) {
                    Some(Pending) => {}
                    _ => peers.pending_queue.push_back(source_peer),
                }
                if peers.catch_up_peer == Some(source_peer) {
                    peers.catch_up_peer = None;
                }
            }
            ContinueCatchUp => {
                // This was not a response, and we're behind the peer, so
                // set it to Pending if it's currently UpToDate.
                if let Some(state) = peers.peer_states.get_mut(&source_peer) {
                    if let UpToDate = *state {
                        *state = Pending;
                        peers.pending_queue.push_back(source_peer);
                    }
                }
            }
            _ => {}
        }
    } else if [Block, FinalizationRecord].contains(&request.variant) {
        match request.distribution_mode() {
            DistributionMode::Direct if consensus_result.is_successful() => {
                // Directly sent blocks and finalization records that are
                // successful (i.e. new and not pending) have special
                // handling for the purposes of catch-up.

                // Previously, this marked the UpToDate peers as pending.
                // That should not be necessary if we simply relay the
                // messages to them.

                // relay rebroadcastable direct messages to non-pending peers, but originator
                for non_pending_peer in peers
                    .peer_states
                    .iter()
                    .filter(|(&id, &state)| id != source_peer && state != Pending)
                    .map(|(&id, _)| id)
                {
                    send_consensus_msg_to_net(
                        node,
                        Vec::new(),
                        Some(P2PNodeId(non_pending_peer)),
                        (request.payload.clone(), request.variant),
                    );
                }
            }
            DistributionMode::Broadcast if consensus_result.is_pending() => {
                // We are missing some context for this message, so mark
                // the peer as pending.
                if let Some(state) = peers.peer_states.get_mut(&source_peer) {
                    if let UpToDate = *state {
                        *state = Pending;
                        peers.pending_queue.push_back(source_peer);
                    }
                }
            }
            _ => {}
        }
    }
}
