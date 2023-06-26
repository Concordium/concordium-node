//! Consensus layer handling.
use anyhow::{bail, ensure};
use crossbeam_channel::TrySendError;

use crate::{
    common::{get_current_stamp, p2p_peer::RemotePeerId},
    configuration::{self, MAX_CATCH_UP_TIME},
    connection::ConnChange,
    consensus_ffi::{
        catch_up::{PeerList, PeerStatus},
        consensus::{ConsensusContainer, ConsensusRuntimeParameters, CALLBACK_QUEUE},
        ffi::{self, ExecuteBlockCallback, StartConsensusConfig},
        helpers::{
            ConsensusFfiResponse,
            PacketType::{self, *},
            QueueMsg,
        },
        messaging::{ConsensusMessage, DistributionMode, MessageType},
    },
    p2p::{
        connectivity::{send_broadcast_message, send_direct_message},
        P2PNode,
    },
    read_or_die, write_or_die,
};
use concordium_base::common::Deserial;

use std::{
    collections::hash_map::Entry::*,
    convert::TryFrom,
    io::{Cursor, Read},
    path::Path,
    sync::{atomic::Ordering, Arc},
};

/// Initializes the consensus layer with the given setup.
pub fn start_consensus_layer(
    conf: &configuration::BakerConfig,
    start_config: StartConsensusConfig,
    private_data: Option<Vec<u8>>,
    appdata_dir: &Path,
) -> anyhow::Result<ConsensusContainer> {
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

    let runtime_parameters = ConsensusRuntimeParameters {
        max_block_size:             u64::from(conf.maximum_block_size),
        block_construction_timeout: u64::from(conf.block_construction_timeout),
        insertions_before_purging:  u64::from(conf.transaction_insertions_before_purge),
        transaction_keep_alive:     u64::from(conf.transaction_keep_alive),
        transactions_purging_delay: u64::from(conf.transactions_purging_delay),
        accounts_cache_size:        conf.account_cache_size,
        modules_cache_size:         conf.modules_cache_size,
    };

    ConsensusContainer::new(runtime_parameters, start_config, private_data, appdata_dir)
}

/// Stop consensus container
pub fn stop_consensus_layer(container: ConsensusContainer) {
    container.stop();
    crate::consensus_ffi::ffi::stop_haskell();
}

/// Obtains the genesis data and baker's private data.
/// If the baker private data is encrypted this will query for the password.
pub fn get_baker_data(
    app_prefs: &configuration::AppPreferences,
    conf: &configuration::BakerConfig,
) -> anyhow::Result<(Vec<u8>, Option<Vec<u8>>)> {
    let mut genesis_loc = app_prefs.get_data_dir().to_path_buf();
    // if the genesis_data_file is absolute this replaces the entire path
    // otherwise the path is appended to the data directory
    genesis_loc.push(&conf.genesis_data_file);

    let genesis_data = match std::fs::File::open(&genesis_loc) {
        Ok(mut file) => {
            let mut read_data = vec![];
            match file.read_to_end(&mut read_data) {
                Ok(_) => read_data,
                Err(e) => bail!("Cannot not read genesis file ({})!", e),
            }
        }
        Err(e) => bail!("Cannot open the genesis file ({})", e),
    };

    let private_data = if let Some(path) = &conf.baker_credentials_file {
        let read_data = match std::fs::read(path) {
            Ok(read_data) => read_data,
            Err(e) => bail!("Cannot open the baker credentials file ({})!", e),
        };
        if conf.decrypt_baker_credentials {
            let et = serde_json::from_slice(&read_data)?;
            let pass = rpassword::read_password_from_tty(Some(
                "Enter password to decrypt baker credentials: ",
            ))?;
            match concordium_base::common::encryption::decrypt(&pass.into(), &et) {
                Ok(d) => Some(d),
                Err(_) => bail!(
                    "Could not decrypt baker credentials. Most likely the password you provided \
                     is incorrect."
                ),
            }
        } else {
            Some(read_data)
        }
    } else {
        None
    };

    Ok((genesis_data, private_data))
}

/// Handles packets coming from other peers.
pub fn handle_pkt_out(
    node: &P2PNode,
    dont_relay_to: Vec<RemotePeerId>,
    peer_id: RemotePeerId, // id of the peer that sent the message.
    msg: Vec<u8>,
    is_broadcast: bool,
) -> anyhow::Result<()> {
    ensure!(!msg.is_empty(), "Packet payload can't be empty");
    let consensus_type = u8::deserial(&mut Cursor::new(&msg[..1]))?;
    let packet_type = PacketType::try_from(consensus_type)?;

    let distribution_mode = if is_broadcast {
        DistributionMode::Broadcast
    } else {
        DistributionMode::Direct
    };
    // length of the actual payload. The message has a 1-byte tag prepended to it.
    let payload_len = msg[1..].len();

    let request = ConsensusMessage::new(
        MessageType::Inbound(peer_id, distribution_mode),
        packet_type,
        Arc::from(msg),
        dont_relay_to,
        None,
    );

    if packet_type == PacketType::Transaction {
        if payload_len > configuration::PROTOCOL_MAX_TRANSACTION_SIZE {
            bail!(
                "Transaction size exceeds {} bytes.",
                configuration::PROTOCOL_MAX_TRANSACTION_SIZE
            )
        }
        if let Err(e) = CALLBACK_QUEUE.send_in_low_priority_message(request) {
            match e.downcast::<TrySendError<QueueMsg<ConsensusMessage>>>()? {
                TrySendError::Full(_) => {
                    node.stats
                        .received_consensus_messages
                        .with_label_values(&[packet_type.label(), "dropped"])
                        .inc();
                    node.bad_events.inc_dropped_low_queue(peer_id);
                }
                TrySendError::Disconnected(_) => {
                    panic!("Low priority consensus queue has been shutdown!")
                }
            }
        }
    } else {
        // high priority message
        if let Err(e) = CALLBACK_QUEUE.send_in_high_priority_message(request) {
            match e.downcast::<TrySendError<QueueMsg<ConsensusMessage>>>()? {
                TrySendError::Full(_) => {
                    node.stats
                        .received_consensus_messages
                        .with_label_values(&[packet_type.label(), "dropped"])
                        .inc();

                    node.bad_events.inc_dropped_high_queue(peer_id);
                }
                TrySendError::Disconnected(_) => {
                    panic!("High priority consensus queue has been shutdown!")
                }
            }
        }
    }

    Ok(())
}

/// Routes a self-made consensus message to the right peers.
pub fn handle_consensus_outbound_msg(
    node: &P2PNode,
    message: ConsensusMessage,
) -> anyhow::Result<()> {
    node.stats.sent_consensus_messages.with_label_values(&[message.variant.label()]).inc();

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
                Some(peer),
                (message.payload.clone(), message.variant),
            );
        }
    } else {
        send_consensus_msg_to_net(
            node,
            message.dont_relay_to(),
            message.target_peer(),
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
) -> anyhow::Result<()> {
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

    let source = request.source_peer();
    // relay external messages to Consensus
    let (consensus_result, finalizer) = send_msg_to_consensus(node, source, consensus, &request)?;
    // adjust the peer state(s) based on the feedback from Consensus
    update_peer_states(node, &request, consensus_result);

    // Update metric tracking received messages.
    node.stats
        .received_consensus_messages
        .with_label_values(&[request.variant.label(), consensus_result.label()])
        .inc();

    // rebroadcast incoming broadcasts if applicable
    if !drop_message
        && request.distribution_mode() == DistributionMode::Broadcast
        && request.variant.is_rebroadcastable()
        && consensus_result.is_rebroadcastable(request.variant)
    {
        send_consensus_msg_to_net(
            node,
            request.dont_relay_to(),
            None,
            (request.payload, request.variant),
        );
    }
    // Execute any finalizer returned from the consensus layer.
    if let Some(callback) = finalizer {
        // Execute the block in the finalizer.
        // There is nothing left to do afterwards.
        let _ = consensus.execute_block(callback);
    } // Else there is nothing to do, the block is processed.

    Ok(())
}

fn send_msg_to_consensus(
    node: &P2PNode,
    source_id: RemotePeerId,
    consensus: &ConsensusContainer,
    message: &ConsensusMessage,
) -> anyhow::Result<(ConsensusFfiResponse, Option<ExecuteBlockCallback>)> {
    let payload = &message.payload[1..]; // non-empty, already checked

    let consensus_response = match message.variant {
        Block => {
            let genesis_index = u32::deserial(&mut Cursor::new(&payload[..4]))?;
            consensus.receive_block(genesis_index, &payload[4..])
        }
        FinalizationMessage => {
            let genesis_index = u32::deserial(&mut Cursor::new(&payload[..4]))?;
            (consensus.send_finalization(genesis_index, &payload[4..]), Option::None)
        }
        FinalizationRecord => {
            let genesis_index = u32::deserial(&mut Cursor::new(&payload[..4]))?;
            (consensus.send_finalization_record(genesis_index, &payload[4..]), Option::None)
        }
        CatchUpStatus => {
            let genesis_index = u32::deserial(&mut Cursor::new(&payload[..4]))?;
            (
                consensus.receive_catch_up_status(
                    genesis_index,
                    &payload[4..],
                    source_id,
                    node.config.catch_up_batch_limit,
                ),
                Option::None,
            )
        }
        Transaction => (consensus.send_transaction(payload).1, Option::None),
    };

    if consensus_response.0.is_acceptable() {
        debug!("Processed a {} from {}", message.variant, source_id);
    } else {
        let num_bad_events = node.bad_events.inc_invalid_messages(source_id);
        // we do log some invalid messages to both ease debugging and see problems in
        // normal circumstances
        if num_bad_events < 10 {
            warn!("Couldn't process a {} due to error code {:?}", message, consensus_response.0);
        }
    }

    Ok(consensus_response)
}

fn send_consensus_msg_to_net(
    node: &P2PNode,
    dont_relay_to: Vec<RemotePeerId>,
    target_id: Option<RemotePeerId>,
    (payload, msg_desc): (Arc<[u8]>, PacketType),
) {
    let sent = if let Some(target_id) = target_id {
        send_direct_message(node, target_id, node.config.default_network, payload)
    } else {
        send_broadcast_message(
            node,
            dont_relay_to.into_iter().collect(),
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
        debug!("Sent a {} containing a {}", target_desc, msg_desc);
    }
}

/// Updates the peer list upon changes to the list of peer nodes.
pub fn update_peer_list(node: &P2PNode) {
    trace!("The peers have changed; updating the catch-up peer list");

    let peer_ids = node.get_node_peer_tokens();

    let mut peers = write_or_die!(node.peers);
    // remove global state peers whose connections were dropped
    peers.peer_states.retain(|id, _| peer_ids.contains(id));
    peers.pending_queue.retain(|id| peer_ids.contains(id));
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

/// Try to catch up with a peer, if one is pending.
fn try_catch_up(node: &P2PNode, consensus: &ConsensusContainer, peers: &mut PeerList) {
    if let Some(id) = peers.next_pending() {
        debug!("Attempting to catch up with peer {}", id);
        peers.catch_up_stamp = get_current_stamp();
        let sent = send_direct_message(
            node,
            id,
            node.config.default_network,
            consensus.get_catch_up_status(),
        );
        if sent > 0 {
            info!(
                "Sent a direct message to peer {} containing a {}",
                id,
                PacketType::CatchUpStatus
            );
        } else {
            // If no packets were sent, then this must not be a valid peer,
            // so remove it from the peers.
            debug!("Could not send catch-up message to peer {}", id);
            peers.catch_up_peer = None;
            peers.peer_states.remove(&id);
        }
    }
}

/// Check whether the peers require catching up.
pub fn check_peer_states(node: &P2PNode, consensus: &ConsensusContainer) {
    // If we have a new genesis block, then mark all peers as pending.
    if node
        .config
        .regenesis_arc
        .trigger_catchup
        .compare_exchange(true, false, Ordering::SeqCst, Ordering::Acquire)
        .is_ok()
    {
        debug!("Regenesis occurred; marking all peers as pending.");
        write_or_die!(node.peers).mark_all_pending();
    }

    // If we are catching-up with a peer, check if the peer has timed-out.
    let now = get_current_stamp();
    let (catch_up_peer, catch_up_stamp) = {
        let peers = read_or_die!(node.peers);
        (peers.catch_up_peer, peers.catch_up_stamp)
    };
    if let Some(peer_id) = catch_up_peer {
        if read_or_die!(node.connections()).get(&peer_id.to_token()).is_some() {
            if now > catch_up_stamp + MAX_CATCH_UP_TIME {
                // Try to remove the peer since it timed-out.
                debug!("Peer {} took too long to catch up; dropping", peer_id);
                // This function may not actually remove the peer, so we do not assume
                // that it will be removed.
                node.register_conn_change(ConnChange::RemovalByToken(peer_id.to_token()));
            }
        } else {
            // Connection no longer exists
            debug!("Connection to catch-up-in-progress peer {} no longer exists", peer_id);
            let peers = &mut write_or_die!(node.peers);
            peers.catch_up_peer = None;
            peers.peer_states.remove(&peer_id);
            try_catch_up(node, consensus, peers);
        }
    } else {
        try_catch_up(node, consensus, &mut write_or_die!(node.peers));
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
            ConsensusFfiResponse::InvalidResult => {
                // Remove the peer since it is incompatible with us.
                debug!(
                    "Catching up with peer {} resulted in incompatible globalstates, dropping and \
                     soft-banning",
                    source_peer
                );
                node.register_conn_change(ConnChange::ExpulsionByToken(source_peer.to_token()));
            }
            ConsensusFfiResponse::DeserializationError => {
                debug!(
                    "The peer {} sent a malformed catchup message, dropping and soft-banning",
                    source_peer
                );
                node.register_conn_change(ConnChange::ExpulsionByToken(source_peer.to_token()));
            }
            e => error!("Unexpected return from `receiveCatchUpStatus`: {:?}", e),
        }
    } else if [Block, FinalizationRecord, FinalizationMessage].contains(&request.variant) {
        match request.distribution_mode() {
            DistributionMode::Direct if consensus_result.is_successful() => {
                // Directly sent blocks, finalization records and finalization messages that are
                // successful (i.e. new and not pending) have special
                // handling for the purposes of catch-up.

                // Previously, this marked the UpToDate peers as pending.
                // That should not be necessary if we simply relay the
                // messages to them.

                // relay rebroadcastable direct messages to non-pending peers, except originator
                for non_pending_peer in peers
                    .peer_states
                    .iter()
                    .filter(|(&id, &state)| id != source_peer && state != Pending)
                    .map(|(&id, _)| id)
                {
                    send_consensus_msg_to_net(
                        node,
                        Vec::new(),
                        Some(non_pending_peer),
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
