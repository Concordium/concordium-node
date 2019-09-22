pub const PAYLOAD_TYPE_LENGTH: u64 = 2;
pub const FILE_NAME_GENESIS_DATA: &str = "genesis.dat";
pub const FILE_NAME_CRYPTO_PROV_DATA: &str = "crypto_providers.json";
pub const FILE_NAME_ID_PROV_DATA: &str = "identity_providers.json";
pub const FILE_NAME_PREFIX_BAKER_PRIVATE: &str = "baker-";
pub const FILE_NAME_SUFFIX_BAKER_PRIVATE: &str = ".dat";

use byteorder::{NetworkEndian, ReadBytesExt, WriteBytesExt};
use circular_queue::CircularQueue;
use digest::Digest;
use failure::Fallible;
use twox_hash::XxHash64;

use std::{
    convert::TryFrom,
    fs::OpenOptions,
    io::{self, Cursor, Read, Seek, SeekFrom, Write},
    mem,
    sync::Arc,
};

use concordium_common::{
    blockchain_types::TransactionHash,
    cache::Cache,
    hybrid_buf::HybridBuf,
    ConsensusFfiResponse,
    PacketType::{self, *},
};

use concordium_consensus::{consensus, ffi};

use concordium_global_state::{
    block::PendingBlock,
    common::{sha256, SerializeToBytes},
    finalization::FinalizationRecord,
    transaction::Transaction,
    tree::{
        messaging::{
            ConsensusMessage, DistributionMode, GlobalStateMessage, GlobalStateResult, MessageType,
        },
        GlobalState, PeerId, PeerState, PeerStatus,
    },
};

use crate::{
    common::P2PNodeId, configuration, network::NetworkId, p2p::p2p_node::*,
    utils::GlobalStateSenders,
};

pub fn start_consensus_layer(
    conf: &configuration::BakerConfig,
    app_prefs: &configuration::AppPreferences,
) -> Option<consensus::ConsensusContainer> {
    info!("Starting up the consensus thread");

    #[cfg(feature = "profiling")]
    ffi::start_haskell(
        &conf.heap_profiling,
        conf.time_profiling,
        conf.backtraces_profiling,
        conf.gc_logging.clone(),
    );
    #[cfg(not(feature = "profiling"))]
    ffi::start_haskell();

    match get_baker_data(app_prefs, conf, conf.baker_id.is_some()) {
        Ok((genesis_data, private_data)) => {
            let consensus = consensus::ConsensusContainer::new(
                u64::from(conf.maximum_block_size),
                conf.scheduler_outcome_logging,
                genesis_data,
                private_data,
                conf.baker_id,
            );
            Some(consensus)
        }
        Err(_) => {
            error!("Can't start the consensus layer!");
            None
        }
    }
}

fn get_baker_data(
    app_prefs: &configuration::AppPreferences,
    conf: &configuration::BakerConfig,
    needs_private: bool,
) -> Fallible<(Vec<u8>, Option<Vec<u8>>)> {
    let mut genesis_loc = app_prefs.get_user_app_dir();
    genesis_loc.push(FILE_NAME_GENESIS_DATA);

    let mut private_loc = app_prefs.get_user_app_dir();

    if let Some(baker_id) = conf.baker_id {
        private_loc.push(format!(
            "{}{}{}",
            FILE_NAME_PREFIX_BAKER_PRIVATE, baker_id, FILE_NAME_SUFFIX_BAKER_PRIVATE
        ))
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
        match OpenOptions::new().read(true).open(&private_loc) {
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

    debug!(
        "Obtained genesis data {:?}",
        sha256(&[&[0u8; 8], genesis_data.as_slice()].concat())
    );
    Ok((genesis_data, private_data))
}

/// Handles packets coming from other peers
pub fn handle_pkt_out(
    dont_relay_to: Vec<P2PNodeId>,
    peer_id: P2PNodeId,
    mut msg: HybridBuf,
    gs_senders: &GlobalStateSenders,
    transactions_cache: &mut Cache<Arc<[u8]>>,
    dedup_queue_finalization: &mut CircularQueue<[u8; 8]>,
    dedup_queue_transaction: &mut CircularQueue<[u8; 8]>,
    is_broadcast: bool,
) -> Fallible<()> {
    ensure!(
        msg.len()? >= msg.position()? + PAYLOAD_TYPE_LENGTH,
        "Message needs at least {} bytes",
        PAYLOAD_TYPE_LENGTH
    );

    let consensus_type = msg.read_u16::<NetworkEndian>()?;
    let packet_type = PacketType::try_from(consensus_type)?;

    // deduplicate finalization messages and transactions
    if packet_type == PacketType::FinalizationMessage {
        let mut hash = [0u8; 8];
        let msg_pos = msg.position()?;
        hash.copy_from_slice(&XxHash64::digest(&msg.remaining_bytes()?));
        msg.seek(SeekFrom::Start(msg_pos))?;

        if dedup_queue_finalization.iter().any(|h| h == &hash) {
            return Ok(());
        } else {
            dedup_queue_finalization.push(hash);
        }
    } else if packet_type == PacketType::Transaction {
        let mut hash = [0u8; 8];
        let msg_pos = msg.position()?;
        hash.copy_from_slice(&XxHash64::digest(&msg.remaining_bytes()?));
        msg.seek(SeekFrom::Start(msg_pos))?;

        if dedup_queue_transaction.iter().any(|h| h == &hash) {
            return Ok(());
        } else {
            dedup_queue_transaction.push(hash);
        }
    }

    let mut payload = Vec::with_capacity(msg.remaining_len()? as usize);
    io::copy(&mut msg, &mut payload)?;
    let payload: Arc<[u8]> = Arc::from(payload);
    let distribution_mode = if is_broadcast {
        DistributionMode::Broadcast
    } else {
        DistributionMode::Direct
    };

    if packet_type == PacketType::Transaction {
        let hash_offset = payload.len() - mem::size_of::<TransactionHash>();
        let hash = TransactionHash::new(&payload[hash_offset..]);
        transactions_cache.insert(hash, payload.clone());
    }

    let request = GlobalStateMessage::ConsensusMessage(ConsensusMessage::new(
        MessageType::Inbound(peer_id.0, distribution_mode),
        packet_type,
        payload,
        dont_relay_to.into_iter().map(P2PNodeId::as_raw).collect(),
    ));

    if is_broadcast {
        gs_senders.send(request)
    } else {
        gs_senders.send_with_priority(request)
    }
}

pub fn handle_global_state_request(
    node: &P2PNode,
    network_id: NetworkId,
    consensus: &mut consensus::ConsensusContainer,
    request: GlobalStateMessage,
    global_state: &mut GlobalState,
) -> Fallible<()> {
    match request {
        GlobalStateMessage::ConsensusMessage(req) => {
            handle_consensus_message(node, network_id, consensus, req, global_state)
        }
        GlobalStateMessage::PeerListUpdate(peer_ids) => {
            // check if there are any new peers or any of the current ones have died
            update_peer_list(global_state, peer_ids);

            // take advantage of the priority queue ordering
            if let Some((&id, state)) = global_state.peers.peek() {
                match state.status {
                    PeerStatus::UpToDate => {
                        // when all catch-up messages have been exchanged,
                        // baking may commence (does nothing if already baking)
                        trace!("Global state: all my peers are up to date");
                        consensus.start_baker();
                        global_state.catch_up_count = 0;
                    }
                    PeerStatus::CatchingUp => {
                        // don't send any catch-up statuses while
                        // there are peers that are catching up
                        if global_state.catch_up_count < 1 {
                            debug!("Global state: I'm catching up with peer {:016x}", id);
                            global_state.catch_up_count += 1;
                        } else {
                            warn!("Global state: peer {:016x} took to long to catch up", id);
                            if let Some(peer_conn) = node
                                .find_connection_by_id(P2PNodeId(id))
                                .map(|conn| conn.token)
                            {
                                node.remove_connection(peer_conn);
                            }
                            global_state.catch_up_count = 0;
                        }
                    }
                    PeerStatus::Pending => {
                        // send a catch-up message to the first Pending peer
                        debug!("Global state: I need to catch up with peer {:016x}", id);
                        send_catch_up_status(node, network_id, consensus, global_state, id)?;
                        global_state.catch_up_count = 0;
                    }
                }
            }

            Ok(())
        }
        _ => unreachable!("A Shutdown message is handled within the cli module"),
    }
}

pub fn handle_consensus_message(
    node: &P2PNode,
    network_id: NetworkId,
    consensus: &mut consensus::ConsensusContainer,
    request: ConsensusMessage,
    global_state: &mut GlobalState,
) -> Fallible<()> {
    if let MessageType::Outbound(_) = request.direction {
        process_internal_gs_entry(node, network_id, request, global_state)?
    } else {
        process_external_gs_entry(node, network_id, consensus, request, global_state)?
    }

    if let Some(ref stats) = node.stats_export_service {
        let stats_values = global_state.stats.query_stats();
        stats.set_gs_block_receipt(stats_values.0 as i64);
        stats.set_gs_block_entry(stats_values.1 as i64);
        stats.set_gs_block_query(stats_values.2 as i64);
        stats.set_gs_finalization_receipt(stats_values.3 as i64);
        stats.set_gs_finalization_entry(stats_values.4 as i64);
        stats.set_gs_finalization_query(stats_values.5 as i64);
    }

    Ok(())
}

fn update_peer_list(global_state: &mut GlobalState, peer_ids: Vec<u64>) {
    // remove global state peers whose connections were dropped
    if global_state.peers.len() > peer_ids.len() {
        let gs_peers = mem::replace(&mut global_state.peers, Default::default());

        global_state.peers.reserve(peer_ids.len());
        for (live_peer, state) in gs_peers
            .into_iter()
            .filter(|(id, _)| peer_ids.contains(&id))
        {
            global_state.peers.push(live_peer, state);
        }
    }

    // include newly added peers
    global_state.peers.reserve(peer_ids.len());
    for id in peer_ids {
        if global_state.peers.get(&id).is_none() {
            global_state
                .peers
                .push(id, PeerState::new(PeerStatus::Pending));
        }
    }
}

fn process_internal_gs_entry(
    node: &P2PNode,
    network_id: NetworkId,
    request: ConsensusMessage,
    global_state: &mut GlobalState,
) -> Fallible<()> {
    let (entry_info, gs_result) = match request.variant {
        PacketType::Block => {
            let block = PendingBlock::new(&request.payload)?;
            (format!("{:?}", block.block), global_state.add_block(block))
        }
        PacketType::FinalizationRecord => {
            let record = FinalizationRecord::deserialize(&request.payload)?;
            (
                format!("{:?}", record),
                global_state.add_finalization(record),
            )
        }
        PacketType::Transaction => {
            let transaction = Transaction::deserialize(&mut Cursor::new(&request.payload))?;
            (
                format!("{:?}", transaction.payload.transaction_type()),
                global_state.add_transaction(transaction, false),
            )
        }
        _ => (request.variant.to_string(), GlobalStateResult::IgnoredEntry),
    };

    match gs_result {
        GlobalStateResult::SuccessfulEntry(entry) => {
            trace!(
                "GlobalState: successfully processed a {} from our consensus layer",
                entry
            );
        }
        GlobalStateResult::IgnoredEntry => {
            trace!(
                "GlobalState: ignoring a {} from our consensus layer",
                request.variant
            );
        }
        GlobalStateResult::Error(e) => global_state.register_error(e),
        _ => {}
    }

    send_consensus_msg_to_net(
        node,
        request.dont_relay_to(),
        request.target_peer().map(P2PNodeId),
        network_id,
        request.variant,
        Some(entry_info),
        &request.payload,
    )
}

fn process_external_gs_entry(
    node: &P2PNode,
    network_id: NetworkId,
    consensus: &mut consensus::ConsensusContainer,
    request: ConsensusMessage,
    global_state: &mut GlobalState,
) -> Fallible<()> {
    let self_node_id = node.self_peer.id;
    let source = P2PNodeId(request.source_peer());

    let gs_result = match request.variant {
        PacketType::Block => {
            let block = PendingBlock::new(&request.payload)?;
            global_state.add_block(block)
        }
        PacketType::FinalizationRecord => {
            let record = FinalizationRecord::deserialize(&request.payload)?;
            global_state.add_finalization(record)
        }
        PacketType::Transaction => {
            let transaction = Transaction::deserialize(&mut Cursor::new(&request.payload))?;
            global_state.add_transaction(transaction, false)
        }
        _ => GlobalStateResult::IgnoredEntry,
    };

    // relay external messages to Consensus
    let consensus_result = send_msg_to_consensus(self_node_id, source, consensus, &request)?;

    // adjust the peer state(s) based on the feedback from Consensus
    manage_peer_states(global_state, &request, consensus_result);

    match gs_result {
        GlobalStateResult::SuccessfulEntry(_entry_type) => {
            trace!(
                "GlobalState: {} successfully processed a {}",
                node.self_peer.id,
                request
            );
        }
        GlobalStateResult::SuccessfulQuery(_result) => {}
        GlobalStateResult::DuplicateEntry => {
            debug!("GlobalState: got a duplicate {}", request);
            return Ok(());
        }
        GlobalStateResult::Error(err) => {
            global_state.register_error(err);
        }
        _ => {}
    }

    // rebroadcast incoming broadcasts if applicable
    if request.distribution_mode() == DistributionMode::Broadcast
        && consensus_result.is_rebroadcastable()
    {
        send_consensus_msg_to_net(
            &node,
            request.dont_relay_to(),
            None,
            network_id,
            request.variant,
            None,
            &request.payload,
        )?;
    }

    Ok(())
}

fn send_msg_to_consensus(
    our_id: P2PNodeId,
    source_id: P2PNodeId,
    consensus: &mut consensus::ConsensusContainer,
    request: &ConsensusMessage,
) -> Fallible<ConsensusFfiResponse> {
    let raw_id = source_id.as_raw();

    let consensus_response = match request.variant {
        Block => consensus.send_block(&request.payload),
        Transaction => consensus.send_transaction(&request.payload),
        FinalizationMessage => consensus.send_finalization(&request.payload),
        FinalizationRecord => consensus.send_finalization_record(&request.payload),
        CatchUpStatus => consensus.receive_catch_up_status(&request.payload, raw_id),
    };

    if consensus_response.is_acceptable() {
        info!("Peer {} processed a {}", our_id, request,);
    } else {
        error!(
            "Peer {} couldn't process a {} due to error code {:?}",
            our_id, request, consensus_response,
        );
    }

    Ok(consensus_response)
}

pub fn send_consensus_msg_to_net(
    node: &P2PNode,
    dont_relay_to: Vec<u64>,
    target_id: Option<P2PNodeId>,
    network_id: NetworkId,
    payload_type: PacketType,
    payload_desc: Option<String>,
    payload: &[u8],
) -> Fallible<()> {
    let self_node_id = node.self_peer.id;
    let mut packet_buffer = HybridBuf::with_capacity(PAYLOAD_TYPE_LENGTH as usize + payload.len())?;
    packet_buffer
        .write_u16::<NetworkEndian>(payload_type as u16)
        .expect("Can't write a packet payload to buffer");
    packet_buffer.write_all(payload)?;
    packet_buffer.rewind()?;

    let result = if target_id.is_some() {
        send_direct_message(node, target_id, network_id, None, packet_buffer)
    } else {
        send_broadcast_message(
            node,
            dont_relay_to.into_iter().map(P2PNodeId).collect(),
            network_id,
            None,
            packet_buffer,
        )
    };

    let target_desc = if let Some(id) = target_id {
        format!("direct message to peer {}", id)
    } else {
        "broadcast".to_string()
    };
    let message_desc = payload_desc.unwrap_or_else(|| payload_type.to_string());

    match result {
        Ok(_) => info!(
            "Peer {} sent a {} containing a {}",
            self_node_id, target_desc, message_desc,
        ),
        Err(_) => error!(
            "Peer {} couldn't send a {} containing a {}!",
            self_node_id, target_desc, message_desc,
        ),
    }
    Ok(())
}

fn send_catch_up_status(
    node: &P2PNode,
    network_id: NetworkId,
    consensus: &mut consensus::ConsensusContainer,
    global_state: &mut GlobalState,
    target: PeerId,
) -> Fallible<()> {
    global_state
        .peers
        .change_priority(&target, PeerState::new(PeerStatus::CatchingUp));

    send_consensus_msg_to_net(
        node,
        vec![],
        Some(P2PNodeId(target)),
        network_id,
        PacketType::CatchUpStatus,
        Some("catch-up status message".to_owned()),
        &consensus.get_catch_up_status(),
    )
}

fn manage_peer_states(
    global_state: &mut GlobalState,
    request: &ConsensusMessage,
    consensus_result: ConsensusFfiResponse,
) {
    use PeerStatus::*;

    let source_peer = request.source_peer();

    if request.variant == CatchUpStatus {
        if consensus_result.is_successful() {
            global_state
                .peers
                .push(source_peer, PeerState::new(UpToDate));
        } else if consensus_result.is_pending() {
            global_state
                .peers
                .push(source_peer, PeerState::new(Pending));
        }
    } else if [Block, FinalizationRecord].contains(&request.variant) {
        match request.distribution_mode() {
            DistributionMode::Direct if consensus_result.is_successful() => {
                let up_to_date_peers = global_state
                    .peers
                    .iter()
                    .filter(|(_, &state)| state.status == UpToDate)
                    .map(|(&id, _)| id)
                    .collect::<Vec<_>>();

                for up_to_date_peer in up_to_date_peers {
                    global_state
                        .peers
                        .change_priority(&up_to_date_peer, PeerState::new(Pending));
                }
            }
            DistributionMode::Broadcast if consensus_result.is_pending() => {
                global_state
                    .peers
                    .push(source_peer, PeerState::new(Pending));
            }
            _ => {}
        }
    }
}
