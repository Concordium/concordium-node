pub const PAYLOAD_TYPE_LENGTH: u64 = 2;
pub const FILE_NAME_GENESIS_DATA: &str = "genesis.dat";
pub const FILE_NAME_CRYPTO_PROV_DATA: &str = "crypto_providers.json";
pub const FILE_NAME_ID_PROV_DATA: &str = "identity_providers.json";
pub const FILE_NAME_PREFIX_BAKER_PRIVATE: &str = "baker-";
pub const FILE_NAME_SUFFIX_BAKER_PRIVATE: &str = ".dat";

use byteorder::{NetworkEndian, ReadBytesExt, WriteBytesExt};
use failure::Fallible;

use std::{
    convert::TryFrom,
    fs::OpenOptions,
    io::{self, Cursor, Read},
    mem,
    sync::Arc,
};

use concordium_common::{
    cache::Cache,
    hybrid_buf::HybridBuf,
    ConsensusFfiResponse,
    PacketType::{self, *},
    RelayOrStopEnvelope, RelayOrStopSender,
};

use concordium_consensus::{consensus, ffi};

use concordium_global_state::{
    block::{BlockHeight, PendingBlock},
    common::{sha256, SerializeToBytes},
    finalization::FinalizationRecord,
    transaction::{Transaction, TransactionHash},
    tree::{
        messaging::{
            ConsensusMessage, DistributionMode, GlobalStateMessage, GlobalStateResult, MessageType,
        },
        GlobalState, Peer, PeerState,
    },
};

use crate::{common::P2PNodeId, configuration, network::NetworkId, p2p::p2p_node::*};

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
            let consensus =
                consensus::ConsensusContainer::new(genesis_data, private_data, conf.baker_id);
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
    gs_sender: &RelayOrStopSender<GlobalStateMessage>,
    transactions_cache: &mut Cache<Arc<[u8]>>,
    is_broadcast: bool,
) -> Fallible<()> {
    ensure!(
        msg.len()? >= msg.position()? + PAYLOAD_TYPE_LENGTH,
        "Message needs at least {} bytes",
        PAYLOAD_TYPE_LENGTH
    );

    let consensus_type = msg.read_u16::<NetworkEndian>()?;
    let packet_type = PacketType::try_from(consensus_type)?;

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

    let request =
        RelayOrStopEnvelope::Relay(GlobalStateMessage::ConsensusMessage(ConsensusMessage::new(
            MessageType::Inbound(peer_id.0, distribution_mode),
            packet_type,
            payload,
            dont_relay_to.into_iter().map(P2PNodeId::as_raw).collect(),
        )));

    gs_sender.send(request)?;

    Ok(())
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
            update_peer_list(global_state, peer_ids);

            if global_state
                .peers
                .iter()
                .all(|peer| peer.state == PeerState::UpToDate)
            {
                trace!("Global state: all my peers are up to date");
                if global_state.peers.len() <= node.max_nodes.unwrap_or(u16::max_value()) as usize {
                    consensus.start_baker();
                }
            } else if !global_state
                .peers
                .iter()
                .any(|peer| peer.state == PeerState::CatchingUp)
            {
                // only send a catch-up status if none of the peers are currently catching up
                send_catch_up_status(node, network_id, consensus, global_state);
            }

            Ok(())
        }
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
    global_state
        .peers
        .retain(|peer| peer_ids.contains(&peer.id));

    for id in peer_ids {
        if global_state
            .peers
            .iter()
            .find(|peer| peer.id == id)
            .is_none()
        {
            global_state
                .peers
                .push_back(Peer::new(id, PeerState::Pending));
        }
    }
}

fn process_internal_gs_entry(
    node: &P2PNode,
    network_id: NetworkId,
    mut request: ConsensusMessage,
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
    );

    Ok(())
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

    // rebroadcast incoming broadcasts
    if request.distribution_mode() == DistributionMode::Broadcast {
        consensus_driven_rebroadcast(node, network_id, consensus_result, request);
    }

    Ok(())
}

fn consensus_driven_rebroadcast(
    node: &P2PNode,
    network_id: NetworkId,
    consensus_result: ConsensusFfiResponse,
    mut request: ConsensusMessage,
) {
    if consensus_result.is_rebroadcastable() {
        send_consensus_msg_to_net(
            &node,
            request.dont_relay_to(),
            None,
            network_id,
            request.variant,
            None,
            &request.payload,
        );
    }
}

pub fn apply_delayed_broadcasts(
    node: &P2PNode,
    network_id: NetworkId,
    baker: &mut consensus::ConsensusContainer,
    global_state: &mut GlobalState,
) -> Fallible<()> {
    let delayed_broadcasts = global_state.get_delayed_broadcasts();

    if delayed_broadcasts.is_empty() {
        return Ok(());
    }

    info!("Applying {} delayed broadcast(s)", delayed_broadcasts.len());

    for request in delayed_broadcasts {
        process_external_gs_entry(node, network_id, baker, request, global_state)?;
    }

    info!("Delayed broadcasts were applied");

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
        CatchUpFinalizationMessagesByPoint => {
            consensus.get_finalization_messages(&request.payload, raw_id)
        }
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
) {
    let self_node_id = node.self_peer.id;
    let mut packet_buffer = Vec::with_capacity(PAYLOAD_TYPE_LENGTH as usize + payload.len());
    packet_buffer
        .write_u16::<NetworkEndian>(payload_type as u16)
        .expect("Can't write a packet payload to buffer");
    packet_buffer.extend(payload);

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
}

// GS-powered catch-up
fn _send_catch_up_response(
    node: &P2PNode,
    global_state: &GlobalState,
    target: P2PNodeId,
    network: NetworkId,
    since: BlockHeight,
) {
    for (block, fin_rec) in global_state.iter_tree_since(since) {
        send_consensus_msg_to_net(
            &node,
            vec![],
            Some(target),
            network,
            PacketType::Block,
            None,
            &block.serialize(),
        );
        if let Some(rec) = fin_rec {
            send_consensus_msg_to_net(
                &node,
                vec![],
                Some(target),
                network,
                PacketType::FinalizationRecord,
                None,
                &rec.serialize(),
            );
        }
    }

    // send a catch-up finish notification if need be
}

fn send_catch_up_status(
    node: &P2PNode,
    network_id: NetworkId,
    consensus: &mut consensus::ConsensusContainer,
    global_state: &mut GlobalState,
) {
    if let Some(peer) = global_state
        .peers
        .iter_mut()
        .find(|peer| peer.state == PeerState::Pending)
    {
        debug!("Global state: peer {:016x} needs to catch up", peer.id);

        peer.state = PeerState::CatchingUp; // TODO: consider a timestamp

        send_consensus_msg_to_net(
            node,
            vec![],
            Some(P2PNodeId(peer.id)),
            network_id,
            PacketType::CatchUpStatus,
            Some("catch-up status message".to_owned()),
            &consensus.get_catch_up_status(),
        );
    }
}

fn manage_peer_states(
    global_state: &mut GlobalState,
    request: &ConsensusMessage,
    consensus_result: ConsensusFfiResponse,
) {
    use ConsensusFfiResponse::*;

    if request.variant == CatchUpStatus {
        if consensus_result.is_successful() {
            if let Some(ref mut peer) = global_state
                .peers
                .iter_mut()
                .find(|peer| peer.id == request.source_peer())
            {
                peer.state = PeerState::UpToDate;
            } else {
                global_state
                    .peers
                    .push_back(Peer::new(request.source_peer(), PeerState::UpToDate));
            }
        } else if [PendingBlock, PendingFinalization].contains(&consensus_result) {
            if let Some(ref mut peer) = global_state
                .peers
                .iter_mut()
                .find(|peer| peer.id == request.source_peer())
            {
                peer.state = PeerState::Pending;
            } else {
                global_state
                    .peers
                    .push_front(Peer::new(request.source_peer(), PeerState::Pending));
            }
        }
    } else if [Block, FinalizationRecord].contains(&request.variant) {
        match request.distribution_mode() {
            DistributionMode::Direct if consensus_result.is_successful() => {
                for peer in global_state
                    .peers
                    .iter_mut()
                    .filter(|peer| peer.state == PeerState::UpToDate)
                {
                    peer.state = PeerState::Pending;
                }
            }
            DistributionMode::Broadcast
                if [PendingBlock, PendingFinalization].contains(&consensus_result) =>
            {
                if let Some(ref mut peer) = global_state
                    .peers
                    .iter_mut()
                    .find(|peer| peer.id == request.source_peer())
                {
                    peer.state = PeerState::Pending;
                } else {
                    global_state
                        .peers
                        .push_front(Peer::new(request.source_peer(), PeerState::Pending));
                }
            }
            _ => {}
        }
    }
}
