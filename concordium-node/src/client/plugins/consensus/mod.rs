pub const PAYLOAD_TYPE_LENGTH: u64 = 2;
pub const FILE_NAME_GENESIS_DATA: &str = "genesis.dat";
pub const FILE_NAME_CRYPTO_PROV_DATA: &str = "crypto_providers.json";
pub const FILE_NAME_ID_PROV_DATA: &str = "identity_providers.json";
pub const FILE_NAME_PREFIX_BAKER_PRIVATE: &str = "baker-";
pub const FILE_NAME_SUFFIX_BAKER_PRIVATE: &str = ".dat";
pub const FILE_NAME_SUFFIX_BAKER_PRIVATE_JSON: &str = "-credentials.json";

use byteorder::{NetworkEndian, ReadBytesExt, WriteBytesExt};
use failure::Fallible;

use std::{
    convert::TryFrom,
    fs::OpenOptions,
    io::{Read, Seek, SeekFrom, Write},
    mem,
    sync::mpsc::TrySendError,
};

use concordium_common::{
    blockchain_types::TransactionHash,
    hybrid_buf::HybridBuf,
    ConsensusFfiResponse,
    PacketType::{self, *},
};

use concordium_consensus::{
    consensus::{self, PeerId, CALLBACK_QUEUE},
    ffi,
};

use concordium_global_state::{
    common::sha256,
    tree::{
        messaging::{ConsensusMessage, DistributionMode, MessageType},
        GlobalState, PeerState, PeerStatus,
    },
};

use crate::{
    common::{get_current_stamp, P2PNodeId},
    configuration::{self, MAX_CATCH_UP_TIME},
    network::NetworkId,
    p2p::p2p_node::*,
};

pub fn start_consensus_layer(
    conf: &configuration::BakerConfig,
    gsptr: &GlobalState,
    genesis_data: Vec<u8>,
    private_data: Option<Vec<u8>>,
    max_logging_level: consensus::ConsensusLogLevel,
) -> consensus::ConsensusContainer {
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

    consensus::ConsensusContainer::new(
        u64::from(conf.maximum_block_size),
        conf.scheduler_outcome_logging,
        genesis_data,
        private_data,
        conf.baker_id,
        gsptr,
        max_logging_level,
    )
}

pub fn get_baker_private_data_json_file(
    app_prefs: &configuration::AppPreferences,
    conf: &configuration::BakerConfig,
) -> Option<String> {
    if let Some(baker_id) = conf.baker_id {
        let mut private_loc = app_prefs.get_user_app_dir();
        private_loc.push(format!(
            "{}{}{}",
            FILE_NAME_PREFIX_BAKER_PRIVATE, baker_id, FILE_NAME_SUFFIX_BAKER_PRIVATE_JSON
        ));
        if let Some(path) = private_loc.to_str() {
            Some(path.to_owned())
        } else {
            None
        }
    } else {
        None
    }
}

pub fn get_baker_data(
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
    node: &P2PNode,
    dont_relay_to: Vec<P2PNodeId>,
    peer_id: P2PNodeId,
    mut msg: HybridBuf,
    is_broadcast: bool,
) -> Fallible<()> {
    ensure!(
        msg.len()? >= msg.position()? + PAYLOAD_TYPE_LENGTH,
        "Message needs at least {} bytes",
        PAYLOAD_TYPE_LENGTH
    );

    let consensus_type = msg.read_u16::<NetworkEndian>()?;
    let packet_type = PacketType::try_from(consensus_type)?;

    if packet_type == PacketType::Transaction {
        let curr_pos = msg.position()?;
        let transaction = msg.remaining_bytes()?.into_owned();
        let hash_offset = transaction.len() - mem::size_of::<TransactionHash>();
        let hash = TransactionHash::new(&transaction[hash_offset..]);
        write_or_die!(node.transactions_cache).insert(hash, transaction);
        msg.seek(SeekFrom::Start(curr_pos))?;
    }

    let distribution_mode = if is_broadcast {
        DistributionMode::Broadcast
    } else {
        DistributionMode::Direct
    };

    let request = ConsensusMessage::new(
        MessageType::Inbound(peer_id.0, distribution_mode),
        packet_type,
        msg,
        dont_relay_to.into_iter().map(P2PNodeId::as_raw).collect(),
    );

    match CALLBACK_QUEUE.send_blocking_msg(request) {
        Ok(_) => {}
        Err(e) => match e.downcast::<TrySendError<ConsensusMessage>>()? {
            TrySendError::Full(_) => warn!("The global state queue is full!"),
            TrySendError::Disconnected(_) => panic!("The global state channel is down!"),
        },
    }

    Ok(())
}

pub fn handle_consensus_message(
    node: &P2PNode,
    network_id: NetworkId,
    consensus: &mut consensus::ConsensusContainer,
    request: ConsensusMessage,
    global_state: &mut GlobalState,
) -> Fallible<()> {
    if let MessageType::Outbound(_) = request.direction {
        process_internal_gs_entry(node, network_id, request)?
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

fn process_internal_gs_entry(
    node: &P2PNode,
    network_id: NetworkId,
    mut request: ConsensusMessage,
) -> Fallible<()> {
    send_consensus_msg_to_net(
        node,
        request.dont_relay_to(),
        node.self_peer.id,
        request.target_peer().map(P2PNodeId),
        network_id,
        request.variant,
        None,
        &request.payload.remaining_bytes()?,
    )
}

fn process_external_gs_entry(
    node: &P2PNode,
    network_id: NetworkId,
    consensus: &mut consensus::ConsensusContainer,
    mut request: ConsensusMessage,
    global_state: &mut GlobalState,
) -> Fallible<()> {
    let self_node_id = node.self_peer.id;
    let source = P2PNodeId(request.source_peer());

    // relay external messages to Consensus
    let consensus_result = send_msg_to_consensus(self_node_id, source, consensus, &mut request)?;

    // adjust the peer state(s) based on the feedback from Consensus
    update_peer_states(global_state, &request, consensus_result);

    // rebroadcast incoming broadcasts if applicable
    if request.distribution_mode() == DistributionMode::Broadcast
        && consensus_result.is_rebroadcastable()
    {
        send_consensus_msg_to_net(
            &node,
            request.dont_relay_to(),
            source,
            None,
            network_id,
            request.variant,
            None,
            &request.payload.remaining_bytes()?,
        )?;
    }

    Ok(())
}

fn send_msg_to_consensus(
    our_id: P2PNodeId,
    source_id: P2PNodeId,
    consensus: &mut consensus::ConsensusContainer,
    request: &mut ConsensusMessage,
) -> Fallible<ConsensusFfiResponse> {
    let raw_id = source_id.as_raw();
    let payload_offset = request.payload.position()?;
    let payload = request.payload.remaining_bytes()?;

    let consensus_response = match request.variant {
        Block => consensus.send_block(&payload),
        Transaction => consensus.send_transaction(&payload),
        FinalizationMessage => consensus.send_finalization(&payload),
        FinalizationRecord => consensus.send_finalization_record(&payload),
        CatchUpStatus => consensus.receive_catch_up_status(&payload, raw_id),
    };

    request.payload.seek(SeekFrom::Start(payload_offset))?;

    if consensus_response.is_acceptable() {
        info!(
            "Peer {} (myself) processed a {} from {}",
            our_id, request.variant, source_id
        );
    } else {
        debug!(
            "Peer {} couldn't process a {} due to error code {:?}",
            our_id, request, consensus_response,
        );
    }

    Ok(consensus_response)
}

#[allow(clippy::too_many_arguments)]
pub fn send_consensus_msg_to_net(
    node: &P2PNode,
    dont_relay_to: Vec<u64>,
    source_id: P2PNodeId,
    target_id: Option<P2PNodeId>,
    network_id: NetworkId,
    payload_type: PacketType,
    payload_desc: Option<String>,
    payload: &[u8],
) -> Fallible<()> {
    let mut packet_buffer = HybridBuf::with_capacity(PAYLOAD_TYPE_LENGTH as usize + payload.len())?;
    packet_buffer
        .write_u16::<NetworkEndian>(payload_type as u16)
        .expect("Can't write a packet payload to buffer");
    packet_buffer.write_all(payload)?;
    packet_buffer.rewind()?;

    let result = if target_id.is_some() {
        send_direct_message(node, source_id, target_id, network_id, packet_buffer)
    } else {
        send_broadcast_message(
            node,
            source_id,
            dont_relay_to.into_iter().map(P2PNodeId).collect(),
            network_id,
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
        Ok(_) => info!("Sent a {} containing a {}", target_desc, message_desc,),
        Err(_) => error!(
            "Couldn't send a {} containing a {}!",
            target_desc, message_desc,
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
    debug!("Global state: I'm catching up with peer {:016x}", target);

    global_state
        .peers
        .change_priority(&target, PeerState::new(PeerStatus::CatchingUp));

    global_state.catch_up_stamp = get_current_stamp();

    send_consensus_msg_to_net(
        node,
        vec![],
        node.self_peer.id,
        Some(P2PNodeId(target)),
        network_id,
        PacketType::CatchUpStatus,
        Some("catch-up status message".to_owned()),
        &consensus.get_catch_up_status(),
    )
}

pub fn update_peer_list(node: &P2PNode, global_state: &mut GlobalState) {
    debug!("The peers have changed; updating the catch-up peer list");

    let peer_ids = node.get_node_peer_ids();

    // remove global state peers whose connections were dropped
    for (live_peer, state) in mem::replace(&mut global_state.peers, Default::default())
        .into_iter()
        .filter(|(id, _)| peer_ids.contains(&id))
    {
        global_state.peers.push(live_peer, state);
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

pub fn check_peer_states(
    node: &P2PNode,
    network_id: NetworkId,
    consensus: &mut consensus::ConsensusContainer,
    global_state: &mut GlobalState,
) -> Fallible<()> {
    use PeerStatus::*;

    // take advantage of the priority queue ordering
    if let Some((id, state)) = global_state.peers.peek().map(|(&i, s)| (i, s)) {
        match state.status {
            CatchingUp => {
                // don't send any catch-up statuses while
                // there are peers that are catching up
                if get_current_stamp() > global_state.catch_up_stamp + MAX_CATCH_UP_TIME {
                    debug!("Global state: peer {:016x} took too long to catch up", id);
                    global_state
                        .peers
                        .change_priority(&id, PeerState::new(Pending));
                }
            }
            Pending => {
                // send a catch-up message to the first Pending peer
                debug!("Global state: I need to catch up with peer {:016x}", id);
                send_catch_up_status(node, network_id, consensus, global_state, id)?;
            }
            UpToDate => {
                if !consensus.is_baking() && consensus.is_active() {
                    consensus.start_baker();
                }
            }
        }
    }

    Ok(())
}

fn update_peer_states(
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
