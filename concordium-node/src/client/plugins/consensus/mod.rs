pub const PAYLOAD_TYPE_LENGTH: u64 = 2;
pub const FILE_NAME_GENESIS_DATA: &str = "genesis.dat";
pub const FILE_NAME_CRYPTO_PROV_DATA: &str = "crypto_providers.json";
pub const FILE_NAME_ID_PROV_DATA: &str = "identity_providers.json";
pub const FILE_NAME_PREFIX_BAKER_PRIVATE: &str = "baker-";
pub const FILE_NAME_SUFFIX_BAKER_PRIVATE: &str = ".dat";

use byteorder::{ByteOrder, NetworkEndian, ReadBytesExt, WriteBytesExt};
use failure::Fallible;

use std::{convert::TryFrom, fs::OpenOptions, io::Read, sync::Arc};

use concordium_common::{
    cache::Cache,
    stats_export_service::StatsExportService,
    PacketType::{self, *},
    RelayOrStopEnvelope, RelayOrStopSender, UCursor,
};

use concordium_consensus::{consensus, ffi};

use concordium_global_state::{
    block::{BlockHeight, PendingBlock},
    common::{sha256, SerializeToBytes},
    finalization::FinalizationRecord,
    transaction::Transaction,
    tree::{
        messaging::{
            ConsensusMessage, DistributionMode, MessageType, SkovError, SkovMetadata, SkovResult,
        },
        Skov, SkovState,
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

    match get_baker_data(app_prefs, conf, true) {
        Ok((genesis_data, private_data)) => {
            let consensus =
                consensus::ConsensusContainer::new(genesis_data, Some(private_data), conf.baker_id);
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
) -> Fallible<(Vec<u8>, Vec<u8>)> {
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
                    Ok(_) => read_data,
                    Err(_) => bail!("Couldn't open up private baker file for reading"),
                }
            }
            Err(e) => bail!("Can't open the private data file ({})!", e),
        }
    } else {
        vec![]
    };

    debug!(
        "Obtained genesis data {:?}",
        sha256(&[&[0u8; 8], genesis_data.as_slice()].concat())
    );
    Ok((genesis_data, private_data))
}

/// Handles packets coming from other peers
pub fn handle_pkt_out(
    peer_id: P2PNodeId,
    mut msg: UCursor,
    skov_sender: &RelayOrStopSender<ConsensusMessage>,
    _transactions_cache: &Cache<Transaction>,
    is_broadcast: bool,
) -> Fallible<()> {
    ensure!(
        msg.len() >= msg.position() + PAYLOAD_TYPE_LENGTH,
        "Message needs at least {} bytes",
        PAYLOAD_TYPE_LENGTH
    );

    let consensus_type = msg.read_u16::<NetworkEndian>()?;
    let packet_type = PacketType::try_from(consensus_type)?;

    let view = msg.read_all_into_view()?;
    let payload = Arc::from(&view.as_slice()[PAYLOAD_TYPE_LENGTH as usize..]);
    let distribution_mode = if is_broadcast {
        DistributionMode::Broadcast
    } else {
        DistributionMode::Direct
    };

    let request = RelayOrStopEnvelope::Relay(ConsensusMessage::new(
        MessageType::Inbound(peer_id.0, distribution_mode),
        packet_type,
        payload,
    ));

    skov_sender.send(request)?;

    Ok(())
}

pub fn handle_global_state_request(
    node: &P2PNode,
    network_id: NetworkId,
    consensus: &mut consensus::ConsensusContainer,
    request: ConsensusMessage,
    skov: &mut Skov,
    stats_exporting: &Option<StatsExportService>,
) -> Fallible<()> {
    if let MessageType::Outbound(_) = request.direction {
        process_internal_skov_entry(node, network_id, request, skov)?
    } else {
        process_external_skov_entry(node, network_id, consensus, request, skov)?
    }

    if let Some(stats) = stats_exporting {
        let stats_values = skov.stats.query_stats();
        stats.set_skov_block_receipt(stats_values.0 as i64);
        stats.set_skov_block_entry(stats_values.1 as i64);
        stats.set_skov_block_query(stats_values.2 as i64);
        stats.set_skov_finalization_receipt(stats_values.3 as i64);
        stats.set_skov_finalization_entry(stats_values.4 as i64);
        stats.set_skov_finalization_query(stats_values.5 as i64);
    }

    Ok(())
}

fn process_internal_skov_entry(
    node: &P2PNode,
    network_id: NetworkId,
    request: ConsensusMessage,
    skov: &mut Skov,
) -> Fallible<()> {
    let (entry_info, skov_result) = match request.variant {
        PacketType::Block => {
            let block = PendingBlock::new(&request.payload)?;
            (format!("{:?}", block.block), skov.add_block(block))
        }
        PacketType::FinalizationRecord => {
            let record = FinalizationRecord::deserialize(&request.payload)?;
            (format!("{:?}", record), skov.add_finalization(record))
        }
        PacketType::CatchupBlockByHash
        | PacketType::CatchupFinalizationRecordByHash
        | PacketType::CatchupFinalizationRecordByIndex => {
            error!("Consensus should not be missing any data!");
            return Ok(());
        }
        _ => (request.variant.to_string(), SkovResult::IgnoredEntry),
    };

    match skov_result {
        SkovResult::SuccessfulEntry(entry) => {
            trace!(
                "Skov: successfully processed a {} from our consensus layer",
                entry
            );
        }
        SkovResult::IgnoredEntry => {
            trace!(
                "Skov: ignoring a {} from our consensus layer",
                request.variant
            );
        }
        SkovResult::Error(e) => skov.register_error(e),
        _ => {}
    }

    send_consensus_msg_to_net(
        node,
        request.target_peer().map(P2PNodeId),
        network_id,
        request.variant,
        Some(entry_info),
        &request.payload,
    );

    Ok(())
}

fn process_external_skov_entry(
    node: &P2PNode,
    network_id: NetworkId,
    consensus: &mut consensus::ConsensusContainer,
    request: ConsensusMessage,
    skov: &mut Skov,
) -> Fallible<()> {
    let self_node_id = node.self_peer.id;
    let source = P2PNodeId(request.source_peer());

    if skov.is_catching_up() {
        if skov.is_broadcast_delay_acceptable() {
            // delay broadcasts during catch-up rounds
            if request.distribution_mode() == DistributionMode::Broadcast {
                info!(
                    "Still catching up; the last received broadcast containing a {} will be \
                     processed after it's finished",
                    request,
                );
                // TODO: this check might not be needed; verify
                if source != self_node_id {
                    skov.delay_broadcast(request);
                }
                return Ok(());
            }
        } else {
            warn!("The catch-up round was taking too long; resuming regular state");
            conclude_catch_up_round(node, network_id, consensus, skov)?;
        }
    }

    let (skov_result, consensus_applicable) = match request.variant {
        PacketType::Block => {
            let block = PendingBlock::new(&request.payload)?;
            let skov_result = skov.add_block(block);
            (skov_result, true)
        }
        PacketType::FinalizationRecord => {
            let record = FinalizationRecord::deserialize(&request.payload)?;
            let skov_result = skov.add_finalization(record);
            (skov_result, true)
        }
        PacketType::GlobalStateMetadata => {
            let skov_result = if skov.peer_metadata.get(&source.0).is_none() {
                let metadata = SkovMetadata::deserialize(&request.payload)?;
                skov.register_peer_metadata(request.source_peer(), metadata)
            } else {
                SkovResult::IgnoredEntry
            };
            (skov_result, false)
        }
        PacketType::GlobalStateMetadataRequest => (skov.get_metadata(), false),
        PacketType::FullCatchupRequest => {
            let since = NetworkEndian::read_u64(&request.payload[..8]);
            send_catch_up_response(node, &skov, source, network_id, since);
            (
                SkovResult::SuccessfulEntry(PacketType::FullCatchupRequest),
                false,
            )
        }
        PacketType::FullCatchupComplete => (
            SkovResult::SuccessfulEntry(PacketType::FullCatchupComplete),
            false,
        ),
        _ => (SkovResult::IgnoredEntry, true), // will be expanded later on
    };

    match skov_result {
        SkovResult::SuccessfulEntry(entry_type) => {
            trace!(
                "Peer {} successfully processed a {}",
                node.self_peer.id,
                request
            );

            // reply to peer metadata with own metadata and begin catching up and/or baking
            match entry_type {
                PacketType::GlobalStateMetadata => {
                    let response_metadata =
                        if let SkovResult::SuccessfulQuery(metadata) = skov.get_metadata() {
                            metadata
                        } else {
                            unreachable!(); // impossible
                        };

                    send_consensus_msg_to_net(
                        &node,
                        Some(source),
                        network_id,
                        PacketType::GlobalStateMetadata,
                        Some(format!("response to a {}", request.variant)),
                        &response_metadata,
                    );

                    if skov.state() == SkovState::JustStarted {
                        if let SkovResult::BestPeer((best_peer, best_meta)) = skov.best_metadata() {
                            if best_meta.is_usable() {
                                send_catch_up_request(node, P2PNodeId(best_peer), network_id, 0);
                                skov.start_catchup_round(SkovState::FullyCatchingUp);
                            } else {
                                consensus.start_baker();
                                skov.data.state = SkovState::Complete;
                            }
                        };
                    }
                }
                PacketType::FullCatchupComplete => {
                    conclude_catch_up_round(node, network_id, consensus, skov)?;
                }
                _ => {}
            }
        }
        SkovResult::SuccessfulQuery(result) => {
            let return_type = match request.variant {
                PacketType::CatchupBlockByHash => PacketType::Block,
                PacketType::CatchupFinalizationRecordByHash => PacketType::FinalizationRecord,
                PacketType::CatchupFinalizationRecordByIndex => PacketType::FinalizationRecord,
                PacketType::GlobalStateMetadataRequest => PacketType::GlobalStateMetadata,
                _ => unreachable!("Impossible packet type in a query result!"),
            };

            send_consensus_msg_to_net(
                &node,
                Some(source),
                network_id,
                return_type,
                Some(format!("response to a {}", request.variant)),
                &result,
            );

            if return_type == PacketType::GlobalStateMetadata && skov.state() == SkovState::Complete
            {
                send_finalization_point(node, consensus, source, network_id);
            }
        }
        SkovResult::DuplicateEntry => {
            warn!("Skov: got a duplicate {}", request);
        }
        SkovResult::Error(err) => {
            match err {
                SkovError::MissingParentBlock(..)
                | SkovError::MissingLastFinalizedBlock(..)
                | SkovError::LastFinalizedNotFinalized(..)
                | SkovError::MissingBlockToFinalize(..) => {
                    let curr_height = skov.data.get_last_finalized_height();
                    send_catch_up_request(node, source, network_id, curr_height);
                    skov.start_catchup_round(SkovState::FullyCatchingUp);
                }
                _ => {}
            }
            skov.register_error(err);
        }
        _ => {}
    }

    // relay external messages to Consensus if they are relevant to it
    if consensus_applicable {
        send_msg_to_consensus(self_node_id, source, consensus, request)?
    }

    if skov.state() == SkovState::PartiallyCatchingUp && skov.is_tree_valid() {
        conclude_catch_up_round(node, network_id, consensus, skov)?;
    }

    Ok(())
}

pub fn apply_delayed_broadcasts(
    node: &P2PNode,
    network_id: NetworkId,
    baker: &mut consensus::ConsensusContainer,
    skov: &mut Skov,
) -> Fallible<()> {
    let delayed_broadcasts = skov.get_delayed_broadcasts();

    if delayed_broadcasts.is_empty() {
        return Ok(());
    }

    info!("Applying {} delayed broadcast(s)", delayed_broadcasts.len());

    for request in delayed_broadcasts {
        process_external_skov_entry(node, network_id, baker, request, skov)?;
    }

    info!("Delayed broadcasts were applied");

    Ok(())
}

fn send_msg_to_consensus(
    our_id: P2PNodeId,
    source_id: P2PNodeId,
    consensus: &mut consensus::ConsensusContainer,
    request: ConsensusMessage,
) -> Fallible<()> {
    let raw_id = source_id.as_raw();

    let consensus_response = match request.variant {
        Block => consensus.send_block(raw_id, &request.payload),
        Transaction => consensus.send_transaction(&request.payload),
        FinalizationMessage => consensus.send_finalization(raw_id, &request.payload),
        FinalizationRecord => consensus.send_finalization_record(raw_id, &request.payload),
        CatchupFinalizationMessagesByPoint => {
            consensus.get_finalization_messages(&request.payload, raw_id)
        }
        _ => unreachable!("Impossible! A Skov-only request was passed on to consensus"),
    };

    if consensus_response.is_acceptable() {
        info!("Peer {} processed a {}", our_id, request,);
    } else {
        error!(
            "Peer {} couldn't process a {} due to error code {:?}",
            our_id, request, consensus_response,
        );
    }

    Ok(())
}

pub fn send_consensus_msg_to_net(
    node: &P2PNode,
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
        send_broadcast_message(node, None, network_id, None, packet_buffer)
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

fn send_finalization_point(
    node: &P2PNode,
    consensus: &consensus::ConsensusContainer,
    target: P2PNodeId,
    network: NetworkId,
) {
    let response = consensus.get_finalization_point();

    send_consensus_msg_to_net(
        node,
        Some(target),
        network,
        PacketType::CatchupFinalizationMessagesByPoint,
        None,
        &response,
    );
}

fn send_catch_up_request(
    node: &P2PNode,
    target: P2PNodeId,
    network: NetworkId,
    since: BlockHeight,
) {
    let packet_type = PacketType::FullCatchupRequest;
    let mut buffer = Vec::with_capacity(PAYLOAD_TYPE_LENGTH as usize);
    buffer
        .write_u16::<NetworkEndian>(packet_type as u16)
        .and_then(|_| buffer.write_u64::<NetworkEndian>(since))
        .expect("Can't write a packet payload to buffer");

    let result = send_direct_message(node, Some(target), network, None, buffer);

    match result {
        Ok(_) => info!(
            "Peer {} sent a direct {} to peer {}",
            node.self_peer.id, packet_type, target,
        ),
        Err(_) => error!(
            "Peer {} couldn't send a direct {} to peer {}!",
            node.self_peer.id, packet_type, target,
        ),
    }
}

fn send_catch_up_response(
    node: &P2PNode,
    skov: &Skov,
    target: P2PNodeId,
    network: NetworkId,
    since: BlockHeight,
) {
    for (block, fin_rec) in skov.iter_tree_since(since) {
        send_consensus_msg_to_net(
            &node,
            Some(target),
            network,
            PacketType::Block,
            None,
            &block.serialize(),
        );
        if let Some(rec) = fin_rec {
            send_consensus_msg_to_net(
                &node,
                Some(target),
                network,
                PacketType::FinalizationRecord,
                None,
                &rec.serialize(),
            );
        }
    }

    let mut blob = Vec::with_capacity(PAYLOAD_TYPE_LENGTH as usize);
    let packet_type = PacketType::FullCatchupComplete;
    blob.write_u16::<NetworkEndian>(packet_type as u16)
        .expect("Can't write a packet payload to buffer");

    send_consensus_msg_to_net(&node, Some(target), network, packet_type, None, &blob);
}

fn conclude_catch_up_round(
    node: &P2PNode,
    network_id: NetworkId,
    consensus: &mut consensus::ConsensusContainer,
    skov: &mut Skov,
) -> Fallible<()> {
    skov.end_catchup_round();
    apply_delayed_broadcasts(node, network_id, consensus, skov)?;

    if !consensus.is_baking() {
        consensus.start_baker();
    }

    Ok(())
}
