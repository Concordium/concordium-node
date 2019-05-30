pub const PAYLOAD_TYPE_LENGTH: u64 = 2;
pub const FILE_NAME_GENESIS_DATA: &str = "genesis.dat";
pub const FILE_NAME_PREFIX_BAKER_PRIVATE: &str = "baker_private_";
pub const FILE_NAME_SUFFIX_BAKER_PRIVATE: &str = ".dat";

use byteorder::{ByteOrder, NetworkEndian, ReadBytesExt, WriteBytesExt};
use failure::Fallible;

use std::{
    collections::HashMap,
    convert::TryFrom,
    fs::OpenOptions,
    io::{Read, Write},
};

use concordium_common::UCursor;

use concordium_consensus::{
    consensus::{self, Bytes},
    ffi::{
        self,
        PacketType::{self, *},
    },
};

use concordium_global_state::{
    block::{BakedBlock, PendingBlock},
    common::{sha256, HashBytes, SerializeToBytes, DELTA_LENGTH, SHA256},
    finalization::{FinalizationMessage, FinalizationRecord},
    tree::{SkovData, SkovError, SkovReq, SkovReqBody, SkovResult, SKOV_QUEUE},
};

use crate::{
    common::{P2PNodeId, PacketDirection},
    configuration,
    network::NetworkId,
    p2p::*,
};

pub fn start_baker(
    conf: &configuration::BakerConfig,
    app_prefs: &configuration::AppPreferences,
) -> Option<consensus::ConsensusContainer> {
    conf.baker_id.and_then(|baker_id| {
        // Check for invalid configuration
        if baker_id > conf.baker_num_bakers {
            // Baker ID is higher than amount of bakers in the network. Bail!
            error!("Baker ID is higher than the number of bakers in the network! Disabling baking");
            return None;
        }

        info!("Starting up baker thread");
        ffi::start_haskell();

        match get_baker_data(app_prefs, conf) {
            Ok((genesis_data, private_data)) => {
                let mut consensus_runner = consensus::ConsensusContainer::default();
                consensus_runner.start_baker(baker_id, genesis_data, private_data);

                Some(consensus_runner)
            }
            Err(_) => {
                error!("Can't read needed data...");
                None
            }
        }
    })
}

fn get_baker_data(
    app_prefs: &configuration::AppPreferences,
    conf: &configuration::BakerConfig,
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

    let (generated_genesis, generated_private_data) =
        if !genesis_loc.exists() || !private_loc.exists() {
            consensus::ConsensusContainer::generate_data(conf.baker_genesis, conf.baker_num_bakers)?
        } else {
            (vec![], HashMap::new())
        };

    let given_genesis = if !genesis_loc.exists() {
        match OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&genesis_loc)
        {
            Ok(mut file) => match file.write_all(&generated_genesis) {
                Ok(_) => generated_genesis,
                Err(_) => bail!("Couldn't write out genesis data"),
            },
            Err(_) => bail!("Couldn't open up genesis file for writing"),
        }
    } else {
        match OpenOptions::new().read(true).open(&genesis_loc) {
            Ok(mut file) => {
                let mut read_data = vec![];
                match file.read_to_end(&mut read_data) {
                    Ok(_) => read_data,
                    Err(_) => bail!("Couldn't read genesis file properly"),
                }
            }
            Err(e) => bail!("Can't open the genesis file ({})!", e),
        }
    };

    let given_private_data = if !private_loc.exists() {
        match OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&private_loc)
        {
            Ok(mut file) => {
                if let Some(baker_id) = conf.baker_id {
                    match file.write_all(&generated_private_data[&(baker_id as i64)]) {
                        Ok(_) => generated_private_data[&(baker_id as i64)].to_owned(),
                        Err(_) => bail!("Couldn't write out private baker data"),
                    }
                } else {
                    bail!("Couldn't write out private baker data");
                }
            }
            Err(_) => bail!("Couldn't open up private baker file for writing"),
        }
    } else {
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
    };

    Ok((given_genesis, given_private_data))
}

pub fn handle_pkt_out(
    node: &mut P2PNode,
    baker: &mut Option<consensus::ConsensusContainer>,
    peer_id: P2PNodeId,
    network_id: NetworkId,
    mut msg: UCursor,
) -> Fallible<()> {
    if let Some(ref mut baker) = baker {
        ensure!(
            msg.len() >= msg.position() + PAYLOAD_TYPE_LENGTH,
            "Message needs at least {} bytes",
            PAYLOAD_TYPE_LENGTH
        );

        let consensus_type = msg.read_u16::<NetworkEndian>()?;
        let view = msg.read_all_into_view()?;
        let content = Box::from(&view.as_slice()[PAYLOAD_TYPE_LENGTH as usize..]);
        let packet_type = PacketType::try_from(consensus_type)?;

        let request_body = match packet_type {
            Block => Some(SkovReqBody::AddBlock(PendingBlock::new(&content)?)),
            FinalizationRecord => Some(SkovReqBody::AddFinalizationRecord(
                FinalizationRecord::deserialize(&content)?,
            )),
            _ => None, // will be expanded later on
        };

        if let Some(body) = request_body {
            let request = SkovReq::new(Some(peer_id.0), body, Some(content.clone()));

            SKOV_QUEUE.send_request(request)?;
        }

        send_msg_to_consensus(node, baker, peer_id, network_id, packet_type, content)
    } else {
        Ok(())
    }
}

pub fn handle_global_state_request(
    node: &mut P2PNode,
    baker: &mut Option<consensus::ConsensusContainer>,
    peer_id: P2PNodeId,
    network_id: NetworkId,
    request: SkovReq,
    skov: &mut SkovData,
) -> Fallible<()> {
    if let Some(ref mut baker) = baker {
        let packet_type = match request.body {
            SkovReqBody::AddBlock(..) => PacketType::Block,
            SkovReqBody::AddFinalizationRecord(..) => PacketType::FinalizationRecord,
            _ => unreachable!(), // will be expanded in due course
        };

        let result = match request.body {
            SkovReqBody::AddBlock(pending_block) => skov.add_block(pending_block),
            SkovReqBody::AddFinalizationRecord(record) => skov.add_finalization(record),
            _ => unreachable!(), // will be expanded alongside Skov
        };

        match result {
            SkovResult::Success => {
                trace!(
                    "Skov: successfully processed a {} from peer {}",
                    packet_type,
                    peer_id
                );
            }
            SkovResult::DuplicateEntry => {
                warn!(
                    "Skov: got a duplicate {} from peer {}",
                    packet_type, peer_id
                );
            }
            SkovResult::Error(e) => {
                warn!("{:?}", e);

                if node.config.global_state_catch_up_requests {
                    match e {
                        SkovError::MissingParentBlock(ref missing, _)
                        | SkovError::MissingBlockToFinalize(ref missing) => {
                            let mut inner_out_bytes =
                                Vec::with_capacity(SHA256 as usize + DELTA_LENGTH as usize);
                            inner_out_bytes.extend_from_slice(missing);
                            inner_out_bytes
                                .write_u64::<NetworkEndian>(0u64)
                                .expect("Can't write to buffer");

                            send_catchup_request_block_by_hash_to_consensus(
                                baker,
                                node,
                                peer_id,
                                network_id,
                                &inner_out_bytes,
                                PacketDirection::Outbound,
                            )?;
                        }
                        SkovError::InvalidLastFinalized(..) => {
                            // TODO
                        }
                    }
                }
            }
        }

        // debug info
        // skov.display_state();
    }

    Ok(())
}

pub fn send_msg_to_consensus(
    node: &mut P2PNode,
    baker: &mut consensus::ConsensusContainer,
    peer_id: P2PNodeId,
    network_id: NetworkId,
    packet_type: PacketType,
    content: Bytes,
) -> Fallible<()> {
    match packet_type {
        Block => send_block_to_consensus(baker, peer_id, content),
        Transaction => send_transaction_to_consensus(baker, peer_id, &content),
        FinalizationMessage => send_finalization_message_to_consensus(baker, peer_id, content),
        FinalizationRecord => send_finalization_record_to_consensus(baker, peer_id, content),
        CatchupBlockByHash => {
            ensure!(
                content.len() == SHA256 as usize + DELTA_LENGTH as usize,
                "{} needs {} bytes",
                CatchupBlockByHash,
                SHA256 + DELTA_LENGTH,
            );
            send_catchup_request_block_by_hash_to_consensus(
                baker,
                node,
                peer_id,
                network_id,
                &content,
                PacketDirection::Inbound,
            )
        }
        CatchupFinalizationRecordByHash => {
            ensure!(
                content.len() == SHA256 as usize,
                "{} needs {} bytes",
                CatchupFinalizationRecordByHash,
                SHA256
            );
            send_catchup_request_finalization_record_by_hash_to_consensus(
                baker,
                node,
                peer_id,
                network_id,
                &content,
                PacketDirection::Inbound,
            )
        }
        CatchupFinalizationRecordByIndex => {
            ensure!(
                content.len() == 8,
                "{} needs {} bytes",
                CatchupFinalizationRecordByIndex,
                8
            );
            send_catchup_request_finalization_record_by_index_to_consensus(
                baker,
                node,
                peer_id,
                network_id,
                &content,
                PacketDirection::Inbound,
            )
        }
        CatchupFinalizationMessagesByPoint => {
            send_catchup_finalization_messages_by_point_to_consensus(baker, peer_id, &content)
        }
    }
}

fn send_transaction_to_consensus(
    baker: &mut consensus::ConsensusContainer,
    peer_id: P2PNodeId,
    content: &[u8],
) -> Fallible<()> {
    baker.send_transaction(content);
    info!("Peer {} sent a transaction to the consensus layer", peer_id);
    Ok(())
}

fn send_finalization_record_to_consensus(
    baker: &mut consensus::ConsensusContainer,
    peer_id: P2PNodeId,
    content: Bytes,
) -> Fallible<()> {
    let record = FinalizationRecord::deserialize(&content)?;

    match baker.send_finalization_record(peer_id.as_raw(), content) {
        0i64 => info!("Peer {} sent a {} to consensus", peer_id, record),
        err_code => error!(
            "Peer {} can't send a finalization record to consensus due to error code #{} (record: \
             {:?})",
            peer_id, err_code, record,
        ),
    }

    Ok(())
}

fn send_finalization_message_to_consensus(
    baker: &mut consensus::ConsensusContainer,
    peer_id: P2PNodeId,
    content: Bytes,
) -> Fallible<()> {
    let message = FinalizationMessage::deserialize(&content)?;

    baker.send_finalization(peer_id.as_raw(), content);
    info!("Peer {} sent a {} to the consensus layer", peer_id, message);

    Ok(())
}

fn send_block_to_consensus(
    baker: &mut consensus::ConsensusContainer,
    peer_id: P2PNodeId,
    content: Bytes,
) -> Fallible<()> {
    let deserialized = BakedBlock::deserialize(&content)?;
    let hash = sha256(&content);

    // send unique blocks to the consensus layer
    match baker.send_block(peer_id.as_raw(), content) {
        0i64 => info!("Peer {} sent a block ({:?}) to consensus", peer_id, hash,),
        err_code => error!(
            "Peer {} can't send a block from network to consensus due to error code #{} (block: \
             {:?})",
            peer_id, err_code, deserialized,
        ),
    }

    Ok(())
}

// Upon handshake completion we ask the consensus layer for a finalization point
// we want to catchup from. This information is relayed to the peer we just
// connected to, which will then emit all finalizations past this point.
fn send_catchup_finalization_messages_by_point_to_consensus(
    baker: &mut consensus::ConsensusContainer,
    peer_id: P2PNodeId,
    content: &[u8],
) -> Fallible<()> {
    match baker.get_finalization_messages(content, peer_id.as_raw())? {
        0i64 => info!(
            "Peer {} requested finalization messages by point from consensus",
            peer_id
        ),
        err_code => error!(
            "Peer {} could not request finalization messages by point from consensus due to error \
             code {} (bytes: {:?}, length: {})",
            peer_id,
            err_code,
            content,
            content.len(),
        ),
    }
    Ok(())
}

macro_rules! send_catchup_request_to_consensus {
    (
        $req_type:expr,
        $node:ident,
        $baker:ident,
        $content:ident,
        $peer_id:ident,
        $network_id:ident,
        $consensus_req_call:expr,
        $packet_direction:expr,
    ) => {{
        debug!("Got a consensus catch-up request for \"{}\"", $req_type);

        if $packet_direction == PacketDirection::Inbound {
            let res = $consensus_req_call($baker, $content)?;
            let return_type = match $req_type {
                CatchupBlockByHash => Block,
                CatchupFinalizationRecordByHash => FinalizationRecord,
                CatchupFinalizationRecordByIndex => FinalizationRecord,
                catchall_val => panic!("Can't respond to catchup type {}", catchall_val),
            };

            if !res.is_empty() && NetworkEndian::read_u64(&res[..8]) > 0 {
                let mut out_bytes = Vec::with_capacity(PAYLOAD_TYPE_LENGTH as usize + res.len());
                out_bytes
                    .write_u16::<NetworkEndian>(return_type as u16)
                    .expect("Can't write to buffer");
                out_bytes.extend(res);

                match &$node.send_message(Some($peer_id), $network_id, None, out_bytes, false) {
                    Ok(_) => info!(
                        "Responded to a catch-up request type \"{}\" from peer {}",
                        $req_type, $peer_id
                    ),
                    Err(_) => error!(
                        "Couldn't respond to a catch-up request type \"{}\" from peer {}!",
                        $req_type, $peer_id
                    ),
                }
            } else {
                error!(
                    "Consensus doesn't have the data to fulfill a catch-up request type \"{}\" \
                     (to obtain a \"{}\") that peer {} requested (response: {:?})",
                    $req_type, return_type, $peer_id, res
                );
            }
        } else {
            let mut out_bytes = Vec::with_capacity(PAYLOAD_TYPE_LENGTH as usize + $content.len());
            out_bytes
                .write_u16::<NetworkEndian>($req_type as u16)
                .expect("Can't write to buffer");
            out_bytes.extend($content);

            match &$node.send_message(Some($peer_id), $network_id, None, out_bytes, false) {
                Ok(_) => info!(
                    "Sent a catch-up request type \"{}\" to peer {}",
                    $req_type, $peer_id
                ),
                Err(_) => error!(
                    "Couldn't respond to a catch-up request type \"{}\" to peer {}!",
                    $req_type, $peer_id
                ),
            }
        }

        Ok(())
    }};
}

// This function requests the finalization record for a certain finalization
// index (this function is triggered by consensus on another peer actively asks
// the p2p layer to request this for it)
fn send_catchup_request_finalization_record_by_index_to_consensus(
    baker: &mut consensus::ConsensusContainer,
    node: &mut P2PNode,
    peer_id: P2PNodeId,
    network_id: NetworkId,
    content: &[u8],
    direction: PacketDirection,
) -> Fallible<()> {
    send_catchup_request_to_consensus!(
        ffi::PacketType::CatchupFinalizationRecordByIndex,
        node,
        baker,
        content,
        peer_id,
        network_id,
        |baker: &consensus::ConsensusContainer, content: &[u8]| -> Fallible<Vec<u8>> {
            let index = NetworkEndian::read_u64(&content[..8]);
            baker.get_indexed_finalization(index)
        },
        direction,
    )
}

fn send_catchup_request_finalization_record_by_hash_to_consensus(
    baker: &mut consensus::ConsensusContainer,
    node: &mut P2PNode,
    peer_id: P2PNodeId,
    network_id: NetworkId,
    content: &[u8],
    direction: PacketDirection,
) -> Fallible<()> {
    send_catchup_request_to_consensus!(
        ffi::PacketType::CatchupFinalizationRecordByHash,
        node,
        baker,
        content,
        peer_id,
        network_id,
        |baker: &consensus::ConsensusContainer, content: &[u8]| -> Fallible<Vec<u8>> {
            baker.get_block_finalization(content)
        },
        direction,
    )
}

fn send_catchup_request_block_by_hash_to_consensus(
    baker: &mut consensus::ConsensusContainer,
    node: &mut P2PNode,
    peer_id: P2PNodeId,
    network_id: NetworkId,
    content: &[u8],
    direction: PacketDirection,
) -> Fallible<()> {
    let hash = HashBytes::new(&content[..SHA256 as usize]);
    let delta = NetworkEndian::read_u64(&content[SHA256 as usize..][..DELTA_LENGTH as usize]);

    if delta == 0 {
        send_catchup_request_to_consensus!(
            ffi::PacketType::CatchupBlockByHash,
            node,
            baker,
            content,
            peer_id,
            network_id,
            |baker: &consensus::ConsensusContainer, content: &[u8]| -> Fallible<Vec<u8>> {
                baker.get_block(content)
            },
            direction,
        )
    } else {
        send_catchup_request_to_consensus!(
            ffi::PacketType::CatchupBlockByHash,
            node,
            baker,
            content,
            peer_id,
            network_id,
            |baker: &consensus::ConsensusContainer, _: &[u8]| -> Fallible<Vec<u8>> {
                baker.get_block_by_delta(&hash, delta)
            },
            direction,
        )
    }
}
