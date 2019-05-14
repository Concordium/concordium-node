use chrono::prelude::{DateTime, Utc};

use std::collections::HashMap;

use crate::{block::*, common::*, finalization::*, transaction::Transaction};

#[derive(Debug)]
pub struct BlockPtr {
    pub hash:           BlockHash,
    pub block:          Block,
    pub parent:         Option<Box<BlockPtr>>,
    pub last_finalized: Option<Box<BlockPtr>>,
    pub height:         BlockHeight,
    // state:       BlockState,
    pub received:  DateTime<Utc>,
    pub validated: DateTime<Utc>,
}

impl BlockPtr {
    pub fn genesis(genesis_bytes: &[u8]) -> Self {
        let genesis_data = GenesisData::deserialize(genesis_bytes).expect("Invalid genesis data");
        let genesis_block = Block::Genesis(genesis_data);
        let timestamp = Utc::now(); // TODO: be more precise when Kontrol is there

        BlockPtr {
            hash:           sha256(genesis_bytes),
            block:          genesis_block,
            parent:         None,
            last_finalized: None,
            height:         0,
            received:       timestamp,
            validated:      timestamp,
        }
    }

    pub fn new(
        pb: PendingBlock,
        parent: Self,
        last_finalized: Self,
        validated: DateTime<Utc>,
    ) -> Self {
        assert_eq!(parent.hash, *pb.block.pointer_ref());
        assert_eq!(last_finalized.hash, *pb.block.last_finalized_ref());

        let height = parent.height + 1;

        Self {
            hash: pb.hash,
            block: Block::Regular(pb.block),
            parent: Some(Box::new(parent)),
            last_finalized: Some(Box::new(last_finalized)),
            height,
            received: pb.received,
            validated,
        }
    }
}

impl PartialEq for BlockPtr {
    fn eq(&self, other: &Self) -> bool { self.hash == other.hash }
}

impl Eq for BlockPtr {}

#[derive(Debug)]
pub enum BlockStatus {
    Alive(BlockPtr),
    Dead,
    Finalized(BlockPtr),
}

#[derive(Debug)]
pub struct PendingBlock {
    hash:     BlockHash,
    block:    BakedBlock,
    received: DateTime<Utc>,
}

impl PendingBlock {
    pub fn new(block: BakedBlock, received: DateTime<Utc>) -> Self {
        let hash = sha256(&block.serialize());

        Self {
            hash,
            block,
            received,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug)]
struct ConsensusStatistics {
    blocks_received:                 u64,
    blocks_verified:                 u64,
    block_last_received:             Option<DateTime<Utc>>,
    block_receive_latency_ema:       f64,
    block_receive_latency_ema_emvar: f64,
    block_receive_period_ema:        Option<f64>,
    block_receive_period_ema_emvar:  Option<f64>,
    block_last_arrived:              Option<DateTime<Utc>>,
    block_arrive_latency_ema:        f64,
    block_arrive_latency_ema_emvar:  f64,
    block_arrive_period_ema:         Option<f64>,
    block_arrive_period_ema_emvar:   Option<f64>,
    transactions_per_block_ema:      f64,
    transactions_per_block_emvar:    f64,
    finalization_count:              u64,
    last_finalized_time:             Option<DateTime<Utc>>,
    finalization_period_ema:         Option<f64>,
    finalization_period_emvar:       Option<f64>,
}

#[allow(dead_code)]
#[derive(Debug)]
struct SkovData {
    block_table:            HashMap<BlockHash, BlockStatus>,
    possibly_pending_table: HashMap<BlockHash, Vec<PendingBlock>>,
    // possibly_pending_queue: , // TODO: decide on a priority queue impl based on use
    // awaiting_last_finalized: , // ditto
    finalization_list:    Vec<(FinalizationRecord, BlockPtr)>,
    finalization_pool:    Vec<(FinalizationIndex, Vec<FinalizationRecord>)>,
    branches:             Vec<BlockPtr>,
    genesis_block_ptr:    BlockPtr,
    focus_block:          BlockPtr,
    pending_transactions: Vec<Transaction>,
    transaction_table:    Vec<Transaction>,
    statistics:           ConsensusStatistics,
}
