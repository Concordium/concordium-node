use chrono::prelude::{DateTime, Utc};

use std::collections::{BinaryHeap, HashMap};

use crate::{block::*, common::{HashBytes, Slot}, finalization::*, transaction::*};

#[derive(Debug)]
pub enum BlockStatus {
    Alive,
    Dead,
    Finalized,
}

#[allow(dead_code)]
#[derive(Debug, Default)]
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

#[derive(Debug, Default)]
pub struct SkovData {
    pub block_table:        HashMap<BlockHash, (BlockPtr, BlockStatus)>,
    possibly_pending_table: HashMap<BlockHash, Vec<PendingBlock>>,
    // possibly_pending_queue: , // TODO: decide on a priority queue impl based on use
    // awaiting_last_finalized: , // ditto
    finalization_list: BinaryHeap<(FinalizationRecord, BlockPtr)>,
    genesis_block_ptr: Option<BlockPtr>,
    // focus_block:          BlockPtr,
    transaction_table: TransactionTable,
}

impl SkovData {
    pub fn add_genesis(&mut self, genesis_block_ptr: BlockPtr) {
        let genesis_finalization_record = FinalizationRecord::genesis(&genesis_block_ptr);

        self.block_table.insert(
            genesis_block_ptr.hash.clone(),
            (genesis_block_ptr.clone(), BlockStatus::Finalized),
        );

        self.finalization_list
            .push((genesis_finalization_record, genesis_block_ptr.clone()));

        self.genesis_block_ptr = Some(genesis_block_ptr);

        debug!("block table: {:?}", self.block_table.keys().collect::<Vec<_>>());
    }

    pub fn add_block(&mut self, pending_block: PendingBlock) -> Option<(BlockPtr, BlockStatus)> {
        let parent_block = self.get_block_by_hash(&pending_block.block.pointer)
            .expect(&format!("Can't find the parent block ({:?}) of block {:?}!", &pending_block.block.pointer, &pending_block.hash))
            .to_owned();

        let last_finalized = self.get_last_finalized().to_owned();
        let block_ptr = BlockPtr::new(pending_block, parent_block, last_finalized, Utc::now());

        let ret = self.block_table.insert(block_ptr.hash.clone(), (block_ptr, BlockStatus::Alive));
        debug!("block table: {:?}", self.block_table.keys().collect::<Vec<_>>());
        ret
    }

    pub fn get_block_by_hash(&self, hash: &HashBytes) -> Option<&BlockPtr> {
        self.block_table.get(hash).map(|(ptr, _)| ptr)
    }

    pub fn get_last_finalized(&self) -> &BlockPtr {
        &self.finalization_list.peek().unwrap().1 // safe; the genesis is always available
    }

    pub fn get_last_finalized_slot(&self) -> Slot {
        self.get_last_finalized().block.slot()
    }

    pub fn get_last_finalized_height(&self) -> BlockHeight {
        self.get_last_finalized().height
    }

    pub fn get_next_finalization_index(&self) -> FinalizationIndex {
        &self.finalization_list.peek().unwrap().0.index + 1 // safe; the genesis is always available
    }

    pub fn add_finalization(&mut self, record: FinalizationRecord) -> bool {
        let block_ptr = if let Some((ref ptr, ref mut status)) = self.block_table
            .get_mut(&record.block_pointer)
        {
            *status = BlockStatus::Finalized;
            ptr.clone()
        } else {
            panic!("Can't find finalized block {:?} in the block table!", record.block_pointer);
        };

        // we should be ok with a linear search, as we are expecting only to keep the most recent
        // finalization records
        if self.finalization_list.iter().find(|&(rec, _)| *rec == record).is_none() {
            self.finalization_list.push((record, block_ptr));
            debug!("finalization list: {:?}", self.finalization_list.clone().into_sorted_vec().iter().map(|(rec, _)| &rec.block_pointer).collect::<Vec<_>>());
            true
        } else {
            false
        }
    }
}
