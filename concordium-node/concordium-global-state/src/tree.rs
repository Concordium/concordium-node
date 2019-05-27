use chrono::prelude::{DateTime, Utc};
use failure::{bail, Fallible};

use concordium_common::{
    into_err, RelayOrStopEnvelope, RelayOrStopReceiver, RelayOrStopSender, safe_lock, RelayOrStopSenderHelper
};

use std::{
    collections::{BinaryHeap, HashMap, HashSet},
    fmt,
    sync::{mpsc, Arc, Mutex},
};

use crate::{
    block::*,
    common::{HashBytes, Slot},
    finalization::*,
    transaction::*,
};

lazy_static! {
    pub static ref SKOV_DATA: Mutex<SkovData> = { Mutex::new(SkovData::default()) };
    pub static ref SKOV_QUEUE: SkovQueue = { SkovQueue::default() };
}

#[derive(Clone)]
pub struct SkovQueue {
    receiver: Arc<Mutex<RelayOrStopReceiver<SkovReq>>>,
    sender:   Arc<Mutex<RelayOrStopSender<SkovReq>>>,
}

impl Default for SkovQueue {
    fn default() -> Self {
        let (sender, receiver) = mpsc::channel::<RelayOrStopEnvelope<SkovReq>>();

        SkovQueue {
            receiver: Arc::new(Mutex::new(receiver)),
            sender:   Arc::new(Mutex::new(sender)),
        }
    }
}

impl SkovQueue {
    pub fn send_request(self, request: SkovReq) -> Fallible<()> {
        into_err!(safe_lock!(self.sender)?.send_msg(request))
    }

    pub fn recv_request(self) -> Fallible<RelayOrStopEnvelope<SkovReq>> {
        into_err!(safe_lock!(self.receiver)?.recv())
    }
}

#[derive(Debug)]
pub struct SkovReq {
    pub source: Option<u64>, // PeerId
    pub body: SkovReqBody,
    pub raw: Option<Box<[u8]>>,
}

impl SkovReq {
    pub fn new(source: Option<u64>, body: SkovReqBody, raw: Option<Box<[u8]>>) -> Self {
        Self { source, body, raw }
    }
}

#[derive(Debug)]
pub enum SkovReqBody {
    AddBlock(PendingBlock),
    GetBlock(HashBytes),
    AddFinalizationRecord(FinalizationRecord),
    GetFinalizationRecord(HashBytes),
}

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
    // the blocks whose parent and last finalized blocks are already in the tree
    pub block_tree: HashMap<BlockHash, (BlockPtr, BlockStatus)>,
    // blocks waiting for their parent to be added to the tree; the key is the parent's hash
    orphan_blocks: HashMap<BlockHash, HashSet<PendingBlock>>,
    // finalization records along with their finalized blocks; those must already be in the tree
    finalization_list: BinaryHeap<(FinalizationRecord, BlockPtr)>,
    // blocks waiting for their last finalized block to be added to the tree
    awaiting_last_finalized: HashMap<BlockHash, HashSet<PendingBlock>>,
    // the pointer to the genesis block; optional only due to SkovData being a lazy_static
    genesis_block_ptr: Option<BlockPtr>,
    // contains transactions
    transaction_table: TransactionTable,
    // focus_block: BlockPtr,
}

impl SkovData {
    pub fn add_genesis(&mut self, genesis_block_ptr: BlockPtr) {
        let genesis_finalization_record = FinalizationRecord::genesis(&genesis_block_ptr);

        self.block_tree.insert(
            genesis_block_ptr.hash.clone(),
            (genesis_block_ptr.clone(), BlockStatus::Finalized),
        );

        self.finalization_list
            .push((genesis_finalization_record, genesis_block_ptr.clone()));

        info!(
            "block tree: [{:?}({:?})]",
            genesis_block_ptr.hash,
            BlockStatus::Finalized,
        );

        self.genesis_block_ptr = Some(genesis_block_ptr);
    }

    pub fn add_block(
        &mut self,
        pending_block: PendingBlock,
    ) -> Fallible<Option<(BlockPtr, BlockStatus)>> {
        // verify if the pending block's parent block is already in the tree
        let parent_block =
            if let Some(block_ptr) = self.get_block_by_hash(&pending_block.block.pointer) {
                block_ptr.to_owned()
            } else {
                let warning = format!(
                    "Couldn't find the parent block ({:?}) of block {:?}; to the pending list!",
                    pending_block.block.pointer, pending_block.hash
                );
                self.queue_orphan_block(pending_block);
                warn!("{}", warning);
                bail!(AddBlockError::MissingParent);
            };

        // the block is not an orphan; check if it is in the pending list
        if let Some(ref mut orphans) = self.orphan_blocks.get_mut(&pending_block.block.pointer) {
            orphans.remove(&pending_block);
        }

        // verify if the pending block's last finalized block is already in the tree
        let last_finalized = self.get_last_finalized().to_owned();
        if last_finalized.hash != pending_block.block.last_finalized {
            let warning = format!(
                "Block {:?} points to a finalization record ({:?}) which is not the last one \
                 ({:?})",
                pending_block.hash, pending_block.block.last_finalized, last_finalized.hash,
            );
            self.queue_block_wo_last_finalized(pending_block);
            warn!("{}", warning);
            bail!(AddBlockError::InvalidLastFinalized);
        }

        // if the above checks pass, a BlockPtr can be created
        let block_ptr = BlockPtr::new(pending_block, parent_block, last_finalized, Utc::now());

        let ret = self
            .block_tree
            .insert(block_ptr.hash.clone(), (block_ptr, BlockStatus::Alive));

        info!("block tree: {:?}", {
            let mut vals = self.block_tree.values().collect::<Vec<_>>();
            vals.sort_by_key(|(ptr, _)| ptr.block.slot());
            vals.into_iter()
                .map(|(ptr, status)| (ptr.hash.to_owned(), status))
                .collect::<Vec<_>>()
        });

        Ok(ret)
    }

    pub fn get_block_by_hash(&self, hash: &HashBytes) -> Option<&BlockPtr> {
        self.block_tree.get(hash).map(|(ptr, _)| ptr)
    }

    pub fn get_finalization_record_by_hash(&self, hash: &HashBytes) -> Option<&FinalizationRecord> {
        self.finalization_list
            .iter()
            .find(|&(rec, _)| rec.block_pointer == *hash)
            .map(|(rec, _)| rec)
    }

    pub fn get_last_finalized(&self) -> &BlockPtr {
        &self.finalization_list.peek().unwrap().1 // safe; the genesis is always available
    }

    pub fn get_last_finalized_slot(&self) -> Slot { self.get_last_finalized().block.slot() }

    pub fn get_last_finalized_height(&self) -> BlockHeight { self.get_last_finalized().height }

    pub fn get_next_finalization_index(&self) -> FinalizationIndex {
        &self.finalization_list.peek().unwrap().0.index + 1 // safe; the genesis is always available
    }

    pub fn add_finalization(&mut self, record: FinalizationRecord) -> bool {
        let block_ptr = if let Some((ref ptr, ref mut status)) =
            self.block_tree.get_mut(&record.block_pointer)
        {
            *status = BlockStatus::Finalized;
            ptr.clone()
        } else {
            error!(
                "Can't find finalized block {:?} in the block table!",
                record.block_pointer
            );
            return true; // a temporary placeholder; we don't want to suggest duplicates
        };

        // we should be ok with a linear search, as we are expecting only to keep the
        // most recent finalization records
        if self
            .finalization_list
            .iter()
            .find(|&(rec, _)| *rec == record)
            .is_none()
        {
            self.finalization_list.push((record, block_ptr));
            debug!(
                "finalization list: {:?}",
                self.finalization_list
                    .clone()
                    .into_sorted_vec()
                    .iter()
                    .map(|(rec, _)| &rec.block_pointer)
                    .collect::<Vec<_>>()
            );
            true
        } else {
            false
        }
    }

    fn queue_orphan_block(&mut self, pending_block: PendingBlock) {
        let parent = pending_block.block.pointer.to_owned();
        let queued = self.orphan_blocks.entry(parent).or_default();

        queued.insert(pending_block);

        info!(
            "orphan blocks: {:?}",
            self.orphan_blocks
                .iter()
                .map(|(parent, pending)| (
                    parent,
                    pending
                        .iter()
                        .map(|pb| pb.hash.to_owned())
                        .collect::<Vec<_>>()
                ))
                .collect::<Vec<_>>()
        );
    }

    fn queue_block_wo_last_finalized(&mut self, pending_block: PendingBlock) {
        let last_finalized = pending_block.block.last_finalized.to_owned();
        let queued = self
            .awaiting_last_finalized
            .entry(last_finalized)
            .or_default();

        queued.insert(pending_block);

        info!(
            "blocks awaiting last finalized: {:?}",
            self.awaiting_last_finalized
                .iter()
                .map(|(last_finalized, pending)| (
                    last_finalized,
                    pending
                        .iter()
                        .map(|pb| pb.hash.to_owned())
                        .collect::<Vec<_>>()
                ))
                .collect::<Vec<_>>()
        );
    }

    pub fn process_orphan_block_queue(&mut self) -> Fallible<()> {
        let missing_parent_hashes = self
            .orphan_blocks
            .keys()
            .map(std::borrow::ToOwned::to_owned)
            .collect::<Vec<_>>();

        for missing_parent in missing_parent_hashes.into_iter() {
            if self
                .block_tree
                .iter()
                .any(|(hash, _)| *hash == missing_parent)
            {
                if let Some(orphans) = self.orphan_blocks.remove(&missing_parent) {
                    for orphan in orphans {
                        self.add_block(orphan)?;
                    }
                }
            }
        }

        Ok(())
    }

    pub fn recheck_missing_parent(&mut self, missing_parent: &HashBytes) -> Fallible<()> {
        if self
            .block_tree
            .iter()
            .any(|(hash, _)| hash == missing_parent)
        {
            if let Some(orphans) = self.orphan_blocks.remove(&missing_parent) {
                for orphan in orphans {
                    self.add_block(orphan)?;
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum AddBlockError {
    MissingParent,
    InvalidLastFinalized,
}

impl fmt::Display for AddBlockError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{:?}", self) }
}
