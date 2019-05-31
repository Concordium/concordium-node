use chrono::prelude::Utc;
use failure::Fallible;

use concordium_common::{
    into_err, safe_lock, RelayOrStopEnvelope, RelayOrStopReceiver, RelayOrStopSender,
    RelayOrStopSenderHelper,
};

use std::{
    collections::{BinaryHeap, HashMap},
    fmt,
    rc::Rc,
    sync::{mpsc, Arc, Mutex},
};

use crate::{
    block::*,
    common::{HashBytes, Slot},
    finalization::*,
    transaction::*,
};

use self::PendingQueue::*;

lazy_static! {
    pub static ref SKOV_QUEUE: SkovQueue = { SkovQueue::default() };
}

pub struct SkovQueue {
    receiver: Arc<Mutex<RelayOrStopReceiver<SkovReq>>>,
    sender:   Mutex<RelayOrStopSender<SkovReq>>,
}

impl Default for SkovQueue {
    fn default() -> Self {
        let (sender, receiver) = mpsc::channel::<RelayOrStopEnvelope<SkovReq>>();

        SkovQueue {
            receiver: Arc::new(Mutex::new(receiver)),
            sender:   Mutex::new(sender),
        }
    }
}

impl SkovQueue {
    pub fn send_request(&self, request: SkovReq) -> Fallible<()> {
        into_err!(safe_lock!(self.sender)?.send_msg(request))
    }

    pub fn recv_request(&self) -> Fallible<RelayOrStopEnvelope<SkovReq>> {
        into_err!(safe_lock!(self.receiver)?.recv())
    }
}

#[derive(Debug)]
pub struct SkovReq {
    pub source: Option<u64>, // PeerId
    pub body:   SkovReqBody,
    pub raw:    Option<Box<[u8]>>,
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
pub enum SkovResult {
    Success,
    DuplicateEntry,
    Error(SkovError),
}

#[derive(PartialEq, Eq)]
// if there are two components, the first one is the target and the second is the source
pub enum SkovError {
    // the parent block is not in the tree
    MissingParentBlock(HashBytes, HashBytes),
    // the target last finalized block is not in the tree
    MissingLastFinalizedBlock(HashBytes, HashBytes),
    // the target last finalized block has not been finalized yet
    LastFinalizedNotFinalized(HashBytes, HashBytes),
    // the target last finalized block is not the last finalized block in the tree
    InvalidLastFinalized(HashBytes, HashBytes),
    // the block pointed to by the finalization record is not in the tree
    MissingBlockToFinalize(HashBytes),
}

impl fmt::Debug for SkovError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let msg = match self {
            SkovError::MissingParentBlock(ref parent, ref pending) => {
                format!("block {:?} is missing parent ({:?})", pending, parent)
            },
            SkovError::MissingLastFinalizedBlock(ref last_finalized, ref pending) => {
                format!("block {:?} is pointing to a last finalized block ({:?}) that is not in the tree", pending, last_finalized)
            },
            SkovError::LastFinalizedNotFinalized(ref last_finalized, ref pending) => {
                format!("block {:?} is pointing to a last finalized block ({:?}) that has not been finalized yet", pending, last_finalized)
            },
            SkovError::InvalidLastFinalized(ref last_finalized, ref pending) => format!(
                "block {:?} wrongly states that {:?} is the last finalized block",
                pending, last_finalized
            ),
            SkovError::MissingBlockToFinalize(ref target) => {
                format!("can't finalize block {:?} as it's not in the tree", target)
            }
        };

        write!(f, "Skov error: {}", msg)
    }
}

#[derive(Debug)]
pub struct SkovData {
    // the blocks whose parent and last finalized blocks are already in the tree
    block_tree: HashMap<BlockHash, Rc<BlockPtr>>,
    // finalization records; the blocks they point to must already be in the tree
    finalization_list: BinaryHeap<FinalizationRecord>,
    // the pointer to the genesis block
    genesis_block_ptr: Rc<BlockPtr>,
    // the last finalized block
    last_finalized: Rc<BlockPtr>,
    // blocks waiting for their parent to be added to the tree; the key is the parent's hash
    awaiting_parent_block: HashMap<BlockHash, Vec<PendingBlock>>,
    // blocks waiting for their last finalized block to be finalized
    awaiting_last_finalized_finalization: HashMap<BlockHash, Vec<PendingBlock>>,
    // blocks waiting for their last finalized block to be inserted in the tree
    awaiting_last_finalized_block: HashMap<BlockHash, Vec<PendingBlock>>,
    // finalization records that point to a block not present in the tree
    inapplicable_finalization_records: Vec<FinalizationRecord>,
    // contains transactions
    transaction_table: TransactionTable,
    // focus_block: BlockPtr,
}

const SKOV_OK_PREALLOCATION_SIZE: usize = 128;
const SKOV_ERR_PREALLOCATION_SIZE: usize = 16;

impl SkovData {
    pub fn new(genesis_data: &[u8]) -> Self {
        let genesis_block_ptr = Rc::new(BlockPtr::genesis(genesis_data));

        let mut finalization_list = BinaryHeap::with_capacity(SKOV_OK_PREALLOCATION_SIZE);
        finalization_list.push(FinalizationRecord::genesis(&genesis_block_ptr));

        let mut block_tree = HashMap::with_capacity(SKOV_OK_PREALLOCATION_SIZE);
        block_tree.insert(genesis_block_ptr.hash.clone(), genesis_block_ptr);

        let genesis_block_ref = block_tree.values().next().unwrap(); // safe; we just put it there
        let last_finalized = Rc::clone(genesis_block_ref);
        let genesis_block_ptr = Rc::clone(genesis_block_ref);

        Self {
            block_tree,
            finalization_list,
            genesis_block_ptr,
            last_finalized,
            awaiting_parent_block: HashMap::with_capacity(SKOV_ERR_PREALLOCATION_SIZE),
            awaiting_last_finalized_finalization: HashMap::with_capacity(SKOV_ERR_PREALLOCATION_SIZE),
            awaiting_last_finalized_block: HashMap::with_capacity(SKOV_ERR_PREALLOCATION_SIZE),
            inapplicable_finalization_records: Vec::with_capacity(SKOV_ERR_PREALLOCATION_SIZE),
            transaction_table: TransactionTable::default(),
        }
    }

    pub fn add_block(&mut self, pending_block: PendingBlock) -> SkovResult {
        // verify if the pending block's parent block is already in the tree
        let parent_block =
            if let Some(parent_ptr) = self.get_block_by_hash(&pending_block.block.pointer) {
                parent_ptr
            } else {
                let error = SkovError::MissingParentBlock(
                    pending_block.block.pointer.clone(),
                    pending_block.hash.clone(),
                );

                self.queue_pending_block(AwaitingParentBlock, pending_block.block.pointer.to_owned(), pending_block);

                return SkovResult::Error(error);
            };

        // verify if the pending block's last finalized block is in the tree and
        // that it had already been finalized
        if let Some(lf_ptr) = self.get_block_by_hash(&pending_block.block.last_finalized) {
            if lf_ptr.status.get() != BlockStatus::Finalized {
                let error = SkovError::LastFinalizedNotFinalized(
                    pending_block.block.last_finalized.clone(),
                    pending_block.hash.clone(),
                );

                self.queue_pending_block(AwaitingLastFinalizedFinalization, pending_block.block.last_finalized.clone(), pending_block);

                return SkovResult::Error(error);
            }
        } else {
            let error = SkovError::MissingLastFinalizedBlock(
                pending_block.block.last_finalized.clone(),
                pending_block.hash.clone(),
            );

            self.queue_pending_block(AwaitingLastFinalizedBlock, pending_block.block.last_finalized.clone(), pending_block);

            return SkovResult::Error(error);
        }

        // verify if the pending block's last finalized block is actually the last
        // finalized one
        if pending_block.block.last_finalized != self.last_finalized.hash {
            let error = SkovError::InvalidLastFinalized(
                pending_block.block.last_finalized.clone(),
                pending_block.hash.clone(),
            );

            // for now, don't break on unaligned last finalized block
            warn!("{:?}", error);
        }

        // if the above checks pass, a BlockPtr can be created
        let block_ptr = BlockPtr::new(
            pending_block,
            Rc::clone(parent_block),
            Rc::clone(&self.last_finalized),
            Utc::now(),
        );

        let housekeeping_hash = block_ptr.hash.clone();

        let insertion_result = self
            .block_tree
            .insert(block_ptr.hash.clone(), Rc::new(block_ptr));

        // the block is now in the block tree; run the housekeeping that can possibly
        // move some queued pending blocks to the tree now
        self.refresh_pending_queue(AwaitingParentBlock, &housekeeping_hash);
        self.refresh_pending_queue(AwaitingLastFinalizedBlock, &housekeeping_hash);

        if insertion_result.is_none() {
            SkovResult::Success
        } else {
            SkovResult::DuplicateEntry
        }
    }

    pub fn get_block_by_hash(&self, hash: &HashBytes) -> Option<&Rc<BlockPtr>> {
        self.block_tree.get(hash)
    }

    pub fn get_finalization_record_by_hash(&self, hash: &HashBytes) -> Option<&FinalizationRecord> {
        self.finalization_list
            .iter()
            .find(|&rec| rec.block_pointer == *hash)
    }

    pub fn get_last_finalized_slot(&self) -> Slot { self.last_finalized.block.slot() }

    pub fn get_last_finalized_height(&self) -> BlockHeight { self.last_finalized.height }

    pub fn get_next_finalization_index(&self) -> FinalizationIndex {
        &self.finalization_list.peek().unwrap().index + 1 // safe; always available
    }

    pub fn add_finalization(&mut self, record: FinalizationRecord) -> SkovResult {
        let housekeeping_hash = record.block_pointer.clone();

        // we need a separate block here in order to be able to keep clones to a
        // minimum, query the block tree only once and still be able to do
        // awaiting_last_finalized housekeeping in this function
        let result = {
            let mut target_block = self.block_tree.get_mut(&record.block_pointer);

            if let Some(ref mut block) = target_block {
                block.status.set(BlockStatus::Finalized);
            } else {
                let error = SkovError::MissingBlockToFinalize(record.block_pointer.clone());

                self.inapplicable_finalization_records.push(record);

                return SkovResult::Error(error);
            }

            // rebind the reference (so we don't have to search for it again) so it's no
            // longer mutable
            let target_block = &*target_block.unwrap(); // safe - we already checked for None

            // we should be ok with a linear search, as we are expecting only to keep the
            // most recent finalization records and the most recent one is always first
            if self
                .finalization_list
                .iter()
                .find(|&rec| *rec == record)
                .is_none()
            {
                self.finalization_list.push(record);
                self.last_finalized = Rc::clone(target_block);

                SkovResult::Success
            } else {
                SkovResult::DuplicateEntry
            }
        };

        // the target last finalized is in the block tree; therefore, check if there are
        // no blocks targetting a missing last finalized block that can apply to be
        // inserted in the tree again now
        self.refresh_pending_queue(AwaitingLastFinalizedFinalization, &housekeeping_hash);

        result
    }

    fn pending_queue_ref(&self, queue: PendingQueue) -> &HashMap<HashBytes, Vec<PendingBlock>> {
        match queue {
            AwaitingParentBlock => &self.awaiting_parent_block,
            AwaitingLastFinalizedBlock => &self.awaiting_last_finalized_block,
            AwaitingLastFinalizedFinalization => &self.awaiting_last_finalized_finalization,
        }
    }

    fn pending_queue_mut(&mut self, queue: PendingQueue) -> &mut HashMap<HashBytes, Vec<PendingBlock>> {
        match queue {
            AwaitingParentBlock => &mut self.awaiting_parent_block,
            AwaitingLastFinalizedBlock => &mut self.awaiting_last_finalized_block,
            AwaitingLastFinalizedFinalization => &mut self.awaiting_last_finalized_finalization,
        }
    }

    fn queue_pending_block(&mut self, queue: PendingQueue, missing_entry: HashBytes, pending_block: PendingBlock) {
        let queued = self
            .pending_queue_mut(queue)
            .entry(missing_entry)
            .or_default();

        queued.push(pending_block);
    }

    fn refresh_pending_queue(&mut self, queue: PendingQueue, target_hash: &HashBytes) {
        if let Some(affected_blocks) = self.pending_queue_mut(queue).remove(target_hash) {
            for pending_block in affected_blocks {
                // silence errors here, as it is a housekeeping operation
                let _ = self.add_block(pending_block);
            }
        }
    }

    fn print_pending_queue(&self, queue: PendingQueue) -> String {
        if self.pending_queue_ref(queue).is_empty() { return String::new() }

        // it's heavy debugging at this point; we don't mind reallocating the string
        let mut output = format!("\n{}: [", queue);

        for (missing, affected) in self.pending_queue_ref(queue) {
            output.push_str(&format!("{:?}: [", missing));
            for pending_hash in affected.iter().map(|pb| &pb.hash) {
                output.push_str(&format!("{:?}, ", pending_hash));
            }
            output.truncate(output.len() - 2);
            output.push_str("]");
        }
        output.push_str("]");

        output
    }

    pub fn display_state(&self) {
        info!(
            "Skov data:\nblock tree: {:?}\nlast finalized: {:?}\nfinalization list: {:?}\
            {}{}{}",
            self.block_tree
                .values()
                .collect::<BinaryHeap<_>>()
                .into_sorted_vec()
                .iter()
                .collect::<Vec<_>>(),
            self.last_finalized.hash,
            self.finalization_list
                .iter()
                .map(|rec| &rec.block_pointer)
                .collect::<Vec<_>>(),
            self.print_pending_queue(AwaitingParentBlock),
            self.print_pending_queue(AwaitingLastFinalizedBlock),
            self.print_pending_queue(AwaitingLastFinalizedFinalization),
        );
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PendingQueue {
    AwaitingParentBlock,
    AwaitingLastFinalizedBlock,
    AwaitingLastFinalizedFinalization,
}

impl fmt::Display for PendingQueue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match *self {
            AwaitingParentBlock => "awaiting parent block",
            AwaitingLastFinalizedBlock => "awaiting last finalized block",
            AwaitingLastFinalizedFinalization => "awaiting last finalized finalization",
        };

        write!(f, "{}", name)
    }
}
