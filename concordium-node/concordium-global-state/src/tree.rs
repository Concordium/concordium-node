use chrono::prelude::Utc;
use failure::Fallible;

use concordium_common::{
    into_err, safe_lock, RelayOrStopEnvelope, RelayOrStopReceiver, RelayOrStopSender,
    RelayOrStopSenderHelper,
};

use std::{
    collections::{BinaryHeap, HashMap, HashSet},
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

#[derive(PartialEq, Eq)]
pub enum SkovError {
    MissingParentBlock(HashBytes, HashBytes), // (target parent, pending block)
    InvalidLastFinalized(HashBytes, HashBytes), // (target last_finalized, pending block)
    MissingBlockToFinalize(HashBytes),
}

impl fmt::Debug for SkovError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let msg = match self {
            SkovError::MissingParentBlock(ref parent, ref pending) => {
                format!("block {:?} is missing parent ({:?})", pending, parent)
            }
            SkovError::InvalidLastFinalized(ref last_finalized, ref pending) => format!(
                "block {:?} wrongly states that {:?} is the last finalized block",
                pending, last_finalized
            ),
            SkovError::MissingBlockToFinalize(ref target) => {
                format!("can't finalize block {:?} as it's not in the tree", target)
            }
        };

        write!(f, "{}", msg)
    }
}

#[derive(Debug)]
pub enum SkovResult {
    Success,
    DuplicateEntry,
    Error(SkovError),
}

#[derive(Debug)]
pub struct SkovData {
    // the blocks whose parent and last finalized blocks are already in the tree
    pub block_tree: HashMap<BlockHash, Rc<BlockPtr>>,
    // blocks waiting for their parent to be added to the tree; the key is the parent's hash
    orphan_blocks: HashMap<BlockHash, HashSet<PendingBlock>>,
    // finalization records; the blocks they point to must already be in the tree
    finalization_list: BinaryHeap<FinalizationRecord>,
    // the last finalized block
    last_finalized: Option<Rc<BlockPtr>>,
    // blocks waiting for their last finalized block to be added to the tree
    awaiting_last_finalized: HashMap<BlockHash, HashSet<PendingBlock>>,
    // the pointer to the genesis block; optional only due to SkovData being a lazy_static
    genesis_block_ptr: Option<Rc<BlockPtr>>,
    // contains transactions
    transaction_table: TransactionTable,
    // focus_block: BlockPtr,
}

impl Default for SkovData {
    fn default() -> Self {
        Self {
            block_tree:              HashMap::with_capacity(100),
            orphan_blocks:           HashMap::with_capacity(10),
            finalization_list:       BinaryHeap::with_capacity(100),
            last_finalized:          None,
            awaiting_last_finalized: HashMap::with_capacity(10),
            genesis_block_ptr:       None,
            transaction_table:       TransactionTable::default(),
        }
    }
}

impl SkovData {
    pub fn add_genesis(&mut self, genesis_data: &[u8]) {
        let genesis_block_ptr = Rc::new(BlockPtr::genesis(genesis_data));

        self.finalization_list
            .push(FinalizationRecord::genesis(&genesis_block_ptr));

        self.genesis_block_ptr = Some(Rc::clone(&genesis_block_ptr));
        self.last_finalized = Some(Rc::clone(&genesis_block_ptr));

        self.block_tree
            .insert(genesis_block_ptr.hash.clone(), genesis_block_ptr);
    }

    pub fn add_block(&mut self, pending_block: PendingBlock) -> SkovResult {
        // verify if the pending block's parent block is already in the tree
        let parent_block =
            if let Some(block_ptr) = self.get_block_by_hash(&pending_block.block.pointer) {
                block_ptr
            } else {
                let error = SkovError::MissingParentBlock(
                    pending_block.block.pointer.clone(),
                    pending_block.hash.clone(),
                );

                self.add_orphan_block(pending_block);

                return SkovResult::Error(error);
            };

        // verify if the pending block's last finalized block is actually the last finalized one
        let last_finalized = self.get_last_finalized();

        if last_finalized.hash != pending_block.block.last_finalized {
            let error = SkovError::InvalidLastFinalized(
                pending_block.block.last_finalized.clone(),
                pending_block.hash.clone(),
            );
            self.queue_block_wo_last_finalized(pending_block);

            return SkovResult::Error(error);
        }

        // if the above checks pass, a BlockPtr can be created
        let block_ptr = BlockPtr::new(
            pending_block,
            Rc::clone(parent_block),
            Rc::clone(last_finalized),
            Utc::now(),
        );

        // the block's parent is in the block tree; therefore, check if there are no orphans
        // that can apply to be inserted to the tree again now
        self.update_orphans(&block_ptr.hash);

        let insertion_result = self
            .block_tree
            .insert(block_ptr.hash.clone(), Rc::new(block_ptr));

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

    pub fn get_last_finalized(&self) -> &Rc<BlockPtr> {
        self.last_finalized.as_ref().unwrap() // safe; always available
    }

    pub fn get_last_finalized_slot(&self) -> Slot { self.get_last_finalized().block.slot() }

    pub fn get_last_finalized_height(&self) -> BlockHeight { self.get_last_finalized().height }

    pub fn get_next_finalization_index(&self) -> FinalizationIndex {
        &self.finalization_list.peek().unwrap().index + 1 // safe; always available
    }

    pub fn add_finalization(&mut self, record: FinalizationRecord) -> SkovResult {
        let mut target_block = self.block_tree.get_mut(&record.block_pointer);

        if let Some(ref mut block) = target_block {
            block.status.set(BlockStatus::Finalized);
        } else {
            let error = SkovError::MissingBlockToFinalize(record.block_pointer);

            // TODO: queue the erroneous candidates

            return SkovResult::Error(error);
        }

        // the target last finalized is in the block tree; therefore, check if there are no
        // blocks missing the last finalized that can apply to be inserted to the tree again now
        // self.update_last_finalized(&record.block_pointer);

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
            self.last_finalized = Some(Rc::clone(target_block));

            SkovResult::Success
        } else {
            SkovResult::DuplicateEntry
        }
    }

    fn add_orphan_block(&mut self, pending_block: PendingBlock) {
        let missing_parent = pending_block.block.pointer.to_owned();
        let parents_orphans = self.orphan_blocks.entry(missing_parent).or_default();

        parents_orphans.insert(pending_block);
    }

    fn queue_block_wo_last_finalized(&mut self, pending_block: PendingBlock) {
        let last_finalized = pending_block.block.last_finalized.to_owned();
        let queued = self
            .awaiting_last_finalized
            .entry(last_finalized)
            .or_default();

        queued.insert(pending_block);
    }

    fn update_orphans(&mut self, parent: &HashBytes) {
        if let Some(orphans) = self.orphan_blocks.remove(parent) {
            for orphan in orphans {
                // we want to silence errors here, as it is a housekeeping operation
                let _ = self.add_block(orphan);
            }
        }
    }

    fn update_last_finalized(&mut self, last_finalized: &HashBytes) {
        if let Some(awaiting_blocks) = self.awaiting_last_finalized.remove(last_finalized) {
            for awaiting in awaiting_blocks {
                // we want to silence errors here, as it is a housekeeping operation
                let _ = self.add_block(awaiting);
            }
        }
    }

    pub fn display_state(&self) {
        info!(
            "block tree: {:?}\nlast finalized: {:?}\nfinalization list: {:?}\norphan blocks: \
             {:?}\nawaiting last finalized: {:?}",
            self.block_tree
                .values()
                .collect::<BinaryHeap<_>>()
                .into_sorted_vec()
                .iter()
                .map(|ptr| (&ptr.hash, &ptr.status))
                .collect::<Vec<_>>(),
            self.get_last_finalized().hash,
            self.finalization_list
                .clone()
                .into_sorted_vec()
                .iter()
                .map(|rec| &rec.block_pointer)
                .collect::<Vec<_>>(),
            self.orphan_blocks
                .iter()
                .map(|(parent, pending)| (
                    parent,
                    pending.iter().map(|pb| &pb.hash).collect::<Vec<_>>()
                ))
                .collect::<Vec<_>>(),
            self.awaiting_last_finalized
                .iter()
                .map(|(last_finalized, pending)| (
                    last_finalized,
                    pending.iter().map(|pb| &pb.hash).collect::<Vec<_>>()
                ))
                .collect::<Vec<_>>()
        );
    }
}
