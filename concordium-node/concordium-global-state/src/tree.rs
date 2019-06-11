use chrono::prelude::{DateTime, Utc};
use circular_queue::CircularQueue;

use std::{
    collections::{BinaryHeap, HashMap},
    fmt,
    rc::Rc,
};

use crate::{
    block::*,
    common::{HashBytes, SerializeToBytes, Slot},
    finalization::*,
    transaction::*,
};

use self::PendingQueueType::*;

#[derive(Debug)]
pub struct SkovReq {
    pub source: Option<u64>, // PeerId
    pub body:   SkovReqBody,
}

impl SkovReq {
    pub fn new(source: Option<u64>, body: SkovReqBody) -> Self { Self { source, body } }
}

#[derive(Debug)]
pub enum SkovReqBody {
    AddBlock(PendingBlock),
    AddFinalizationRecord(FinalizationRecord),
    GetBlock(HashBytes, Delta),
    GetFinalizationRecordByHash(HashBytes),
    GetFinalizationRecordByIdx(FinalizationIndex),
}

type Bytes = Box<[u8]>;

#[derive(Debug)]
pub enum SkovResult {
    SuccessfulEntry,
    SuccessfulQuery(Bytes),
    DuplicateEntry,
    Error(SkovError),
}

impl fmt::Display for SkovResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let msg = match self {
            SkovResult::SuccessfulEntry => "successful entry".to_owned(),
            SkovResult::SuccessfulQuery(_) => "successful query".to_owned(),
            SkovResult::DuplicateEntry => "duplicate entry".to_owned(),
            SkovResult::Error(e) => e.to_string(),
        };

        write!(f, "Skov: {}", msg)
    }
}

#[derive(Debug, PartialEq, Eq)]
// if there are two components, the first one is the target and the second is
// the source
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
    // the requested block is not available
    MissingBlock(HashBytes, Delta),
    // the requested finalization record for the given block hash is not available
    MissingFinalizationRecordByHash(HashBytes),
    // the requested finalization record with the given finalization index is not available
    MissingFinalizationRecordByIdx(FinalizationIndex),
}

impl fmt::Display for SkovError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let msg = match self {
            SkovError::MissingParentBlock(ref parent, ref pending) => format!(
                "block {:?} is pointing to a parent ({:?}) that is not in the tree",
                pending, parent
            ),
            SkovError::MissingLastFinalizedBlock(ref last_finalized, ref pending) => format!(
                "block {:?} is pointing to a last finalized block ({:?}) that is not in the tree",
                pending, last_finalized
            ),
            SkovError::LastFinalizedNotFinalized(ref last_finalized, ref pending) => format!(
                "block {:?} is pointing to a last finalized block ({:?}) that has not been \
                 finalized yet",
                pending, last_finalized
            ),
            SkovError::InvalidLastFinalized(ref last_finalized, ref pending) => format!(
                "block {:?} wrongly states that {:?} is the last finalized block",
                pending, last_finalized
            ),
            SkovError::MissingBlockToFinalize(ref target) => format!(
                "finalization record for block {:?} references a block that is not in the tree",
                target
            ),
            SkovError::MissingBlock(ref hash, delta) => format!(
                "requested block {:?} delta {} is not available",
                hash, delta
            ),
            SkovError::MissingFinalizationRecordByHash(ref hash) => format!(
                "requested finalization record for block {:?} is not available",
                hash
            ),
            SkovError::MissingFinalizationRecordByIdx(index) => format!(
                "requested finalization record for index {} is not available",
                index
            ),
        };

        write!(f, "error: {}", msg)
    }
}

pub struct Skov {
    pub data:  SkovData,
    pub stats: SkovStats,
}

macro_rules! add_entry {
    ($entry_foo:ident, $entry_type:ty, $addition_stat:ident, $timestamp_stat:ident) => {
        pub fn $entry_foo(&mut self, entry: $entry_type) -> SkovResult {
            let timestamp_entry = Utc::now();
            let result = self.data.$entry_foo(entry);
            let timestamp_addition = Utc::now();
            let addition_duration = (timestamp_addition - timestamp_entry).num_microseconds().unwrap_or(0);

            self.stats.$addition_stat.push(addition_duration as u64);

            if let SkovResult::SuccessfulEntry = result {
                self.stats.$timestamp_stat.push(timestamp_entry);
            };

            result
        }
    };
}

macro_rules! get_object {
    ($query_foo:ident($($query_arg:ident: $query_arg_ty:ty),+), $err_kind:ident, $query_stat:ident) => {
        pub fn $query_foo(&mut self, $($query_arg: $query_arg_ty),+) -> SkovResult {
            let timestamp_start = Utc::now();
            let ret = self.data.$query_foo($($query_arg),+);
            let timestamp_end = Utc::now();
            let query_duration = (timestamp_end - timestamp_start).num_microseconds().unwrap_or(0);

            self.stats.$query_stat.push(query_duration as u64);

            if let Some(ret) = ret {
                SkovResult::SuccessfulQuery(ret.serialize())
            } else {
                SkovResult::Error(SkovError::$err_kind($($query_arg.to_owned()),+))
            }
        }
    }
}

impl Skov {
    add_entry!(
        add_block,
        PendingBlock,
        add_block_timings,
        block_arrival_times
    );

    add_entry!(
        add_finalization,
        FinalizationRecord,
        add_finalization_timings,
        finalization_times
    );

    get_object!(
        get_block(hash: &HashBytes, delta: Delta),
        MissingBlock,
        query_block_timings
    );

    get_object!(
        get_finalization_record_by_hash(hash: &HashBytes),
        MissingFinalizationRecordByHash,
        query_finalization_timings
    );

    get_object!(
        get_finalization_record_by_idx(idx: FinalizationIndex),
        MissingFinalizationRecordByIdx,
        query_finalization_timings
    );

    pub fn new(genesis_data: &[u8]) -> Self {
        Self {
            data:  SkovData::new(genesis_data),
            stats: SkovStats::new(16),
        }
    }

    pub fn register_error(&mut self, err: SkovError) { self.stats.errors.push(err) }

    pub fn display_state(&self) {
        fn sorted_block_map(map: &HashMap<HashBytes, Rc<BlockPtr>>) -> Vec<&Rc<BlockPtr>> {
            map.values().collect::<BinaryHeap<_>>().into_sorted_vec()
        }

        info!(
            "Skov data:\nblock tree: {:?}\nlast finalized: {:?}\nfinalization list: {:?}\ntree \
             candidates: {:?}{}{}{}",
            sorted_block_map(&self.data.block_tree),
            self.data.last_finalized.hash,
            self.data
                .finalization_list
                .iter()
                .map(|rec| &rec.block_pointer)
                .collect::<Vec<_>>(),
            sorted_block_map(&self.data.tree_candidates),
            self.data.print_pending_queue(AwaitingParentBlock),
            self.data.print_pending_queue(AwaitingLastFinalizedBlock),
            self.data
                .print_pending_queue(AwaitingLastFinalizedFinalization),
        );
    }

    pub fn display_stats(&self) {
        info!("Skov stats: {}", self.stats);
    }
}

type PendingQueue = HashMap<BlockHash, Vec<PendingBlock>>;

#[derive(Debug)]
pub struct SkovData {
    // finalized blocks
    block_tree: HashMap<BlockHash, Rc<BlockPtr>>,
    // finalization records; the blocks they point to must already be in the tree
    finalization_list: Vec<FinalizationRecord>,
    // the pointer to the genesis block
    genesis_block_ptr: Rc<BlockPtr>,
    // the last finalized block
    last_finalized: Rc<BlockPtr>,
    // valid blocks (parent and last finalized blocks are already in the tree) pending finalization
    tree_candidates: HashMap<BlockHash, Rc<BlockPtr>>,
    // blocks waiting for their parent to be added to the tree; the key is the parent's hash
    awaiting_parent_block: PendingQueue,
    // blocks waiting for their last finalized block to be finalized
    awaiting_last_finalized_finalization: PendingQueue,
    // blocks waiting for their last finalized block to be inserted in the tree
    awaiting_last_finalized_block: PendingQueue,
    // finalization records that point to a block not present in the tree
    inapplicable_finalization_records: Vec<FinalizationRecord>,
    // contains transactions
    transaction_table: TransactionTable,
    // focus_block: BlockPtr,
}

impl SkovData {
    fn new(genesis_data: &[u8]) -> Self {
        const SKOV_LONG_PREALLOCATION_SIZE: usize = 128;
        const SKOV_SHORT_PREALLOCATION_SIZE: usize = 16;
        const SKOV_ERR_PREALLOCATION_SIZE: usize = 16;

        let genesis_block_ptr = Rc::new(BlockPtr::genesis(genesis_data));

        let mut finalization_list = Vec::with_capacity(SKOV_LONG_PREALLOCATION_SIZE);
        finalization_list.push(FinalizationRecord::genesis(&genesis_block_ptr));

        let mut block_tree = HashMap::with_capacity(SKOV_LONG_PREALLOCATION_SIZE);
        block_tree.insert(genesis_block_ptr.hash.clone(), genesis_block_ptr);

        let genesis_block_ref = block_tree.values().next().unwrap(); // safe; we just put it there
        let last_finalized = Rc::clone(genesis_block_ref);
        let genesis_block_ptr = Rc::clone(genesis_block_ref);

        Self {
            block_tree,
            finalization_list,
            genesis_block_ptr,
            last_finalized,
            tree_candidates: HashMap::with_capacity(SKOV_SHORT_PREALLOCATION_SIZE),
            awaiting_parent_block: HashMap::with_capacity(SKOV_ERR_PREALLOCATION_SIZE),
            awaiting_last_finalized_finalization: HashMap::with_capacity(
                SKOV_ERR_PREALLOCATION_SIZE,
            ),
            awaiting_last_finalized_block: HashMap::with_capacity(SKOV_ERR_PREALLOCATION_SIZE),
            inapplicable_finalization_records: Vec::with_capacity(SKOV_ERR_PREALLOCATION_SIZE),
            transaction_table: TransactionTable::default(),
        }
    }

    fn add_block(&mut self, pending_block: PendingBlock) -> SkovResult {
        // verify if the pending block's parent block is among tree candidates
        // or already in the tree
        let parent_block = if let Some(parent_ptr) = self.get_block(&pending_block.block.pointer, 0)
        {
            parent_ptr
        } else {
            let error = SkovError::MissingParentBlock(
                pending_block.block.pointer.clone(),
                pending_block.hash.clone(),
            );

            self.queue_pending_block(
                AwaitingParentBlock,
                pending_block.block.pointer.to_owned(),
                pending_block,
            );

            return SkovResult::Error(error);
        };

        // verify if the pending block's last finalized block is in the tree and
        // that it had already been finalized
        if let Some(lf_ptr) = self.block_tree.get(&pending_block.block.last_finalized) {
            if lf_ptr.status.get() != BlockStatus::Finalized {
                let error = SkovError::LastFinalizedNotFinalized(
                    pending_block.block.last_finalized.clone(),
                    pending_block.hash.clone(),
                );

                self.queue_pending_block(
                    AwaitingLastFinalizedFinalization,
                    pending_block.block.last_finalized.clone(),
                    pending_block,
                );

                return SkovResult::Error(error);
            }
        } else {
            let error = SkovError::MissingLastFinalizedBlock(
                pending_block.block.last_finalized.clone(),
                pending_block.hash.clone(),
            );

            self.queue_pending_block(
                AwaitingLastFinalizedBlock,
                pending_block.block.last_finalized.clone(),
                pending_block,
            );

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

        // put the new block pointer in the tree candidate queue where it will await a
        // finalization record
        let insertion_result = self
            .tree_candidates
            .insert(block_ptr.hash.clone(), Rc::new(block_ptr));

        // the block is now in the block tree; run housekeeping that
        // can possibly move some queued pending blocks to the tree now
        self.refresh_pending_queue(AwaitingParentBlock, &housekeeping_hash);
        self.refresh_pending_queue(AwaitingLastFinalizedBlock, &housekeeping_hash);

        if insertion_result.is_none() {
            SkovResult::SuccessfulEntry
        } else {
            SkovResult::DuplicateEntry
        }
    }

    fn get_block(&self, hash: &HashBytes, delta: Delta) -> Option<&Rc<BlockPtr>> {
        let target_block = self
            .tree_candidates
            .get(hash)
            .or_else(|| self.block_tree.get(hash));

        if delta == 0 {
            target_block
        } else {
            // obtain the block's descendant
            let reference_block = target_block?;
            self.get_blocks_at_height(reference_block.height + delta)
                .find(|&candidate| reference_block.is_ancestor_of(candidate))
        }
    }

    fn get_blocks_at_height(&self, height: BlockHeight) -> impl Iterator<Item = &Rc<BlockPtr>> {
        self.tree_candidates
            .values()
            .filter(move |ptr| ptr.height == height)
            .chain(
                self.block_tree
                    .values()
                    .filter(move |ptr| ptr.height == height),
            )
    }

    fn get_finalization_record_by_hash(&self, hash: &HashBytes) -> Option<&FinalizationRecord> {
        self.finalization_list
            .iter()
            .rev() // it's most probable that it's near the end
            .find(|&rec| rec.block_pointer == *hash)
    }

    fn get_finalization_record_by_idx(
        &self,
        idx: FinalizationIndex,
    ) -> Option<&FinalizationRecord> {
        self.finalization_list
            .iter()
            .rev() // it's most probable that it's near the end
            .find(|&rec| rec.index == idx)
    }

    pub fn get_last_finalized_slot(&self) -> Slot { self.last_finalized.block.slot() }

    pub fn get_last_finalized_height(&self) -> BlockHeight { self.last_finalized.height }

    pub fn get_next_finalization_index(&self) -> FinalizationIndex {
        &self.finalization_list.last().unwrap().index + 1 // safe; always available
    }

    fn add_finalization(&mut self, record: FinalizationRecord) -> SkovResult {
        // check for duplicates first, as all bakers broadcast it; we should be ok with
        // a linear search, as we are expecting only to keep the most recent
        // finalization records and the most recent one is always first
        if self.finalization_list.iter().any(|rec| *rec == record) {
            return SkovResult::DuplicateEntry; // don't mind these duplicates, at least not yet
        }

        let housekeeping_hash = record.block_pointer.clone();

        let mut target_block = self.tree_candidates.remove_entry(&record.block_pointer);

        if let Some((_, ref mut block)) = target_block {
            block.status.set(BlockStatus::Finalized);
        } else {
            let error = SkovError::MissingBlockToFinalize(record.block_pointer.clone());

            self.inapplicable_finalization_records.push(record);

            return SkovResult::Error(error);
        }

        let (target_hash, target_block) = target_block.unwrap(); // safe - we've already checked

        self.last_finalized = Rc::clone(&target_block);
        self.block_tree.insert(target_hash, target_block);
        self.finalization_list.push(record);

        // prune the tree candidate queue, as some of the blocks can probably be dropped
        // now
        self.prune_candidate_list();

        // a new finalization record was registered; check for any blocks pending their
        // last finalized block's finalization
        self.refresh_pending_queue(AwaitingLastFinalizedFinalization, &housekeeping_hash);

        SkovResult::SuccessfulEntry
    }

    fn pending_queue_ref(&self, queue: PendingQueueType) -> &PendingQueue {
        match queue {
            AwaitingParentBlock => &self.awaiting_parent_block,
            AwaitingLastFinalizedBlock => &self.awaiting_last_finalized_block,
            AwaitingLastFinalizedFinalization => &self.awaiting_last_finalized_finalization,
        }
    }

    fn pending_queue_mut(&mut self, queue: PendingQueueType) -> &mut PendingQueue {
        match queue {
            AwaitingParentBlock => &mut self.awaiting_parent_block,
            AwaitingLastFinalizedBlock => &mut self.awaiting_last_finalized_block,
            AwaitingLastFinalizedFinalization => &mut self.awaiting_last_finalized_finalization,
        }
    }

    fn queue_pending_block(
        &mut self,
        queue: PendingQueueType,
        missing_entry: HashBytes,
        pending_block: PendingBlock,
    ) {
        let queued = self
            .pending_queue_mut(queue)
            .entry(missing_entry)
            .or_default();

        queued.push(pending_block);
    }

    fn refresh_pending_queue(&mut self, queue: PendingQueueType, target_hash: &HashBytes) {
        if let Some(affected_blocks) = self.pending_queue_mut(queue).remove(target_hash) {
            for pending_block in affected_blocks {
                debug!("Reattempted to add block {:?} to the tree", target_hash);
                // silence errors here, as it is a housekeeping operation
                let _ = self.add_block(pending_block);
            }
        }
    }

    fn prune_candidate_list(&mut self) {
        let current_height = self.last_finalized.height;

        self.tree_candidates
            .retain(|_, candidate| candidate.height >= current_height)
    }

    fn print_pending_queue(&self, queue: PendingQueueType) -> String {
        if self.pending_queue_ref(queue).is_empty() {
            return String::new();
        }

        // it's heavy debugging at this point; we don't mind reallocating the string
        let mut output = format!("\n{}: [", queue);

        for (missing, affected) in self.pending_queue_ref(queue) {
            output.push_str(&format!("{:?}: [", missing));
            for pending_hash in affected.iter().map(|pb| &pb.hash) {
                output.push_str(&format!("{:?}, ", pending_hash));
            }
            output.truncate(output.len() - 2);
            output.push_str("], ");
        }
        output.truncate(output.len() - 2);
        output.push_str("]");

        output
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PendingQueueType {
    AwaitingParentBlock,
    AwaitingLastFinalizedBlock,
    AwaitingLastFinalizedFinalization,
}

impl fmt::Display for PendingQueueType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match *self {
            AwaitingParentBlock => "awaiting parent block",
            AwaitingLastFinalizedBlock => "awaiting last finalized block",
            AwaitingLastFinalizedFinalization => "awaiting last finalized finalization",
        };

        write!(f, "{}", name)
    }
}

#[derive(Debug)]
pub struct SkovStats {
    block_arrival_times:        CircularQueue<DateTime<Utc>>,
    finalization_times:         CircularQueue<DateTime<Utc>>,
    add_block_timings:          CircularQueue<u64>,
    add_finalization_timings:   CircularQueue<u64>,
    query_block_timings:        CircularQueue<u64>,
    query_finalization_timings: CircularQueue<u64>,
    errors:                     Vec<SkovError>,
}

impl SkovStats {
    fn new(timing_queue_len: usize) -> Self {
        Self {
            block_arrival_times:        CircularQueue::with_capacity(timing_queue_len),
            finalization_times:         CircularQueue::with_capacity(timing_queue_len),
            add_block_timings:          CircularQueue::with_capacity(timing_queue_len),
            add_finalization_timings:   CircularQueue::with_capacity(timing_queue_len),
            query_block_timings:        CircularQueue::with_capacity(timing_queue_len),
            query_finalization_timings: CircularQueue::with_capacity(timing_queue_len),
            errors:                     Vec::with_capacity(1), /* usually just one error appears
                                                                * in the
                                                                * beginning */
        }
    }
}

impl fmt::Display for SkovStats {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn wma(values: impl Iterator<Item = u64>, n: u64) -> u64 {
            let mass: u64 = (0..n).sum();
            if mass == 0 {
                0
            } else {
                let sum = values.enumerate().fold(0, |sum, (i, val)| {
                    let weight = n - (i as u64);
                    sum + val * weight
                });

                sum / mass
            }
        }

        fn get_avg_duration(times: &CircularQueue<DateTime<Utc>>) -> u64 {
            let diffs = times
                .iter()
                .zip(times.iter().skip(1))
                .map(|(&t1, &t2)| t1 - t2)
                .map(|diff| diff.num_milliseconds() as u64);

            wma(diffs, times.len() as u64) / 1000 // get the result in seconds
        }

        write!(
            f,
            "block receipt/entry/query: {}s/{}us/{}us; finalization receipt/entry/query: \
             {}s/{}us/{}us{}",
            get_avg_duration(&self.block_arrival_times),
            wma(
                self.add_block_timings.iter().cloned(),
                self.add_block_timings.len() as u64
            ),
            wma(
                self.query_block_timings.iter().cloned(),
                self.query_block_timings.len() as u64
            ),
            get_avg_duration(&self.finalization_times),
            wma(
                self.add_finalization_timings.iter().cloned(),
                self.add_finalization_timings.len() as u64
            ),
            wma(
                self.query_finalization_timings.iter().cloned(),
                self.query_finalization_timings.len() as u64
            ),
            if !self.errors.is_empty() {
                format!(", {} error(s)", self.errors.len())
            } else {
                "".to_owned()
            },
        )
    }
}
