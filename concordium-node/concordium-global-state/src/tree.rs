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
/// The type of messages passed in the Skov request channel.
///
/// It contains an optional identifier of the source peer if it is not our own consensus layer.
pub struct SkovReq {
    pub source: Option<u64>, // PeerId
    pub body:   SkovReqBody,
}

impl SkovReq {
    pub fn new(source: Option<u64>, body: SkovReqBody) -> Self { Self { source, body } }
}

#[derive(Debug)]
/// Carries a specific Skov request.
///
/// Each variant has a corresponding handler function that either introduces a new object
/// to Skov or queries the global state for it.
pub enum SkovReqBody {
    AddBlock(PendingBlock),
    AddFinalizationRecord(FinalizationRecord),
    GetBlock(HashBytes, Delta),
    GetFinalizationRecordByHash(HashBytes),
    GetFinalizationRecordByIdx(FinalizationIndex),
}

#[derive(Debug)]
/// Holds a response for a request to Skov.
///
/// Depending on the request, the result can either be just a status or contain the requested
/// data.
pub enum SkovResult {
    SuccessfulEntry,
    SuccessfulQuery(Box<[u8]>),
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
/// Indicates an erroneous result of a request to Skov.
///
/// If there are two components, the first one is the target and the second is
/// the source
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

/// Holds the global state and related statistics.
pub struct Skov {
    pub data:  SkovData,
    pub stats: SkovStats,
}

/// Returns a result of an operation and its duration.
macro_rules! timed {
    ($operation:expr) => {{
        let timestamp_start = Utc::now();
        let result = $operation;
        let timestamp_end = Utc::now();

        (result, (timestamp_end - timestamp_start).num_microseconds().unwrap_or(0))
    }};
}

/// Creates a function to add a new object to Skov.
macro_rules! add_entry {
    ($(#[$doc:meta])*$entry_foo:ident, $entry_type:ty, $addition_stat:ident, $timestamp_stat:ident) => {
        $(#[$doc])*
        pub fn $entry_foo(&mut self, entry: $entry_type) -> SkovResult {
            let timestamp_entry = Utc::now();
            let (result, addition_duration) = timed!(self.data.$entry_foo(entry));

            self.stats.$addition_stat.push(addition_duration as u64);

            if let SkovResult::SuccessfulEntry = result {
                self.stats.$timestamp_stat.push(timestamp_entry);
            };

            result
        }
    };
}

/// Creates a function to obtain an object from Skov.
macro_rules! get_object {
    ($(#[$doc:meta])*$query_foo:ident($($query_arg:ident: $query_arg_ty:ty),+), $err_kind:ident, $query_stat:ident) => {
        $(#[$doc])*
        pub fn $query_foo(&mut self, $($query_arg: $query_arg_ty),+) -> SkovResult {
            let (result, query_duration) = timed!(self.data.$query_foo($($query_arg),+));

            self.stats.$query_stat.push(query_duration as u64);

            if let Some(result) = result {
                SkovResult::SuccessfulQuery(result.serialize())
            } else {
                SkovResult::Error(SkovError::$err_kind($($query_arg.to_owned()),+))
            }
        }
    }
}

impl Skov {
    add_entry!(
        #[doc="Attempts to include a `PendingBlock` in the block tree candidate queue.\n\n\
            This operation can succeed only if the parent block and last finalized block \
            of the candidate entry are already in the tree. Successful inclusion can result \
            in existing pending blocks being promoted to the candidate queue as well."]
        add_block,
        PendingBlock,
        add_block_timings,
        block_arrival_times
    );

    add_entry!(
        #[doc="Attempts to finalize a block referenced by the given `FinalizationRecord`.\n\n\
            A successful finalization results in the target block being moved from the tree \
            candidate queue to the block tree; it is also followed by a housekeeping operation \
            that involves checking whether any existing tree candidates are not waiting \
            for that block to be finalized."]
        add_finalization,
        FinalizationRecord,
        add_finalization_timings,
        finalization_times
    );

    get_object!(
        #[doc="Queries Skov for a block with the given hash or, if the given `Delta` is \
            greater than zero, for its descendant that is `delta` generations below it."]
        get_block(hash: &HashBytes, delta: Delta),
        MissingBlock,
        query_block_timings
    );

    get_object!(
        #[doc="Queries Skov for the finalization record of a block with the given hash."]
        get_finalization_record_by_hash(hash: &HashBytes),
        MissingFinalizationRecordByHash,
        query_finalization_timings
    );

    get_object!(
        #[doc="Queries Skov for the finalization record of the given finalization round index."]
        get_finalization_record_by_idx(idx: FinalizationIndex),
        MissingFinalizationRecordByIdx,
        query_finalization_timings
    );

    #[doc(hidden)]
    pub fn new(genesis_data: &[u8]) -> Self {
        Self {
            data:  SkovData::new(genesis_data),
            stats: SkovStats::new(16),
        }
    }

    /// Save a Skov error.
    pub fn register_error(&mut self, err: SkovError) { self.stats.errors.push(err) }

    #[doc(hidden)]
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

    #[doc(hidden)]
    pub fn display_stats(&self) {
        info!("Skov stats: {}", self.stats);
    }
}

/// An alias used to represent queues for blocks that are not yet applicable to the tree.
///
/// The key is the missing block's hash and the values are affected pending blocks.
type PendingQueue = HashMap<BlockHash, Vec<PendingBlock>>;

#[derive(Debug)]
/// Holds the global state objects.
pub struct SkovData {
    /// finalized blocks AKA the blockchain
    block_tree: HashMap<BlockHash, Rc<BlockPtr>>,
    /// finalization records; the blocks they point to are in the tree
    finalization_list: Vec<FinalizationRecord>,
    /// the genesis block
    genesis_block_ptr: Rc<BlockPtr>,
    /// the last finalized block
    last_finalized: Rc<BlockPtr>,
    /// valid blocks (parent and last finalized blocks are already in Skov) pending finalization
    tree_candidates: HashMap<BlockHash, Rc<BlockPtr>>,
    /// blocks waiting for their parent to be added to the tree
    awaiting_parent_block: PendingQueue,
    /// blocks waiting for their last finalized block to actually be finalized
    awaiting_last_finalized_finalization: PendingQueue,
    /// blocks waiting for their last finalized block to be included in the tree
    awaiting_last_finalized_block: PendingQueue,
    /// finalization records that point to blocks not present in the tree
    inapplicable_finalization_records: Vec<FinalizationRecord>,
    /// contains transactions
    transaction_table: TransactionTable,
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
        // verify that the pending block's parent block is among tree candidates
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

        // verify that the pending block's last finalized block is in the block tree
        // (which entails that it had been finalized); if not, check the tree candidate queue
        if self.block_tree.get(&pending_block.block.last_finalized).is_some() {
            // nothing to do here
        } else if self.tree_candidates.get(&pending_block.block.last_finalized).is_some() {
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

        // the block is now in the tree candidate queue; run housekeeping that
        // can possibly promote some other queued pending blocks
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
