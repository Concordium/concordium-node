use chrono::prelude::{DateTime, Utc};
use circular_queue::CircularQueue;
use concordium_common::{indexed_vec::IndexedVec, PacketType};
use failure::{format_err, Fallible};
use hash_hasher::{HashBuildHasher, HashedMap, HashedSet};
use linked_hash_map::LinkedHashMap;
use nohash_hasher::IntMap;
use rkv::{Rkv, SingleStore, StoreOptions};

use std::{convert::TryFrom, fmt, mem, rc::Rc};

use crate::{block::*, finalization::*, transaction::*};

pub type PeerId = u64;

pub mod messaging;

use messaging::{ConsensusMessage, GlobalMetadata, GlobalStateError, GlobalStateResult};

use self::PendingQueueType::*;

/// Holds the global state and related statistics.
pub struct GlobalState<'a> {
    pub data:          GlobalData<'a>,
    pub stats:         GlobalStats,
    pub peer_metadata: IntMap<PeerId, GlobalMetadata>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ProcessingState {
    JustStarted = 0,
    FullyCatchingUp,
    PartiallyCatchingUp,
    Complete,
}

impl TryFrom<u8> for ProcessingState {
    type Error = failure::Error;

    fn try_from(value: u8) -> Fallible<Self> {
        match value {
            0 => Ok(ProcessingState::JustStarted),
            1 => Ok(ProcessingState::FullyCatchingUp),
            2 => Ok(ProcessingState::PartiallyCatchingUp),
            3 => Ok(ProcessingState::Complete),
            _ => Err(format_err!(
                "Unsupported GlobalState state value: {}!",
                value
            )),
        }
    }
}

/// Returns a result of an operation and its duration.
#[macro_export]
macro_rules! timed {
    ($operation:expr) => {{
        let timestamp_start = Utc::now();
        let result = $operation;
        let timestamp_end = Utc::now();

        (
            result,
            (timestamp_end - timestamp_start)
                .num_microseconds()
                .unwrap_or(0),
        )
    }};
}

impl<'a> GlobalState<'a> {
    #[doc(hidden)]
    pub fn new(genesis_data: &[u8], kvs_env: &'a Rkv) -> Self {
        const MOVING_AVERAGE_QUEUE_LEN: usize = 16;

        Self {
            data:          GlobalData::new(genesis_data, &kvs_env),
            stats:         GlobalStats::new(MOVING_AVERAGE_QUEUE_LEN),
            peer_metadata: Default::default(),
        }
    }

    /// Save a GlobalState error.
    pub fn register_error(&mut self, err: GlobalStateError) {
        warn!("{}", err);
        self.stats.errors.push(err)
    }

    /// Indicate that a catch-up round has commenced and that it must conclude
    /// before any new global state input is accepted.
    pub fn start_catchup_round(&mut self, kind: ProcessingState) -> GlobalStateResult {
        self.data.state = kind;
        info!("A catch-up round has begun");
        GlobalStateResult::Housekeeping
    }

    /// Indicate that a catch-up round has finished.
    pub fn end_catchup_round(&mut self) -> GlobalStateResult {
        self.data.state = ProcessingState::Complete;
        self.peer_metadata.clear();
        info!("A catch-up round was successfully completed");
        GlobalStateResult::Housekeeping
    }

    pub fn is_catching_up(&self) -> bool {
        self.state() == ProcessingState::FullyCatchingUp
            || self.state() == ProcessingState::PartiallyCatchingUp
    }

    pub fn is_tree_valid(&self) -> bool {
        self.data.finalized_blocks.len() + self.data.live_blocks.len() > 1
            && self.data.pending_queue_ref(AwaitingParentBlock).is_empty()
            && self
                .data
                .pending_queue_ref(AwaitingLastFinalizedBlock)
                .is_empty()
            && self
                .data
                .pending_queue_ref(AwaitingLastFinalizedFinalization)
                .is_empty()
            && self.data.inapplicable_finalization_records.is_empty()
    }

    pub fn state(&self) -> ProcessingState { self.data.state }

    pub fn delay_broadcast(&mut self, broadcast: ConsensusMessage) {
        self.data.delayed_broadcasts.push(broadcast);
    }

    pub fn finalization_span(&self) -> u64 { self.data.finalization_span() }

    pub fn get_delayed_broadcasts(&mut self) -> Vec<ConsensusMessage> {
        mem::replace(&mut self.data.delayed_broadcasts, Vec::new())
    }

    pub fn is_broadcast_delay_acceptable(&self) -> bool {
        // don't count finalization messages, as they can be numerous and
        // their number is not representative of the duration of the delay
        self.data
            .delayed_broadcasts
            .iter()
            .filter(|msg| msg.variant == PacketType::Block)
            .count()
            <= self.finalization_span() as usize
    }

    pub fn register_peer_metadata(&mut self, peer: u64, meta: GlobalMetadata) -> GlobalStateResult {
        self.peer_metadata.insert(peer, meta);
        GlobalStateResult::SuccessfulEntry(PacketType::GlobalStateMetadata)
    }

    pub fn best_metadata(&self) -> GlobalStateResult {
        let best_metadata = self
            .peer_metadata
            .iter()
            .max_by_key(|(_, meta)| *meta)
            .map(|(id, meta)| (id.to_owned(), meta.to_owned()))
            .unwrap(); // infallible

        GlobalStateResult::BestPeer(best_metadata)
    }

    pub fn iter_tree_since(
        &self,
        since: BlockHeight,
    ) -> impl Iterator<Item = (&Block, Option<&FinalizationRecord>)> {
        self.data.iter_tree_since(since)
    }

    #[doc(hidden)]
    pub fn display_state(&self) {
        info!(
            "GlobalState data:\nblock tree: {:?}\nlast finalized: {:?}\nfinalization list: \
             {:?}\ntree candidates: {:?}{}{}{}{}\npeer metadata: {:?}\nstatus: {:?}",
            self.data.finalized_blocks.keys().collect::<Vec<_>>(),
            self.data.last_finalized.hash,
            self.data
                .finalization_records
                .iter()
                .filter_map(|e| e.as_ref())
                .map(|rec| &rec.block_pointer)
                .collect::<Vec<_>>(),
            self.data.live_blocks.keys().collect::<Vec<_>>(),
            self.data.print_inapplicable_finalizations(),
            self.data.print_pending_queue(AwaitingParentBlock),
            self.data.print_pending_queue(AwaitingLastFinalizedBlock),
            self.data
                .print_pending_queue(AwaitingLastFinalizedFinalization),
            self.peer_metadata,
            self.data.state,
        );
    }

    #[doc(hidden)]
    pub fn display_stats(&self) {
        info!("GlobalState stats: {}", self.stats);
    }
}

/// An alias used to represent queues for blocks that are not yet applicable to
/// the tree.
///
/// The key is the missing block's hash and the values are affected pending
/// blocks.
type PendingQueue = HashedMap<BlockHash, HashedSet<PendingBlock>>;

/// Holds the global state objects.
#[allow(dead_code)]
pub struct GlobalData<'a> {
    /// the kvs handle
    kvs_env: &'a Rkv,
    /// finalized blocks AKA the blockchain
    pub finalized_blocks: LinkedHashMap<BlockHash, Rc<BlockPtr>, HashBuildHasher>,
    /// persistent storage for finalized blocks
    finalized_block_store: SingleStore,
    /// finalization records; the blocks they point to are in the tree
    pub finalization_records: IndexedVec<FinalizationRecord>,
    /// the genesis block
    pub genesis_block_ptr: Rc<BlockPtr>,
    /// the last finalized block
    pub last_finalized: Rc<BlockPtr>,
    /// valid blocks (parent and last finalized blocks are already in
    /// GlobalState) pending finalization
    pub live_blocks: LinkedHashMap<BlockHash, Rc<BlockPtr>, HashBuildHasher>,
    /// blocks waiting for their parent to be added to the tree
    awaiting_parent_block: PendingQueue,
    /// blocks waiting for their last finalized block to actually be finalized
    awaiting_last_finalized_finalization: PendingQueue,
    /// blocks waiting for their last finalized block to be included in the tree
    awaiting_last_finalized_block: PendingQueue,
    /// finalization records that point to blocks not present in the tree
    inapplicable_finalization_records: HashedMap<BlockHash, FinalizationRecord>,
    /// contains transactions
    transaction_table: TransactionTable,
    /// incoming broacasts rejected during a catch-up round
    delayed_broadcasts: Vec<ConsensusMessage>,
    /// the current processing state (catching up etc.)
    pub state: ProcessingState,
}

impl<'a> GlobalData<'a> {
    fn new(genesis_data: &[u8], kvs_env: &'a Rkv) -> Self {
        const GS_LONG_PREALLOCATION_SIZE: usize = 128;
        const GS_SHORT_PREALLOCATION_SIZE: usize = 16;
        const GS_ERR_PREALLOCATION_SIZE: usize = 16;

        let genesis_block_ptr = Rc::new(BlockPtr::genesis(genesis_data));

        let mut finalization_records = IndexedVec::with_capacity(GS_LONG_PREALLOCATION_SIZE);
        finalization_records.insert(0, FinalizationRecord::genesis(&genesis_block_ptr));

        let finalized_block_store = kvs_env
            .open_single("blocks", StoreOptions::create())
            .unwrap();

        {
            // don't actually persist blocks yet
            let mut kvs_writer = kvs_env.write().unwrap(); // infallible
            finalized_block_store
                .clear(&mut kvs_writer)
                .expect("Can't clear the block store");
        }

        let mut finalized_blocks = LinkedHashMap::with_capacity_and_hasher(
            GS_LONG_PREALLOCATION_SIZE,
            HashBuildHasher::default(),
        );
        finalized_blocks.insert(genesis_block_ptr.hash.clone(), genesis_block_ptr);

        let genesis_block_ref = finalized_blocks.values().next().unwrap(); // safe; we just put it there
        let last_finalized = Rc::clone(genesis_block_ref);
        let genesis_block_ptr = Rc::clone(genesis_block_ref);

        let genesis_to_store = Rc::clone(genesis_block_ref);

        let mut skov = Self {
            kvs_env,
            finalized_block_store,
            finalized_blocks,
            finalization_records,
            genesis_block_ptr,
            last_finalized,
            live_blocks: LinkedHashMap::with_capacity_and_hasher(
                GS_SHORT_PREALLOCATION_SIZE,
                HashBuildHasher::default(),
            ),
            awaiting_parent_block: hashed!(HashedMap, GS_ERR_PREALLOCATION_SIZE),
            awaiting_last_finalized_finalization: hashed!(HashedMap, GS_ERR_PREALLOCATION_SIZE),
            awaiting_last_finalized_block: hashed!(HashedMap, GS_ERR_PREALLOCATION_SIZE),
            inapplicable_finalization_records: hashed!(HashedMap, GS_ERR_PREALLOCATION_SIZE),
            transaction_table: TransactionTable::default(),
            delayed_broadcasts: Vec::new(),
            state: ProcessingState::JustStarted,
        };

        // store the genesis block
        skov.store_block(&genesis_to_store);

        skov
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

    fn print_inapplicable_finalizations(&self) -> String {
        if !self.inapplicable_finalization_records.is_empty() {
            format!(
                "\ninapplicable finalization records: {:?}",
                self.inapplicable_finalization_records
                    .keys()
                    .collect::<Vec<_>>()
            )
        } else {
            String::new()
        }
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
pub struct GlobalStats {
    block_arrival_times:        CircularQueue<DateTime<Utc>>,
    finalization_times:         CircularQueue<DateTime<Utc>>,
    add_block_timings:          CircularQueue<u64>,
    add_finalization_timings:   CircularQueue<u64>,
    query_block_timings:        CircularQueue<u64>,
    query_finalization_timings: CircularQueue<u64>,
    errors:                     Vec<GlobalStateError>,
}

type StatsSnapshot = (u64, u64, u64, u64, u64, u64);

impl GlobalStats {
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

    pub fn query_stats(&self) -> StatsSnapshot {
        (
            get_avg_duration(&self.block_arrival_times),
            wma(
                self.add_block_timings.iter().cloned(),
                self.add_block_timings.len() as u64,
            ),
            wma(
                self.query_block_timings.iter().cloned(),
                self.query_block_timings.len() as u64,
            ),
            get_avg_duration(&self.finalization_times),
            wma(
                self.add_finalization_timings.iter().cloned(),
                self.add_finalization_timings.len() as u64,
            ),
            wma(
                self.query_finalization_timings.iter().cloned(),
                self.query_finalization_timings.len() as u64,
            ),
        )
    }
}

fn wma(values: impl Iterator<Item = u64>, n: u64) -> u64 {
    if n == 0 {
        return 0;
    }

    let mass: u64 = (1..=n).sum();
    let sum = values.enumerate().fold(0, |sum, (i, val)| {
        let weight = n - (i as u64);
        sum + val * weight
    });

    sum / mass
}

fn get_avg_duration(times: &CircularQueue<DateTime<Utc>>) -> u64 {
    let diffs = times
        .iter()
        .zip(times.iter().skip(1))
        .map(|(&t1, &t2)| t1 - t2)
        .map(|diff| diff.num_milliseconds() as u64);

    wma(diffs, times.len() as u64) / 1000 // milliseconds to seconds
}

impl fmt::Display for GlobalStats {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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

pub mod growth;
pub mod query;
