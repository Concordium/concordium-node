use byteorder::{ByteOrder, NetworkEndian, WriteBytesExt};
use chrono::prelude::{DateTime, Utc};
use circular_queue::CircularQueue;
use concordium_common::{indexed_vec::IndexedVec, PacketType};
use failure::{format_err, Fallible};
use hash_hasher::{HashBuildHasher, HashedMap, HashedSet};
use linked_hash_map::LinkedHashMap;
use nohash_hasher::IntMap;
use rkv::{Rkv, SingleStore, StoreOptions};

use std::{
    cmp::Ordering,
    convert::TryFrom,
    fmt,
    io::{Cursor, Read},
    mem::{self, size_of},
    rc::Rc,
};

use crate::{
    block::*,
    common::{create_serialization_cursor, SerializeToBytes},
    finalization::*,
    transaction::*,
};

pub type PeerId = u64;

pub mod messaging;

use messaging::{ConsensusMessage, SkovError, SkovResult};

use self::PendingQueueType::*;

/// Holds the global state and related statistics.
pub struct Skov<'a> {
    pub data:          SkovData<'a>,
    pub stats:         SkovStats,
    pub peer_metadata: IntMap<PeerId, SkovMetadata>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SkovState {
    JustStarted = 0,
    FullyCatchingUp,
    PartiallyCatchingUp,
    Complete,
}

impl TryFrom<u8> for SkovState {
    type Error = failure::Error;

    fn try_from(value: u8) -> Fallible<Self> {
        match value {
            0 => Ok(SkovState::JustStarted),
            1 => Ok(SkovState::FullyCatchingUp),
            2 => Ok(SkovState::PartiallyCatchingUp),
            3 => Ok(SkovState::Complete),
            _ => Err(format_err!("Unsupported Skov state value: {}!", value)),
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

impl<'a> Skov<'a> {
    #[doc(hidden)]
    pub fn new(genesis_data: &[u8], kvs_env: &'a Rkv) -> Self {
        const MOVING_AVERAGE_QUEUE_LEN: usize = 16;

        Self {
            data:          SkovData::new(genesis_data, &kvs_env),
            stats:         SkovStats::new(MOVING_AVERAGE_QUEUE_LEN),
            peer_metadata: Default::default(),
        }
    }

    /// Save a Skov error.
    pub fn register_error(&mut self, err: SkovError) {
        warn!("{}", err);
        self.stats.errors.push(err)
    }

    /// Indicate that a catch-up round has commenced and that it must conclude
    /// before any new global state input is accepted.
    pub fn start_catchup_round(&mut self, kind: SkovState) -> SkovResult {
        self.data.state = kind;
        info!("A catch-up round has begun");
        SkovResult::Housekeeping
    }

    /// Indicate that a catch-up round has finished.
    pub fn end_catchup_round(&mut self) -> SkovResult {
        self.data.state = SkovState::Complete;
        self.peer_metadata.clear();
        info!("A catch-up round was successfully completed");
        SkovResult::Housekeeping
    }

    pub fn is_catching_up(&self) -> bool {
        self.state() == SkovState::FullyCatchingUp || self.state() == SkovState::PartiallyCatchingUp
    }

    pub fn is_tree_valid(&self) -> bool {
        self.data.block_tree.len() + self.data.tree_candidates.len() > 1
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

    pub fn state(&self) -> SkovState { self.data.state }

    pub fn delay_broadcast(&mut self, broadcast: ConsensusMessage) {
        self.data.delayed_broadcasts.push(broadcast);
    }

    pub fn get_delayed_broadcasts(&mut self) -> Vec<ConsensusMessage> {
        mem::replace(&mut self.data.delayed_broadcasts, Vec::new())
    }

    pub fn get_metadata(&self) -> SkovResult {
        let metadata = SkovMetadata {
            finalized_height: self.data.get_last_finalized_height(),
            n_pending_blocks: self.data.tree_candidates.len() as u64,
            state:            self.data.state,
        };

        SkovResult::SuccessfulQuery(metadata.serialize())
    }

    pub fn register_peer_metadata(&mut self, peer: u64, meta: SkovMetadata) -> SkovResult {
        self.peer_metadata.insert(peer, meta);
        SkovResult::SuccessfulEntry(PacketType::GlobalStateMetadata)
    }

    pub fn best_metadata(&self) -> SkovResult {
        let best_metadata = self
            .peer_metadata
            .iter()
            .max_by_key(|(_, meta)| *meta)
            .map(|(id, meta)| (id.to_owned(), meta.to_owned()))
            .unwrap(); // infallible

        SkovResult::BestPeer(best_metadata)
    }

    #[doc(hidden)]
    pub fn display_state(&self) {
        info!(
            "Skov data:\nblock tree: {:?}\nlast finalized: {:?}\nfinalization list: {:?}\ntree \
             candidates: {:?}{}{}{}{}\npeer metadata: {:?}\nstatus: {:?}",
            self.data.block_tree.keys().collect::<Vec<_>>(),
            self.data.last_finalized.hash,
            self.data
                .finalization_list
                .iter()
                .filter_map(|e| e.as_ref())
                .map(|rec| &rec.block_pointer)
                .collect::<Vec<_>>(),
            self.data.tree_candidates.keys().collect::<Vec<_>>(),
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
        info!("Skov stats: {}", self.stats);
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
pub struct SkovData<'a> {
    /// the kvs handle
    kvs_env: &'a Rkv,
    /// finalized blocks AKA the blockchain
    pub block_tree: LinkedHashMap<BlockHash, Rc<BlockPtr>, HashBuildHasher>,
    /// persistent storage for finalized blocks
    finalized_blocks: SingleStore,
    /// finalization records; the blocks they point to are in the tree
    pub finalization_list: IndexedVec<FinalizationRecord>,
    /// the genesis block
    genesis_block_ptr: Rc<BlockPtr>,
    /// the last finalized block
    last_finalized: Rc<BlockPtr>,
    /// valid blocks (parent and last finalized blocks are already in Skov)
    /// pending finalization
    pub tree_candidates: LinkedHashMap<BlockHash, Rc<BlockPtr>, HashBuildHasher>,
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
    pub state: SkovState,
}

impl<'a> SkovData<'a> {
    fn new(genesis_data: &[u8], kvs_env: &'a Rkv) -> Self {
        const SKOV_LONG_PREALLOCATION_SIZE: usize = 128;
        const SKOV_SHORT_PREALLOCATION_SIZE: usize = 16;
        const SKOV_ERR_PREALLOCATION_SIZE: usize = 16;

        let genesis_block_ptr = Rc::new(BlockPtr::genesis(genesis_data));

        let mut finalization_list = IndexedVec::with_capacity(SKOV_LONG_PREALLOCATION_SIZE);
        finalization_list.insert(0, FinalizationRecord::genesis(&genesis_block_ptr));

        let finalized_blocks = kvs_env
            .open_single("blocks", StoreOptions::create())
            .unwrap();

        {
            // don't actually persist blocks yet
            let mut kvs_writer = kvs_env.write().unwrap(); // infallible
            finalized_blocks
                .clear(&mut kvs_writer)
                .expect("Can't clear the block store");
        }

        let mut block_tree = LinkedHashMap::with_capacity_and_hasher(
            SKOV_LONG_PREALLOCATION_SIZE,
            HashBuildHasher::default(),
        );
        block_tree.insert(genesis_block_ptr.hash.clone(), genesis_block_ptr);

        let genesis_block_ref = block_tree.values().next().unwrap(); // safe; we just put it there
        let last_finalized = Rc::clone(genesis_block_ref);
        let genesis_block_ptr = Rc::clone(genesis_block_ref);

        let genesis_to_store = Rc::clone(genesis_block_ref);

        let mut skov = Self {
            kvs_env,
            finalized_blocks,
            block_tree,
            finalization_list,
            genesis_block_ptr,
            last_finalized,
            tree_candidates: LinkedHashMap::with_capacity_and_hasher(
                SKOV_SHORT_PREALLOCATION_SIZE,
                HashBuildHasher::default(),
            ),
            awaiting_parent_block: hashed!(HashedMap, SKOV_ERR_PREALLOCATION_SIZE),
            awaiting_last_finalized_finalization: hashed!(HashedMap, SKOV_ERR_PREALLOCATION_SIZE),
            awaiting_last_finalized_block: hashed!(HashedMap, SKOV_ERR_PREALLOCATION_SIZE),
            inapplicable_finalization_records: hashed!(HashedMap, SKOV_ERR_PREALLOCATION_SIZE),
            transaction_table: TransactionTable::default(),
            delayed_broadcasts: Vec::new(),
            state: SkovState::JustStarted,
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
pub struct SkovStats {
    block_arrival_times:        CircularQueue<DateTime<Utc>>,
    finalization_times:         CircularQueue<DateTime<Utc>>,
    add_block_timings:          CircularQueue<u64>,
    add_finalization_timings:   CircularQueue<u64>,
    query_block_timings:        CircularQueue<u64>,
    query_finalization_timings: CircularQueue<u64>,
    errors:                     Vec<SkovError>,
}

type StatsSnapshot = (u64, u64, u64, u64, u64, u64);

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

impl fmt::Display for SkovStats {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SkovMetadata {
    pub finalized_height: BlockHeight,
    pub n_pending_blocks: u64,
    pub state:            SkovState,
}

impl SkovMetadata {
    pub fn is_empty(&self) -> bool { self.finalized_height == 0 && self.n_pending_blocks == 0 }
}

impl PartialOrd for SkovMetadata {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let result = if self.finalized_height != other.finalized_height {
            self.finalized_height.cmp(&other.finalized_height)
        } else {
            self.n_pending_blocks.cmp(&other.n_pending_blocks)
        };

        Some(result)
    }
}

impl Ord for SkovMetadata {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap() // infallible
    }
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for SkovMetadata {
    type Source = &'a [u8];

    fn deserialize(bytes: Self::Source) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let finalized_height = NetworkEndian::read_u64(&read_ty!(&mut cursor, BlockHeight));
        let n_pending_blocks = NetworkEndian::read_u64(&read_ty!(&mut cursor, u64));
        let state = SkovState::try_from(read_const_sized!(&mut cursor, 1)[0])?;

        Ok(SkovMetadata {
            finalized_height,
            n_pending_blocks,
            state,
        })
    }

    fn serialize(&self) -> Box<[u8]> {
        let mut cursor = create_serialization_cursor(size_of::<BlockHeight>() + 8 + 1);

        let _ = cursor.write_u64::<NetworkEndian>(self.finalized_height);
        let _ = cursor.write_u64::<NetworkEndian>(self.n_pending_blocks);
        let _ = cursor.write_u8(self.state as u8);

        cursor.into_inner()
    }
}

pub mod growth;
pub mod query;
