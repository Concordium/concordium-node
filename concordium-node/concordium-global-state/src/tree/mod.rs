use chrono::prelude::{DateTime, Utc};
use circular_queue::CircularQueue;
use concordium_common::blockchain_types::BlockHash;
use hash_hasher::{HashBuildHasher, HashedMap};
use linked_hash_map::LinkedHashMap;
use rkv::{Rkv, SingleStore, StoreOptions};

use std::{
    fmt, mem,
    sync::{Arc, RwLock},
};

use crate::{block::*, finalization::*, transaction::*};

pub mod messaging;

use messaging::{ConsensusMessage, GlobalStateError};

use self::PendingQueueType::*;

/// Holds the global state and related statistics.
#[repr(C)]
pub struct GlobalState {
    pub data:  GlobalData,
    pub stats: GlobalStats,
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

impl GlobalState {
    #[doc(hidden)]
    pub fn new(genesis_data: &[u8], kvs: Arc<RwLock<Rkv>>, persistent: bool) -> Self {
        const MOVING_AVERAGE_QUEUE_LEN: usize = 16;

        Self {
            data:  GlobalData::new(genesis_data, kvs, persistent),
            stats: GlobalStats::new(MOVING_AVERAGE_QUEUE_LEN),
        }
    }

    /// Save a GlobalState error.
    pub fn register_error(&mut self, err: GlobalStateError) {
        debug!("{}", err);
        self.stats.errors.push(err)
    }

    pub fn delay_broadcast(&mut self, broadcast: ConsensusMessage) {
        self.data.delayed_broadcasts.push(broadcast);
    }

    pub fn finalization_span(&self) -> u64 { self.data.finalization_span() }

    pub fn get_delayed_broadcasts(&mut self) -> Vec<ConsensusMessage> {
        mem::replace(&mut self.data.delayed_broadcasts, Vec::new())
    }

    #[doc(hidden)]
    pub fn display_stats(&self) {
        info!("GlobalState stats: {}", self.stats);
    }
}

/// Holds the global state objects.
#[allow(dead_code)]
#[repr(C)]
pub struct GlobalData {
    /// the kvs handle
    pub kvs_env: Arc<RwLock<Rkv>>,
    /// persistent storage for finalized blocks
    pub finalized_block_store: SingleStore,
    /// finalization records; the blocks they point to are in the tree
    pub finalization_records_store: SingleStore,
    /// the genesis block
    pub genesis_block_ptr: Arc<BlockPtr>,
    /// the last finalized block
    pub last_finalized: Arc<BlockPtr>,
    /// the last finalization record
    pub last_finalization_record: FinalizationRecord,
    /// valid blocks (parent and last finalized blocks are already in
    /// GlobalState) pending finalization
    pub live_blocks: LinkedHashMap<BlockHash, Arc<BlockPtr>, HashBuildHasher>,
    /// blocks waiting to be included ion the tree
    pending_blocks: HashedMap<BlockHash, Arc<PendingBlock>>,
    /// contains transactions
    transaction_table: TransactionTable,
    /// incoming broacasts rejected during a catch-up round
    delayed_broadcasts: Vec<ConsensusMessage>,
}

impl GlobalData {
    fn new(genesis_data: &[u8], kvs_env: Arc<RwLock<Rkv>>, persistent: bool) -> Self {
        const GS_SHORT_PREALLOCATION_SIZE: usize = 16;
        const GS_ERR_PREALLOCATION_SIZE: usize = 16;

        let genesis_block_ptr = Arc::new(
            BlockPtr::genesis(genesis_data).expect("Can't create the genesis data BlockPtr"),
        );

        let last_finalization_record = FinalizationRecord::genesis(&genesis_block_ptr);

        let finalized_block_store = {
            kvs_env
                .read()
                .unwrap()
                .open_single("blocks", StoreOptions::create())
                .expect("Couldn't open kvs store when reading the finalized block store")
        };

        let finalization_records_store = {
            kvs_env
                .read()
                .unwrap()
                .open_single("records", StoreOptions::create())
                .expect("Couldn't open kvs store when reading the finalized record store")
        };

        if !persistent {
            let kvs = kvs_env.write().unwrap();
            let mut kvs_writer = kvs
                .write()
                .expect("Couldn't open kvs store when cleaning up the storage");
            finalized_block_store
                .clear(&mut kvs_writer)
                .map_err(|err| panic!("Can't clear the block store due to {}", err))
                .ok();
        }

        let last_finalized = genesis_block_ptr.clone();

        Self {
            kvs_env,
            finalized_block_store,
            finalization_records_store,
            genesis_block_ptr: genesis_block_ptr.clone(),
            last_finalized,
            last_finalization_record,
            live_blocks: LinkedHashMap::with_capacity_and_hasher(
                GS_SHORT_PREALLOCATION_SIZE,
                HashBuildHasher::default(),
            ),
            pending_blocks: hashed!(HashedMap, GS_ERR_PREALLOCATION_SIZE),
            transaction_table: TransactionTable::default(),
            delayed_broadcasts: Vec::new(),
        }
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
