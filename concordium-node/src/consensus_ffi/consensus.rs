use crate::consensus_ffi::{
    blockchain_types::BlockHash,
    ffi::{
        consensus_runner, get_consensus_ptr, startBaker, stopBaker, stopConsensus,
        NotificationHandlers,
    },
    helpers::{QueueReceiver, QueueSyncSender, RelayOrStopSenderHelper},
    messaging::ConsensusMessage,
};
use parking_lot::Condvar;
use std::{
    convert::TryFrom,
    path::Path,
    sync::{
        atomic::{AtomicBool, AtomicPtr, Ordering},
        Arc, Mutex, RwLock,
    },
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ConsensusLogLevel {
    Error = 1,
    Warning,
    Info,
    Debug,
    Trace,
}

impl TryFrom<u8> for ConsensusLogLevel {
    type Error = anyhow::Error;

    fn try_from(value: u8) -> anyhow::Result<Self> {
        Ok(match value {
            1 => Self::Error,
            2 => Self::Warning,
            3 => Self::Info,
            4 => Self::Debug,
            _ => Self::Trace,
        })
    }
}

pub const CONSENSUS_QUEUE_DEPTH_OUT_HI: usize = 8 * 1024;
pub const CONSENSUS_QUEUE_DEPTH_OUT_LO: usize = 16 * 1024;
pub const CONSENSUS_QUEUE_DEPTH_IN_HI: usize = 16 * 1024;
pub const CONSENSUS_QUEUE_DEPTH_IN_LO: usize = 32 * 1024;

pub struct ConsensusInboundQueues {
    pub receiver_high_priority: Mutex<QueueReceiver<ConsensusMessage>>,
    pub sender_high_priority:   QueueSyncSender<ConsensusMessage>,
    pub receiver_low_priority:  Mutex<QueueReceiver<ConsensusMessage>>,
    pub sender_low_priority:    QueueSyncSender<ConsensusMessage>,
    pub signaler:               Condvar,
}

impl Default for ConsensusInboundQueues {
    fn default() -> Self {
        let (sender_high_priority, receiver_high_priority) =
            crossbeam_channel::bounded(CONSENSUS_QUEUE_DEPTH_IN_HI);
        let (sender_low_priority, receiver_low_priority) =
            crossbeam_channel::bounded(CONSENSUS_QUEUE_DEPTH_IN_LO);
        Self {
            receiver_high_priority: Mutex::new(receiver_high_priority),
            sender_high_priority,
            receiver_low_priority: Mutex::new(receiver_low_priority),
            sender_low_priority,
            signaler: Default::default(),
        }
    }
}

pub struct ConsensusOutboundQueues {
    pub receiver_high_priority: Mutex<QueueReceiver<ConsensusMessage>>,
    pub sender_high_priority:   QueueSyncSender<ConsensusMessage>,
    pub receiver_low_priority:  Mutex<QueueReceiver<ConsensusMessage>>,
    pub sender_low_priority:    QueueSyncSender<ConsensusMessage>,
    pub signaler:               Condvar,
}

impl Default for ConsensusOutboundQueues {
    fn default() -> Self {
        let (sender_high_priority, receiver_high_priority) =
            crossbeam_channel::bounded(CONSENSUS_QUEUE_DEPTH_OUT_HI);
        let (sender_low_priority, receiver_low_priority) =
            crossbeam_channel::bounded(CONSENSUS_QUEUE_DEPTH_OUT_LO);
        Self {
            receiver_high_priority: Mutex::new(receiver_high_priority),
            sender_high_priority,
            receiver_low_priority: Mutex::new(receiver_low_priority),
            sender_low_priority,
            signaler: Default::default(),
        }
    }
}

#[derive(Default)]
pub struct ConsensusQueues {
    pub inbound:  ConsensusInboundQueues,
    pub outbound: ConsensusOutboundQueues,
}

impl ConsensusQueues {
    pub fn send_in_high_priority_message(&self, message: ConsensusMessage) -> anyhow::Result<()> {
        self.inbound
            .sender_high_priority
            .send_msg(message)
            .map(|_| {
                self.inbound.signaler.notify_one();
            })
            .map_err(|e| e.into())
    }

    pub fn send_in_low_priority_message(&self, message: ConsensusMessage) -> anyhow::Result<()> {
        self.inbound
            .sender_low_priority
            .send_msg(message)
            .map(|_| {
                self.inbound.signaler.notify_one();
            })
            .map_err(|e| e.into())
    }

    pub fn send_out_message(&self, message: ConsensusMessage) -> anyhow::Result<()> {
        self.outbound
            .sender_low_priority
            .send_msg(message)
            .map(|_| {
                self.outbound.signaler.notify_one();
            })
            .map_err(|e| e.into())
    }

    pub fn send_out_blocking_msg(&self, message: ConsensusMessage) -> anyhow::Result<()> {
        self.outbound
            .sender_high_priority
            .send_blocking_msg(message)
            .map(|_| {
                self.outbound.signaler.notify_one();
            })
            .map_err(|e| e.into())
    }

    pub fn clear(&self) {
        if let Ok(ref mut q) = self.outbound.receiver_low_priority.try_lock() {
            debug!(
                "Drained the Consensus outbound low priority queue for {} element(s)",
                q.try_iter().count()
            );
        }
        if let Ok(ref mut q) = self.outbound.receiver_high_priority.try_lock() {
            debug!(
                "Drained the Consensus outbound high priority queue for {} element(s)",
                q.try_iter().count()
            );
        }
        if let Ok(ref mut q) = self.inbound.receiver_low_priority.try_lock() {
            debug!(
                "Drained the Consensus inbound low priority queue for {} element(s)",
                q.try_iter().count()
            );
        }
        if let Ok(ref mut q) = self.inbound.receiver_high_priority.try_lock() {
            debug!(
                "Drained the Consensus inbound high priority queue for {} element(s)",
                q.try_iter().count()
            );
        }
    }

    pub fn stop(&self) -> anyhow::Result<()> {
        self.outbound.sender_low_priority.send_stop()?;
        self.outbound.sender_high_priority.send_stop()?;
        self.outbound.signaler.notify_one();
        self.inbound.sender_low_priority.send_stop()?;
        self.inbound.sender_high_priority.send_stop()?;
        self.inbound.signaler.notify_one();
        Ok(())
    }
}

lazy_static! {
    pub static ref CALLBACK_QUEUE: ConsensusQueues = ConsensusQueues::default();
}

/// If a consensus instance is
/// - `Active` it is either a baker or a member of the finalization committee
/// - `Passive` it is neither a baker nor a member of the finalization committee
#[derive(Clone, PartialEq)]
pub enum ConsensusType {
    Active,
    Passive,
}

impl std::fmt::Display for ConsensusType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ConsensusType::Active => write!(f, "Active"),
            ConsensusType::Passive => write!(f, "Passive"),
        }
    }
}

#[derive(Clone)]
pub struct ConsensusRuntimeParameters {
    /// Size limit on blocks baked by this node in bytes.
    pub max_block_size:             u64,
    /// Timeout in milliseconds for executing transactions when constructing a
    /// block.
    pub block_construction_timeout: u64,
    pub insertions_before_purging:  u64,
    /// Time in seconds that a transaction is kept before being a candidate for
    /// purging.
    pub transaction_keep_alive:     u64,
    /// Number of seconds between automatic transaction table purging runs.
    pub transactions_purging_delay: u64,
    /// Number of accounts that may be cached in memory.
    pub accounts_cache_size:        u32,
}

#[derive(Clone)]
pub struct ConsensusContainer {
    pub runtime_parameters: ConsensusRuntimeParameters,
    pub is_baking:          Arc<AtomicBool>,
    pub consensus:          Arc<AtomicPtr<consensus_runner>>,
    pub genesis:            Arc<[u8]>,
    pub consensus_type:     ConsensusType,
}

impl ConsensusContainer {
    pub fn new(
        runtime_parameters: ConsensusRuntimeParameters,
        genesis_data: Vec<u8>,
        private_data: Option<Vec<u8>>,
        max_log_level: ConsensusLogLevel,
        appdata_dir: &Path,
        regenesis_arc: Arc<Regenesis>,
    ) -> anyhow::Result<(Self, NotificationHandlers)> {
        info!("Starting up the consensus layer");

        let consensus_type = if private_data.is_some() {
            ConsensusType::Active
        } else {
            ConsensusType::Passive
        };

        match get_consensus_ptr(
            &runtime_parameters,
            genesis_data.clone(),
            private_data,
            max_log_level,
            appdata_dir,
            regenesis_arc,
        ) {
            Ok((consensus_ptr, notification_handlers)) => Ok((
                Self {
                    runtime_parameters,
                    is_baking: Arc::new(AtomicBool::new(false)),
                    consensus: Arc::new(AtomicPtr::new(consensus_ptr)),
                    genesis: Arc::from(genesis_data),
                    consensus_type,
                },
                notification_handlers,
            )),
            Err(e) => Err(e),
        }
    }

    pub fn stop(&self) {
        self.stop_baker();
        let consensus = self.consensus.load(Ordering::SeqCst);
        unsafe {
            stopConsensus(consensus);
        }
        CALLBACK_QUEUE.clear();
    }

    pub fn start_baker(&self) -> bool {
        if !self.is_active() || self.is_baking() {
            return false;
        }

        info!("Commencing baking");

        let consensus = self.consensus.load(Ordering::SeqCst);
        unsafe {
            startBaker(consensus);
        }
        self.is_baking.store(true, Ordering::SeqCst);

        true
    }

    pub fn stop_baker(&self) -> bool {
        if !self.is_active() || !self.is_baking() {
            return false;
        }

        info!("Stopping baking");

        let consensus = self.consensus.load(Ordering::SeqCst);
        unsafe {
            stopBaker(consensus);
        }
        self.is_baking.store(false, Ordering::SeqCst);

        true
    }

    pub fn is_baking(&self) -> bool { self.is_baking.load(Ordering::SeqCst) }

    pub fn is_active(&self) -> bool { self.consensus_type == ConsensusType::Active }
}

/// A Regenesis object consists of a list of the genesis block hashes and a flag
/// that is set when regenesis occurs to trigger catch-up with peers.
pub struct Regenesis {
    /// The list of genesis block hashes, in order of increasing genesis index.
    pub blocks:          RwLock<Vec<BlockHash>>,
    /// A flag that is set to indicate that peers should be added to the
    /// catch-up queue.
    pub trigger_catchup: AtomicBool,
    /// A flag that is set to indicate to drop all network connections.
    /// Triggered on an unrecognized protocol update.
    pub stop_network:    AtomicBool,
}

impl Default for Regenesis {
    fn default() -> Self {
        Regenesis {
            blocks:          RwLock::new(vec![]),
            trigger_catchup: AtomicBool::new(false),
            stop_network:    AtomicBool::new(false),
        }
    }
}

impl Regenesis {
    pub fn from_blocks(regenesis_blocks: Vec<BlockHash>) -> Self {
        Regenesis {
            blocks:          RwLock::new(regenesis_blocks),
            trigger_catchup: AtomicBool::new(false),
            stop_network:    AtomicBool::new(false),
        }
    }
}
