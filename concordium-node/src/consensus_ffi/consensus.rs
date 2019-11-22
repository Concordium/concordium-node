use concordium_common::{
    blockchain_types::BakerId, into_err, QueueReceiver, QueueSyncSender, RelayOrStopSenderHelper,
};
use failure::Fallible;

use std::{
    collections::HashMap,
    convert::TryFrom,
    sync::{
        atomic::{AtomicBool, AtomicPtr, Ordering},
        Arc, Mutex,
    },
};

use crossbeam_channel;

use parking_lot::Condvar;

use crate::ffi::*;
use globalstate_rust::tree::{messaging::ConsensusMessage, GlobalState};

pub type PeerId = u64;
pub type PrivateData = HashMap<i64, Vec<u8>>;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ConsensusLogLevel {
    Error = 1,
    Warning,
    Info,
    Debug,
    Trace,
}

impl TryFrom<u8> for ConsensusLogLevel {
    type Error = failure::Error;

    fn try_from(value: u8) -> Fallible<Self> {
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
    pub signaler:               Arc<Condvar>,
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
    pub signaler:               Arc<Condvar>,
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

pub struct ConsensusQueues {
    pub inbound:                ConsensusInboundQueues,
    pub outbound:               ConsensusOutboundQueues,
    pub receiver_peer_notifier: Mutex<QueueReceiver<()>>,
    pub sender_peer_notifier:   QueueSyncSender<()>,
}

impl Default for ConsensusQueues {
    fn default() -> Self {
        let (sender_peer_notifier, receiver_peer_notifier) = crossbeam_channel::bounded(100);
        Self {
            inbound: Default::default(),
            outbound: Default::default(),
            receiver_peer_notifier: Mutex::new(receiver_peer_notifier),
            sender_peer_notifier,
        }
    }
}

impl ConsensusQueues {
    pub fn send_in_high_priority_message(&self, message: ConsensusMessage) -> Fallible<()> {
        self.inbound
            .sender_high_priority
            .send_msg(message)
            .map(|_| {
                self.inbound.signaler.notify_one();
            })
            .map_err(|e| e.into())
    }

    pub fn send_in_low_priority_message(&self, message: ConsensusMessage) -> Fallible<()> {
        self.inbound
            .sender_low_priority
            .send_msg(message)
            .map(|_| {
                self.inbound.signaler.notify_one();
            })
            .map_err(|e| e.into())
    }

    pub fn send_out_message(&self, message: ConsensusMessage) -> Fallible<()> {
        self.outbound
            .sender_low_priority
            .send_msg(message)
            .map(|_| {
                self.outbound.signaler.notify_one();
            })
            .map_err(|e| e.into())
    }

    pub fn send_out_blocking_msg(&self, message: ConsensusMessage) -> Fallible<()> {
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
        if let Ok(ref mut q) = self.receiver_peer_notifier.try_lock() {
            debug!(
                "Drained the Consensus peers notification control queue for {} element(s)",
                q.try_iter().count()
            );
        }
    }

    pub fn stop(&self) -> Fallible<()> {
        into_err!(self.outbound.sender_low_priority.send_stop())?;
        into_err!(self.outbound.sender_high_priority.send_stop())?;
        into_err!(self.inbound.sender_low_priority.send_stop())?;
        into_err!(self.inbound.sender_high_priority.send_stop())?;
        into_err!(self.sender_peer_notifier.send_stop())?;
        Ok(())
    }
}

lazy_static! {
    pub static ref CALLBACK_QUEUE: ConsensusQueues = { ConsensusQueues::default() };
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
pub struct ConsensusContainer {
    pub max_block_size: u64,
    pub baker_id:       Option<BakerId>,
    pub is_baking:      Arc<AtomicBool>,
    pub consensus:      Arc<AtomicPtr<consensus_runner>>,
    pub genesis:        Arc<[u8]>,
    pub consensus_type: ConsensusType,
}

impl ConsensusContainer {
    pub fn new(
        max_block_size: u64,
        enable_transfer_logging: bool,
        genesis_data: Vec<u8>,
        private_data: Option<Vec<u8>>,
        baker_id: Option<BakerId>,
        gsptr: GlobalState,
        max_log_level: ConsensusLogLevel,
    ) -> Fallible<Self> {
        info!("Starting up the consensus layer");

        let consensus_type = if private_data.is_some() {
            ConsensusType::Active
        } else {
            ConsensusType::Passive
        };

        match get_consensus_ptr(
            max_block_size,
            enable_transfer_logging,
            genesis_data.clone(),
            private_data,
            gsptr,
            max_log_level,
        ) {
            Ok(consensus_ptr) => Ok(Self {
                max_block_size,
                baker_id,
                is_baking: Arc::new(AtomicBool::new(false)),
                consensus: Arc::new(AtomicPtr::new(consensus_ptr)),
                genesis: Arc::from(genesis_data),
                consensus_type,
            }),
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
