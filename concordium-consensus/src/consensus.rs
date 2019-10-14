use concordium_common::{
    blockchain_types::BakerId, into_err, QueueReceiver, QueueSyncSender, RelayOrStopSenderHelper,
};
use failure::Fallible;

use std::{
    collections::HashMap,
    convert::TryFrom,
    sync::{
        atomic::{AtomicBool, AtomicPtr, Ordering},
        mpsc, Arc, Mutex,
    },
};

use crate::ffi::*;
use concordium_global_state::tree::{messaging::ConsensusMessage, GlobalState};

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

const CONSENSUS_QUEUE_DEPTH_HI: usize = 4 * 1024;
const CONSENSUS_QUEUE_DEPTH_LO: usize = 64 * 1024;

pub struct ConsensusQueues {
    pub receiver_hi: Mutex<QueueReceiver<ConsensusMessage>>,
    pub sender_hi:   QueueSyncSender<ConsensusMessage>,
    pub receiver_lo: Mutex<QueueReceiver<ConsensusMessage>>,
    pub sender_lo:   QueueSyncSender<ConsensusMessage>,
}

impl Default for ConsensusQueues {
    fn default() -> Self {
        let (sender_hi, receiver_hi) = mpsc::sync_channel(CONSENSUS_QUEUE_DEPTH_HI);
        let (sender_lo, receiver_lo) = mpsc::sync_channel(CONSENSUS_QUEUE_DEPTH_LO);
        Self {
            receiver_hi: Mutex::new(receiver_hi),
            sender_hi,
            receiver_lo: Mutex::new(receiver_lo),
            sender_lo,
        }
    }
}

impl ConsensusQueues {
    pub fn send_message(&self, message: ConsensusMessage) -> Fallible<()> {
        into_err!(self.sender_lo.send_msg(message))
    }

    pub fn send_blocking_msg(&self, message: ConsensusMessage) -> Fallible<()> {
        into_err!(self.sender_hi.send_blocking_msg(message))
    }

    pub fn clear(&self) {
        if let Ok(ref mut q) = self.receiver_lo.try_lock() {
            debug!(
                "Drained the Consensus low priority queue for {} element(s)",
                q.try_iter().count()
            );
        }
        if let Ok(ref mut q) = self.receiver_hi.try_lock() {
            debug!(
                "Drained the Consensus high priority queue for {} element(s)",
                q.try_iter().count()
            );
        }
    }

    pub fn stop(&self) -> Fallible<()> {
        into_err!(self.sender_lo.send_stop())?;
        into_err!(self.sender_hi.send_stop())?;
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
        gsptr: &GlobalState,
        max_log_level: ConsensusLogLevel,
    ) -> Self {
        info!("Starting up the consensus layer");

        let consensus_type = if private_data.is_some() {
            ConsensusType::Active
        } else {
            ConsensusType::Passive
        };

        let consensus_ptr = get_consensus_ptr(
            max_block_size,
            enable_transfer_logging,
            genesis_data.clone(),
            private_data,
            gsptr,
            max_log_level,
        );

        Self {
            max_block_size,
            baker_id,
            is_baking: Arc::new(AtomicBool::new(false)),
            consensus: Arc::new(AtomicPtr::new(consensus_ptr)),
            genesis: Arc::from(genesis_data),
            consensus_type,
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
