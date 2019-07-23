use concordium_common::{
    into_err, RelayOrStopEnvelope, RelayOrStopReceiver, RelayOrStopSenderHelper,
    RelayOrStopSyncSender,
};
use failure::Fallible;

#[cfg(test)]
use std::time::Duration;
use std::{
    collections::HashMap,
    sync::{
        atomic::{AtomicBool, AtomicPtr, Ordering},
        mpsc, Arc, Mutex,
    },
};

use crate::ffi::*;
use concordium_global_state::{block::*, tree::ConsensusMessage};

pub type PeerId = u64;
pub type PrivateData = HashMap<i64, Vec<u8>>;

pub struct ConsensusOutQueue {
    receiver_request: Arc<Mutex<RelayOrStopReceiver<ConsensusMessage>>>,
    sender_request:   RelayOrStopSyncSender<ConsensusMessage>,
}

const SYNC_CHANNEL_BOUND: usize = 64;

impl Default for ConsensusOutQueue {
    fn default() -> Self {
        let (sender_request, receiver_request) =
            mpsc::sync_channel::<RelayOrStopEnvelope<ConsensusMessage>>(SYNC_CHANNEL_BOUND);
        ConsensusOutQueue {
            receiver_request: Arc::new(Mutex::new(receiver_request)),
            sender_request,
        }
    }
}

impl ConsensusOutQueue {
    pub fn send_message(&self, message: ConsensusMessage) -> Fallible<()> {
        into_err!(self.sender_request.send_msg(message))
    }

    pub fn recv_message(&self) -> Fallible<RelayOrStopEnvelope<ConsensusMessage>> {
        into_err!(safe_lock!(self.receiver_request).recv())
    }

    pub fn clear(&self) {
        if let Ok(ref mut q) = self.receiver_request.try_lock() {
            debug!(
                "Drained the Consensus request queue for {} element(s)",
                q.try_iter().count()
            );
        }
    }

    pub fn stop(&self) -> Fallible<()> {
        into_err!(self.sender_request.send_stop())?;
        Ok(())
    }
}

lazy_static! {
    pub static ref CALLBACK_QUEUE: ConsensusOutQueue = { ConsensusOutQueue::default() };
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
    pub baker:          Option<BakerId>,
    pub consensus:      Arc<AtomicPtr<consensus_runner>>,
    pub genesis:        Arc<[u8]>,
    pub consensus_type: ConsensusType,
    pub baker_running:  Arc<AtomicBool>,
}

impl ConsensusContainer {
    pub fn new(
        genesis_data: Vec<u8>,
        private_data: Option<Vec<u8>>,
        baker_id: Option<BakerId>,
    ) -> Self {
        info!("Starting up the consensus layer");

        let consensus_type = if private_data.is_some() {
            ConsensusType::Active
        } else {
            ConsensusType::Passive
        };

        let consensus_ptr = get_consensus_ptr(genesis_data.clone(), private_data);

        Self {
            baker: baker_id,
            consensus: Arc::new(AtomicPtr::new(consensus_ptr)),
            genesis: Arc::from(genesis_data),
            consensus_type,
            baker_running: Arc::new(AtomicBool::new(false)),
        }
    }

    pub fn stop(&self) {
        let consensus = self.consensus.load(Ordering::SeqCst);
        unsafe {
            stopBaker(consensus);
            stopConsensus(consensus);
        }
        self.baker_running.store(true, Ordering::SeqCst);
    }

    pub fn start_baker(&self) -> bool {
        if self.consensus_type == ConsensusType::Passive {
            return false;
        }
        if self.baker_running.load(Ordering::SeqCst) {
            return false;
        }

        if self.baker.is_none() {
            return false;
        }
        let consensus = self.consensus.load(Ordering::SeqCst);
        unsafe {
            startBaker(consensus);
        }
        self.baker_running.store(true, Ordering::SeqCst);
        true
    }

    pub fn stop_baker(&self) -> bool {
        if self.consensus_type == ConsensusType::Passive {
            return false;
        }
        if !self.baker_running.load(Ordering::SeqCst) {
            return false;
        }
        let consensus = self.consensus.load(Ordering::SeqCst);
        unsafe {
            stopBaker(consensus);
        }
        CALLBACK_QUEUE.clear();
        true
    }

    pub fn is_baking(&self) -> bool { self.baker_running.load(Ordering::SeqCst) }
}

pub fn catchup_enqueue(request: ConsensusMessage) {
    let request_info = format!("{:?}", request.payload);

    match CALLBACK_QUEUE.send_message(request) {
        Ok(_) => debug!("Queueing a catch-up request: {}", request_info),
        _ => error!("Couldn't queue a catch-up request ({})", request_info),
    }
}
