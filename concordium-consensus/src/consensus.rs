use concordium_common::{
    into_err, RelayOrStopEnvelope, RelayOrStopReceiver, RelayOrStopSenderHelper,
    RelayOrStopSyncSender,
};
use failure::{bail, Fallible};

#[cfg(test)]
use std::time::Duration;
use std::{
    collections::HashMap,
    convert::TryFrom,
    fmt, mem, str,
    sync::{mpsc, Arc, Mutex, RwLock},
    thread, time,
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

#[cfg(test)]
impl ConsensusOutQueue {
    pub fn try_recv_message(self) -> Fallible<RelayOrStopEnvelope<ConsensusMessage>> {
        into_err!(safe_lock!(self.receiver_request).try_recv())
    }

    pub fn recv_timeout_message(
        self,
        timeout: Duration,
    ) -> Fallible<RelayOrStopEnvelope<ConsensusMessage>> {
        into_err!(safe_lock!(self.receiver_request).recv_timeout(timeout))
    }
}

lazy_static! {
    pub static ref CALLBACK_QUEUE: ConsensusOutQueue = { ConsensusOutQueue::default() };
    pub static ref GENERATED_PRIVATE_DATA: RwLock<PrivateData> = { RwLock::new(HashMap::new()) };
    pub static ref GENERATED_GENESIS_DATA: RwLock<Option<Vec<u8>>> = { RwLock::new(None) };
}

#[derive(Clone)]
pub struct ConsensusContainer {
    pub baker:     Option<BakerId>,
    pub consensus: Arc<AtomicPtr<consensus_runner>>,
    pub genesis:   Arc<[u8]>,
}

impl ConsensusContainer {
    pub fn new(genesis_data: Vec<u8>, private_data: Vec<u8>) -> Self {
        info!("Starting up the consensus layer");

        let consensus_ptr = get_consensus_ptr(genesis_data.clone(), private_data);

        Self {
            baker:     None,
            consensus: Arc::new(AtomicPtr::new(consensus_ptr)),
            genesis:   Arc::from(genesis_data),
        }
    }

    pub fn start_baker(&mut self, baker_id: u64) {
        self.baker = Some(baker_id);
        let consensus = self.consensus.load(Ordering::SeqCst);

        unsafe {
            startBaker(consensus);
        }
    }

    pub fn stop(&self) {
        let consensus = self.consensus.load(Ordering::SeqCst);
        unsafe {
            stopBaker(consensus);
        }
        CALLBACK_QUEUE.clear();
    }

    pub fn generate_data(
        genesis_time: u64,
        num_bakers: u64,
        crypto_providers: &str,
        id_providers: &str,
    ) -> Fallible<(Vec<u8>, PrivateData)> {
        if let Ok(ref mut lock) = GENERATED_GENESIS_DATA.write() {
            **lock = None;
        }

        if let Ok(ref mut lock) = GENERATED_PRIVATE_DATA.write() {
            lock.clear();
        }

        let res = unsafe {
            makeGenesisData(
                genesis_time,
                num_bakers,
                std::ffi::CString::new(crypto_providers)
                    .expect("CString::new failed")
                    .as_ptr() as *const u8,
                std::ffi::CString::new(id_providers)
                    .expect("CString::new failed")
                    .as_ptr() as *const u8,
                on_genesis_generated,
                on_private_data_generated,
            )
        };

        match ConsensusFfiResponse::try_from(res) {
            Ok(ConsensusFfiResponse::Success) => {}
            Ok(ConsensusFfiResponse::CryptographicProvidersNotLoaded) => {
                error!("Baker can't start: Couldn't read cryptographic providers file!");
                return Err(failure::Error::from(BakerNotRunning));
            }
            Ok(ConsensusFfiResponse::IdentityProvidersNotLoaded) => {
                error!("Baker can't start: Couldn't read identity providers file!");
                return Err(failure::Error::from(BakerNotRunning));
            }
            _ => unreachable!(),
        }

        for _ in 0..num_bakers {
            if !safe_read!(GENERATED_GENESIS_DATA).is_some()
                || safe_read!(GENERATED_PRIVATE_DATA).len() < num_bakers as usize
            {
                thread::sleep(time::Duration::from_millis(200));
            }
        }

        let genesis_data = match GENERATED_GENESIS_DATA.write() {
            Ok(ref mut genesis) if genesis.is_some() => genesis.take().unwrap(),
            _ => bail!("Didn't get genesis data from Haskell"),
        };

        if let Ok(priv_data) = GENERATED_PRIVATE_DATA.read() {
            if priv_data.len() < num_bakers as usize {
                bail!("Didn't get private data from Haskell");
            } else {
                return Ok((genesis_data, priv_data.clone()));
            }
        } else {
            bail!("Didn't get private data from Haskell");
        }
    }

    pub fn is_baking(&self) -> bool {
        // @TODO Replace with real check
        true
    }
}

pub fn catchup_enqueue(request: ConsensusMessage) {
    let request_info = format!("{:?}", request.payload);

    match CALLBACK_QUEUE.send_message(request) {
        Ok(_) => debug!("Queueing a catch-up request: {}", request_info),
        _ => error!("Couldn't queue a catch-up request ({})", request_info),
    }
}
