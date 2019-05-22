use concordium_common::{
    into_err, RelayOrStopEnvelope, RelayOrStopReceiver, RelayOrStopSender, RelayOrStopSenderHelper,
};
use failure::{bail, Fallible};

#[cfg(test)]
use std::time::Duration;
use std::{
    collections::{HashMap, HashSet},
    fmt, str,
    sync::{mpsc, Arc, Mutex, RwLock},
    thread, time,
};

use crate::{
    block::*, common::HashBytes, fails::BakerNotRunning, ffi::*, finalization::*, tree::*,
};

pub type PeerId = u64;
pub type Delta = u64;

pub type FinalizationCatchupTuple = (Option<PeerId>, FinalizationMessage);

#[derive(Clone)]
pub struct ConsensusOutQueue {
    receiver_block:               Arc<Mutex<RelayOrStopReceiver<BakedBlock>>>,
    sender_block:                 Arc<Mutex<RelayOrStopSender<BakedBlock>>>,
    receiver_finalization:        Arc<Mutex<RelayOrStopReceiver<FinalizationCatchupTuple>>>,
    sender_finalization:          Arc<Mutex<RelayOrStopSender<FinalizationCatchupTuple>>>,
    receiver_finalization_record: Arc<Mutex<RelayOrStopReceiver<FinalizationRecord>>>,
    sender_finalization_record:   Arc<Mutex<RelayOrStopSender<FinalizationRecord>>>,
    receiver_catchup_queue:       Arc<Mutex<RelayOrStopReceiver<CatchupRequest>>>,
    sender_catchup_queue:         Arc<Mutex<RelayOrStopSender<CatchupRequest>>>,
}

impl Default for ConsensusOutQueue {
    fn default() -> Self {
        let (sender, receiver) = mpsc::channel::<RelayOrStopEnvelope<BakedBlock>>();
        let (sender_finalization, receiver_finalization) =
            mpsc::channel::<RelayOrStopEnvelope<FinalizationCatchupTuple>>();
        let (sender_finalization_record, receiver_finalization_record) =
            mpsc::channel::<RelayOrStopEnvelope<FinalizationRecord>>();
        let (sender_catchup, receiver_catchup) =
            mpsc::channel::<RelayOrStopEnvelope<CatchupRequest>>();
        ConsensusOutQueue {
            receiver_block:               Arc::new(Mutex::new(receiver)),
            sender_block:                 Arc::new(Mutex::new(sender)),
            receiver_finalization:        Arc::new(Mutex::new(receiver_finalization)),
            sender_finalization:          Arc::new(Mutex::new(sender_finalization)),
            receiver_finalization_record: Arc::new(Mutex::new(receiver_finalization_record)),
            sender_finalization_record:   Arc::new(Mutex::new(sender_finalization_record)),
            receiver_catchup_queue:       Arc::new(Mutex::new(receiver_catchup)),
            sender_catchup_queue:         Arc::new(Mutex::new(sender_catchup)),
        }
    }
}

macro_rules! empty_queue {
    ($queue:expr, $name:tt) => {
        if let Ok(ref mut q) = $queue.try_lock() {
            debug!(
                "Drained the queue \"{}\" for {} element(s)",
                $name,
                q.try_iter().count()
            );
        }
    };
}

impl ConsensusOutQueue {
    pub fn send_block(self, block: BakedBlock) -> Fallible<()> {
        into_err!(safe_lock!(self.sender_block).send_msg(block))
    }

    pub fn recv_block(self) -> Fallible<RelayOrStopEnvelope<BakedBlock>> {
        into_err!(safe_lock!(self.receiver_block).recv())
    }

    pub fn try_recv_block(self) -> Fallible<RelayOrStopEnvelope<BakedBlock>> {
        into_err!(safe_lock!(self.receiver_block).try_recv())
    }

    pub fn send_finalization(self, msg: (Option<PeerId>, FinalizationMessage)) -> Fallible<()> {
        into_err!(safe_lock!(self.sender_finalization).send_msg(msg))
    }

    pub fn recv_finalization(
        self,
    ) -> Fallible<RelayOrStopEnvelope<(Option<PeerId>, FinalizationMessage)>> {
        into_err!(safe_lock!(self.receiver_finalization).recv())
    }

    pub fn try_recv_finalization(
        self,
    ) -> Fallible<RelayOrStopEnvelope<(Option<PeerId>, FinalizationMessage)>> {
        into_err!(safe_lock!(self.receiver_finalization).try_recv())
    }

    pub fn send_finalization_record(self, rec: FinalizationRecord) -> Fallible<()> {
        into_err!(safe_lock!(self.sender_finalization_record).send_msg(rec))
    }

    pub fn recv_finalization_record(self) -> Fallible<RelayOrStopEnvelope<FinalizationRecord>> {
        into_err!(safe_lock!(self.receiver_finalization_record).recv())
    }

    pub fn try_recv_finalization_record(self) -> Fallible<RelayOrStopEnvelope<FinalizationRecord>> {
        into_err!(safe_lock!(self.receiver_finalization_record).try_recv())
    }

    pub fn send_catchup(self, rec: CatchupRequest) -> Fallible<()> {
        into_err!(safe_lock!(self.sender_catchup_queue).send_msg(rec))
    }

    pub fn recv_catchup(self) -> Fallible<RelayOrStopEnvelope<CatchupRequest>> {
        into_err!(safe_lock!(self.receiver_catchup_queue).recv())
    }

    pub fn try_recv_catchup(self) -> Fallible<RelayOrStopEnvelope<CatchupRequest>> {
        into_err!(safe_lock!(self.receiver_catchup_queue).try_recv())
    }

    pub fn clear(&self) {
        empty_queue!(self.receiver_block, "Block queue");
        empty_queue!(self.receiver_finalization, "Finalization messages queue");
        empty_queue!(
            self.receiver_finalization_record,
            "Finalization record queue"
        );
        empty_queue!(self.receiver_catchup_queue, "Catch-up queue");
    }

    pub fn stop(&self) -> Fallible<()> {
        into_err!(safe_lock!(self.sender_block).send_stop())?;
        into_err!(safe_lock!(self.sender_finalization).send_stop())?;
        into_err!(safe_lock!(self.sender_finalization_record).send_stop())?;
        into_err!(safe_lock!(self.sender_catchup_queue).send_stop())?;
        Ok(())
    }
}

#[cfg(test)]
impl ConsensusOutQueue {
    pub fn recv_timeout_block(
        self,
        timeout: Duration,
    ) -> Fallible<RelayOrStopEnvelope<BakedBlock>> {
        into_err!(safe_lock!(self.receiver_block).recv_timeout(timeout))
    }
}

macro_rules! baker_running_wrapper {
    ($self:ident, $call:expr) => {{
        safe_read!($self.bakers)
            .values()
            .next()
            .map(|baker| $call(baker))
            .ok_or_else(|| failure::Error::from(BakerNotRunning))
    }};
}

lazy_static! {
    pub static ref CALLBACK_QUEUE: ConsensusOutQueue = { ConsensusOutQueue::default() };
    pub static ref GENERATED_PRIVATE_DATA: RwLock<HashMap<i64, Vec<u8>>> =
        { RwLock::new(HashMap::new()) };
    pub static ref GENERATED_GENESIS_DATA: RwLock<Option<Vec<u8>>> = { RwLock::new(None) };
    pub static ref SKOV_DATA: RwLock<SkovData> = { RwLock::new(SkovData::default()) };
    pub static ref REQUESTED_CATCH_UPS: RwLock<HashSet<CatchupRequest>> =
        { RwLock::new(HashSet::default()) };
}

type PrivateData = HashMap<i64, Vec<u8>>;

#[derive(Clone, Default)]
pub struct ConsensusContainer {
    bakers: Arc<RwLock<HashMap<BakerId, ConsensusBaker>>>,
}

impl ConsensusContainer {
    pub fn start_baker(&mut self, baker_id: u64, genesis_data: Vec<u8>, private_data: Vec<u8>) {
        safe_write!(self.bakers).insert(
            baker_id,
            ConsensusBaker::new(baker_id, genesis_data, private_data),
        );
    }

    pub fn stop_baker(&mut self, baker_id: BakerId) {
        let bakers = &mut safe_write!(self.bakers);

        if CALLBACK_QUEUE.stop().is_err() {
            error!("Some queues couldn't send a stop signal");
        };

        match bakers.get_mut(&baker_id) {
            Some(baker) => baker.stop(),
            None => error!("Can't find baker"),
        }

        bakers.remove(&baker_id);

        if bakers.is_empty() {
            CALLBACK_QUEUE.clear();
        }
    }

    pub fn out_queue(&self) -> ConsensusOutQueue { CALLBACK_QUEUE.clone() }

    pub fn send_block(&self, peer_id: PeerId, block: &BakedBlock) -> i64 {
        // When running tests we need to validate sending packets back, as we have no
        // higher outer processing loop with `Skov` to handle this.
        if cfg!(test) {
            for (id, baker) in safe_read!(self.bakers).iter() {
                if block.baker_id != *id {
                    // We have found a baker to send it to, which didn't also bake the block,
                    // so we'll do an early return at this point with the response code
                    // from consensus.
                    return baker.send_block(peer_id, block);
                }
            }
        } else if let Some((_, baker)) = safe_read!(self.bakers).iter().next() {
            // We have a baker to send it to, so we 'll do an early return at this point
            // with the response code from consensus.
            return baker.send_block(peer_id, block);
        }
        // If we didn't do an early return with the response code from consensus, we
        // emit a -1 to signal we didn't find any baker we could pass this
        // request on to.
        -1
    }

    pub fn send_finalization(&self, peer_id: PeerId, msg: &FinalizationMessage) -> i64 {
        if let Some((_, baker)) = safe_read!(self.bakers).iter().next() {
            baker.send_finalization(peer_id, msg);
        }
        // As consensus doesn't return a status for this call, we assume success, i.e.
        // 0.
        0
    }

    pub fn send_finalization_record(&self, peer_id: PeerId, rec: &FinalizationRecord) -> i64 {
        if let Some((_, baker)) = safe_read!(self.bakers).iter().next() {
            return baker.send_finalization_record(peer_id, rec);
        }
        // If we didn't do an early return with the response code from consensus, we
        // emit a -1 to signal we didn't find any baker we could pass this
        // request on to.
        -1
    }

    pub fn send_transaction(&self, tx: &[u8]) -> i64 {
        if let Some((_, baker)) = safe_read!(self.bakers).iter().next() {
            return baker.send_transaction(tx.to_vec());
        }
        // If we didn't do an early return with the response code from consensus, we
        // emit a -1 to signal we didn't find any baker we could pass this
        // request on to.
        -1
    }

    pub fn generate_data(genesis_time: u64, num_bakers: u64) -> Fallible<(Vec<u8>, PrivateData)> {
        if let Ok(ref mut lock) = GENERATED_GENESIS_DATA.write() {
            **lock = None;
        }

        if let Ok(ref mut lock) = GENERATED_PRIVATE_DATA.write() {
            lock.clear();
        }

        unsafe {
            makeGenesisData(
                genesis_time,
                num_bakers,
                on_genesis_generated,
                on_private_data_generated,
            );
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

    pub fn get_consensus_status(&self) -> Fallible<String> {
        baker_running_wrapper!(self, |baker: &ConsensusBaker| baker.get_consensus_status())
    }

    pub fn get_block_info(&self, block_hash: &str) -> Fallible<String> {
        baker_running_wrapper!(self, |baker: &ConsensusBaker| baker
            .get_block_info(block_hash))
    }

    pub fn get_ancestors(&self, block_hash: &str, amount: u64) -> Fallible<String> {
        baker_running_wrapper!(self, |baker: &ConsensusBaker| baker
            .get_ancestors(block_hash, amount))
    }

    pub fn get_branches(&self) -> Fallible<String> {
        baker_running_wrapper!(self, |baker: &ConsensusBaker| baker.get_branches())
    }

    pub fn get_last_final_account_list(&self) -> Fallible<Vec<u8>> {
        baker_running_wrapper!(self, |baker: &ConsensusBaker| baker
            .get_last_final_account_list())
    }

    pub fn get_last_final_instance_info(&self, block_hash: &[u8]) -> Fallible<Vec<u8>> {
        baker_running_wrapper!(self, |baker: &ConsensusBaker| baker
            .get_last_final_instance_info(block_hash))
    }

    pub fn get_last_final_account_info(&self, block_hash: &[u8]) -> Fallible<Vec<u8>> {
        baker_running_wrapper!(self, |baker: &ConsensusBaker| baker
            .get_last_final_account_info(block_hash))
    }

    pub fn get_last_final_instances(&self) -> Fallible<Vec<u8>> {
        baker_running_wrapper!(self, |baker: &ConsensusBaker| baker
            .get_last_final_instances())
    }

    pub fn get_block(&self, block_hash: &[u8]) -> Fallible<Vec<u8>> {
        baker_running_wrapper!(self, |baker: &ConsensusBaker| baker.get_block(block_hash))
    }

    pub fn get_block_by_delta(&self, block_hash: &[u8], delta: u64) -> Fallible<Vec<u8>> {
        baker_running_wrapper!(self, |baker: &ConsensusBaker| baker
            .get_block_by_delta(block_hash, delta))
    }

    pub fn get_block_finalization(&self, block_hash: &[u8]) -> Fallible<Vec<u8>> {
        baker_running_wrapper!(self, |baker: &ConsensusBaker| baker
            .get_block_finalization(block_hash))
    }

    pub fn get_indexed_finalization(&self, index: u64) -> Fallible<Vec<u8>> {
        baker_running_wrapper!(self, |baker: &ConsensusBaker| baker
            .get_indexed_finalization(index))
    }

    pub fn get_finalization_point(&self) -> Fallible<Vec<u8>> {
        baker_running_wrapper!(self, |baker: &ConsensusBaker| baker
            .get_finalization_point())
    }

    pub fn get_finalization_messages(&self, request: &[u8], peer_id: PeerId) -> Fallible<i64> {
        baker_running_wrapper!(self, |baker: &ConsensusBaker| baker
            .get_finalization_messages(request, peer_id))
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum CatchupRequest {
    BlockByHash(PeerId, HashBytes, Delta),
    FinalizationMessagesByPoint(PeerId, FinalizationMessage),
    FinalizationRecordByHash(PeerId, HashBytes),
    FinalizationRecordByIndex(PeerId, FinalizationIndex),
}

impl fmt::Display for CatchupRequest {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                CatchupRequest::BlockByHash(_, hash, delta) => {
                    format!("block {:?} delta {}", hash, delta)
                }
                CatchupRequest::FinalizationMessagesByPoint(..) => {
                    "finalization messages by point".to_string()
                }
                CatchupRequest::FinalizationRecordByHash(_, hash) => {
                    format!("finalization record of {:?}", hash)
                }
                CatchupRequest::FinalizationRecordByIndex(_, index) => {
                    format!("finalization record by index ({})", index)
                }
            }
        )
    }
}

pub fn catchup_enqueue(request: CatchupRequest) {
    let request_info = format!("{:?}", request);
    debug!("Produced a catch-up request: {}", request_info);

    if let Ok(ref mut old_requests) = REQUESTED_CATCH_UPS.write() {
        if old_requests.insert(request.clone()) {
            match CALLBACK_QUEUE.clone().send_catchup(request) {
                Ok(_) => debug!("Queueing a catch-up request: {}", request_info),
                _ => error!("Didn't queue catch-up request properly"),
            }
        } else {
            error!("Duplicate catch-up request: {}", request_info);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use concordium_common::RelayOrStopEnvelope;
    use std::{
        sync::{Once, ONCE_INIT},
        time::Duration,
    };

    static INIT: Once = ONCE_INIT;

    fn setup() { INIT.call_once(|| env_logger::init()); }

    macro_rules! bakers_test {
        ($genesis_time:expr, $num_bakers:expr, $blocks_num:expr) => {
            let (genesis_data, private_data) =
                ConsensusContainer::generate_data($genesis_time, $num_bakers)
                    .unwrap_or_else(|_| panic!("Couldn't read Haskell data"));
            let mut consensus_container = ConsensusContainer::default();

            for i in 0..$num_bakers {
                &consensus_container.start_baker(
                    i,
                    genesis_data.clone(),
                    private_data.get(&(i as i64)).unwrap().to_vec(),
                );
            }

            let relay_th_guard = Arc::new(RwLock::new(true));
            let _th_guard = Arc::clone(&relay_th_guard);
            let _th_container = consensus_container.clone();
            let _aux_th = thread::spawn(move || loop {
                thread::sleep(Duration::from_millis(1_000));
                if let Ok(val) = _th_guard.read() {
                    if !*val {
                        debug!("Terminating relay thread, zapping..");
                        return;
                    }
                }
                while let Ok(RelayOrStopEnvelope::Relay(msg)) =
                    &_th_container.out_queue().try_recv_finalization()
                {
                    debug!("Relaying {:?}", msg);
                    &_th_container.send_finalization(1, &msg.1);
                }
                while let Ok(RelayOrStopEnvelope::Relay(rec)) =
                    &_th_container.out_queue().try_recv_finalization_record()
                {
                    debug!("Relaying {:?}", rec);
                    &_th_container.send_finalization_record(1, rec);
                }
            });

            for i in 0..$blocks_num {
                match &consensus_container
                    .out_queue()
                    .recv_timeout_block(Duration::from_millis(500_000))
                {
                    Ok(RelayOrStopEnvelope::Relay(msg)) => {
                        debug!("{} Got block data => {:?}", i, msg);
                        &consensus_container.send_block(1, msg);
                    }
                    Err(msg) => panic!(format!("No message at {}! {}", i, msg)),
                    _ => {}
                }
            }
            debug!("Stopping relay thread");
            if let Ok(mut guard) = relay_th_guard.write() {
                *guard = false;
            }
            _aux_th.join().unwrap();

            debug!("Shutting down bakers");
            for i in 0..$num_bakers {
                &consensus_container.stop_baker(i);
            }
            debug!("Test concluded");
        };
    }

    #[allow(unused_macros)]
    macro_rules! baker_test_tx {
        ($genesis_time:expr, $retval:expr, $data:expr) => {
            debug!("Performing TX test call to Haskell via FFI");
            let (genesis_data, private_data) =
                match ConsensusContainer::generate_data($genesis_time, 1) {
                    Ok((genesis, private_data)) => (genesis, private_data),
                    _ => panic!("Couldn't read Haskell data"),
                };
            let mut consensus_container = ConsensusContainer::new(genesis_data);
            &consensus_container.start_baker(0, private_data.get(&(0 as i64)).unwrap().to_vec());
            assert_eq!(consensus_container.send_transaction($data), $retval as i64);
            &consensus_container.stop_baker(0);
        };
    }

    #[test]
    pub fn consensus_tests() {
        setup();
        start_haskell();
        bakers_test!(0, 5, 10);
        bakers_test!(0, 10, 5);
        // Re-enable when we have acorn sc-tx tests possible
        // baker_test_tx!(0, 0,
        // &"{\"txAddr\":\"31\",\"txSender\":\"53656e6465723a203131\",\"txMessage\":\"
        // Increment\",\"txNonce\":\"
        // de8bb42d9c1ea10399a996d1875fc1a0b8583d21febc4e32f63d0e7766554dc1\"}".
        // to_string()); baker_test_tx!(0, 1,
        // &"{\"txAddr\":\"31\",\"txSender\":\"53656e6465723a203131\",\"txMessage\":\"
        // Incorrect\",\"txNonce\":\"
        // de8bb42d9c1ea10399a996d1875fc1a0b8583d21febc4e32f63d0e7766554dc1\"}".
        // to_string());
        stop_haskell();
    }
}
