use crate::{block::*, fails::BakerNotRunning, ffi::*, finalization::*};

use concordium_common::into_err;
use failure::{bail, Fallible};
use std::{
    collections::HashMap,
    str,
    sync::{mpsc, Arc, Mutex, RwLock},
    thread,
    time::{self, Duration},
};

pub type PeerId = u64;

#[derive(Clone)]
pub struct ConsensusOutQueue {
    receiver_block: Arc<Mutex<mpsc::Receiver<BakedBlock>>>,
    sender_block: Arc<Mutex<mpsc::Sender<BakedBlock>>>,
    receiver_finalization: Arc<Mutex<mpsc::Receiver<FinalizationMessage>>>,
    sender_finalization: Arc<Mutex<mpsc::Sender<FinalizationMessage>>>,
    receiver_finalization_record: Arc<Mutex<mpsc::Receiver<FinalizationRecord>>>,
    sender_finalization_record: Arc<Mutex<mpsc::Sender<FinalizationRecord>>>,
    receiver_catchup_queue: Arc<Mutex<mpsc::Receiver<CatchupRequest>>>,
    sender_catchup_queue: Arc<Mutex<mpsc::Sender<CatchupRequest>>>,
    receiver_finalization_catchup_queue: Arc<Mutex<mpsc::Receiver<(PeerId, FinalizationMessage)>>>,
    sender_finalization_catchup_queue: Arc<Mutex<mpsc::Sender<(PeerId, FinalizationMessage)>>>,
}

impl Default for ConsensusOutQueue {
    fn default() -> Self {
        let (sender, receiver) = mpsc::channel::<BakedBlock>();
        let (sender_finalization, receiver_finalization) = mpsc::channel::<FinalizationMessage>();
        let (sender_finalization_record, receiver_finalization_record) =
            mpsc::channel::<FinalizationRecord>();
        let (sender_catchup, receiver_catchup) = mpsc::channel::<CatchupRequest>();
        let (sender_finalization_catchup, receiver_finalization_catchup) =
            mpsc::channel::<(PeerId, FinalizationMessage)>();
        ConsensusOutQueue {
            receiver_block: Arc::new(Mutex::new(receiver)),
            sender_block: Arc::new(Mutex::new(sender)),
            receiver_finalization: Arc::new(Mutex::new(receiver_finalization)),
            sender_finalization: Arc::new(Mutex::new(sender_finalization)),
            receiver_finalization_record: Arc::new(Mutex::new(receiver_finalization_record)),
            sender_finalization_record: Arc::new(Mutex::new(sender_finalization_record)),
            receiver_catchup_queue: Arc::new(Mutex::new(receiver_catchup)),
            sender_catchup_queue: Arc::new(Mutex::new(sender_catchup)),
            receiver_finalization_catchup_queue: Arc::new(Mutex::new(
                receiver_finalization_catchup,
            )),
            sender_finalization_catchup_queue: Arc::new(Mutex::new(sender_finalization_catchup)),
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
        into_err!(safe_lock!(self.sender_block).send(block))
    }

    pub fn recv_block(self) -> Fallible<BakedBlock> {
        into_err!(safe_lock!(self.receiver_block).recv())
    }

    pub fn recv_timeout_block(self, timeout: Duration) -> Fallible<BakedBlock> {
        into_err!(safe_lock!(self.receiver_block).recv_timeout(timeout))
    }

    pub fn try_recv_block(self) -> Fallible<BakedBlock> {
        into_err!(safe_lock!(self.receiver_block).try_recv())
    }

    pub fn send_finalization(self, msg: FinalizationMessage) -> Fallible<()> {
        into_err!(safe_lock!(self.sender_finalization).send(msg))
    }

    pub fn recv_finalization(self) -> Fallible<FinalizationMessage> {
        into_err!(safe_lock!(self.receiver_finalization).recv())
    }

    pub fn recv_timeout_finalization(self, timeout: Duration) -> Fallible<FinalizationMessage> {
        into_err!(safe_lock!(self.receiver_finalization).recv_timeout(timeout))
    }

    pub fn try_recv_finalization(self) -> Fallible<FinalizationMessage> {
        into_err!(safe_lock!(self.receiver_finalization).try_recv())
    }

    pub fn send_finalization_record(self, rec: FinalizationRecord) -> Fallible<()> {
        into_err!(safe_lock!(self.sender_finalization_record).send(rec))
    }

    pub fn recv_finalization_record(self) -> Fallible<FinalizationRecord> {
        into_err!(safe_lock!(self.receiver_finalization_record).recv())
    }

    pub fn recv_timeout_finalization_record(
        self,
        timeout: Duration,
    ) -> Fallible<FinalizationRecord> {
        into_err!(safe_lock!(self.receiver_finalization_record).recv_timeout(timeout))
    }

    pub fn try_recv_finalization_record(self) -> Fallible<FinalizationRecord> {
        into_err!(safe_lock!(self.receiver_finalization_record).try_recv())
    }

    pub fn send_catchup(self, rec: CatchupRequest) -> Fallible<()> {
        into_err!(safe_lock!(self.sender_catchup_queue).send(rec))
    }

    pub fn recv_catchup(self) -> Fallible<CatchupRequest> {
        into_err!(safe_lock!(self.receiver_catchup_queue).recv())
    }

    pub fn recv_timeout_catchup(self, timeout: Duration) -> Fallible<CatchupRequest> {
        into_err!(safe_lock!(self.receiver_catchup_queue).recv_timeout(timeout))
    }

    pub fn try_recv_catchup(self) -> Fallible<CatchupRequest> {
        into_err!(safe_lock!(self.receiver_catchup_queue).try_recv())
    }

    pub fn send_finalization_catchup(self, rec: (PeerId, FinalizationMessage)) -> Fallible<()> {
        into_err!(safe_lock!(self.sender_finalization_catchup_queue).send(rec))
    }

    pub fn recv_finalization_catchup(self) -> Fallible<(PeerId, FinalizationMessage)> {
        into_err!(safe_lock!(self.receiver_finalization_catchup_queue).recv())
    }

    pub fn recv_timeout_finalization_catchup(
        self,
        timeout: Duration,
    ) -> Fallible<(PeerId, FinalizationMessage)> {
        into_err!(safe_lock!(self.receiver_finalization_catchup_queue).recv_timeout(timeout))
    }

    pub fn try_recv_finalization_catchup(self) -> Fallible<(PeerId, FinalizationMessage)> {
        into_err!(safe_lock!(self.receiver_finalization_catchup_queue).try_recv())
    }

    pub fn clear(&self) {
        empty_queue!(self.receiver_block, "Block queue");
        empty_queue!(self.receiver_finalization, "Finalization messages queue");
        empty_queue!(
            self.receiver_finalization_record,
            "Finalization record queue"
        );
        empty_queue!(self.receiver_catchup_queue, "Catch-up queue");
        empty_queue!(
            self.receiver_finalization_catchup_queue,
            "Finalization message catch-up queue"
        );
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
}

type GenesisData = Vec<u8>;
type PrivateData = HashMap<i64, Vec<u8>>;

#[derive(Clone)]
pub struct ConsensusContainer {
    pub genesis_data: GenesisData,
    bakers:           Arc<RwLock<HashMap<BakerId, ConsensusBaker>>>,
}

impl ConsensusContainer {
    pub fn new(genesis_data: Vec<u8>) -> Self {
        ConsensusContainer {
            genesis_data,
            bakers: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub fn start_baker(&mut self, baker_id: u64, private_data: Vec<u8>) {
        safe_write!(self.bakers).insert(
            baker_id,
            ConsensusBaker::new(baker_id, &self.genesis_data, private_data),
        );
    }

    pub fn stop_baker(&mut self, baker_id: BakerId) {
        let bakers = &mut safe_write!(self.bakers);

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
        for (id, baker) in safe_read!(self.bakers).iter() {
            if block.baker_id() != *id {
                return baker.send_block(peer_id, block);
            }
        }
        1
    }

    pub fn send_finalization(&self, peer_id: PeerId, msg: &FinalizationMessage) -> i64 {
        if let Some((_, baker)) = safe_read!(self.bakers).iter().next() {
            baker.send_finalization(peer_id, msg);
        }
        -1
    }

    pub fn send_finalization_record(&self, peer_id: PeerId, rec: &FinalizationRecord) -> i64 {
        if let Some((_, baker)) = safe_read!(self.bakers).iter().next() {
            return baker.send_finalization_record(peer_id, rec);
        }
        -1
    }

    pub fn send_transaction(&self, tx: &[u8]) -> i64 {
        if let Some((_, baker)) = safe_read!(self.bakers).iter().next() {
            return baker.send_transaction(tx.to_vec());
        }
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

pub enum CatchupRequest {
    BlockByHash(PeerId, Vec<u8>),
    FinalizationRecordByHash(PeerId, Vec<u8>),
    FinalizationRecordByIndex(PeerId, FinalizationIndex),
}

pub fn catchup_enqueue(req: CatchupRequest) {
    match CALLBACK_QUEUE.clone().send_catchup(req) {
        Ok(_) => debug!("Queueing catch-up request"),
        _ => error!("Didn't queue catch-up request properly"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{
        sync::{Once, ONCE_INIT},
        time::Duration,
    };

    static INIT: Once = ONCE_INIT;

    #[derive(Debug, Clone)]
    pub enum NetworkStep {
        Handshake(u16),
        Broadcast(u16),
    }

    fn setup() { INIT.call_once(|| env_logger::init()); }

    macro_rules! bakers_test {
        ($genesis_time:expr, $num_bakers:expr, $blocks_num:expr) => {
            let (genesis_data, private_data) =
                ConsensusContainer::generate_data($genesis_time, $num_bakers)
                    .unwrap_or_else(|_| panic!("Couldn't read Haskell data"));
            let mut consensus_container = ConsensusContainer::new(genesis_data);

            for i in 0..$num_bakers {
                &consensus_container
                    .start_baker(i, private_data.get(&(i as i64)).unwrap().to_vec());
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
                while let Ok(msg) = &_th_container.out_queue().try_recv_finalization() {
                    debug!("Relaying {:?}", msg);
                    &_th_container.send_finalization(1, msg);
                }
                while let Ok(rec) = &_th_container.out_queue().try_recv_finalization_record() {
                    debug!("Relaying {:?}", rec);
                    &_th_container.send_finalization_record(1, rec);
                }
            });

            for i in 0..$blocks_num {
                match &consensus_container
                    .out_queue()
                    .recv_timeout_block(Duration::from_millis(500_000))
                {
                    Ok(msg) => {
                        debug!("{} Got block data => {:?}", i, msg);
                        &consensus_container.send_block(1, msg);
                    }
                    Err(msg) => panic!(format!("No message at {}! {}", i, msg)),
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
