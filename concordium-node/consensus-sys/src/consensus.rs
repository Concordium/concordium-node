use byteorder::{NetworkEndian, ReadBytesExt};
use curryrs::hsrt::{start, stop};
use failure::{bail, Fallible};

use std::{
    collections::HashMap,
    ffi::{CStr, CString},
    io::Cursor,
    os::raw::c_char,
    slice, str,
    sync::{
        atomic::{AtomicPtr, Ordering},
        mpsc, Arc, Mutex, RwLock,
    },
    thread,
    time::{self, Duration},
};

use crate::{block::*, finalization::*};

#[repr(C)]
pub struct baker_runner {
    private: [u8; 0],
}

extern "C" {
    pub fn startBaker(
        genesis_data: *const u8,
        genesis_data_len: i64,
        private_data: *const u8,
        private_data_len: i64,
        bake_callback: extern "C" fn(i64, *const u8, i64),
        log_callback: extern "C" fn(c_char, c_char, *const u8),
    ) -> *mut baker_runner;
    pub fn printBlock(block_data: *const u8, data_length: i64);
    pub fn receiveBlock(baker: *mut baker_runner, block_data: *const u8, data_length: i64) -> i64;
    pub fn receiveFinalization(
        baker: *mut baker_runner,
        finalization_data: *const u8,
        data_length: i64,
    );
    pub fn receiveFinalizationRecord(
        baker: *mut baker_runner,
        finalization_data: *const u8,
        data_length: i64,
    ) -> i64;
    pub fn receiveTransaction(baker: *mut baker_runner, tx: *const u8, data_length: i64) -> i64;
    pub fn stopBaker(baker: *mut baker_runner);
    pub fn makeGenesisData(
        genesis_time: u64,
        num_bakers: u64,
        genesis_callback: extern "C" fn(data: *const u8, data_length: i64),
        baker_private_data_callback: extern "C" fn(
            baker_id: i64,
            data: *const u8,
            data_length: i64,
        ),
    );
    pub fn getConsensusStatus(baker: *mut baker_runner) -> *const c_char;
    pub fn getBlockInfo(baker: *mut baker_runner, block_hash: *const u8) -> *const c_char;
    pub fn getAncestors(
        baker: *mut baker_runner,
        block_hash: *const u8,
        amount: u64,
    ) -> *const c_char;
    pub fn getBranches(baker: *mut baker_runner) -> *const c_char;
    pub fn getLastFinalAccountList(baker: *mut baker_runner) -> *const u8;
    pub fn getLastFinalInstances(baker: *mut baker_runner) -> *const c_char;
    pub fn getLastFinalAccountInfo(
        baker: *mut baker_runner,
        block_hash: *const c_char,
    ) -> *const c_char;
    pub fn getLastFinalInstanceInfo(
        baker: *mut baker_runner,
        block_hash: *const c_char,
    ) -> *const c_char;
    pub fn freeCStr(hstring: *const c_char);
}

#[derive(Clone)]
pub struct ConsensusBaker {
    id:           u64,
    genesis_data: Vec<u8>,
    private_data: Vec<u8>,
    runner:       Arc<AtomicPtr<baker_runner>>,
}

macro_rules! wrap_c_call_string {
    ($self:ident, $baker:ident, $c_call:expr) => {{
        let $baker = $self.runner.load(Ordering::SeqCst);
        unsafe {
            let c_string = $c_call($baker);
            let r = CStr::from_ptr(c_string).to_str().unwrap().to_owned();
            freeCStr(c_string);
            r
        }
    }};
}

macro_rules! wrap_send_data_to_c {
    ($self:ident, $data:expr, $c_call:expr) => {{
        let baker = $self.runner.load(Ordering::SeqCst);
        let len = $data.len();
        unsafe {
            return $c_call(
                baker,
                CString::from_vec_unchecked($data).as_ptr() as *const u8,
                len as i64,
            );
        };
    }};
}

macro_rules! wrap_c_call_bytes {
    ($self:ident, $c_call:expr) => {{
        let baker = $self.runner.load(Ordering::SeqCst);
        unsafe {
            let res = $c_call(baker) as *const u8;
            let raw_size = slice::from_raw_parts(res, 4);
            let mut raw_len_buf = Cursor::new(&raw_size[0..4]);
            let ret = match raw_len_buf.read_u32::<NetworkEndian>() {
                Ok(size) => slice::from_raw_parts(res, 4 + size as usize)[4..].to_owned(),
                _ => vec![],
            };
            freeCStr(res as *const i8);
            ret
        }
    }};
}

impl ConsensusBaker {
    pub fn new(baker_id: u64, genesis_data: Vec<u8>, private_data: Vec<u8>) -> Self {
        info!("Starting up baker {}", baker_id);
        let c_string_genesis = unsafe { CString::from_vec_unchecked(genesis_data.clone()) };
        let c_string_private_data = unsafe { CString::from_vec_unchecked(private_data.clone()) };
        let baker = unsafe {
            startBaker(
                c_string_genesis.as_ptr() as *const u8,
                genesis_data.len() as i64,
                c_string_private_data.as_ptr() as *const u8,
                private_data.len() as i64,
                on_block_baked,
                on_log_emited,
            )
        };
        ConsensusBaker {
            id: baker_id,
            genesis_data,
            private_data,
            runner: Arc::new(AtomicPtr::new(baker)),
        }
    }

    pub fn stop(&self) {
        let baker = self.runner.load(Ordering::SeqCst);
        unsafe {
            stopBaker(baker);
        }
    }

    pub fn send_block(&self, block: &Block) -> i64 {
        wrap_send_data_to_c!(self, block.serialize(), receiveBlock)
    }

    pub fn send_finalization(&self, msg: &FinalizationMessage) {
        wrap_send_data_to_c!(self, msg.serialize(), receiveFinalization);
    }

    pub fn send_finalization_record(&self, rec: &FinalizationRecord) -> i64 {
        wrap_send_data_to_c!(self, rec.serialize(), receiveFinalizationRecord)
    }

    pub fn send_transaction(&self, data: Vec<u8>) -> i64 {
        let baker = self.runner.load(Ordering::SeqCst);
        let len = data.len();
        unsafe {
            receiveTransaction(
                baker,
                CString::from_vec_unchecked(data).as_ptr() as *const u8,
                len as i64,
            )
        }
    }

    pub fn get_consensus_status(&self) -> String {
        wrap_c_call_string!(self, baker, |baker| getConsensusStatus(baker))
    }

    pub fn get_block_info(&self, block_hash: &str) -> String {
        let c_str = CString::new(block_hash).unwrap();
        wrap_c_call_string!(self, baker, |baker| getBlockInfo(
            baker,
            c_str.as_ptr() as *const u8
        ))
    }

    pub fn get_ancestors(&self, block_hash: &str, amount: u64) -> String {
        let c_str = CString::new(block_hash).unwrap();
        wrap_c_call_string!(self, baker, |baker| getAncestors(
            baker,
            c_str.as_ptr() as *const u8,
            amount
        ))
    }

    pub fn get_branches(&self) -> String {
        wrap_c_call_string!(self, baker, |baker| getBranches(baker))
    }

    pub fn get_last_final_account_list(&self) -> Vec<u8> {
        wrap_c_call_bytes!(self, |baker| getLastFinalAccountList(baker))
    }

    pub fn get_last_final_instances(&self) -> Vec<u8> {
        wrap_c_call_bytes!(self, |baker| getLastFinalInstances(baker))
    }

    pub fn get_last_final_account_info(&self, _account_address: &[u8]) -> Vec<u8> {
        wrap_c_call_bytes!(self, |baker| getLastFinalAccountInfo(
            baker,
            _account_address.as_ptr() as *const i8
        ))
    }

    pub fn get_last_final_instance_info(&self, _contract_instance_address: &[u8]) -> Vec<u8> {
        wrap_c_call_bytes!(self, |baker| getLastFinalInstanceInfo(
            baker,
            _contract_instance_address.as_ptr() as *const i8
        ))
    }
}

#[derive(Clone)]
pub struct ConsensusOutQueue {
    receiver_block:               Arc<Mutex<mpsc::Receiver<Block>>>,
    sender_block:                 Arc<Mutex<mpsc::Sender<Block>>>,
    receiver_finalization:        Arc<Mutex<mpsc::Receiver<FinalizationMessage>>>,
    sender_finalization:          Arc<Mutex<mpsc::Sender<FinalizationMessage>>>,
    receiver_finalization_record: Arc<Mutex<mpsc::Receiver<FinalizationRecord>>>,
    sender_finalization_record:   Arc<Mutex<mpsc::Sender<FinalizationRecord>>>,
}

impl Default for ConsensusOutQueue {
    fn default() -> Self {
        let (sender, receiver) = mpsc::channel::<Block>();
        let (sender_finalization, receiver_finalization) = mpsc::channel::<FinalizationMessage>();
        let (sender_finalization_record, receiver_finalization_record) =
            mpsc::channel::<FinalizationRecord>();
        ConsensusOutQueue {
            receiver_block:               Arc::new(Mutex::new(receiver)),
            sender_block:                 Arc::new(Mutex::new(sender)),
            receiver_finalization:        Arc::new(Mutex::new(receiver_finalization)),
            sender_finalization:          Arc::new(Mutex::new(sender_finalization)),
            receiver_finalization_record: Arc::new(Mutex::new(receiver_finalization_record)),
            sender_finalization_record:   Arc::new(Mutex::new(sender_finalization_record)),
        }
    }
}

impl ConsensusOutQueue {
    pub fn send_block(self, block: Block) -> Fallible<()> {
        into_err!(safe_lock!(self.sender_block).send(block))
    }

    pub fn recv_block(self) -> Fallible<Block> {
        into_err!(safe_lock!(self.receiver_block).recv())
    }

    pub fn recv_timeout_block(self, timeout: Duration) -> Fallible<Block> {
        into_err!(safe_lock!(self.receiver_block).recv_timeout(timeout))
    }

    pub fn try_recv_block(self) -> Fallible<Block> {
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

    pub fn clear(&self) {
        if let Ok(ref mut q) = self.receiver_block.try_lock() {
            debug!("Drained queue for {} element(s)", q.try_iter().count());
        }
        if let Ok(ref mut q) = self.receiver_finalization.try_lock() {
            debug!("Drained queue for {} element(s)", q.try_iter().count());
        }
        if let Ok(ref mut q) = self.receiver_finalization_record.try_lock() {
            debug!("Drained queue for {} element(s)", q.try_iter().count());
        }
    }
}

lazy_static! {
    static ref CALLBACK_QUEUE: ConsensusOutQueue = { ConsensusOutQueue::default() };
    static ref GENERATED_PRIVATE_DATA: RwLock<HashMap<i64, Vec<u8>>> =
        { RwLock::new(HashMap::new()) };
    static ref GENERATED_GENESIS_DATA: RwLock<Option<Vec<u8>>> = { RwLock::new(None) };
}

type GenesisData = Vec<u8>;
type PrivateData = HashMap<i64, Vec<u8>>;

#[derive(Clone)]
pub struct ConsensusContainer {
    genesis_data: Vec<u8>,
    bakers:       Arc<RwLock<HashMap<u64, ConsensusBaker>>>,
}

impl ConsensusContainer {
    pub fn new(genesis_data: Vec<u8>) -> Self {
        ConsensusContainer {
            genesis_data,
            bakers: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    #[cfg(windows)]
    pub fn start_haskell() {
        info!("Starting up Haskell runner");
        start();
    }

    #[cfg(not(windows))]
    pub fn start_haskell() {
        info!("Starting up Haskell runner");
        start("".to_string());
    }

    pub fn stop_haskell() {
        info!("Stopping Haskell runner");
        stop();
    }

    pub fn start_baker(&mut self, baker_id: u64, private_data: Vec<u8>) {
        safe_write!(self.bakers).insert(
            baker_id,
            ConsensusBaker::new(baker_id, self.genesis_data.clone(), private_data),
        );
    }

    pub fn stop_baker(&mut self, baker_id: u64) {
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

    pub fn send_block(&self, block: &Block) -> i64 {
        for (id, baker) in safe_read!(self.bakers).iter() {
            if block.baker_id() != *id {
                return baker.send_block(block);
            }
        }
        1
    }

    pub fn send_finalization(&self, msg: &FinalizationMessage) -> i64 {
        if let Some((_, baker)) = safe_read!(self.bakers).iter().next() {
            baker.send_finalization(msg);
        }
        -1
    }

    pub fn send_finalization_record(&self, rec: &FinalizationRecord) -> i64 {
        if let Some((_, baker)) = safe_read!(self.bakers).iter().next() {
            return baker.send_finalization_record(rec);
        }
        0
    }

    pub fn send_transaction(&self, tx: &[u8]) -> i64 {
        if let Some((_, baker)) = safe_read!(self.bakers).iter().next() {
            return baker.send_transaction(tx.to_vec());
        }
        -1
    }

    pub fn generate_data(
        genesis_time: u64,
        num_bakers: u64,
    ) -> Fallible<(GenesisData, PrivateData)> {
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
        let genesis_data: Vec<u8> = match GENERATED_GENESIS_DATA.write() {
            Ok(ref mut genesis) if genesis.is_some() => genesis.take().unwrap(),
            _ => bail!("Didn't get genesis from haskell"),
        };
        if let Ok(priv_data) = GENERATED_PRIVATE_DATA.read() {
            if priv_data.len() < num_bakers as usize {
                bail!("Didn't get private data from haskell");
            } else {
                return Ok((genesis_data, priv_data.clone()));
            }
        } else {
            bail!("Didn't get private data from haskell");
        }
    }

    pub fn get_consensus_status(&self) -> Option<String> {
        safe_read!(self.bakers)
            .values()
            .next()
            .map(ConsensusBaker::get_consensus_status)
    }

    pub fn get_block_info(&self, block_hash: &str) -> Option<String> {
        safe_read!(self.bakers)
            .values()
            .next()
            .map(|baker| baker.get_block_info(block_hash))
    }

    pub fn get_ancestors(&self, block_hash: &str, amount: u64) -> Option<String> {
        safe_read!(self.bakers)
            .values()
            .next()
            .map(|baker| baker.get_ancestors(block_hash, amount))
    }

    pub fn get_branches(&self) -> Option<String> {
        safe_read!(self.bakers)
            .values()
            .next()
            .map(ConsensusBaker::get_branches)
    }

    pub fn get_last_final_account_list(&self) -> Option<Vec<u8>> {
        safe_read!(self.bakers)
            .values()
            .next()
            .map(ConsensusBaker::get_last_final_account_list)
    }

    pub fn get_last_final_instance_info(&self, block_hash: &[u8]) -> Option<Vec<u8>> {
        safe_read!(self.bakers)
            .values()
            .next()
            .map(|baker| baker.get_last_final_instance_info(block_hash))
    }

    pub fn get_last_final_account_info(&self, block_hash: &[u8]) -> Option<Vec<u8>> {
        safe_read!(self.bakers)
            .values()
            .next()
            .map(|baker| baker.get_last_final_account_info(block_hash))
    }

    pub fn get_last_final_instances(&self) -> Option<Vec<u8>> {
        safe_read!(self.bakers)
            .values()
            .next()
            .map(ConsensusBaker::get_last_final_instances)
    }
}

extern "C" fn on_genesis_generated(genesis_data: *const u8, data_length: i64) {
    unsafe {
        let s = slice::from_raw_parts(genesis_data as *const u8, data_length as usize);
        *safe_write!(GENERATED_GENESIS_DATA) = Some(s.to_owned());
    }
}

extern "C" fn on_private_data_generated(baker_id: i64, private_data: *const u8, data_length: i64) {
    unsafe {
        let s = slice::from_raw_parts(private_data as *const u8, data_length as usize);
        safe_write!(GENERATED_PRIVATE_DATA).insert(baker_id, s.to_owned());
    }
}

extern "C" fn on_block_baked(block_type: i64, block_data: *const u8, data_length: i64) {
    debug!("Callback hit - queueing message");
    unsafe {
        let s = slice::from_raw_parts(block_data as *const u8, data_length as usize);
        match block_type {
            0 => match Block::deserialize(s) {
                Ok(block) => match CALLBACK_QUEUE.clone().send_block(block) {
                    Ok(_) => {
                        debug!("Queueing {} block bytes", data_length);
                    }
                    _ => error!("Didn't queue block message properly"),
                },
                Err(e) => error!("Deserialization of block failed: {:?}", e),
            },
            1 => match FinalizationMessage::deserialize(s) {
                Ok(msg) => match CALLBACK_QUEUE.clone().send_finalization(msg) {
                    Ok(_) => {
                        debug!("Queueing {} bytes of finalization", s.len());
                    }
                    _ => error!("Didn't queue finalization message properly"),
                },
                Err(e) => error!("Deserialization of finalization message failed: {:?}", e),
            },
            2 => match FinalizationRecord::deserialize(s) {
                Ok(rec) => match CALLBACK_QUEUE.clone().send_finalization_record(rec) {
                    Ok(_) => {
                        debug!("Queueing {} bytes of finalization record", s.len());
                    }
                    _ => error!("Didn't queue finalization record message properly"),
                },
                Err(e) => error!("Deserialization of finalization record failed: {:?}", e),
            },
            _ => error!("Received invalid callback type"),
        }
    }
}

/// Following the implementation of the log crate, error = 1, warning = 2, info
/// = 3, any other option is considered as debug.
extern "C" fn on_log_emited(identifier: c_char, log_level: c_char, log_message: *const u8) {
    fn identifier_to_string(id: c_char) -> &'static str {
        match id {
            1 => "Runner",
            2 => "Afgjort",
            3 => "Birk",
            4 => "Crypto",
            5 => "Kontrol",
            6 => "Skov",
            _ => "Baker",
        }
    }
    let s = unsafe { CStr::from_ptr(log_message as *const c_char) }
        .to_str()
        .expect("log_callback: unable to decode invalid UTF-8 values");
    let i = identifier_to_string(identifier);
    match log_level as u8 {
        1 => error!("{}: {}", i, s),
        2 => warn!("{}: {}", i, s),
        3 => info!("{}: {}", i, s),
        _ => debug!("{}: {}", i, s),
    };
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
                match ConsensusContainer::generate_data($genesis_time, $num_bakers) {
                    Ok((genesis, private_data)) => (genesis, private_data),
                    _ => panic!("Couldn't read haskell data"),
                };
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
                    &_th_container.send_finalization(msg);
                }
                while let Ok(rec) = &_th_container.out_queue().try_recv_finalization_record() {
                    debug!("Relaying {:?}", rec);
                    &_th_container.send_finalization_record(rec);
                }
            });

            for i in 0..$blocks_num {
                match &consensus_container
                    .out_queue()
                    .recv_timeout_block(Duration::from_millis(500_000))
                {
                    Ok(msg) => {
                        debug!("{} Got block data => {:?}", i, msg);
                        &consensus_container.send_block(msg);
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
                    _ => panic!("Couldn't read haskell data"),
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
        ConsensusContainer::start_haskell();
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
        ConsensusContainer::stop_haskell();
    }
}
