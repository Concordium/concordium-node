use byteorder::{BigEndian, ReadBytesExt};
use curryrs::hsrt::{start, stop};
use std::boxed::Box;
use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::io::Cursor;
use std::os::raw::c_char;
use std::sync::atomic::{AtomicPtr, Ordering};
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time;
use std::time::Duration;
use std::{slice, str};

#[repr(C)]
pub struct baker_runner {
    private: [u8; 0],
}

extern "C" {
    pub fn startBaker(genesis_data: *const u8,
                      genesis_data_len: i64,
                      private_data: *const u8,
                      private_data_len: i64,
                      bake_callback: extern "C" fn(i64, *const u8, i64),
                      log_callback: extern "C" fn(c_char, c_char, *const u8))
                      -> *mut baker_runner;
    pub fn printBlock(block_data: *const u8, data_length: i64);
    pub fn receiveBlock(baker: *mut baker_runner, block_data: *const u8, data_length: i64);
    pub fn receiveFinalization(baker: *mut baker_runner,
                               finalization_data: *const u8,
                               data_length: i64);
    pub fn receiveFinalizationRecord(baker: *mut baker_runner,
                                     finalization_data: *const u8,
                                     data_length: i64);
    pub fn receiveTransaction(baker: *mut baker_runner, tx: *const u8) -> i64;
    pub fn stopBaker(baker: *mut baker_runner);
    pub fn makeGenesisData(genesis_time: u64,
                           num_bakers: u64,
                           genesis_callback: extern "C" fn(data: *const u8, data_length: i64),
                           baker_private_data_callback: extern "C" fn(baker_id: i64,
                                         data: *const u8,
                                         data_length: i64));
    pub fn freeCStr(hstring: *const c_char);
    pub fn getBestBlockInfo(baker: *mut baker_runner) -> *const c_char;
}

#[derive(Clone)]
pub struct ConsensusBaker {
    id: u64,
    genesis_data: Vec<u8>,
    private_data: Vec<u8>,
    runner: Arc<AtomicPtr<baker_runner>>,
}

impl ConsensusBaker {
    pub fn new(baker_id: u64, genesis_data: Vec<u8>, private_data: Vec<u8>) -> Self {
        info!("Starting up baker {}", baker_id);
        let c_string_genesis = unsafe { CString::from_vec_unchecked(genesis_data.clone()) };
        let c_string_private_data = unsafe { CString::from_vec_unchecked(private_data.clone()) };
        let baker = unsafe {
            startBaker(c_string_genesis.as_ptr() as *const u8,
                       genesis_data.len() as i64,
                       c_string_private_data.as_ptr() as *const u8,
                       private_data.len() as i64,
                       on_block_baked,
                       on_log_emited)
        };
        ConsensusBaker { id: baker_id,
                         genesis_data,
                         private_data,
                         runner: Arc::new(AtomicPtr::new(baker)) }
    }

    pub fn stop(&self) {
        unsafe {
            let baker = self.runner.load(Ordering::SeqCst);
            stopBaker(baker);
        }
    }

    pub fn send_block(&self, data: &Block) {
        unsafe {
            let baker = self.runner.load(Ordering::SeqCst);
            let serialized = data.serialize().unwrap();
            let len = serialized.len();
            let c_string = CString::from_vec_unchecked(serialized);
            receiveBlock(baker, c_string.as_ptr() as *const u8, len as i64);
        }
    }

    pub fn send_finalization(&self, data: Vec<u8>) {
        unsafe {
            let baker = self.runner.load(Ordering::SeqCst);
            let len = data.len();
            let c_string = CString::from_vec_unchecked(data);
            receiveFinalization(baker, c_string.as_ptr() as *const u8, len as i64);
        }
    }

    pub fn send_finalization_record(&self, data: Vec<u8>) {
        unsafe {
            let baker = self.runner.load(Ordering::SeqCst);
            let len = data.len();
            let c_string = CString::from_vec_unchecked(data);
            receiveFinalizationRecord(baker, c_string.as_ptr() as *const u8, len as i64);
        }
    }

    pub fn send_transaction(&self, data: &str) -> i64 {
        unsafe {
            let baker = self.runner.load(Ordering::SeqCst);
            let c_string = CString::new(data).unwrap();
            receiveTransaction(baker, c_string.as_ptr() as *const u8)
        }
    }

    pub fn get_best_block_info(&self) -> String {
        unsafe {
            let baker = self.runner.load(Ordering::SeqCst);
            let c_string = getBestBlockInfo(baker);
            let r = CStr::from_ptr(c_string).to_str().unwrap().to_owned();
            freeCStr(c_string);
            r
        }
    }
}

unsafe impl Send for ConsensusBaker {}
unsafe impl Sync for ConsensusBaker {}

#[derive(Clone)]
pub struct ConsensusOutQueue {
    receiver_block: Arc<Mutex<mpsc::Receiver<Box<Block>>>>,
    sender_block: Arc<Mutex<mpsc::Sender<Box<Block>>>>,
    receiver_finalization: Arc<Mutex<mpsc::Receiver<Vec<u8>>>>,
    sender_finalization: Arc<Mutex<mpsc::Sender<Vec<u8>>>>,
    receiver_finalization_record: Arc<Mutex<mpsc::Receiver<Vec<u8>>>>,
    sender_finalization_record: Arc<Mutex<mpsc::Sender<Vec<u8>>>>,
}

impl ConsensusOutQueue {
    pub fn new() -> Self {
        let (sender, receiver) = mpsc::channel::<Box<Block>>();
        let (sender_finalization, receiver_finalization) = mpsc::channel::<Vec<u8>>();
        let (sender_finalization_record, receiver_finalization_record) =
            mpsc::channel::<Vec<u8>>();
        ConsensusOutQueue { receiver_block: Arc::new(Mutex::new(receiver)),
                            sender_block: Arc::new(Mutex::new(sender)),
                            receiver_finalization: Arc::new(Mutex::new(receiver_finalization)),
                            sender_finalization: Arc::new(Mutex::new(sender_finalization)),
                            receiver_finalization_record:
                                Arc::new(Mutex::new(receiver_finalization_record)),
                            sender_finalization_record:
                                Arc::new(Mutex::new(sender_finalization_record)) }
    }

    pub fn send_block(self, data: Box<Block>) -> Result<(), mpsc::SendError<Box<Block>>> {
        self.sender_block.lock().unwrap().send(data)
    }

    pub fn recv_block(self) -> Result<Box<Block>, mpsc::RecvError> {
        self.receiver_block.lock().unwrap().recv()
    }

    pub fn recv_timeout_block(self,
                              timeout: Duration)
                              -> Result<Box<Block>, mpsc::RecvTimeoutError> {
        self.receiver_block.lock().unwrap().recv_timeout(timeout)
    }

    pub fn try_recv_block(self) -> Result<Box<Block>, mpsc::TryRecvError> {
        self.receiver_block.lock().unwrap().try_recv()
    }

    pub fn send_finalization(self,
                             data: Vec<u8>)
                             -> Result<(), mpsc::SendError<Vec<u8>>> {
        self.sender_finalization.lock().unwrap().send(data)
    }

    pub fn recv_finalization(self) -> Result<Vec<u8>, mpsc::RecvError> {
        self.receiver_finalization.lock().unwrap().recv()
    }

    pub fn recv_timeout_finalization(self,
                                     timeout: Duration)
                                     -> Result<Vec<u8>, mpsc::RecvTimeoutError> {
        self.receiver_finalization
            .lock()
            .unwrap()
            .recv_timeout(timeout)
    }

    pub fn try_recv_finalization(self) -> Result<Vec<u8>, mpsc::TryRecvError> {
        self.receiver_finalization.lock().unwrap().try_recv()
    }

    pub fn send_finalization_record(self,
                                    data: Vec<u8>)
                                    -> Result<(), mpsc::SendError<Vec<u8>>> {
        self.sender_finalization_record.lock().unwrap().send(data)
    }

    pub fn recv_finalization_record(self) -> Result<Vec<u8>, mpsc::RecvError> {
        self.receiver_finalization_record.lock().unwrap().recv()
    }

    pub fn recv_timeout_finalization_record(self,
                                            timeout: Duration)
                                            -> Result<Vec<u8>, mpsc::RecvTimeoutError> {
        self.receiver_finalization_record
            .lock()
            .unwrap()
            .recv_timeout(timeout)
    }

    pub fn try_recv_finalization_record(self) -> Result<Vec<u8>, mpsc::TryRecvError> {
        self.receiver_finalization_record.lock().unwrap().try_recv()
    }

    pub fn clear(self) {
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
    static ref CALLBACK_QUEUE: ConsensusOutQueue = { ConsensusOutQueue::new() };
    static ref GENERATED_PRIVATE_DATA: Mutex<HashMap<i64, Vec<u8>>> =
        { Mutex::new(HashMap::new()) };
    static ref GENERATED_GENESIS_DATA: Mutex<Option<Vec<u8>>> = { Mutex::new(None) };
}

#[derive(Clone)]
pub struct ConsensusContainer {
    genesis_data: Vec<u8>,
    bakers: Arc<Mutex<HashMap<u64, ConsensusBaker>>>,
}

impl ConsensusContainer {
    pub fn new(genesis_data: Vec<u8>) -> Self {
        ConsensusContainer { genesis_data: genesis_data,
                             bakers: Arc::new(Mutex::new(HashMap::new())) }
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
        self.bakers
            .lock()
            .unwrap()
            .insert(baker_id,
                    ConsensusBaker::new(baker_id, self.genesis_data.clone(), private_data));
    }

    pub fn stop_baker(&mut self, baker_id: u64) {
        if let Ok(ref mut bakers) = self.bakers.lock() {
            match bakers.get_mut(&baker_id) {
                Some(baker) => baker.stop(),
                None => error!("Can't find baker"),
            }
            bakers.remove(&baker_id);
            if bakers.is_empty() {
                CALLBACK_QUEUE.clone().clear();
            }
        } else {
            panic!("Can't obtain lock on consensus container bakers");
        }
    }

    pub fn out_queue(&self) -> ConsensusOutQueue {
        CALLBACK_QUEUE.clone()
    }

    pub fn send_block(&self, block: &Block) {
        for (id, baker) in &*self.bakers.lock().unwrap() {
            if block.baker_id != *id {
                baker.send_block(&block);
            }
        }
    }

    pub fn send_finalization(&self, pkt: &[u8]) {
        for (_, baker) in &*self.bakers.lock().unwrap() {
            baker.send_finalization(pkt.to_vec());
        }
    }

    pub fn send_finalization_record(&self, pkt: &[u8]) {
        for (_, baker) in &*self.bakers.lock().unwrap() {
            baker.send_finalization_record(pkt.to_vec());
        }
    }

    pub fn send_transaction(&self, tx: &String) -> i64 {
        if let Some(baker) = self.bakers.lock().unwrap().values_mut().next() {
            return baker.send_transaction(&tx);
        }
        -1
    }

    pub fn get_best_block_info(&self) -> Option<String> {
        if let Some(baker) = self.bakers.lock().unwrap().values_mut().next() {
            return Some(baker.get_best_block_info());
        }
        None
    }

    pub fn generate_data(genesis_time: u64,
                         num_bakers: u64)
                         -> Result<(Vec<u8>, HashMap<i64, Vec<u8>>), &'static str> {
        if let Ok(ref mut lock) = GENERATED_GENESIS_DATA.lock() {
            **lock = None;
        }
        if let Ok(ref mut lock) = GENERATED_PRIVATE_DATA.lock() {
            lock.clear();
        }
        unsafe {
            makeGenesisData(genesis_time,
                            num_bakers,
                            on_genesis_generated,
                            on_private_data_generated);
        }
        for _ in 0..num_bakers {
            if !GENERATED_GENESIS_DATA.lock().unwrap().is_some()
               || GENERATED_PRIVATE_DATA.lock().unwrap().len() < num_bakers as usize
            {
                thread::sleep(time::Duration::from_millis(200));
            }
        }
        let genesis_data: Vec<u8> = match GENERATED_GENESIS_DATA.lock() {
            Ok(ref mut genesis) if genesis.is_some() => genesis.clone().unwrap(),
            _ => return Err("Didn't get genesis from haskell"),
        };
        if let Ok(priv_data) = GENERATED_PRIVATE_DATA.lock() {
            if priv_data.len() < num_bakers as usize {
                return Err("Didn't get private data from haskell");
            } else {
                return Ok((genesis_data, priv_data.clone()));
            }
        } else {
            return Err("Didn't get private data from haskell");
        }
    }
}

extern "C" fn on_genesis_generated(genesis_data: *const u8, data_length: i64) {
    unsafe {
        let s = str::from_utf8_unchecked(slice::from_raw_parts(genesis_data as *const u8,
                                                               data_length as usize));
        *GENERATED_GENESIS_DATA.lock().unwrap() = Some(s.to_owned().into_bytes());
    }
}

extern "C" fn on_private_data_generated(baker_id: i64, private_data: *const u8, data_length: i64) {
    unsafe {
        let s = str::from_utf8_unchecked(slice::from_raw_parts(private_data as *const u8,
                                                               data_length as usize));
        GENERATED_PRIVATE_DATA.lock()
                              .unwrap()
                              .insert(baker_id, s.to_owned().into_bytes());
    }
}

extern "C" fn on_block_baked(block_type: i64, block_data: *const u8, data_length: i64) {
    debug!("Callback hit - queueing message");
    unsafe {
        let s = str::from_utf8_unchecked(slice::from_raw_parts(block_data as *const u8,
                                                               data_length as usize));
        match block_type {
            0 => {
                match Block::deserialize(s.as_bytes()) {
                    Some(block) => {
                        match CALLBACK_QUEUE.clone().send_block(Box::new(block)) {
                            Ok(_) => {
                                debug!("Queueing {} block bytes", data_length);
                            }
                            _ => error!("Didn't queue block message properly"),
                        }
                    }
                    _ => error!("Deserialization of block failed!"),
                }
            }
            1 => {
                match CALLBACK_QUEUE.clone()
                                    .send_finalization(s.to_owned().into_bytes())
                {
                    Ok(_) => {
                        debug!("Queueing {} bytes of finalization", s.len());
                    }
                    _ => error!("Didn't queue finalization message properly"),
                }
            }
            2 => {
                match CALLBACK_QUEUE.clone()
                                    .send_finalization_record(s.to_owned().into_bytes())
                {
                    Ok(_) => {
                        debug!("Queueing {} bytes of finalization record", s.len());
                    }
                    _ => error!("Didn't queue finalization record message properly"),
                }
            }
            _ => error!("Received invalid callback type"),
        }
    }
}

/// Following the implementation of the log crate, error = 1, warning = 2, info = 3, any other option is considered as debug.
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
    let s = unsafe {CStr::from_ptr(log_message as *const c_char)}.to_str()
        .expect("log_callback: unable to decode invalid UTF-8 values");
    let i = identifier_to_string(identifier).to_string();
    match log_level as u8 {
        1 => error!("{}: {}", i, s),
        2 => warn!("{}: {}", i, s),
        3 => info!("{}: {}", i, s),
        _ => debug!("{}: {}", i, s),
    };
}

#[derive(Debug)]
pub struct Nonce {
    hash: Vec<u8>,
    proof: Vec<u8>,
}

impl Nonce {
    pub fn new(hash: Vec<u8>, proof: Vec<u8>) -> Self {
        Nonce { hash, proof }
    }
}

#[derive(Debug)]
pub struct Block {
    slot_id: u64,
    baker_id: u64,
    data: Vec<u8>,
}

impl Block {
    pub fn deserialize(data: &[u8]) -> Option<Self> {
        let mut curr_pos = 0;
        let mut slot_id_bytes = Cursor::new(&data[curr_pos..][..8]);
        let slot_id = match slot_id_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        curr_pos += 8 + 32;
        let mut baker_id_bytes = Cursor::new(&data[curr_pos..][..8]);
        let baker_id = match baker_id_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        Some(Block { slot_id,
                     baker_id,
                     data: data.to_owned() })
    }

    pub fn serialize(&self) -> Result<Vec<u8>, &'static str> {
        Ok(self.data.to_owned())
    }

    pub fn slot_id(&self) -> u64 {
        self.slot_id
    }

    pub fn baker_id(&self) -> u64 {
        self.baker_id
    }
}

#[cfg(test)]
mod tests {
    use crate::consensus::*;
    use std::sync::{Once, ONCE_INIT};
    use std::time::Duration;

    static INIT: Once = ONCE_INIT;

    #[derive(Debug, Clone)]
    pub enum NetworkStep {
        Handshake(u16),
        Broadcast(u16),
    }

    fn setup() {
        INIT.call_once(|| env_logger::init());
    }

    #[test]
    pub fn serialization_deserialize_block_000() {
        setup();
        let input =
            vec![0, 0, 0, 0, 9, 60, 250, 52, 203, 177, 255, 13, 4, 179, 160, 197, 194, 34, 84,
                 186, 123, 247, 222, 246, 39, 60, 144, 3, 126, 183, 208, 197, 207, 80, 228, 15,
                 218, 177, 206, 219, 0, 0, 0, 0, 0, 0, 0, 4, 91, 79, 253, 56, 152, 63, 243, 146,
                 178, 101, 220, 59, 0, 215, 209, 152, 245, 237, 204, 118, 246, 80, 236, 206, 174,
                 33, 172, 241, 118, 132, 36, 208, 106, 143, 223, 92, 102, 126, 60, 231, 13, 232,
                 238, 120, 7, 245, 9, 213, 161, 61, 161, 174, 129, 171, 106, 110, 4, 122, 20, 198,
                 72, 119, 161, 12, 175, 220, 218, 40, 41, 62, 209, 135, 254, 161, 249, 131, 245,
                 195, 145, 0, 70, 170, 101, 248, 152, 252, 191, 72, 76, 111, 146, 107, 78, 212,
                 30, 212, 238, 60, 247, 236, 20, 142, 224, 186, 91, 159, 49, 191, 132, 52, 195,
                 121, 233, 85, 189, 48, 96, 175, 234, 112, 97, 36, 242, 144, 202, 66, 198, 109,
                 84, 249, 0, 78, 63, 162, 52, 1, 3, 24, 135, 151, 21, 93, 15, 160, 24, 40, 169,
                 25, 45, 145, 153, 30, 141, 28, 140, 200, 240, 63, 98, 215, 193, 186, 178, 84, 53,
                 198, 123, 147, 181, 167, 60, 105, 11, 81, 83, 58, 61, 203, 244, 191, 1, 27, 193,
                 163, 100, 53, 77, 177, 194, 175, 73, 5, 203, 177, 255, 13, 4, 179, 160, 197, 194,
                 34, 84, 186, 123, 247, 222, 246, 39, 60, 144, 3, 126, 183, 208, 197, 207, 80,
                 228, 15, 218, 177, 206, 219, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 25,
                 222, 218, 238, 169, 232, 56, 230, 13, 183, 57, 66, 109, 127, 52, 37, 103, 213,
                 230, 6, 146, 183, 79, 92, 57, 134, 242, 175, 212, 247, 179, 156, 87, 113, 25, 89,
                 234, 196, 242, 52, 204, 84, 139, 223, 8, 38, 198, 13, 210, 197, 193, 159, 232,
                 175, 181, 172, 169, 164, 174, 44, 113, 186, 202, 1];
        let deserialized = Block::deserialize(&input);
        assert!(&deserialized.is_some());
        let block = deserialized.unwrap();
        assert_eq!(&block.baker_id, &4);
    }

    macro_rules! bakers_test {
        ($genesis_time: expr, $num_bakers:expr, $blocks_num:expr) => {
            let (genesis_data, private_data) =
                match ConsensusContainer::generate_data($genesis_time, $num_bakers) {
                    Ok((genesis, private_data)) => (genesis.clone(), private_data.clone()),
                    _ => panic!("Couldn't read haskell data"),
                };
            let mut consensus_container = ConsensusContainer::new(genesis_data);

            for i in 0..$num_bakers {
                &consensus_container.start_baker(i,
                                                 private_data.get(&(i as i64)).unwrap().to_vec());
            }

            let relay_th_guard = Arc::new(Mutex::new(true));
            let _th_guard = relay_th_guard.clone();
            let _th_container = consensus_container.clone();
            let _aux_th = thread::spawn(move || {
                loop {
                    thread::sleep(Duration::from_millis(1_000));
                    if let Ok(val) = _th_guard.lock() {
                        if !*val {
                            debug!("Terminating relay thread, zapping..");
                            return;
                        }
                    }
                    while let Ok(msg) = &_th_container.out_queue().try_recv_finalization() {
                        debug!("Relaying finalization");
                        &_th_container.send_finalization(msg);
                    }
                    while let Ok(msg) = &_th_container.out_queue().try_recv_finalization_record() {
                        debug!("Relaying finalization record");
                        &_th_container.send_finalization_record(msg);
                    }
                }
            });

            for i in 0..$blocks_num {
                match &consensus_container.out_queue()
                                          .recv_timeout_block(Duration::from_millis(500_000))
                {
                    Ok(msg) => {
                        debug!("{} Got block data => {:?}", i, msg);
                        &consensus_container.send_block(msg);
                    }
                    Err(msg) => panic!(format!("No message at {}! {}", i, msg)),
                }
            }
            debug!("Now attempting to get best block via FFI to Haskell");
            match &consensus_container.get_best_block_info() {
                Some(ref best_block) => {
                    assert!(best_block.contains("globalState"));
                }
                _ => panic!("Didn't get best block back!"),
            }

            debug!("Stopping relay thread");
            if let Ok(mut guard) = relay_th_guard.lock() {
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
        ($genesis_time: expr, $retval: expr, $data: expr) => {
            debug!("Performing TX test call to Haskell via FFI");
            let (genesis_data, private_data) =
                match ConsensusContainer::generate_data($genesis_time, 1) {
                    Ok((genesis, private_data)) => (genesis.clone(), private_data.clone()),
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
        //baker_test_tx!(0, 0, &"{\"txAddr\":\"31\",\"txSender\":\"53656e6465723a203131\",\"txMessage\":\"Increment\",\"txNonce\":\"de8bb42d9c1ea10399a996d1875fc1a0b8583d21febc4e32f63d0e7766554dc1\"}".to_string());
        //baker_test_tx!(0, 1, &"{\"txAddr\":\"31\",\"txSender\":\"53656e6465723a203131\",\"txMessage\":\"Incorrect\",\"txNonce\":\"de8bb42d9c1ea10399a996d1875fc1a0b8583d21febc4e32f63d0e7766554dc1\"}".to_string());
        ConsensusContainer::stop_haskell();
    }
}
