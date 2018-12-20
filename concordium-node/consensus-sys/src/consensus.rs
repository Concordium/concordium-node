use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};
use curryrs::hsrt::{start, stop};
use std::boxed::Box;
use std::collections::HashMap;
use std::ffi::CString;
use std::io::Cursor;
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
                      bake_callback: extern "C" fn(*const u8, i64))
                      -> *mut baker_runner;
    pub fn printBlock(block_data: *const u8, data_length: i64);
    pub fn receiveBlock(baker: *mut baker_runner, block_data: *const u8, data_length: i64);
    pub fn receiveTransaction(baker: *mut baker_runner,
                              n0: u64,
                              n1: u64,
                              n2: u64,
                              n3: u64,
                              transaction_data: *const u8,
                              data_length: i64);
    pub fn stopBaker(baker: *mut baker_runner);
    pub fn makeGenesisData(genesis_time: u64,
                           num_bakers: u64,
                           genesis_callback: extern "C" fn(data: *const u8, data_length: i64),
                           baker_private_data_callback: extern "C" fn(baker_id: i64,
                                         data: *const u8,
                                         data_length: i64));
}

#[derive(Clone)]
pub struct ConsensusBaker {
    id: u64,
    genesis_data: Vec<u8>,
    private_data: Vec<u8>,
    runner: Arc<Mutex<*mut baker_runner>>,
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
                       on_block_baked)
        };
        ConsensusBaker { id: baker_id,
                         genesis_data: genesis_data,
                         private_data: private_data,
                         runner: Arc::new(Mutex::new(baker)), }
    }

    pub fn stop(&self) {
        unsafe {
            let baker = &*self.runner.lock().unwrap();
            stopBaker(*baker);
        }
    }

    pub fn send_block(&self, data: &Block) {
        unsafe {
            let baker = &*self.runner.lock().unwrap();
            let serialized = data.serialize().unwrap();
            let len = serialized.len();
            let c_string = CString::from_vec_unchecked(serialized);
            receiveBlock(*baker, c_string.as_ptr() as *const u8, len as i64);
        }
    }

    pub fn send_transaction(&self, data: &Transaction) {
        unsafe {
            let baker = &*self.runner.lock().unwrap();
            let len = data.data().len();
            let c_string = CString::from_vec_unchecked(data.data().to_vec());
            receiveTransaction(*baker,
                               data.n0(),
                               data.n1(),
                               data.n2(),
                               data.n3(),
                               c_string.as_ptr() as *const u8,
                               len as i64);
        }
    }
}

unsafe impl Send for ConsensusBaker {}
unsafe impl Sync for ConsensusBaker {}

#[derive(Clone)]
pub struct ConsensusOutQueue {
    receiver: Arc<Mutex<mpsc::Receiver<Box<Block>>>>,
    sender: Arc<Mutex<mpsc::Sender<Box<Block>>>>,
}

impl ConsensusOutQueue {
    pub fn new() -> Self {
        let (sender, receiver) = mpsc::channel::<Box<Block>>();
        ConsensusOutQueue { receiver: Arc::new(Mutex::new(receiver)),
                            sender: Arc::new(Mutex::new(sender)), }
    }

    pub fn send(self, data: Box<Block>) -> Result<(), mpsc::SendError<Box<Block>>> {
        self.sender.lock().unwrap().send(data)
    }

    pub fn recv(self) -> Result<Box<Block>, mpsc::RecvError> {
        self.receiver.lock().unwrap().recv()
    }

    pub fn recv_timeout(self, timeout: Duration) -> Result<Box<Block>, mpsc::RecvTimeoutError> {
        self.receiver.lock().unwrap().recv_timeout(timeout)
    }

    pub fn try_recv(self) -> Result<Box<Block>, mpsc::TryRecvError> {
        self.receiver.lock().unwrap().try_recv()
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
                             bakers: Arc::new(Mutex::new(HashMap::new())), }
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
                    ConsensusBaker::new(baker_id, self.genesis_data.clone(), private_data.clone()));
    }

    pub fn stop_baker(&mut self, baker_id: u64) {
        match self.bakers.lock().unwrap().get_mut(&baker_id) {
            Some(baker) => baker.stop(),
            None => error!("Can't find baker"),
        }
        self.bakers.lock().unwrap().remove(&baker_id);
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

    pub fn send_transaction(&self, tx: &Transaction) {
        for (_, baker) in &*self.bakers.lock().unwrap() {
            baker.send_transaction(&tx);
        }
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
                return Ok((genesis_data.clone(), priv_data.clone()));
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
        *GENERATED_GENESIS_DATA.lock().unwrap() = Some(s.as_bytes().to_vec().clone());
    }
}

extern "C" fn on_private_data_generated(baker_id: i64, private_data: *const u8, data_length: i64) {
    unsafe {
        let s = str::from_utf8_unchecked(slice::from_raw_parts(private_data as *const u8,
                                                               data_length as usize));
        GENERATED_PRIVATE_DATA.lock()
                              .unwrap()
                              .insert(baker_id, s.as_bytes().to_vec().clone());
    }
}

extern "C" fn on_block_baked(block_data: *const u8, data_length: i64) {
    debug!("Callback hit - queueing message");
    unsafe {
        let s = str::from_utf8_unchecked(slice::from_raw_parts(block_data as *const u8,
                                                               data_length as usize));
        match Block::deserialize(s.as_bytes()) {
            Some(block) => {
                match CALLBACK_QUEUE.clone().send(Box::new(block)) {
                    Ok(_) => {
                        debug!("Queueing {} bytes", data_length);
                    }
                    _ => error!("Didn't queue message properly"),
                }
            }
            _ => error!("Deserialization failed!"),
        }
    }
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
    pointer: Vec<u8>,
    baker_id: u64,
    proof: Vec<u8>,
    nonce: Nonce,
    last_finalized: Vec<u8>,
    data: Vec<u8>,
    signature: Vec<u8>,
}

impl Block {
    pub fn deserialize(data: &[u8]) -> Option<Self> {
        if data.len() < 64 {
            return None;
        }
        let mut curr_pos = 0;
        let mut slot_id_bytes = Cursor::new(&data[curr_pos..(curr_pos + 8)]);
        let slot_id = match slot_id_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        curr_pos += 8;
        let mut pointer_size_bytes = Cursor::new(&data[curr_pos..(curr_pos + 8)]);
        let pointer_size = match pointer_size_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        curr_pos += 8;
        if data.len() < (curr_pos + pointer_size as usize) {
            return None;
        }
        let pointer = &data[curr_pos..(curr_pos + pointer_size as usize)];
        curr_pos += pointer_size as usize;
        let mut baker_id_bytes = Cursor::new(&data[curr_pos..(curr_pos + 8)]);
        let baker_id = match baker_id_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        curr_pos += 8;
        let mut proof_size_bytes = Cursor::new(&data[curr_pos..(curr_pos + 8)]);
        let proof_size = match proof_size_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        curr_pos += 8;
        if data.len() < (curr_pos + proof_size as usize) {
            return None;
        }
        let proof = &data[curr_pos..(curr_pos + proof_size as usize)];
        curr_pos += proof_size as usize;
        let mut nonce_hash_size_bytes = Cursor::new(&data[curr_pos..(curr_pos + 8)]);
        let nonce_hash_size = match nonce_hash_size_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        curr_pos += 8;
        if data.len() < (curr_pos + nonce_hash_size as usize) {
            return None;
        }
        let nonce_hash = &data[curr_pos..(curr_pos + nonce_hash_size as usize)];
        curr_pos += nonce_hash_size as usize;
        let mut nonce_proof_size_bytes = Cursor::new(&data[curr_pos..(curr_pos + 8)]);
        let nonce_proof_size = match nonce_proof_size_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        curr_pos += 8;
        if data.len() < (curr_pos + nonce_proof_size as usize) {
            return None;
        }
        let nonce_proof = &data[curr_pos..(curr_pos + nonce_proof_size as usize)];
        curr_pos += nonce_proof_size as usize;
        let mut last_finalized_size_bytes = Cursor::new(&data[curr_pos..(curr_pos + 8)]);
        let last_finalized_size = match last_finalized_size_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        curr_pos += 8;
        if data.len() < (curr_pos + last_finalized_size as usize) {
            return None;
        }
        let last_finalized = &data[curr_pos..(curr_pos + last_finalized_size as usize)];
        curr_pos += last_finalized_size as usize;
        let mut data_size_bytes = Cursor::new(&data[curr_pos..(curr_pos + 8)]);
        let data_size = match data_size_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        curr_pos += 8;
        if data.len() < (curr_pos + data_size as usize) {
            return None;
        }
        let block_data = &data[curr_pos..(curr_pos + data_size as usize)];
        curr_pos += data_size as usize;
        let mut signature_size_bytes = Cursor::new(&data[curr_pos..(curr_pos + 8)]);
        let signature_size = match signature_size_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        curr_pos += 8;
        if data.len() < (curr_pos + signature_size as usize) {
            return None;
        }
        let signature = &data[curr_pos..(curr_pos + signature_size as usize)];
        Some(Block { slot_id: slot_id,
                     pointer: pointer.to_vec(),
                     baker_id: baker_id,
                     proof: proof.to_vec(),
                     nonce: Nonce::new(nonce_hash.to_vec(), nonce_proof.to_vec()),
                     last_finalized: last_finalized.to_vec(),
                     data: block_data.to_vec(),
                     signature: signature.to_vec(), })
    }

    pub fn serialize(&self) -> Result<Vec<u8>, &'static str> {
        let mut out: Vec<u8> = vec![];
        match out.write_u64::<BigEndian>(self.slot_id) {
            Ok(_) => {}
            Err(_) => return Err("Can't write slot ID"),
        }
        match out.write_u64::<BigEndian>(self.pointer.len() as u64) {
            Ok(_) => {}
            Err(_) => return Err("Can't write size of pointer"),
        }
        out.extend(&self.pointer);
        match out.write_u64::<BigEndian>(self.baker_id) {
            Ok(_) => {}
            Err(_) => return Err("Can't write baker ID"),
        }
        match out.write_u64::<BigEndian>(self.proof.len() as u64) {
            Ok(_) => {}
            Err(_) => return Err("Can't write size of proof"),
        }
        out.extend(&self.proof);
        match out.write_u64::<BigEndian>(self.nonce.hash.len() as u64) {
            Ok(_) => {}
            Err(_) => return Err("Can't write size of nonce hash"),
        }
        out.extend(&self.nonce.hash);
        match out.write_u64::<BigEndian>(self.nonce.proof.len() as u64) {
            Ok(_) => {}
            Err(_) => return Err("Can't write size of nonce proof"),
        }
        out.extend(&self.nonce.proof);
        match out.write_u64::<BigEndian>(self.last_finalized.len() as u64) {
            Ok(_) => {}
            Err(_) => return Err("Can't write size of last finalized"),
        }
        out.extend(&self.last_finalized);
        match out.write_u64::<BigEndian>(self.data.len() as u64) {
            Ok(_) => {}
            Err(_) => return Err("Can't write size of data"),
        }
        out.extend(&self.data);
        match out.write_u64::<BigEndian>(self.signature.len() as u64) {
            Ok(_) => {}
            Err(_) => return Err("Can't write size of signature"),
        }
        out.extend(&self.signature);
        Ok(out)
    }

    pub fn slot_id(&self) -> u64 {
        self.slot_id
    }

    pub fn baker_id(&self) -> u64 {
        self.baker_id
    }
}

#[derive(Debug)]
pub struct Transaction {
    n0: u64,
    n1: u64,
    n2: u64,
    n3: u64,
    data: Vec<u8>,
}

impl Transaction {
    pub fn new(n0: u64, n1: u64, n2: u64, n3: u64, data: &[u8]) -> Self {
        Transaction { n0: n0,
                      n1: n1,
                      n2: n2,
                      n3: n3,
                      data: data.to_vec(), }
    }

    pub fn deserialize(data: &[u8]) -> Option<Self> {
        if data.len() < 40 {
            return None;
        }
        let mut curr_pos = 0;
        let mut n0_bytes = Cursor::new(&data[curr_pos..(curr_pos + 8)]);
        let n0 = match n0_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        curr_pos += 8;
        let mut n1_bytes = Cursor::new(&data[curr_pos..(curr_pos + 8)]);
        let n1 = match n1_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        curr_pos += 8;
        let mut n2_bytes = Cursor::new(&data[curr_pos..(curr_pos + 8)]);
        let n2 = match n2_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        curr_pos += 8;
        let mut n3_bytes = Cursor::new(&data[curr_pos..(curr_pos + 8)]);
        let n3 = match n3_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        curr_pos += 8;
        let mut data_size_bytes = Cursor::new(&data[curr_pos..(curr_pos + 8)]);
        let data_size = match data_size_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        curr_pos += 8;
        if data.len() < (curr_pos + data_size as usize) {
            return None;
        }
        let bytes = &data[curr_pos..(curr_pos + data_size as usize)];
        Some(Transaction { n0: n0,
                           n1: n1,
                           n2: n2,
                           n3: n3,
                           data: bytes.to_vec(), })
    }

    pub fn serialize(&self) -> Result<Vec<u8>, &'static str> {
        let mut out: Vec<u8> = vec![];
        match out.write_u64::<BigEndian>(self.n0) {
            Ok(_) => {}
            Err(_) => return Err("Can't write n0"),
        }
        match out.write_u64::<BigEndian>(self.n1) {
            Ok(_) => {}
            Err(_) => return Err("Can't write n1"),
        }
        match out.write_u64::<BigEndian>(self.n2) {
            Ok(_) => {}
            Err(_) => return Err("Can't write n2"),
        }
        match out.write_u64::<BigEndian>(self.n3) {
            Ok(_) => {}
            Err(_) => return Err("Can't write n3"),
        }
        match out.write_u64::<BigEndian>(self.data.len() as u64) {
            Ok(_) => {}
            Err(_) => return Err("Can't write size of data"),
        }
        out.extend(&self.data);
        Ok(out)
    }

    pub fn n0(&self) -> u64 {
        self.n0
    }

    pub fn n1(&self) -> u64 {
        self.n1
    }

    pub fn n2(&self) -> u64 {
        self.n2
    }

    pub fn n3(&self) -> u64 {
        self.n3
    }

    pub fn data(&self) -> &Vec<u8> {
        &self.data
    }
}

#[cfg(test)]
mod tests {
    use consensus::*;
    use std::time::Duration;

    #[test]
    pub fn serialization_deserialize_block_000() {
        let input =
            vec![0, 0, 0, 0, 9, 49, 55, 40, 0, 0, 0, 0, 0, 0, 0, 32, 61, 187, 166, 54, 160, 207,
                 17, 196, 1, 20, 226, 192, 124, 63, 47, 212, 116, 98, 227, 71, 183, 220, 24, 105,
                 202, 174, 83, 196, 132, 228, 118, 43, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0,
                 0, 32, 0, 237, 33, 62, 77, 99, 148, 79, 85, 137, 48, 44, 156, 100, 175, 51, 186,
                 255, 163, 86, 128, 243, 13, 117, 168, 226, 64, 126, 242, 49, 227, 115, 0, 0, 0,
                 0, 0, 0, 0, 32, 142, 136, 201, 55, 51, 69, 17, 166, 161, 211, 186, 255, 50, 14,
                 228, 255, 163, 71, 60, 19, 43, 186, 241, 221, 100, 125, 56, 55, 28, 50, 21, 90,
                 0, 0, 0, 0, 0, 0, 0, 32, 142, 136, 201, 55, 51, 69, 17, 166, 161, 211, 186, 255,
                 50, 14, 228, 255, 163, 71, 60, 19, 43, 186, 241, 221, 100, 125, 56, 55, 28, 50,
                 21, 90, 0, 0, 0, 0, 0, 0, 0, 32, 61, 187, 166, 54, 160, 207, 17, 196, 1, 20, 226,
                 192, 124, 63, 47, 212, 116, 98, 227, 71, 183, 220, 24, 105, 202, 174, 83, 196,
                 132, 228, 118, 43, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 32, 10, 193, 40, 250, 207, 100, 88, 157, 126, 179, 127, 194, 212, 122, 35,
                 34, 30, 116, 251, 115, 245, 53, 67, 254, 113, 127, 202, 83, 135, 130, 208, 220];
        let deserialized = Block::deserialize(&input);
        assert!(&deserialized.is_some());
        let block = deserialized.unwrap();
        assert_eq!(&block.slot_id, &154220328);
        assert_eq!(&block.baker_id, &3);
        assert_eq!(&block.pointer,
                   &vec![61, 187, 166, 54, 160, 207, 17, 196, 1, 20, 226, 192, 124, 63, 47, 212,
                         116, 98, 227, 71, 183, 220, 24, 105, 202, 174, 83, 196, 132, 228, 118,
                         43]);
        assert_eq!(&block.proof,
                   &vec![0, 237, 33, 62, 77, 99, 148, 79, 85, 137, 48, 44, 156, 100, 175, 51,
                         186, 255, 163, 86, 128, 243, 13, 117, 168, 226, 64, 126, 242, 49, 227,
                         115]);
        assert_eq!(&block.nonce.hash,
                   &vec![142, 136, 201, 55, 51, 69, 17, 166, 161, 211, 186, 255, 50, 14, 228,
                         255, 163, 71, 60, 19, 43, 186, 241, 221, 100, 125, 56, 55, 28, 50, 21,
                         90]);
        assert_eq!(&block.nonce.proof,
                   &vec![142, 136, 201, 55, 51, 69, 17, 166, 161, 211, 186, 255, 50, 14, 228,
                         255, 163, 71, 60, 19, 43, 186, 241, 221, 100, 125, 56, 55, 28, 50, 21,
                         90]);
        assert_eq!(&block.last_finalized,
                   &vec![61, 187, 166, 54, 160, 207, 17, 196, 1, 20, 226, 192, 124, 63, 47, 212,
                         116, 98, 227, 71, 183, 220, 24, 105, 202, 174, 83, 196, 132, 228, 118,
                         43]);
        assert_eq!(&block.data, &vec![0, 0, 0, 0, 0, 0, 0, 0]);
        assert_eq!(&block.signature,
                   &vec![10, 193, 40, 250, 207, 100, 88, 157, 126, 179, 127, 194, 212, 122, 35,
                         34, 30, 116, 251, 115, 245, 53, 67, 254, 113, 127, 202, 83, 135, 130,
                         208, 220]);
    }

    #[test]
    pub fn serialization_serialize_block_000() {
        let expected_out =
            vec![0, 0, 0, 0, 9, 49, 55, 40, 0, 0, 0, 0, 0, 0, 0, 32, 61, 187, 166, 54, 160, 207,
                 17, 196, 1, 20, 226, 192, 124, 63, 47, 212, 116, 98, 227, 71, 183, 220, 24, 105,
                 202, 174, 83, 196, 132, 228, 118, 43, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0,
                 0, 32, 0, 237, 33, 62, 77, 99, 148, 79, 85, 137, 48, 44, 156, 100, 175, 51, 186,
                 255, 163, 86, 128, 243, 13, 117, 168, 226, 64, 126, 242, 49, 227, 115, 0, 0, 0,
                 0, 0, 0, 0, 32, 142, 136, 201, 55, 51, 69, 17, 166, 161, 211, 186, 255, 50, 14,
                 228, 255, 163, 71, 60, 19, 43, 186, 241, 221, 100, 125, 56, 55, 28, 50, 21, 90,
                 0, 0, 0, 0, 0, 0, 0, 32, 142, 136, 201, 55, 51, 69, 17, 166, 161, 211, 186, 255,
                 50, 14, 228, 255, 163, 71, 60, 19, 43, 186, 241, 221, 100, 125, 56, 55, 28, 50,
                 21, 90, 0, 0, 0, 0, 0, 0, 0, 32, 61, 187, 166, 54, 160, 207, 17, 196, 1, 20, 226,
                 192, 124, 63, 47, 212, 116, 98, 227, 71, 183, 220, 24, 105, 202, 174, 83, 196,
                 132, 228, 118, 43, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 32, 10, 193, 40, 250, 207, 100, 88, 157, 126, 179, 127, 194, 212, 122, 35,
                 34, 30, 116, 251, 115, 245, 53, 67, 254, 113, 127, 202, 83, 135, 130, 208, 220];
        let block =
            Block { slot_id: 154220328,
                    baker_id: 3,
                    pointer: vec![61, 187, 166, 54, 160, 207, 17, 196, 1, 20, 226, 192, 124,
                                  63, 47, 212, 116, 98, 227, 71, 183, 220, 24, 105, 202, 174,
                                  83, 196, 132, 228, 118, 43],
                    proof: vec![0, 237, 33, 62, 77, 99, 148, 79, 85, 137, 48, 44, 156, 100,
                                175, 51, 186, 255, 163, 86, 128, 243, 13, 117, 168, 226, 64,
                                126, 242, 49, 227, 115],
                    last_finalized: vec![61, 187, 166, 54, 160, 207, 17, 196, 1, 20, 226, 192,
                                         124, 63, 47, 212, 116, 98, 227, 71, 183, 220, 24, 105,
                                         202, 174, 83, 196, 132, 228, 118, 43],
                    data: vec![0, 0, 0, 0, 0, 0, 0, 0],
                    signature: vec![10, 193, 40, 250, 207, 100, 88, 157, 126, 179, 127, 194,
                                    212, 122, 35, 34, 30, 116, 251, 115, 245, 53, 67, 254, 113,
                                    127, 202, 83, 135, 130, 208, 220],
                    nonce: Nonce::new(vec![142, 136, 201, 55, 51, 69, 17, 166, 161, 211, 186,
                                           255, 50, 14, 228, 255, 163, 71, 60, 19, 43, 186,
                                           241, 221, 100, 125, 56, 55, 28, 50, 21, 90],
                                      vec![142, 136, 201, 55, 51, 69, 17, 166, 161, 211, 186,
                                           255, 50, 14, 228, 255, 163, 71, 60, 19, 43, 186,
                                           241, 221, 100, 125, 56, 55, 28, 50, 21, 90]), };
        assert_eq!(expected_out, Block::serialize(&block).unwrap());
    }

    #[test]
    pub fn serialization_serialize_deserialize_transaction_000() {
        let tx = Transaction::new(100, 200, 300, 400, &[61, 187, 166]);
        match tx.serialize() {
            Ok(bytes) => {
                if let Some(tx_des) = Transaction::deserialize(&bytes) {
                    assert_eq!(tx.n0(), tx_des.n0());
                    assert_eq!(tx.n1(), tx_des.n1());
                    assert_eq!(tx.n2(), tx_des.n2());
                    assert_eq!(tx.n3(), tx_des.n3());
                    assert_eq!(tx.data(), tx_des.data());
                } else {
                    panic!("Couldn't deserialize");
                }
            }
            Err(_) => panic!("Couldn't serialize"),
        }
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
            for i in 0..$blocks_num {
                match &consensus_container.out_queue()
                                          .recv_timeout(Duration::from_millis(240_000))
                {
                    Ok(msg) => {
                        println!("Got a proper message back {:?}", msg);
                        &consensus_container.send_block(msg);
                    }
                    _ => panic!(format!("No message at {}!", i)),
                }
            }
            for i in 0..$num_bakers {
                &consensus_container.stop_baker(i);
            }
        };
    }

    #[test]
    #[ignore]
    pub fn consensus_tests() {
        ConsensusContainer::start_haskell();
        bakers_test!(0, 5, 10);
        bakers_test!(0, 10, 5);
        ConsensusContainer::stop_haskell();
    }
}
