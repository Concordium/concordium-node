use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};
use curryrs::hsrt::{start, stop};
use std::boxed::Box;
use std::collections::HashMap;
use std::ffi::CString;
use std::io::Cursor;
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use std::{slice, str};

#[repr(C)]
pub struct baker_runner {
    private: [u8; 0],
}

extern "C" {
    pub fn startBaker(genesis_time: u64,
                      number_of_bakers: u64,
                      baker_index: u64,
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
}

#[derive(Clone)]
pub struct ConsensusBaker {
    id: u64,
    runner: Arc<Mutex<*mut baker_runner>>,
}

impl ConsensusBaker {
    pub fn new(genesis: u64, num_of_bakers: u64, baker_index: u64) -> Self {
        info!("Starting up baker {}/{}/{}",
              genesis, num_of_bakers, baker_index);
        let baker = unsafe { startBaker(genesis, num_of_bakers, baker_index, on_block_baked) };
        ConsensusBaker { id: baker_index,
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
}

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
}

#[derive(Clone)]
pub struct ConsensusContainer {
    bakers: Arc<Mutex<HashMap<u64, ConsensusBaker>>>,
}

impl ConsensusContainer {
    pub fn new() -> Self {
        ConsensusContainer { bakers: Arc::new(Mutex::new(HashMap::new())), }
    }

    pub fn start_haskell(&self) {
        info!("Starting up Haskell runner");
        start("".to_string());
    }

    pub fn stop_haskell(&self) {
        info!("Stopping Haskell runner");
        stop();
    }

    pub fn start_baker(&mut self, genesis: u64, number_of_bakers: u64, baker_index: u64) {
        self.bakers
            .lock()
            .unwrap()
            .insert(baker_index,
                    ConsensusBaker::new(genesis, number_of_bakers, baker_index));
    }

    pub fn stop_baker(&mut self, baker_index: u64) {
        match self.bakers.lock().unwrap().get_mut(&baker_index) {
            Some(baker) => baker.stop(),
            None => error!("Can't find baker"),
        }
        self.bakers.lock().unwrap().remove(&baker_index);
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
        let pointer = &data[(curr_pos + 8)..(curr_pos + 8 + pointer_size as usize)];
        curr_pos += 8 + pointer_size as usize;
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
        let proof = &data[(curr_pos + 8)..(curr_pos + 8 + proof_size as usize)];
        curr_pos += 8 + proof_size as usize;
        let mut nonce_hash_size_bytes = Cursor::new(&data[curr_pos..(curr_pos + 8)]);
        let nonce_hash_size = match nonce_hash_size_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        let nonce_hash = &data[(curr_pos + 8)..(curr_pos + 8 + nonce_hash_size as usize)];
        curr_pos += 8 + nonce_hash_size as usize;
        let mut nonce_proof_size_bytes = Cursor::new(&data[curr_pos..(curr_pos + 8)]);
        let nonce_proof_size = match nonce_proof_size_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        let nonce_proof = &data[(curr_pos + 8)..(curr_pos + 8 + nonce_proof_size as usize)];
        curr_pos += 8 + nonce_proof_size as usize;
        let mut last_finalized_size_bytes = Cursor::new(&data[curr_pos..(curr_pos + 8)]);
        let last_finalized_size = match last_finalized_size_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        let last_finalized = &data[(curr_pos + 8)..(curr_pos + 8 + last_finalized_size as usize)];
        curr_pos += 8 + last_finalized_size as usize;
        let mut data_size_bytes = Cursor::new(&data[curr_pos..(curr_pos + 8)]);
        let data_size = match data_size_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        let block_data = &data[(curr_pos + 8)..(curr_pos + 8 + data_size as usize)];
        curr_pos += 8 + data_size as usize;
        let mut signature_size_bytes = Cursor::new(&data[curr_pos..(curr_pos + 8)]);
        let signature_size = match signature_size_bytes.read_u64::<BigEndian>() {
            Ok(num) => num,
            _ => return None,
        };
        let signature = &data[(curr_pos + 8)..(curr_pos + 8 + signature_size as usize)];
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

    macro_rules! bakers_test {
        ($num_bakers:expr, $blocks_num:expr) => {
            let mut consensus_container = ConsensusContainer::new();
            for i in 0..$num_bakers {
                &consensus_container.start_baker(0, $num_bakers, i);
            }
            for _ in 0..$blocks_num {
                match &consensus_container.out_queue()
                                          .recv_timeout(Duration::from_millis(60_000))
                {
                    Ok(msg) => {
                        println!("Got a proper message back {:?}", msg);
                        &consensus_container.send_block(msg);
                    }
                    _ => panic!("No message!"),
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
        start("".to_string());
        bakers_test!(5, 10);
        bakers_test!(10, 5);
        stop();
    }
}
