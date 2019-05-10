use byteorder::{NetworkEndian, ReadBytesExt};
use chrono::prelude::*;
use curryrs::hsrt::{start, stop};
use failure::{bail, format_err, Fallible};

use std::{
    collections::HashMap,
    convert::TryFrom,
    ffi::{CStr, CString},
    fmt::{Display, Formatter, Result},
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

use crate::{block::*, common, fails::BakerNotRunning, finalization::*};
use concordium_common::into_err;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PacketType {
    Block = 0,
    Transaction,
    FinalizationRecord,
    FinalizationMessage,
    CatchupBlockByHash,
    CatchupFinalizationRecordByHash,
    CatchupFinalizationRecordByIndex,
    CatchupFinalizationMessagesByPoint,
}

static PACKET_TYPE_FROM_INT: &[PacketType] = &[
    PacketType::Block,
    PacketType::Transaction,
    PacketType::FinalizationRecord,
    PacketType::FinalizationMessage,
    PacketType::CatchupBlockByHash,
    PacketType::CatchupFinalizationRecordByHash,
    PacketType::CatchupFinalizationRecordByIndex,
    PacketType::CatchupFinalizationMessagesByPoint,
];

impl TryFrom<u16> for PacketType {
    type Error = failure::Error;

    #[inline]
    fn try_from(value: u16) -> Fallible<PacketType> {
        PACKET_TYPE_FROM_INT
            .get(value as usize)
            .cloned()
            .ok_or_else(|| format_err!("Unsupported packet type"))
    }
}

impl Display for PacketType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            PacketType::Block => write!(f, "block"),
            PacketType::Transaction => write!(f, "transaction"),
            PacketType::FinalizationRecord => write!(f, "finalization record"),
            PacketType::FinalizationMessage => write!(f, "finalization message"),
            PacketType::CatchupBlockByHash => write!(f, "catch-up block by hash"),
            PacketType::CatchupFinalizationRecordByHash => {
                write!(f, "catch-up finalization record by hash")
            }
            PacketType::CatchupFinalizationRecordByIndex => {
                write!(f, "catch-up finalization record by index")
            }
            PacketType::CatchupFinalizationMessagesByPoint => {
                write!(f, "catch-up finalization messages by point")
            }
        }
    }
}

#[repr(C)]
pub struct baker_runner {
    private: [u8; 0],
}

type PeerId = u64;
type FinalizationRecordIndex = u64;
type ConsensusDataOutCallback = extern "C" fn(i64, *const u8, i64);
type LogCallback = extern "C" fn(c_char, c_char, *const u8);
type CatchupFinalizationRequestByBlockHashCallback =
    extern "C" fn(peer_id: PeerId, data: *const u8);
type CatchupFinalizationRequestByFinalizationIndexCallback =
    extern "C" fn(peer_id: PeerId, finalization_index: u64);
type CatchupFinalizationMessagesSenderCallback =
    extern "C" fn(peer_id: PeerId, payload: *const u8, payload_length: i64);
type GenerateKeypairCallback = extern "C" fn(baker_id: i64, data: *const u8, data_length: i64);
type GenerateGenesisDataCallback = extern "C" fn(data: *const u8, data_length: i64);

extern "C" {
    pub fn startBaker(
        genesis_data: *const u8,
        genesis_data_len: i64,
        private_data: *const u8,
        private_data_len: i64,
        bake_callback: ConsensusDataOutCallback,
        log_callback: LogCallback,
        missing_block_callback: CatchupFinalizationRequestByBlockHashCallback,
        missing_finalization_records_by_hash_callback: CatchupFinalizationRequestByBlockHashCallback,
        missing_finalization_records_by_index_callback: CatchupFinalizationRequestByFinalizationIndexCallback,
    ) -> *mut baker_runner;
    pub fn printBlock(block_data: *const u8, data_length: i64);
    pub fn receiveBlock(
        baker: *mut baker_runner,
        peer_id: PeerId,
        block_data: *const u8,
        data_length: i64,
    ) -> i64;
    pub fn receiveFinalization(
        baker: *mut baker_runner,
        peer_id: PeerId,
        finalization_data: *const u8,
        data_length: i64,
    );
    pub fn receiveFinalizationRecord(
        baker: *mut baker_runner,
        peer_id: PeerId,
        finalization_data: *const u8,
        data_length: i64,
    ) -> i64;
    pub fn receiveTransaction(baker: *mut baker_runner, tx: *const u8, data_length: i64) -> i64;
    pub fn stopBaker(baker: *mut baker_runner);
    pub fn makeGenesisData(
        genesis_time: u64,
        num_bakers: u64,
        genesis_callback: GenerateGenesisDataCallback,
        baker_private_data_callback: GenerateKeypairCallback,
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
    pub fn getBlock(baker: *mut baker_runner, block_hash: *const u8) -> *const u8;
    pub fn getBlockFinalization(baker: *mut baker_runner, block_hash: *const u8) -> *const u8;
    pub fn getIndexedFinalization(
        baker: *mut baker_runner,
        finalization_index: FinalizationRecordIndex,
    ) -> *const u8;
    pub fn getFinalizationMessages(
        baker: *mut baker_runner,
        peer_id: PeerId,
        request: *const u8,
        request_lenght: i64,
        callback: CatchupFinalizationMessagesSenderCallback,
    ) -> i64;
    pub fn getFinalizationPoint(baker: *mut baker_runner) -> *const u8;
    pub fn freeCStr(hstring: *const c_char);
}

#[derive(Clone)]
pub struct ConsensusBaker {
    id:            u64,
    genesis_block: Arc<Block>,
    private_data:  Vec<u8>,
    runner:        Arc<AtomicPtr<baker_runner>>,
}

enum CallbackType {
    Block = 0,
    FinalizationMessage,
    FinalizationRecord,
}

impl TryFrom<u8> for CallbackType {
    type Error = failure::Error;

    fn try_from(byte: u8) -> Fallible<Self> {
        match byte {
            0 => Ok(CallbackType::Block),
            1 => Ok(CallbackType::FinalizationMessage),
            2 => Ok(CallbackType::FinalizationRecord),
            _ => Err(format_err!("Received invalid callback type: {}", byte)),
        }
    }
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
    ($self:ident, $peer_id:ident, $data:expr, $c_call:expr) => {{
        let baker = $self.runner.load(Ordering::SeqCst);
        let len = $data.len();
        unsafe {
            return $c_call(
                baker,
                $peer_id,
                CString::from_vec_unchecked($data.into()).as_ptr() as *const u8,
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

macro_rules! wrap_c_call {
    ($self:ident, $c_call:expr) => {{
        let baker = $self.runner.load(Ordering::SeqCst);
        unsafe { $c_call(baker) }
    }};
}

impl ConsensusBaker {
    pub fn new(baker_id: u64, genesis_block: Arc<Block>, private_data: Vec<u8>) -> Self {
        info!("Starting up baker {}", baker_id);

        let genesis_data_serialized = genesis_block.get_genesis_data().serialize();
        let genesis_data_len = genesis_data_serialized.len();

        let c_string_genesis =
            unsafe { CString::from_vec_unchecked(genesis_data_serialized.into()) };
        let c_string_private_data = unsafe { CString::from_vec_unchecked(private_data.clone()) };
        let baker = unsafe {
            startBaker(
                c_string_genesis.as_ptr() as *const u8,
                genesis_data_len as i64,
                c_string_private_data.as_ptr() as *const u8,
                private_data.len() as i64,
                on_consensus_data_out,
                on_log_emited,
                on_catchup_block_by_hash,
                on_catchup_finalization_record_by_hash,
                on_catchup_finalization_record_by_index,
            )
        };
        ConsensusBaker {
            id: baker_id,
            genesis_block: Arc::clone(&genesis_block),
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

    pub fn send_block(&self, peer_id: PeerId, block: &Block) -> i64 {
        wrap_send_data_to_c!(self, peer_id, block.serialize(), receiveBlock)
    }

    pub fn send_finalization(&self, peer_id: PeerId, msg: &FinalizationMessage) {
        wrap_send_data_to_c!(self, peer_id, msg.serialize(), receiveFinalization);
    }

    pub fn send_finalization_record(&self, peer_id: PeerId, rec: &FinalizationRecord) -> i64 {
        wrap_send_data_to_c!(self, peer_id, rec.serialize(), receiveFinalizationRecord)
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

    pub fn get_finalization_point(&self) -> Vec<u8> {
        wrap_c_call_bytes!(self, |baker| getFinalizationPoint(baker))
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

    pub fn get_block(&self, _block_hash: &[u8]) -> Vec<u8> {
        wrap_c_call_bytes!(self, |baker| getBlock(baker, _block_hash.as_ptr()))
    }

    pub fn get_block_finalization(&self, _block_hash: &[u8]) -> Vec<u8> {
        wrap_c_call_bytes!(self, |baker| getBlockFinalization(
            baker,
            _block_hash.as_ptr()
        ))
    }

    pub fn get_indexed_finalization(&self, index: FinalizationRecordIndex) -> Vec<u8> {
        wrap_c_call_bytes!(self, |baker| getIndexedFinalization(baker, index))
    }

    pub fn get_finalization_messages(&self, request: &[u8], peer_id: PeerId) -> i64 {
        wrap_c_call!(self, |baker| getFinalizationMessages(
            baker,
            peer_id,
            request.as_ptr(),
            request.len() as i64,
            on_finalization_message_catchup_out
        ))
    }
}

#[derive(Clone)]
pub struct ConsensusOutQueue {
    receiver_block: Arc<Mutex<mpsc::Receiver<Block>>>,
    sender_block: Arc<Mutex<mpsc::Sender<Block>>>,
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
        let (sender, receiver) = mpsc::channel::<Block>();
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
    pub fn send_block(self, block: Block) -> Fallible<()> {
        into_err!(safe_lock!(self.sender_block).send(block))
    }

    pub fn recv_block(self) -> Fallible<Block> { into_err!(safe_lock!(self.receiver_block).recv()) }

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
    static ref CALLBACK_QUEUE: ConsensusOutQueue = { ConsensusOutQueue::default() };
    static ref GENERATED_PRIVATE_DATA: RwLock<HashMap<i64, Vec<u8>>> =
        { RwLock::new(HashMap::new()) };
    static ref GENERATED_GENESIS_DATA: RwLock<Option<Vec<u8>>> = { RwLock::new(None) };
}

type PrivateData = HashMap<i64, Vec<u8>>;

#[derive(Clone)]
pub struct ConsensusContainer {
    genesis_block: Arc<Block>,
    bakers:        Arc<RwLock<HashMap<u64, ConsensusBaker>>>,
}

impl ConsensusContainer {
    pub fn new(genesis_data: Vec<u8>) -> Self {
        let genesis_block = Block {
            slot: 0,
            data: BlockData::GenesisData(
                GenesisData::deserialize(&genesis_data)
                    .expect("Failed to deserialize genesis data!"),
            ),
        };

        info!("Created a genesis block: {:?}", genesis_block);

        ConsensusContainer {
            genesis_block: Arc::new(genesis_block),
            bakers:        Arc::new(RwLock::new(HashMap::new())),
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
            ConsensusBaker::new(baker_id, Arc::clone(&self.genesis_block), private_data),
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

    pub fn send_block(&self, peer_id: PeerId, block: &Block) -> i64 {
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
        0
    }

    pub fn send_transaction(&self, tx: &[u8]) -> i64 {
        if let Some((_, baker)) = safe_read!(self.bakers).iter().next() {
            return baker.send_transaction(tx.to_vec());
        }
        -1
    }

    pub fn generate_data(num_bakers: u64) -> Fallible<(Vec<u8>, PrivateData)> {
        // doesn't seem to apply yet
        let genesis_time = Utc::now().timestamp_millis() as u64;

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

extern "C" fn on_consensus_data_out(block_type: i64, block_data: *const u8, data_length: i64) {
    debug!("Callback hit - queueing message");

    unsafe {
        let data = slice::from_raw_parts(block_data as *const u8, data_length as usize);
        let callback_type = match CallbackType::try_from(block_type as u8) {
            Ok(ct) => ct,
            Err(e) => {
                error!("{}", e);
                return;
            }
        };

        match callback_type {
            CallbackType::Block => match Block::deserialize(data) {
                Ok(block) => match CALLBACK_QUEUE.clone().send_block(block) {
                    Ok(_) => debug!("Queueing {} block bytes", data_length),
                    _ => error!("Didn't queue block message properly"),
                },
                Err(e) => error!("Deserialization of block failed: {:?}", e),
            },
            CallbackType::FinalizationMessage => match FinalizationMessage::deserialize(data) {
                Ok(msg) => match CALLBACK_QUEUE.clone().send_finalization(msg) {
                    Ok(_) => debug!("Queueing {} bytes of finalization", data.len()),
                    _ => error!("Didn't queue finalization message properly"),
                },
                Err(e) => error!("Deserialization of finalization message failed: {:?}", e),
            },
            CallbackType::FinalizationRecord => match FinalizationRecord::deserialize(data) {
                Ok(rec) => match CALLBACK_QUEUE.clone().send_finalization_record(rec) {
                    Ok(_) => debug!("Queueing {} bytes of finalization record", data.len()),
                    _ => error!("Didn't queue finalization record message properly"),
                },
                Err(e) => error!("Deserialization of finalization record failed: {:?}", e),
            },
        }
    }
}

extern "C" fn on_catchup_block_by_hash(peer_id: PeerId, hash: *const u8) {
    debug!("Got a request for catchup from consensus");
    unsafe {
        let s = slice::from_raw_parts(hash, common::SHA256 as usize).to_vec();
        catchup_en_queue(CatchupRequest::BlockByHash(peer_id, s));
    }
}

extern "C" fn on_catchup_finalization_record_by_hash(peer_id: PeerId, hash: *const u8) {
    debug!("Got a request for catchup from consensus");
    unsafe {
        let s = slice::from_raw_parts(hash, common::SHA256 as usize).to_vec();
        catchup_en_queue(CatchupRequest::FinalizationRecordByHash(peer_id, s));
    }
}

extern "C" fn on_catchup_finalization_record_by_index(
    peer_id: PeerId,
    index: FinalizationRecordIndex,
) {
    catchup_en_queue(CatchupRequest::FinalizationRecordByIndex(peer_id, index));
}

pub enum CatchupRequest {
    BlockByHash(PeerId, Vec<u8>),
    FinalizationRecordByHash(PeerId, Vec<u8>),
    FinalizationRecordByIndex(PeerId, FinalizationRecordIndex),
}

fn catchup_en_queue(req: CatchupRequest) {
    match CALLBACK_QUEUE.clone().send_catchup(req) {
        Ok(_) => debug!("Queueing catchup request"),
        _ => error!("Didn't queue catchup requestproperly"),
    }
}

extern "C" fn on_finalization_message_catchup_out(peer_id: PeerId, data: *const u8, len: i64) {
    debug!("Callback hit - queueing message");
    unsafe {
        let s = slice::from_raw_parts(data as *const u8, len as usize);
        match FinalizationMessage::deserialize(s) {
            Ok(msg) => match CALLBACK_QUEUE
                .clone()
                .send_finalization_catchup((peer_id, msg))
            {
                Ok(_) => debug!("Queueing {} bytes of finalization", s.len()),
                _ => error!("Didn't queue finalization message properly"),
            },
            Err(e) => error!("Deserialization of finalization message failed: {:?}", e),
        }
    }
}

/// Following the implementation of the log crate, error = 1, warning = 2, info
/// = 3, 4 = debug, any other option is considered as trace.
extern "C" fn on_log_emited(identifier: c_char, log_level: c_char, log_message: *const u8) {
    fn identifier_to_string(id: c_char) -> &'static str {
        match id {
            1 => "Runner",
            2 => "Afgjort",
            3 => "Birk",
            4 => "Crypto",
            5 => "Kontrol",
            6 => "Skov",
            7 => "Baker",
            _ => "External",
        }
    }

    let msg = unsafe { CStr::from_ptr(log_message as *const c_char) }
        .to_str()
        .expect("log_callback: unable to decode as UTF-8");
    let id = identifier_to_string(identifier);

    match log_level as u8 {
        1 => error!("{}: {}", id, msg),
        2 => warn!("{}: {}", id, msg),
        3 => info!("{}: {}", id, msg),
        4 => debug!("{}: {}", id, msg),
        _ => trace!("{}: {}", id, msg),
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
        ($num_bakers:expr, $blocks_num:expr) => {
            let (genesis_data, private_data) = ConsensusContainer::generate_data($num_bakers)
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
        ConsensusContainer::start_haskell();
        bakers_test!(5, 10);
        bakers_test!(10, 5);
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
