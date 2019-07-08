use byteorder::{NetworkEndian, ReadBytesExt};
use failure::{format_err, Fallible};

use std::{
    convert::TryFrom,
    ffi::{CStr, CString},
    fmt,
    io::Cursor,
    mem,
    os::raw::{c_char, c_int},
    slice,
    sync::{
        atomic::{AtomicBool, AtomicPtr, Ordering},
        Once, ONCE_INIT,
    },
};

use crate::consensus::*;
use concordium_global_state::{block::*, common, finalization::*};

extern "C" {
    pub fn hs_init(argc: *mut c_int, argv: *mut *mut *mut c_char);
    pub fn hs_init_with_rtsopts(argc: &c_int, argv: *const *const *const c_char);
    pub fn hs_exit();
}

static START_ONCE: Once = ONCE_INIT;
static STOP_ONCE: Once = ONCE_INIT;
static STOPPED: AtomicBool = AtomicBool::new(false);

/// Initialize the Haskell runtime. This function is safe to call more than
/// once, and will do nothing on subsequent calls.
///
/// The runtime will automatically be shutdown at program exit, or you can stop
/// it earlier with `stop`.
#[cfg(all(not(windows), feature = "profiling"))]
pub fn start_haskell(heap: &str, time: bool) {
    START_ONCE.call_once(|| {
        start_haskell_init(heap, time);
        unsafe {
            ::libc::atexit(stop_nopanic);
        }
    });
}

#[cfg(not(feature = "profiling"))]
pub fn start_haskell() {
    START_ONCE.call_once(|| {
        start_haskell_init();
        unsafe {
            ::libc::atexit(stop_nopanic);
        }
    });
}

#[cfg(all(not(windows), feature = "profiling"))]
fn start_haskell_init(heap: &str, time: bool) {
    let program_name = std::env::args().take(1).next().unwrap();
    let mut args = vec![program_name.as_str()];

    match heap {
        "cost" => {
            args.push("+RTS");
            args.push("-hc");
        }
        "module" => {
            args.push("+RTS");
            args.push("-hm");
        }
        "description" => {
            args.push("+RTS");
            args.push("-hd");
        }
        "type" => {
            args.push("+RTS");
            args.push("-hy");
        }
        "none" => {}
        _ => {
            error!("Wrong heap profiling option provided: {}", heap);
        }
    }

    if time {
        if args.len() == 1 {
            args.push("+RTS");
        }
        args.push("-p");
    }

    if args.len() > 1 {
        args.push("-RTS");
    }

    info!(
        "Starting baker with the following profiling arguments {:?}",
        args
    );
    let args = args
        .iter()
        .map(|arg| CString::new(*arg).unwrap())
        .collect::<Vec<CString>>();
    let c_args = args
        .iter()
        .map(|arg| arg.as_ptr())
        .collect::<Vec<*const c_char>>();
    let ptr_c_argc = &(c_args.len() as c_int);
    let ptr_c_argv = &c_args.as_ptr();
    unsafe {
        hs_init_with_rtsopts(ptr_c_argc, ptr_c_argv as *const *const *const c_char);
    }
}

#[cfg(all(not(windows), not(feature = "profiling")))]
fn start_haskell_init() {
    let program_name = std::env::args().take(1).next();
    let args = program_name
        .into_iter()
        .map(|arg| CString::new(arg).unwrap())
        .collect::<Vec<CString>>();
    let c_args = args
        .iter()
        .map(|arg| arg.as_ptr())
        .collect::<Vec<*const c_char>>();
    let ptr_c_argc = &(c_args.len() as c_int);
    let ptr_c_argv = &c_args.as_ptr();
    unsafe {
        hs_init_with_rtsopts(ptr_c_argc, ptr_c_argv as *const *const *const c_char);
    }
}

#[cfg(windows)]
fn start_haskell_init(_: bool, _: bool) {
    // GHC on Windows ignores hs_init arguments and uses GetCommandLineW instead
    // See https://hackage.haskell.org/package/base-4.9.0.0/docs/src/GHC.Environment.html
    let mut argv0 = *b"\0";
    let mut argv = [argv0.as_mut_ptr() as *mut c_char, ptr::null_mut()];
    let mut argc = 1;
    unsafe {
        hs_init(&mut argc, &mut argv.as_mut_ptr());
    }
}

/// Stop the Haskell runtime before the program exits. This function may only be
/// called once during a program's execution.
///
/// It is safe, but not useful, to call this before the runtime has started.
///
/// # Panics
///
/// Will panic if called more than once.
pub fn stop_haskell() {
    if STOPPED.swap(true, Ordering::SeqCst) {
        panic!("The GHC runtime may only be stopped once. See \
                https://downloads.haskell.org/%7Eghc/latest/docs/html/users_guide\
                /ffi-chap.html#id1 ");
    }
    stop_nopanic();
}

extern "C" fn stop_nopanic() {
    STOP_ONCE.call_once(|| {
        unsafe { hs_exit() }; // does nothing if hs_init_count <= 0
    });
}

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
            .copied()
            .ok_or_else(|| format_err!("Unsupported packet type ({})", value))
    }
}

impl fmt::Display for PacketType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            PacketType::Block => "block",
            PacketType::Transaction => "transaction",
            PacketType::FinalizationRecord => "finalization record",
            PacketType::FinalizationMessage => "finalization message",
            PacketType::CatchupBlockByHash => "\"catch-up block by hash\" request",
            PacketType::CatchupFinalizationRecordByHash => {
                "\"catch-up finalization record by hash\" request"
            }
            PacketType::CatchupFinalizationRecordByIndex => {
                "\"catch-up finalization record by index\" request"
            }
            PacketType::CatchupFinalizationMessagesByPoint => {
                "\"catch-up finalization messages by point\" request"
            }
        };

        write!(f, "{}", name)
    }
}

#[derive(Debug)]
pub enum ConsensusFfiResponse {
    BakerNotFound = -1,
    Success,
    DeserializationError,
    InvalidResult,
    PendingBlock,
    PendingFinalization,
    Asynchronous,
    DuplicateEntry,
    Stale,
    IncorrectFinalizationSession,
}

impl ConsensusFfiResponse {
    pub fn is_acceptable(&self) -> bool {
        use ConsensusFfiResponse::*;

        match self {
            BakerNotFound | DeserializationError | InvalidResult => false,
            _ => true,
        }
    }
}

impl TryFrom<i64> for ConsensusFfiResponse {
    type Error = failure::Error;

    #[inline]
    fn try_from(value: i64) -> Fallible<ConsensusFfiResponse> {
        use ConsensusFfiResponse::*;

        match value {
            -1 => Ok(BakerNotFound),
            0 => Ok(Success),
            1 => Ok(DeserializationError),
            2 => Ok(InvalidResult),
            3 => Ok(PendingBlock),
            4 => Ok(PendingFinalization),
            5 => Ok(Asynchronous),
            6 => Ok(DuplicateEntry),
            7 => Ok(Stale),
            8 => Ok(IncorrectFinalizationSession),
            _ => Err(format_err!("Unsupported FFI return code ({})", value)),
        }
    }
}

#[repr(C)]
pub struct baker_runner {
    private: [u8; 0],
}

type ConsensusDataOutCallback = extern "C" fn(i64, *const u8, i64);
type LogCallback = extern "C" fn(c_char, c_char, *const u8);
type CatchupFinalizationRequestByBlockHashDeltaCallback =
    unsafe extern "C" fn(peer_id: PeerId, hash: *const u8, delta: Delta);
type CatchupFinalizationRequestByBlockHashCallback =
    unsafe extern "C" fn(peer_id: PeerId, hash: *const u8);
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
        missing_block_callback: CatchupFinalizationRequestByBlockHashDeltaCallback,
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
    ) -> i64;
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

    // Consensus queries
    pub fn getConsensusStatus(baker: *mut baker_runner) -> *const c_char;
    pub fn getBlockInfo(baker: *mut baker_runner, block_hash: *const u8) -> *const c_char;
    pub fn getAncestors(
        baker: *mut baker_runner,
        block_hash: *const u8,
        amount: u64,
    ) -> *const c_char;
    pub fn getBranches(baker: *mut baker_runner) -> *const c_char;

    // State queries
    pub fn getAccountList(baker: *mut baker_runner, block_hash: *const u8) -> *const c_char;
    pub fn getInstances(baker: *mut baker_runner, block_hash: *const u8) -> *const c_char;
    pub fn getAccountInfo(
        baker: *mut baker_runner,
        block_hash: *const u8,
        account_address: *const u8,
    ) -> *const c_char;
    pub fn getInstanceInfo(
        baker: *mut baker_runner,
        block_hash: *const u8,
        contract_address: *const u8,
    ) -> *const c_char;
    pub fn getRewardStatus(baker: *mut baker_runner, block_hash: *const u8) -> *const c_char;
    pub fn getBirkParameters(baker: *mut baker_runner, block_hash: *const u8) -> *const c_char;
    pub fn getModuleList(baker: *mut baker_runner, block_hash: *const u8) -> *const c_char;
    pub fn getModuleSource(
        baker: *mut baker_runner,
        block_hash: *const u8,
        module_ref: *const u8,
    ) -> *const u8;
    pub fn getBlock(baker: *mut baker_runner, block_hash: *const u8) -> *const u8;
    pub fn getBlockDelta(
        baker: *mut baker_runner,
        block_hash: *const u8,
        delta: Delta,
    ) -> *const u8;
    pub fn getBlockFinalization(baker: *mut baker_runner, block_hash: *const u8) -> *const u8;
    pub fn getIndexedFinalization(
        baker: *mut baker_runner,
        finalization_index: FinalizationIndex,
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

pub struct ConsensusBaker {
    pub id:           BakerId,
    pub genesis_data: Bytes,
    pub runner:       AtomicPtr<baker_runner>,
}

impl ConsensusBaker {
    pub fn new(baker_id: BakerId, genesis_data: Vec<u8>, private_data: Vec<u8>) -> Self {
        info!("Starting up baker {}", baker_id);

        let genesis_data_len = genesis_data.len();
        let private_data_len = private_data.len();

        let c_string_genesis = unsafe { CString::from_vec_unchecked(genesis_data.clone()) };
        let c_string_private_data = unsafe { CString::from_vec_unchecked(private_data) };

        let baker = unsafe {
            startBaker(
                c_string_genesis.as_ptr() as *const u8,
                genesis_data_len as i64,
                c_string_private_data.as_ptr() as *const u8,
                private_data_len as i64,
                on_consensus_data_out,
                on_log_emited,
                on_catchup_block_by_hash,
                on_catchup_finalization_record_by_hash,
                on_catchup_finalization_record_by_index,
            )
        };

        // private_data appears to (might be too early to deserialize yet) contain:
        // a u64 BakerId
        // 3 32B-long ByteStrings (with u64 length prefixes), the latter 2 of which are
        // 32B of unknown content
        // 2x identical 32B-long byte sequences

        ConsensusBaker {
            id:           baker_id,
            genesis_data: genesis_data.into_boxed_slice(),
            runner:       AtomicPtr::new(baker),
        }
    }

    pub fn stop(&self) {
        let baker = self.runner.load(Ordering::SeqCst);
        unsafe {
            stopBaker(baker);
        }
    }

    pub fn send_block(&self, peer_id: PeerId, block: &[u8]) -> ConsensusFfiResponse {
        wrap_send_data_to_c!(self, peer_id, block, receiveBlock)
    }

    pub fn send_finalization(&self, peer_id: PeerId, msg: &[u8]) -> ConsensusFfiResponse {
        wrap_send_data_to_c!(self, peer_id, msg, receiveFinalization)
    }

    pub fn send_finalization_record(&self, peer_id: PeerId, rec: &[u8]) -> ConsensusFfiResponse {
        wrap_send_data_to_c!(self, peer_id, rec, receiveFinalizationRecord)
    }

    pub fn send_transaction(&self, data: Vec<u8>) -> ConsensusFfiResponse {
        let baker = self.runner.load(Ordering::SeqCst);
        let len = data.len();

        let result = unsafe {
            receiveTransaction(
                baker,
                CString::from_vec_unchecked(data).as_ptr() as *const u8,
                len as i64,
            )
        };

        ConsensusFfiResponse::try_from(result)
            .unwrap_or_else(|code| panic!("Unknown FFI return code: {}", code))
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

    pub fn get_account_list(&self, block_hash: &str) -> String {
        let block_hash = CString::new(block_hash).unwrap();
        wrap_c_call_string!(self, baker, |baker| getAccountList(
            baker,
            block_hash.as_ptr() as *const u8
        ))
    }

    pub fn get_instances(&self, block_hash: &str) -> String {
        let block_hash = CString::new(block_hash).unwrap();
        wrap_c_call_string!(self, baker, |baker| getInstances(
            baker,
            block_hash.as_ptr() as *const u8
        ))
    }

    pub fn get_account_info(&self, block_hash: &str, account_address: &str) -> String {
        let block_hash = CString::new(block_hash).unwrap();
        let account_address = CString::new(account_address).unwrap();
        wrap_c_call_string!(self, baker, |baker| getAccountInfo(
            baker,
            block_hash.as_ptr() as *const u8,
            account_address.as_ptr() as *const u8
        ))
    }

    pub fn get_instance_info(&self, block_hash: &str, contract_address: &str) -> String {
        let block_hash = CString::new(block_hash).unwrap();
        let contract_address = CString::new(contract_address).unwrap();
        wrap_c_call_string!(self, baker, |baker| getInstanceInfo(
            baker,
            block_hash.as_ptr() as *const u8,
            contract_address.as_ptr() as *const u8
        ))
    }

    pub fn get_reward_status(&self, block_hash: &str) -> String {
        let block_hash = CString::new(block_hash).unwrap();
        wrap_c_call_string!(self, baker, |baker| getRewardStatus(
            baker,
            block_hash.as_ptr() as *const u8,
        ))
    }

    pub fn get_birk_parameters(&self, block_hash: &str) -> String {
        let block_hash = CString::new(block_hash).unwrap();
        wrap_c_call_string!(self, baker, |baker| getBirkParameters(
            baker,
            block_hash.as_ptr() as *const u8,
        ))
    }

    pub fn get_module_list(&self, block_hash: &str) -> String {
        let block_hash = CString::new(block_hash).unwrap();
        wrap_c_call_string!(self, baker, |baker| getModuleList(
            baker,
            block_hash.as_ptr() as *const u8,
        ))
    }

    pub fn get_module_source(&self, block_hash: &str, module_ref: &str) -> Vec<u8> {
        let block_hash = CString::new(block_hash).unwrap();
        let module_ref = CString::new(module_ref).unwrap();
        wrap_c_call_bytes!(self, |baker| getModuleSource(
            baker,
            block_hash.as_ptr() as *const u8,
            module_ref.as_ptr() as *const u8
        ))
    }

    pub fn get_block(&self, _block_hash: &[u8]) -> Vec<u8> {
        wrap_c_call_bytes!(self, |baker| getBlock(baker, _block_hash.as_ptr()))
    }

    pub fn get_block_by_delta(&self, _block_hash: &[u8], delta: Delta) -> Vec<u8> {
        wrap_c_call_bytes!(self, |baker| getBlockDelta(
            baker,
            _block_hash.as_ptr(),
            delta
        ))
    }

    pub fn get_block_finalization(&self, _block_hash: &[u8]) -> Vec<u8> {
        wrap_c_call_bytes!(self, |baker| getBlockFinalization(
            baker,
            _block_hash.as_ptr()
        ))
    }

    pub fn get_indexed_finalization(&self, index: FinalizationIndex) -> Vec<u8> {
        wrap_c_call_bytes!(self, |baker| getIndexedFinalization(baker, index))
    }

    pub fn get_finalization_messages(
        &self,
        request: &[u8],
        peer_id: PeerId,
    ) -> ConsensusFfiResponse {
        wrap_c_call!(self, |baker| getFinalizationMessages(
            baker,
            peer_id,
            request.as_ptr(),
            request.len() as i64,
            on_finalization_message_catchup_out
        ))
    }
}

pub enum CallbackType {
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

pub extern "C" fn on_genesis_generated(genesis_data: *const u8, data_length: i64) {
    unsafe {
        let s = slice::from_raw_parts(genesis_data as *const u8, data_length as usize);
        *safe_write!(GENERATED_GENESIS_DATA) = Some(s.to_owned());
    }
}

pub extern "C" fn on_private_data_generated(
    baker_id: i64,
    private_data: *const u8,
    data_length: i64,
) {
    unsafe {
        let s = slice::from_raw_parts(private_data as *const u8, data_length as usize);
        safe_write!(GENERATED_PRIVATE_DATA).insert(baker_id, s.to_owned());
    }
}

pub extern "C" fn on_consensus_data_out(block_type: i64, block_data: *const u8, data_length: i64) {
    debug!("Callback hit - queueing message");

    unsafe {
        let data = Box::from(slice::from_raw_parts(
            block_data as *const u8,
            data_length as usize,
        ));

        let callback_type = match CallbackType::try_from(block_type as u8) {
            Ok(ct) => ct,
            Err(e) => {
                error!("{}", e);
                return;
            }
        };

        let message_variant = match callback_type {
            CallbackType::Block => PacketType::Block,
            CallbackType::FinalizationMessage => PacketType::FinalizationMessage,
            CallbackType::FinalizationRecord => PacketType::FinalizationRecord,
        };

        let message = ConsensusMessage::new(message_variant, None, data);

        match CALLBACK_QUEUE.send_message(message) {
            Ok(_) => debug!("Queueing a {} of {} bytes", message_variant, data_length),
            _ => error!("Couldn't queue a {} properly", message_variant),
        };
    }
}

pub unsafe extern "C" fn on_catchup_block_by_hash(peer_id: PeerId, hash: *const u8, delta: Delta) {
    let mut payload = slice::from_raw_parts(hash, common::SHA256 as usize).to_owned();
    let delta_array = mem::transmute::<Delta, [u8; 8]>(delta);
    payload.extend_from_slice(&delta_array);
    let payload = payload.into_boxed_slice();

    catchup_enqueue(ConsensusMessage::new(
        PacketType::CatchupBlockByHash,
        Some(peer_id),
        payload,
    ));
}

pub unsafe extern "C" fn on_catchup_finalization_record_by_hash(peer_id: PeerId, hash: *const u8) {
    let payload = Box::from(slice::from_raw_parts(hash, common::SHA256 as usize));

    catchup_enqueue(ConsensusMessage::new(
        PacketType::CatchupFinalizationRecordByHash,
        Some(peer_id),
        payload,
    ));
}

pub extern "C" fn on_catchup_finalization_record_by_index(
    peer_id: PeerId,
    index: FinalizationIndex,
) {
    let payload = unsafe { Box::from(mem::transmute::<FinalizationIndex, [u8; 8]>(index)) };

    catchup_enqueue(ConsensusMessage::new(
        PacketType::CatchupFinalizationRecordByIndex,
        Some(peer_id),
        payload,
    ));
}

pub extern "C" fn on_finalization_message_catchup_out(peer_id: PeerId, data: *const u8, len: i64) {
    debug!("Got a catch-up request for finalization messages for point from consensus",);
    unsafe {
        let payload = Box::from(slice::from_raw_parts(data as *const u8, len as usize));

        catchup_enqueue(ConsensusMessage::new(
            PacketType::FinalizationMessage,
            Some(peer_id),
            payload,
        ))
    }
}

/// Following the implementation of the log crate, error = 1, warning = 2, info
/// = 3, 4 = debug, any other option is considered as trace.
pub extern "C" fn on_log_emited(identifier: c_char, log_level: c_char, log_message: *const u8) {
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
        3 | 4 => debug!("{}: {}", id, msg),
        _ => trace!("{}: {}", id, msg),
    };
}
