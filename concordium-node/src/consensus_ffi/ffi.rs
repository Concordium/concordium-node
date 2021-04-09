use crate::{
    consensus_ffi::{
        blockchain_types::BlockHash,
        catch_up::*,
        consensus::*,
        helpers::{ConsensusFfiResponse, ConsensusIsInBakingCommitteeResponse, PacketType},
        messaging::*,
    },
    write_or_die,
};
use byteorder::{NetworkEndian, ReadBytesExt};
use crypto_common::Serial;
use failure::{bail, format_err, Fallible};
use std::{
    convert::TryFrom,
    ffi::{CStr, CString},
    io::{Cursor, Write},
    os::raw::{c_char, c_int},
    path::PathBuf,
    slice,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Once, RwLock,
    },
};

extern "C" {
    pub fn hs_init(argc: *mut c_int, argv: *mut *mut *mut c_char);
    pub fn hs_init_with_rtsopts(argc: &c_int, argv: *const *const *const c_char);
    pub fn hs_exit();
}

static START_ONCE: Once = Once::new();
static STOP_ONCE: Once = Once::new();
static STOPPED: AtomicBool = AtomicBool::new(false);
pub type Delta = u64;

/// Initialize the Haskell runtime. This function is safe to call more than
/// once, and will do nothing on subsequent calls.
///
/// The runtime will automatically be shutdown at program exit, or you can stop
/// it earlier with `stop`.
///
/// If `rts_flags` doesn't contain `--install-signal-handlers` we explicitly
/// set this to `no` to avoid the runtime consuming the signals before the outer
/// rust part embedding the runtime.
#[cfg(feature = "profiling")]
pub fn start_haskell(
    heap: &str,
    stack: bool,
    time: bool,
    exceptions: bool,
    gc_log: Option<String>,
    profile_sampling_interval: &str,
    rts_flags: &[String],
) {
    START_ONCE.call_once(|| {
        start_haskell_init(
            heap,
            stack,
            time,
            exceptions,
            gc_log,
            profile_sampling_interval,
            rts_flags,
        );
        unsafe {
            ::libc::atexit(stop_nopanic);
        }
    });
}

#[cfg(not(feature = "profiling"))]
pub fn start_haskell(rts_flags: &[String]) {
    START_ONCE.call_once(|| {
        start_haskell_init(rts_flags);
        unsafe {
            ::libc::atexit(stop_nopanic);
        }
    });
}

#[cfg(feature = "profiling")]
fn start_haskell_init(
    heap: &str,
    stack: bool,
    time: bool,
    exceptions: bool,
    gc_log: Option<String>,
    profile_sampling_interval: &str,
    rts_flags: &[String],
) {
    let program_name = std::env::args().take(1).next().unwrap();
    let mut args = vec![program_name];

    // We don't check for stack here because it should only be enabled if
    // heap profiling is enabled.
    if heap != "none" || time || gc_log.is_some() || profile_sampling_interval != "0.1" {
        args.push("+RTS".to_owned());
        args.push("-L100".to_owned());
    }

    match heap {
        "cost" => {
            args.push("-hc".to_owned());
            if stack {
                args.push("-xt".to_owned());
            }
        }
        "module" => {
            args.push("-hm".to_owned());
            if stack {
                args.push("-xt".to_owned());
            }
        }
        "description" => {
            args.push("-hd".to_owned());
            if stack {
                args.push("-xt".to_owned());
            }
        }
        "type" => {
            args.push("-hy".to_owned());
            if stack {
                args.push("-xt".to_owned());
            }
        }
        "none" => {}
        _ => {
            error!("Wrong heap profiling option provided: {}", heap);
        }
    }

    if profile_sampling_interval != "0.1" {
        args.push(format!("-i{}", profile_sampling_interval));
    }

    if time {
        args.push("-p".to_owned());
    }

    if let Some(log) = gc_log {
        args.push(format!("-S{}", log));
    }

    if exceptions {
        if args.len() == 1 {
            args.push("+RTS".to_owned());
        }
        args.push("-xc".to_owned());
    }

    if args.len() == 1 {
        args.push("+RTS".to_owned())
    }

    if rts_flags.iter().all(|arg| !arg.trim().starts_with("--install-signal-handlers")) {
        args.push("--install-signal-handlers=no".to_owned());
    }

    for flag in rts_flags {
        if !flag.trim().is_empty() {
            args.push(flag.to_owned());
        }
    }

    if args.len() > 1 {
        args.push("-RTS".to_owned());
    }

    info!("Starting consensus with the following profiling arguments {:?}", args);
    let args =
        args.iter().map(|arg| CString::new(arg.as_bytes()).unwrap()).collect::<Vec<CString>>();
    let c_args = args.iter().map(|arg| arg.as_ptr()).collect::<Vec<*const c_char>>();
    let ptr_c_argc = &(c_args.len() as c_int);
    let ptr_c_argv = &c_args.as_ptr();
    unsafe {
        hs_init_with_rtsopts(ptr_c_argc, ptr_c_argv as *const *const *const c_char);
    }
}

#[cfg(not(feature = "profiling"))]
fn start_haskell_init(rts_flags: &[String]) {
    let program_name = std::env::args().take(1).next().unwrap();
    let mut args = vec![program_name];
    if !rts_flags.is_empty() {
        args.push("+RTS".to_owned());
        if rts_flags.iter().all(|arg| !arg.trim().starts_with("--install-signal-handlers")) {
            args.push("--install-signal-handlers=no".to_owned());
        }
        for flag in rts_flags {
            if !flag.trim().is_empty() {
                args.push(flag.to_owned());
            }
        }
        args.push("-RTS".to_owned());
    } else {
        args.push("+RTS".to_owned());
        args.push("--install-signal-handlers=no".to_owned());
        args.push("-RTS".to_owned());
    }
    let args =
        args.iter().map(|arg| CString::new(arg.as_bytes()).unwrap()).collect::<Vec<CString>>();
    let c_args = args.iter().map(|arg| arg.as_ptr()).collect::<Vec<*const c_char>>();
    let ptr_c_argc = &(c_args.len() as c_int);
    let ptr_c_argv = &c_args.as_ptr();
    unsafe {
        hs_init_with_rtsopts(ptr_c_argc, ptr_c_argv as *const *const *const c_char);
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

#[repr(C)]
pub struct consensus_runner {
    private: [u8; 0],
}

type LogCallback = extern "C" fn(c_char, c_char, *const u8);
type BroadcastCallback = extern "C" fn(i64, *const u8, i64);
type CatchUpStatusCallback = extern "C" fn(*const u8, i64);
type DirectMessageCallback =
    extern "C" fn(peer_id: PeerId, message_type: i64, msg: *const c_char, msg_len: i64);
type RegenesisCallback = unsafe extern "C" fn(*const RwLock<Vec<BlockHash>>, *const u8);
type RegenesisFreeCallback = unsafe extern "C" fn(*const RwLock<Vec<BlockHash>>);

#[allow(improper_ctypes)]
extern "C" {
    pub fn startConsensus(
        max_block_size: u64,
        block_construction_timeout: u64,
        insertions_before_purging: u64,
        transaction_keep_alive: u64,
        transactions_purging_delay: u64,
        genesis_data: *const u8,
        genesis_data_len: i64,
        private_data: *const u8,
        private_data_len: i64,
        broadcast_callback: BroadcastCallback,
        catchup_status_callback: CatchUpStatusCallback,
        regenesis_arc: *const RwLock<Vec<BlockHash>>,
        free_regenesis_arc: RegenesisFreeCallback,
        regenesis_callback: RegenesisCallback,
        maximum_log_level: u8,
        log_callback: LogCallback,
        appdata_dir: *const u8,
        appdata_dir_len: i64,
        database_connection_url: *const u8,
        database_connection_url_len: i64,
        runner_ptr_ptr: *mut *mut consensus_runner,
    ) -> i64;
    pub fn startConsensusPassive(
        max_block_size: u64,
        block_construction_timeout: u64,
        insertions_before_purging: u64,
        transaction_keep_alive: u64,
        transactions_purging_delay: u64,
        genesis_data: *const u8,
        genesis_data_len: i64,
        catchup_status_callback: CatchUpStatusCallback,
        regenesis_arc: *const RwLock<Vec<BlockHash>>,
        free_regenesis_arc: RegenesisFreeCallback,
        regenesis_callback: RegenesisCallback,
        maximum_log_level: u8,
        log_callback: LogCallback,
        appdata_dir: *const u8,
        appdata_dir_len: i64,
        database_connection_url: *const u8,
        database_connection_url_len: i64,
        runner_ptr_ptr: *mut *mut consensus_runner,
    ) -> i64;
    #[allow(improper_ctypes)]
    pub fn startBaker(consensus: *mut consensus_runner);
    pub fn receiveBlock(
        consensus: *mut consensus_runner,
        block_data: *const u8,
        data_length: i64,
    ) -> i64;
    pub fn receiveFinalization(
        consensus: *mut consensus_runner,
        finalization_data: *const u8,
        data_length: i64,
    ) -> i64;
    pub fn receiveFinalizationRecord(
        consensus: *mut consensus_runner,
        finalization_data: *const u8,
        data_length: i64,
    ) -> i64;
    pub fn receiveTransaction(
        consensus: *mut consensus_runner,
        tx: *const u8,
        data_length: i64,
    ) -> i64;
    pub fn stopBaker(consensus: *mut consensus_runner);
    pub fn stopConsensus(consensus: *mut consensus_runner);

    // Consensus queries
    pub fn getConsensusStatus(consensus: *mut consensus_runner) -> *const c_char;
    pub fn getBlockInfo(consensus: *mut consensus_runner, block_hash: *const u8) -> *const c_char;
    pub fn getAncestors(
        consensus: *mut consensus_runner,
        block_hash: *const u8,
        amount: u64,
    ) -> *const c_char;
    pub fn getBranches(consensus: *mut consensus_runner) -> *const c_char;

    // State queries
    pub fn getAccountList(consensus: *mut consensus_runner, block_hash: *const u8)
        -> *const c_char;
    pub fn getInstances(consensus: *mut consensus_runner, block_hash: *const u8) -> *const c_char;
    pub fn getAccountInfo(
        consensus: *mut consensus_runner,
        block_hash: *const u8,
        account_address: *const u8,
    ) -> *const c_char;
    pub fn getInstanceInfo(
        consensus: *mut consensus_runner,
        block_hash: *const u8,
        contract_address: *const u8,
    ) -> *const c_char;
    pub fn getRewardStatus(
        consensus: *mut consensus_runner,
        block_hash: *const u8,
    ) -> *const c_char;
    pub fn getBirkParameters(
        consensus: *mut consensus_runner,
        block_hash: *const u8,
    ) -> *const c_char;
    pub fn getModuleList(consensus: *mut consensus_runner, block_hash: *const u8) -> *const c_char;
    pub fn getModuleSource(
        consensus: *mut consensus_runner,
        block_hash: *const u8,
        module_ref: *const u8,
    ) -> *const u8;
    pub fn freeCStr(hstring: *const c_char);
    pub fn getCatchUpStatus(consensus: *mut consensus_runner) -> *const u8;
    pub fn receiveCatchUpStatus(
        consensus: *mut consensus_runner,
        peer_id: PeerId,
        msg: *const u8,
        msg_len: i64,
        object_limit: i64,
        direct_callback: DirectMessageCallback,
    ) -> i64;
    pub fn bakerIdBestBlock(consensus: *mut consensus_runner) -> i64;
    pub fn checkIfWeAreFinalizer(consensus: *mut consensus_runner) -> u8;
    pub fn getAccountNonFinalizedTransactions(
        consensus: *mut consensus_runner,
        account_address: *const u8,
    ) -> *const c_char;
    pub fn getBlockSummary(
        consensus: *mut consensus_runner,
        block_hash: *const u8,
    ) -> *const c_char;
    pub fn getBlocksAtHeight(consensus: *mut consensus_runner, block_height: u64) -> *const c_char;
    pub fn getTransactionStatus(
        consensus: *mut consensus_runner,
        transaction_hash: *const u8,
    ) -> *const c_char;
    pub fn getTransactionStatusInBlock(
        consensus: *mut consensus_runner,
        transaction_hash: *const u8,
        block_hash: *const u8,
    ) -> *const c_char;
    pub fn getNextAccountNonce(
        consensus: *mut consensus_runner,
        account_address: *const u8,
    ) -> *const c_char;
    pub fn getAllIdentityProviders(
        consensus: *mut consensus_runner,
        block_hash: *const u8,
    ) -> *const c_char;
    pub fn getAllAnonymityRevokers(
        consensus: *mut consensus_runner,
        block_hash: *const u8,
    ) -> *const c_char;
    pub fn getCryptographicParameters(
        consensus: *mut consensus_runner,
        block_hash: *const u8,
    ) -> *const c_char;
    pub fn importBlocks(
        consensus: *mut consensus_runner,
        import_file_path: *const u8,
        import_file_path_len: i64,
    ) -> u8;
}

// TODO : Simplify arguments to function, or group with struct
#[allow(clippy::too_many_arguments)]
pub fn get_consensus_ptr(
    max_block_size: u32,
    block_construction_timeout: u32,
    insertions_before_purging: u32,
    transaction_keep_alive: u32,
    transactions_purging_delay: u32,
    genesis_data: Vec<u8>,
    private_data: Option<Vec<u8>>,
    maximum_log_level: ConsensusLogLevel,
    appdata_dir: &PathBuf,
    database_connection_url: &str,
    regenesis_arc: Arc<RwLock<Vec<BlockHash>>>,
) -> Fallible<*mut consensus_runner> {
    let genesis_data_len = genesis_data.len();

    let c_string_genesis = unsafe { CString::from_vec_unchecked(genesis_data) };

    let mut runner_ptr = std::ptr::null_mut();
    let runner_ptr_ptr = &mut runner_ptr;
    let ret_code = match private_data {
        Some(ref private_data_bytes) => {
            let private_data_len = private_data_bytes.len();
            let appdata_buf = appdata_dir.as_path().to_str().unwrap();
            unsafe {
                let c_string_private_data =
                    CString::from_vec_unchecked(private_data_bytes.to_owned());
                startConsensus(
                    max_block_size,
                    block_construction_timeout,
                    insertions_before_purging,
                    transaction_keep_alive,
                    transactions_purging_delay,
                    c_string_genesis.as_ptr() as *const u8,
                    genesis_data_len as i64,
                    c_string_private_data.as_ptr() as *const u8,
                    private_data_len as i64,
                    broadcast_callback,
                    catchup_status_callback,
                    Arc::into_raw(regenesis_arc),
                    free_regenesis_arc,
                    regenesis_callback,
                    maximum_log_level as u8,
                    on_log_emited,
                    appdata_buf.as_ptr() as *const u8,
                    appdata_buf.len() as i64,
                    database_connection_url.as_ptr() as *const u8,
                    database_connection_url.len() as i64,
                    runner_ptr_ptr,
                )
            }
        }
        None => {
            let appdata_buf = appdata_dir.as_path().to_str().unwrap();
            unsafe {
                {
                    startConsensusPassive(
                        max_block_size,
                        block_construction_timeout,
                        insertions_before_purging,
                        transaction_keep_alive,
                        transactions_purging_delay,
                        c_string_genesis.as_ptr() as *const u8,
                        genesis_data_len as i64,
                        catchup_status_callback,
                        Arc::into_raw(regenesis_arc),
                        free_regenesis_arc,
                        regenesis_callback,
                        maximum_log_level as u8,
                        on_log_emited,
                        appdata_buf.as_ptr() as *const u8,
                        appdata_buf.len() as i64,
                        database_connection_url.as_ptr() as *const u8,
                        database_connection_url.len() as i64,
                        runner_ptr_ptr,
                    )
                }
            }
        }
    };
    match ret_code {
        0 => Ok(runner_ptr),
        // NB: the following errors should be in line with
        // the enumeration defined by `toStartResult` in External.hs
        1 => bail!("Cannot decode given genesis data."),
        2 => bail!("Cannot decode baker keys."),
        3 => bail!("Error reading database files."),
        4 => bail!("Given block state path is a directory, not a file."),
        5 => bail!("Cannot read block state database due to incorrect permissions."),
        6 => bail!("Cannot read tree state database due to incorrect permissions."),
        7 => bail!("Cannot open supplied tree state database."),
        8 => bail!("Genesis block is not in the supplied tree state database."),
        9 => bail!("Database genesis block does not align with the supplied genesis data."),
        10 => bail!("Database invariant violation. See logs for details."),
        n => bail!("Unknown error code: {}.", n),
    }
}

impl ConsensusContainer {
    pub fn send_block(&self, block: &[u8]) -> ConsensusFfiResponse {
        wrap_send_data_to_c!(self, block, receiveBlock)
    }

    pub fn send_finalization(&self, msg: &[u8]) -> ConsensusFfiResponse {
        wrap_send_data_to_c!(self, msg, receiveFinalization)
    }

    pub fn send_finalization_record(&self, rec: &[u8]) -> ConsensusFfiResponse {
        wrap_send_data_to_c!(self, rec, receiveFinalizationRecord)
    }

    pub fn send_transaction(&self, data: &[u8]) -> ConsensusFfiResponse {
        let consensus = self.consensus.load(Ordering::SeqCst);
        let len = data.len();

        let result = unsafe {
            receiveTransaction(
                consensus,
                CString::from_vec_unchecked(data.to_vec()).as_ptr() as *const u8,
                len as i64,
            )
        };

        let return_code = ConsensusFfiResponse::try_from(result);

        return_code.unwrap_or_else(|code| panic!("Unknown FFI return code: {}", code))
    }

    pub fn get_consensus_status(&self) -> String {
        wrap_c_call_string!(self, consensus, |consensus| getConsensusStatus(consensus))
    }

    pub fn get_block_info(&self, block_hash: &str) -> String {
        let c_str = CString::new(block_hash).unwrap();
        wrap_c_call_string!(self, consensus, |consensus| getBlockInfo(
            consensus,
            c_str.as_ptr() as *const u8
        ))
    }

    pub fn get_blocks_at_height(&self, block_height: u64) -> String {
        wrap_c_call_string!(self, consensus, |consensus| getBlocksAtHeight(consensus, block_height))
    }

    pub fn get_ancestors(&self, block_hash: &str, amount: u64) -> String {
        let c_str = CString::new(block_hash).unwrap();
        wrap_c_call_string!(self, consensus, |consensus| getAncestors(
            consensus,
            c_str.as_ptr() as *const u8,
            amount
        ))
    }

    pub fn get_branches(&self) -> String {
        wrap_c_call_string!(self, consensus, |consensus| getBranches(consensus))
    }

    pub fn get_account_list(&self, block_hash: &str) -> String {
        let block_hash = CString::new(block_hash).unwrap();
        wrap_c_call_string!(self, consensus, |consensus| getAccountList(
            consensus,
            block_hash.as_ptr() as *const u8
        ))
    }

    pub fn get_instances(&self, block_hash: &str) -> String {
        let block_hash = CString::new(block_hash).unwrap();
        wrap_c_call_string!(self, consensus, |consensus| getInstances(
            consensus,
            block_hash.as_ptr() as *const u8
        ))
    }

    pub fn get_account_info(&self, block_hash: &str, account_address: &str) -> String {
        let block_hash = CString::new(block_hash).unwrap();
        let account_address = CString::new(account_address).unwrap();
        wrap_c_call_string!(self, consensus, |consensus| getAccountInfo(
            consensus,
            block_hash.as_ptr() as *const u8,
            account_address.as_ptr() as *const u8
        ))
    }

    pub fn get_instance_info(&self, block_hash: &str, contract_address: &str) -> String {
        let block_hash = CString::new(block_hash).unwrap();
        let contract_address = CString::new(contract_address).unwrap();
        wrap_c_call_string!(self, consensus, |consensus| getInstanceInfo(
            consensus,
            block_hash.as_ptr() as *const u8,
            contract_address.as_ptr() as *const u8
        ))
    }

    pub fn get_reward_status(&self, block_hash: &str) -> String {
        let block_hash = CString::new(block_hash).unwrap();
        wrap_c_call_string!(self, consensus, |consensus| getRewardStatus(
            consensus,
            block_hash.as_ptr() as *const u8
        ))
    }

    pub fn get_birk_parameters(&self, block_hash: &str) -> String {
        let block_hash = CString::new(block_hash).unwrap();
        wrap_c_call_string!(self, consensus, |consensus| getBirkParameters(
            consensus,
            block_hash.as_ptr() as *const u8
        ))
    }

    pub fn get_module_list(&self, block_hash: &str) -> String {
        let block_hash = CString::new(block_hash).unwrap();
        wrap_c_call_string!(self, consensus, |consensus| getModuleList(
            consensus,
            block_hash.as_ptr() as *const u8
        ))
    }

    pub fn get_module_source(&self, block_hash: &str, module_ref: &str) -> Vec<u8> {
        let block_hash = CString::new(block_hash).unwrap();
        let module_ref = CString::new(module_ref).unwrap();
        wrap_c_call_bytes!(self, |consensus| getModuleSource(
            consensus,
            block_hash.as_ptr() as *const u8,
            module_ref.as_ptr() as *const u8
        ))
    }

    pub fn get_catch_up_status(&self) -> Arc<[u8]> {
        wrap_c_call_payload!(
            self,
            |consensus| getCatchUpStatus(consensus),
            &(PacketType::CatchUpStatus as u8).to_be_bytes()
        )
    }

    pub fn receive_catch_up_status(
        &self,
        request: &[u8],
        peer_id: PeerId,
        object_limit: i64,
    ) -> ConsensusFfiResponse {
        wrap_c_call!(self, |consensus| receiveCatchUpStatus(
            consensus,
            peer_id,
            request.as_ptr(),
            request.len() as i64,
            object_limit,
            direct_callback
        ))
    }

    pub fn in_baking_committee(&self) -> ConsensusIsInBakingCommitteeResponse {
        wrap_c_committee_call!(self, |consensus| bakerIdBestBlock(consensus))
    }

    pub fn in_finalization_committee(&self) -> bool {
        wrap_c_bool_call!(self, |consensus| checkIfWeAreFinalizer(consensus))
    }

    pub fn get_account_non_finalized_transactions(&self, account_address: &str) -> String {
        let account_address = CString::new(account_address).unwrap();
        wrap_c_call_string!(self, consensus, |consensus| {
            getAccountNonFinalizedTransactions(consensus, account_address.as_ptr() as *const u8)
        })
    }

    pub fn get_block_summary(&self, block_hash: &str) -> String {
        let block_hash = CString::new(block_hash).unwrap();
        wrap_c_call_string!(self, consensus, |consensus| getBlockSummary(
            consensus,
            block_hash.as_ptr() as *const u8
        ))
    }

    pub fn get_transaction_status(&self, transaction_hash: &str) -> String {
        let transaction_hash = CString::new(transaction_hash).unwrap();
        wrap_c_call_string!(self, consensus, |consensus| getTransactionStatus(
            consensus,
            transaction_hash.as_ptr() as *const u8
        ))
    }

    pub fn get_transaction_status_in_block(
        &self,
        transaction_hash: &str,
        block_hash: &str,
    ) -> String {
        let transaction_hash = CString::new(transaction_hash).unwrap();
        let block_hash = CString::new(block_hash).unwrap();
        wrap_c_call_string!(self, consensus, |consensus| getTransactionStatusInBlock(
            consensus,
            transaction_hash.as_ptr() as *const u8,
            block_hash.as_ptr() as *const u8
        ))
    }

    pub fn get_next_account_nonce(&self, account_address: &str) -> String {
        let account_address = CString::new(account_address).unwrap();
        wrap_c_call_string!(self, consensus, |consensus| getNextAccountNonce(
            consensus,
            account_address.as_ptr() as *const u8
        ))
    }

    pub fn get_identity_providers(&self, block_hash: &str) -> String {
        let block_hash = CString::new(block_hash).unwrap();
        wrap_c_call_string!(self, consensus, |consensus| getAllIdentityProviders(
            consensus,
            block_hash.as_ptr() as *const u8
        ))
    }

    pub fn get_anonymity_revokers(&self, block_hash: &str) -> String {
        let block_hash = CString::new(block_hash).unwrap();
        wrap_c_call_string!(self, consensus, |consensus| getAllAnonymityRevokers(
            consensus,
            block_hash.as_ptr() as *const u8
        ))
    }

    pub fn get_cryptographic_parameters(&self, block_hash: &str) -> String {
        let block_hash = CString::new(block_hash).unwrap();
        wrap_c_call_string!(self, consensus, |consensus| getCryptographicParameters(
            consensus,
            block_hash.as_ptr() as *const u8
        ))
    }

    pub fn import_blocks(&self, import_file_path: &[u8]) -> u8 {
        let consensus = self.consensus.load(Ordering::SeqCst);
        let len = import_file_path.len();

        unsafe {
            importBlocks(
                consensus,
                CString::from_vec_unchecked(import_file_path.to_vec()).as_ptr() as *const u8,
                len as i64,
            )
        }
    }
}

pub enum CallbackType {
    Block = 0,
    FinalizationMessage,
    FinalizationRecord,
    CatchUpStatus,
}

impl TryFrom<u8> for CallbackType {
    type Error = failure::Error;

    fn try_from(byte: u8) -> Fallible<Self> {
        match byte {
            0 => Ok(CallbackType::Block),
            1 => Ok(CallbackType::FinalizationMessage),
            2 => Ok(CallbackType::FinalizationRecord),
            3 => Ok(CallbackType::CatchUpStatus),
            _ => Err(format_err!("Received invalid callback type: {}", byte)),
        }
    }
}

pub extern "C" fn on_finalization_message_catchup_out(peer_id: PeerId, data: *const u8, len: i64) {
    unsafe {
        let msg_variant = PacketType::FinalizationMessage;
        let payload = slice::from_raw_parts(data as *const u8, len as usize);
        let mut full_payload = Vec::with_capacity(1 + payload.len());
        (msg_variant as u8).serial(&mut full_payload);

        full_payload.write_all(&payload).unwrap(); // infallible
        let full_payload = Arc::from(full_payload);

        let msg = ConsensusMessage::new(
            MessageType::Outbound(Some(peer_id)),
            PacketType::FinalizationMessage,
            full_payload,
            vec![],
            None,
        );

        match CALLBACK_QUEUE.send_out_blocking_msg(msg) {
            Ok(_) => trace!("Queueing a {} of {} bytes", msg_variant, len),
            Err(e) => error!("Couldn't queue a {} properly: {}", msg_variant, e),
        };
    }
}

macro_rules! sending_callback {
    ($target:expr, $msg_type:expr, $msg:expr, $msg_length:expr, $omit_status:expr) => {
        unsafe {
            let callback_type = match CallbackType::try_from($msg_type as u8) {
                Ok(ct) => ct,
                Err(e) => {
                    error!("{}", e);
                    return;
                }
            };

            let msg_variant = match callback_type {
                CallbackType::Block => PacketType::Block,
                CallbackType::FinalizationMessage => PacketType::FinalizationMessage,
                CallbackType::FinalizationRecord => PacketType::FinalizationRecord,
                CallbackType::CatchUpStatus => PacketType::CatchUpStatus,
            };

            let payload = slice::from_raw_parts($msg as *const u8, $msg_length as usize);
            let mut full_payload = Vec::with_capacity(1 + payload.len());
            (msg_variant as u8).serial(&mut full_payload);
            full_payload.write_all(&payload).unwrap(); // infallible
            let full_payload = Arc::from(full_payload);

            let msg = ConsensusMessage::new(
                MessageType::Outbound($target),
                msg_variant,
                full_payload,
                vec![],
                $omit_status,
            );

            match CALLBACK_QUEUE.send_out_blocking_msg(msg) {
                Ok(_) => trace!("Queueing a {} of {} bytes", msg_variant, $msg_length),
                Err(e) => error!("Couldn't queue a {} properly: {}", msg_variant, e),
            };
        }
    };
}

pub extern "C" fn broadcast_callback(msg_type: i64, msg: *const u8, msg_length: i64) {
    trace!("Broadcast callback hit - queueing message");
    sending_callback!(None, msg_type, msg, msg_length, None);
}

pub extern "C" fn direct_callback(
    peer_id: PeerId,
    msg_type: i64,
    msg: *const c_char,
    msg_length: i64,
) {
    trace!("Direct callback hit - queueing message");
    sending_callback!(Some(peer_id), msg_type, msg, msg_length, None);
}

pub extern "C" fn catchup_status_callback(msg: *const u8, msg_length: i64) {
    trace!("Catch-up status callback hit - queueing message");
    sending_callback!(
        None,
        CallbackType::CatchUpStatus,
        msg,
        msg_length,
        Some(PeerStatus::Pending)
    );
}

/// A callback function that will append a regenesis block to the list of known
/// regenesis hashes.
///
/// # Safety
///
/// The first argument has to be an Arc<RwLock<Vec<BlockHash>>> which was
/// converted into raw, as it will be casted into that type. The second argument
/// has to be a pointer to a bytestring of length 32.
pub unsafe extern "C" fn regenesis_callback(
    arc: *const RwLock<Vec<BlockHash>>,
    block_hash: *const u8,
) {
    trace!("Regenesis callback hit");
    write_or_die!(Arc::from_raw(arc)).push(
        BlockHash::new(std::slice::from_raw_parts(block_hash, 32))
            .expect("The slice is exactly 32 bytes so ::new must succeed."),
    );
}

/// A callback to free the regenesis Arc.
///
/// # Safety
///
/// The given pointer must be an Arc<RwLock<Vec<BlockHash>>> which was converted
/// into raw.
pub unsafe extern "C" fn free_regenesis_arc(ptr: *const RwLock<Vec<BlockHash>>) {
    if ptr.is_null() {
        return;
    }

    Arc::from_raw(ptr);
}

/// Following the implementation of the log crate, error = 1, warning = 2, info
/// = 3, 4 = debug, any other option is considered as trace.
pub extern "C" fn on_log_emited(identifier: c_char, log_level: c_char, log_message: *const u8) {
    // These types are defined in `Concordium.Logger` inside the `globalstate_types`
    // package.
    fn identifier_to_string(id: c_char) -> &'static str {
        match id {
            0 => "Runner",
            1 => "Afgjort",
            2 => "Birk",
            3 => "Crypto",
            4 => "Kontrol",
            5 => "Skov",
            6 => "Baker",
            7 => "External",
            8 => "GlobalState",
            9 => "BlockState",
            10 => "TreeState",
            11 => "LMDB",
            12 => "Scheduler",
            _ => "Unknown",
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
