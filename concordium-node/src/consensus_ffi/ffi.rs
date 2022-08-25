use crate::{
    common::p2p_peer::RemotePeerId,
    consensus_ffi::{
        blockchain_types::BlockHash,
        catch_up::*,
        consensus::*,
        helpers::{
            ConsensusFfiResponse, ConsensusIsInBakingCommitteeResponse, ConsensusQueryResponse,
            PacketType,
        },
        messaging::*,
    },
    write_or_die,
};
use anyhow::{anyhow, bail, Context};
use byteorder::{NetworkEndian, ReadBytesExt};
use crypto_common::Serial;
use std::{
    convert::{TryFrom, TryInto},
    ffi::{CStr, CString},
    io::{Cursor, Write},
    os::raw::{c_char, c_int},
    path::Path,
    ptr, slice,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Once,
    },
};

/// A type used in this module to document that a given value is intended
/// to be used as a peer id.
type PeerIdFFI = u64;

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
    args.push("+RTS".to_owned());
    if !rts_flags.is_empty() {
        if rts_flags.iter().all(|arg| !arg.trim().starts_with("--install-signal-handlers")) {
            args.push("--install-signal-handlers=no".to_owned());
        }
        for flag in rts_flags {
            if !flag.trim().is_empty() {
                args.push(flag.to_owned());
            }
        }
    } else {
        args.push("--install-signal-handlers=no".to_owned());
    }
    args.push("-RTS".to_owned());
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
type BroadcastCallback = extern "C" fn(i64, u32, *const u8, i64);
type CatchUpStatusCallback = extern "C" fn(u32, *const u8, i64);
type DirectMessageCallback = extern "C" fn(
    peer_id: PeerIdFFI,
    message_type: i64,
    genesis_index: u32,
    msg: *const c_char,
    msg_len: i64,
);
type RegenesisCallback = unsafe extern "C" fn(*const Regenesis, *const u8);
type RegenesisFreeCallback = unsafe extern "C" fn(*const Regenesis);

/// A type of callback that is used to copy the provided data at the end of the
/// given vector.
type CopyToVecCallback = extern "C" fn(*mut Vec<u8>, *const u8, i64);

/// Context necessary for Haskell code/Consensus to send notifications on
/// important events to Rust code (i.e., RPC server, or network layer).
pub struct NotifyContext {
    /// Notification channel for newly added blocks. This is an unbounded
    /// channel since it makes the implementation simpler. This should not be a
    /// problem since the consumer of this channel is a dedicated task and
    /// blocks are not added to the tree that quickly. So there should not be
    /// much contention for this.
    blocks:           futures::channel::mpsc::UnboundedSender<Arc<[u8]>>,
    /// Notification channel for newly finalized blocks. See
    /// [NotificationContext::blocks] documentation for why having an unbounded
    /// channel is OK here, and is unlikely to lead to resource exhaustion.
    finalized_blocks: futures::channel::mpsc::UnboundedSender<Arc<[u8]>>,
}

/// A type of callback used to notify Rust code of important events. The
/// callback is called with
/// - the context
/// - the event type (0 for block arrived, 1 for block finalized)
/// - pointer to a byte array containing the serialized event
/// - length of the data
///
/// The callback should not retain references to supplied data after the exit.
type NotifyCallback = unsafe extern "C" fn(*mut NotifyContext, u8, *const u8, u64);

pub struct NotificationHandlers {
    pub blocks:           futures::channel::mpsc::UnboundedReceiver<Arc<[u8]>>,
    pub finalized_blocks: futures::channel::mpsc::UnboundedReceiver<Arc<[u8]>>,
}

#[allow(improper_ctypes)]
extern "C" {
    pub fn startConsensus(
        max_block_size: u64,
        block_construction_timeout: u64,
        insertions_before_purging: u64,
        transaction_keep_alive: u64,
        transactions_purging_delay: u64,
        accounts_cache_size: u32,
        genesis_data: *const u8,
        genesis_data_len: i64,
        private_data: *const u8,
        private_data_len: i64,
        notify_context: *mut NotifyContext,
        notify_callback: NotifyCallback,
        broadcast_callback: BroadcastCallback,
        catchup_status_callback: CatchUpStatusCallback,
        regenesis_arc: *const Regenesis,
        free_regenesis_arc: RegenesisFreeCallback,
        regenesis_callback: RegenesisCallback,
        maximum_log_level: u8,
        log_callback: LogCallback,
        appdata_dir: *const u8,
        appdata_dir_len: i64,
        runner_ptr_ptr: *mut *mut consensus_runner,
    ) -> i64;
    pub fn startConsensusPassive(
        max_block_size: u64,
        block_construction_timeout: u64,
        insertions_before_purging: u64,
        transaction_keep_alive: u64,
        transactions_purging_delay: u64,
        accounts_cache_size: u32,
        genesis_data: *const u8,
        genesis_data_len: i64,
        notify_context: *mut NotifyContext,
        notify_callback: NotifyCallback,
        catchup_status_callback: CatchUpStatusCallback,
        regenesis_arc: *const Regenesis,
        free_regenesis_arc: RegenesisFreeCallback,
        regenesis_callback: RegenesisCallback,
        maximum_log_level: u8,
        log_callback: LogCallback,
        appdata_dir: *const u8,
        appdata_dir_len: i64,
        runner_ptr_ptr: *mut *mut consensus_runner,
    ) -> i64;
    #[allow(improper_ctypes)]
    pub fn startBaker(consensus: *mut consensus_runner);
    pub fn receiveBlock(
        consensus: *mut consensus_runner,
        genesis_index: u32,
        block_data: *const u8,
        data_length: i64,
    ) -> i64;
    pub fn receiveFinalizationMessage(
        consensus: *mut consensus_runner,
        genesis_index: u32,
        finalization_data: *const u8,
        data_length: i64,
    ) -> i64;
    pub fn receiveFinalizationRecord(
        consensus: *mut consensus_runner,
        genesis_index: u32,
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
    pub fn getBlockInfo(
        consensus: *mut consensus_runner,
        block_hash: *const c_char,
    ) -> *const c_char;
    pub fn getAncestors(
        consensus: *mut consensus_runner,
        block_hash: *const c_char,
        amount: u64,
    ) -> *const c_char;
    pub fn getBranches(consensus: *mut consensus_runner) -> *const c_char;

    // State queries
    pub fn getAccountList(
        consensus: *mut consensus_runner,
        block_hash: *const c_char,
    ) -> *const c_char;
    pub fn getInstances(
        consensus: *mut consensus_runner,
        block_hash: *const c_char,
    ) -> *const c_char;
    pub fn getAccountInfo(
        consensus: *mut consensus_runner,
        block_hash: *const c_char,
        account_address: *const c_char,
    ) -> *const c_char;
    pub fn getInstanceInfo(
        consensus: *mut consensus_runner,
        block_hash: *const c_char,
        contract_address: *const c_char,
    ) -> *const c_char;
    pub fn invokeContract(
        consensus: *mut consensus_runner,
        block_hash: *const c_char,
        context: *const c_char,
    ) -> *const c_char;
    pub fn getRewardStatus(
        consensus: *mut consensus_runner,
        block_hash: *const c_char,
    ) -> *const c_char;
    pub fn getBirkParameters(
        consensus: *mut consensus_runner,
        block_hash: *const c_char,
    ) -> *const c_char;
    pub fn getModuleList(
        consensus: *mut consensus_runner,
        block_hash: *const c_char,
    ) -> *const c_char;
    pub fn getModuleSource(
        consensus: *mut consensus_runner,
        block_hash: *const c_char,
        module_ref: *const c_char,
    ) -> *const u8;
    pub fn getBakerList(consensus: *mut consensus_runner, block_hash: *const u8) -> *const c_char;
    pub fn getPoolStatus(
        consensus: *mut consensus_runner,
        block_hash: *const u8,
        passive_delegation: bool,
        baker_id: u64,
    ) -> *const c_char;
    pub fn freeCStr(hstring: *const c_char);
    pub fn getCatchUpStatus(
        consensus: *mut consensus_runner,
        genesis_index: *mut u32,
        msg: *mut *const u8,
    ) -> i64;
    pub fn receiveCatchUpStatus(
        consensus: *mut consensus_runner,
        peer_id: PeerIdFFI,
        genesis_index: u32,
        msg: *const u8,
        msg_len: i64,
        object_limit: i64,
        direct_callback: DirectMessageCallback,
    ) -> i64;
    pub fn bakerStatusBestBlock(
        consensus: *mut consensus_runner,
        baker_id: *mut u64,
        has_baker_id: *mut u8,
    ) -> u8;
    pub fn checkIfWeAreFinalizer(consensus: *mut consensus_runner) -> u8;
    pub fn checkIfRunning(consensus: *mut consensus_runner) -> u8;
    pub fn getAccountNonFinalizedTransactions(
        consensus: *mut consensus_runner,
        account_address: *const c_char,
    ) -> *const c_char;
    pub fn getBlockSummary(
        consensus: *mut consensus_runner,
        block_hash: *const c_char,
    ) -> *const c_char;
    pub fn getBlocksAtHeight(
        consensus: *mut consensus_runner,
        block_height: u64,
        genesis_index: u32,
        restrict: u8,
    ) -> *const c_char;
    pub fn getTransactionStatus(
        consensus: *mut consensus_runner,
        transaction_hash: *const c_char,
    ) -> *const c_char;
    pub fn getTransactionStatusInBlock(
        consensus: *mut consensus_runner,
        transaction_hash: *const c_char,
        block_hash: *const c_char,
    ) -> *const c_char;
    pub fn getNextAccountNonce(
        consensus: *mut consensus_runner,
        account_address: *const c_char,
    ) -> *const c_char;
    pub fn getAllIdentityProviders(
        consensus: *mut consensus_runner,
        block_hash: *const c_char,
    ) -> *const c_char;
    pub fn getAllAnonymityRevokers(
        consensus: *mut consensus_runner,
        block_hash: *const c_char,
    ) -> *const c_char;
    pub fn getCryptographicParameters(
        consensus: *mut consensus_runner,
        block_hash: *const c_char,
    ) -> *const c_char;
    pub fn importBlocks(
        consensus: *mut consensus_runner,
        import_file_path: *const u8,
        import_file_path_len: i64,
    ) -> i64;
    pub fn stopImportingBlocks(consensus: *mut consensus_runner);

    pub fn freeByteArray(hstring: *const u8);

    // Functions related to V2 GRPC interface.

    /// Get an information about a specific account.
    pub fn getAccountInfoV2(
        consensus: *mut consensus_runner,
        block_id_type: u8,
        block_hash: *const u8,
        acc_type: u8,
        acc_id: *const u8,
        out_hash: *mut u8,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Stream a list of all accounts.
    pub fn getAccountListV2(
        consensus: *mut consensus_runner,
        sender: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_hash: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Stream a list of all modules.
    pub fn getModuleListV2(
        consensus: *mut consensus_runner,
        sender: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_hash: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Stream the source of a smart contract module.
    pub fn getModuleSourceV2(
        consensus: *mut consensus_runner,
        block_id_type: u8,
        block_hash: *const u8,
        module_ref: *const u8,
        out_hash: *mut u8,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Stream a list of smart contract instances.
    pub fn getInstanceListV2(
        consensus: *mut consensus_runner,
        sender: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_hash: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get an information about a specific smart contract instance.
    pub fn getInstanceInfoV2(
        consensus: *mut consensus_runner,
        block_id_type: u8,
        block_hash: *const u8,
        addr_index: u64,
        addr_subindex: u64,
        out_hash: *mut u8,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Stream a list of ancestors for the given block.
    pub fn getAncestorsV2(
        consensus: *mut consensus_runner,
        sender: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_hash: *const u8,
        depth: u64,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;
}

/// This is the callback invoked by consensus on newly arrived, and newly
/// finalized blocks. The callback enqueues the block in the corresponding
/// channel for further processing. The other end of the channel is owned by a
/// background task that forwards data from the channels into any currently
/// active RPC clients.
unsafe extern "C" fn notify_callback(
    notify_context: *mut NotifyContext,
    ty: u8,
    data_ptr: *const u8,
    data_len: u64,
) {
    let sender = &*notify_context;
    match ty {
        0u8 => {
            if sender
                .blocks
                .unbounded_send(std::slice::from_raw_parts(data_ptr, data_len as usize).into())
                .is_err()
            {
                error!("Failed to enqueue block that arrived.");
                // do nothing. The error here should only happen if the
                // receiver is disconnected, which means that the task
                // forwarding events has been killed. That should never happen,
                // and if it does indicates a disastrous situation.
            }
        }
        1u8 => {
            if sender
                .finalized_blocks
                .unbounded_send(std::slice::from_raw_parts(data_ptr, data_len as usize).into())
                .is_err()
            {
                error!("Failed to enqueue finalized block.");
                // do nothing. The error here should only happen if the
                // receiver is disconnected, which means that the task
                // forwarding events has been killed. That should never happen,
                // and if it does indicates a disastrous situation.
            }
        }
        unexpected => {
            error!("Unexpected notification type {}. This is a bug.", unexpected);
            // do nothing
        }
    }
}

pub fn get_consensus_ptr(
    runtime_parameters: &ConsensusRuntimeParameters,
    genesis_data: Vec<u8>,
    private_data: Option<Vec<u8>>,
    maximum_log_level: ConsensusLogLevel,
    appdata_dir: &Path,
    regenesis_arc: Arc<Regenesis>,
) -> anyhow::Result<(*mut consensus_runner, NotificationHandlers)> {
    let genesis_data_len = genesis_data.len();
    let (sender, receiver) = futures::channel::mpsc::unbounded();
    let (sender_finalized, receiver_finalized) = futures::channel::mpsc::unbounded();
    let notify_context = NotifyContext {
        blocks:           sender,
        finalized_blocks: sender_finalized,
    };
    let notification_handlers = NotificationHandlers {
        blocks:           receiver,
        finalized_blocks: receiver_finalized,
    };
    let mut runner_ptr = std::ptr::null_mut();
    let runner_ptr_ptr = &mut runner_ptr;
    let ret_code = match private_data {
        Some(ref private_data_bytes) => {
            let private_data_len = private_data_bytes.len();
            let appdata_buf = appdata_dir.to_str().unwrap();
            unsafe {
                startConsensus(
                    runtime_parameters.max_block_size,
                    runtime_parameters.block_construction_timeout,
                    runtime_parameters.insertions_before_purging,
                    runtime_parameters.transaction_keep_alive,
                    runtime_parameters.transactions_purging_delay,
                    runtime_parameters.accounts_cache_size,
                    genesis_data.as_ptr(),
                    genesis_data_len as i64,
                    private_data_bytes.as_ptr(),
                    private_data_len as i64,
                    Box::into_raw(Box::new(notify_context)),
                    notify_callback,
                    broadcast_callback,
                    catchup_status_callback,
                    Arc::into_raw(regenesis_arc),
                    free_regenesis_arc,
                    regenesis_callback,
                    maximum_log_level as u8,
                    on_log_emited,
                    appdata_buf.as_ptr() as *const u8,
                    appdata_buf.len() as i64,
                    runner_ptr_ptr,
                )
            }
        }
        None => {
            let appdata_buf = appdata_dir.to_str().unwrap();
            unsafe {
                {
                    startConsensusPassive(
                        runtime_parameters.max_block_size,
                        runtime_parameters.block_construction_timeout,
                        runtime_parameters.insertions_before_purging,
                        runtime_parameters.transaction_keep_alive,
                        runtime_parameters.transactions_purging_delay,
                        runtime_parameters.accounts_cache_size,
                        genesis_data.as_ptr(),
                        genesis_data_len as i64,
                        Box::into_raw(Box::new(notify_context)),
                        notify_callback,
                        catchup_status_callback,
                        Arc::into_raw(regenesis_arc),
                        free_regenesis_arc,
                        regenesis_callback,
                        maximum_log_level as u8,
                        on_log_emited,
                        appdata_buf.as_ptr() as *const u8,
                        appdata_buf.len() as i64,
                        runner_ptr_ptr,
                    )
                }
            }
        }
    };
    match ret_code {
        0 => Ok((runner_ptr, notification_handlers)),
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
        11 => bail!("Block state database has incorrect version information."),
        n => bail!("Unknown error code: {}.", n),
    }
}

impl ConsensusContainer {
    pub fn send_block(&self, genesis_index: u32, block: &[u8]) -> ConsensusFfiResponse {
        wrap_send_data_to_c!(self, genesis_index, block, receiveBlock)
    }

    pub fn send_finalization(&self, genesis_index: u32, msg: &[u8]) -> ConsensusFfiResponse {
        wrap_send_data_to_c!(self, genesis_index, msg, receiveFinalizationMessage)
    }

    pub fn send_finalization_record(&self, genesis_index: u32, rec: &[u8]) -> ConsensusFfiResponse {
        wrap_send_data_to_c!(self, genesis_index, rec, receiveFinalizationRecord)
    }

    pub fn send_transaction(&self, data: &[u8]) -> ConsensusFfiResponse {
        let consensus = self.consensus.load(Ordering::SeqCst);
        let len = data.len();

        let result = unsafe { receiveTransaction(consensus, data.as_ptr(), len as i64) };

        let return_code = ConsensusFfiResponse::try_from(result);

        return_code.unwrap_or_else(|code| panic!("Unknown FFI return code: {}", code))
    }

    pub fn get_consensus_status(&self) -> String {
        wrap_c_call_string!(self, consensus, |consensus| getConsensusStatus(consensus))
    }

    pub fn get_block_info(&self, block_hash: &str) -> anyhow::Result<String> {
        let c_str = CString::new(block_hash)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| getBlockInfo(
            consensus,
            c_str.as_ptr()
        )))
    }

    pub fn get_blocks_at_height(
        &self,
        block_height: u64,
        genesis_index: u32,
        restrict: bool,
    ) -> String {
        wrap_c_call_string!(self, consensus, |consensus| getBlocksAtHeight(
            consensus,
            block_height,
            genesis_index,
            restrict as u8
        ))
    }

    pub fn get_ancestors(&self, block_hash: &str, amount: u64) -> anyhow::Result<String> {
        let c_str = CString::new(block_hash)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| getAncestors(
            consensus,
            c_str.as_ptr(),
            amount
        )))
    }

    pub fn get_branches(&self) -> String {
        wrap_c_call_string!(self, consensus, |consensus| getBranches(consensus))
    }

    pub fn get_account_list(&self, block_hash: &str) -> anyhow::Result<String> {
        let block_hash = CString::new(block_hash)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| getAccountList(
            consensus,
            block_hash.as_ptr()
        )))
    }

    pub fn get_instances(&self, block_hash: &str) -> anyhow::Result<String> {
        let block_hash = CString::new(block_hash)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| getInstances(
            consensus,
            block_hash.as_ptr()
        )))
    }

    pub fn get_account_info(
        &self,
        block_hash: &str,
        account_address: &str,
    ) -> anyhow::Result<String> {
        let block_hash = CString::new(block_hash)?;
        let account_address = CString::new(account_address)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| getAccountInfo(
            consensus,
            block_hash.as_ptr(),
            account_address.as_ptr()
        )))
    }

    pub fn get_instance_info(
        &self,
        block_hash: &str,
        contract_address: &str,
    ) -> anyhow::Result<String> {
        let block_hash = CString::new(block_hash)?;
        let contract_address = CString::new(contract_address)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| getInstanceInfo(
            consensus,
            block_hash.as_ptr(),
            contract_address.as_ptr()
        )))
    }

    pub fn invoke_contract(&self, block_hash: &str, context: &str) -> anyhow::Result<String> {
        let block_hash = CString::new(block_hash)?;
        let context = CString::new(context)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| invokeContract(
            consensus,
            block_hash.as_ptr(),
            context.as_ptr()
        )))
    }

    pub fn get_reward_status(&self, block_hash: &str) -> anyhow::Result<String> {
        let block_hash = CString::new(block_hash)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| getRewardStatus(
            consensus,
            block_hash.as_ptr()
        )))
    }

    pub fn get_birk_parameters(&self, block_hash: &str) -> anyhow::Result<String> {
        let block_hash = CString::new(block_hash)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| getBirkParameters(
            consensus,
            block_hash.as_ptr()
        )))
    }

    pub fn get_module_list(&self, block_hash: &str) -> anyhow::Result<String> {
        let block_hash = CString::new(block_hash)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| getModuleList(
            consensus,
            block_hash.as_ptr()
        )))
    }

    pub fn get_module_source(&self, block_hash: &str, module_ref: &str) -> anyhow::Result<Vec<u8>> {
        let block_hash = CString::new(block_hash)?;
        let module_ref = CString::new(module_ref)?;
        Ok(wrap_c_call_bytes!(self, |consensus| getModuleSource(
            consensus,
            block_hash.as_ptr(),
            module_ref.as_ptr()
        )))
    }

    pub fn get_baker_list(&self, block_hash: &str) -> anyhow::Result<String> {
        let block_hash = CString::new(block_hash)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| getBakerList(
            consensus,
            block_hash.as_ptr() as *const u8
        )))
    }

    pub fn get_pool_status(
        &self,
        block_hash: &str,
        passive_delegation: bool,
        baker_id: u64,
    ) -> anyhow::Result<String> {
        let block_hash = CString::new(block_hash)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| getPoolStatus(
            consensus,
            block_hash.as_ptr() as *const u8,
            passive_delegation,
            baker_id,
        )))
    }

    /// Construct a catch-up request message. The message includes the packet
    /// type byte and genesis index.
    pub fn get_catch_up_status(&self) -> Arc<[u8]> {
        let consensus = self.consensus.load(Ordering::SeqCst);

        unsafe {
            let mut genesis_index: u32 = 0;
            let mut message_bytes: *const u8 = ptr::null();
            let message_length =
                getCatchUpStatus(consensus, &mut genesis_index, &mut message_bytes);
            let slice = &slice::from_raw_parts(message_bytes, message_length as usize);
            let mut ret = Vec::with_capacity(5 + slice.len());
            ret.extend_from_slice(&(PacketType::CatchUpStatus as u8).to_be_bytes());
            ret.extend_from_slice(&genesis_index.to_be_bytes());
            ret.extend_from_slice(slice);
            freeCStr(message_bytes as *const i8);
            Arc::from(ret)
        }
    }

    pub fn receive_catch_up_status(
        &self,
        genesis_index: u32,
        request: &[u8],
        peer_id: RemotePeerId,
        object_limit: i64,
    ) -> ConsensusFfiResponse {
        wrap_c_call!(self, |consensus| receiveCatchUpStatus(
            consensus,
            peer_id.into(),
            genesis_index,
            request.as_ptr(),
            request.len() as i64,
            object_limit,
            direct_callback
        ))
    }

    /// Gets baker status of the node along with the baker ID registered in the
    /// baker credentials used, if available.
    ///
    /// Note: the return type does not use an Option<u64>, which would be more
    /// natural, because a weird issue on Windows caused node_info to
    /// produce the wrong result.
    pub fn in_baking_committee(&self) -> (ConsensusIsInBakingCommitteeResponse, bool, u64) {
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut baker_id: u64 = 0;
        let mut has_baker_id: u8 = 0;

        let result = unsafe { bakerStatusBestBlock(consensus, &mut baker_id, &mut has_baker_id) };

        let status = ConsensusIsInBakingCommitteeResponse::try_from(result).unwrap_or_else(|err| {
            unreachable!("An error occured when trying to convert FFI return code: {}", err)
        });

        (status, has_baker_id != 0, baker_id)
    }

    pub fn in_finalization_committee(&self) -> bool {
        wrap_c_bool_call!(self, |consensus| checkIfWeAreFinalizer(consensus))
    }

    /// Checks if consensus is running, i.e. if consensus has been shut down,
    /// this will return false.
    pub fn is_consensus_running(&self) -> bool {
        wrap_c_bool_call!(self, |consensus| checkIfRunning(consensus))
    }

    pub fn get_account_non_finalized_transactions(
        &self,
        account_address: &str,
    ) -> anyhow::Result<String> {
        let account_address = CString::new(account_address)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| {
            getAccountNonFinalizedTransactions(consensus, account_address.as_ptr())
        }))
    }

    pub fn get_block_summary(&self, block_hash: &str) -> anyhow::Result<String> {
        let block_hash = CString::new(block_hash)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| getBlockSummary(
            consensus,
            block_hash.as_ptr()
        )))
    }

    pub fn get_transaction_status(&self, transaction_hash: &str) -> anyhow::Result<String> {
        let transaction_hash = CString::new(transaction_hash)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| getTransactionStatus(
            consensus,
            transaction_hash.as_ptr()
        )))
    }

    pub fn get_transaction_status_in_block(
        &self,
        transaction_hash: &str,
        block_hash: &str,
    ) -> anyhow::Result<String> {
        let transaction_hash = CString::new(transaction_hash)?;
        let block_hash = CString::new(block_hash)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| getTransactionStatusInBlock(
            consensus,
            transaction_hash.as_ptr(),
            block_hash.as_ptr()
        )))
    }

    pub fn get_next_account_nonce(&self, account_address: &str) -> anyhow::Result<String> {
        let account_address = CString::new(account_address)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| getNextAccountNonce(
            consensus,
            account_address.as_ptr(),
        )))
    }

    pub fn get_identity_providers(&self, block_hash: &str) -> anyhow::Result<String> {
        let block_hash = CString::new(block_hash)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| getAllIdentityProviders(
            consensus,
            block_hash.as_ptr(),
        )))
    }

    pub fn get_anonymity_revokers(&self, block_hash: &str) -> anyhow::Result<String> {
        let block_hash = CString::new(block_hash)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| getAllAnonymityRevokers(
            consensus,
            block_hash.as_ptr(),
        )))
    }

    pub fn get_cryptographic_parameters(&self, block_hash: &str) -> anyhow::Result<String> {
        let block_hash = CString::new(block_hash)?;
        Ok(wrap_c_call_string!(self, consensus, |consensus| getCryptographicParameters(
            consensus,
            block_hash.as_ptr(),
        )))
    }

    /// Import blocks from the given file path. If the file exists and the node
    /// could import all blocks from the file `Ok(())` is returned. Otherwise an
    /// error is returned.
    pub fn import_blocks(&self, import_file_path: &Path) -> anyhow::Result<()> {
        let consensus = self.consensus.load(Ordering::SeqCst);

        let path_bytes =
            import_file_path.as_os_str().to_str().context("Cannot decode path.")?.as_bytes();

        let len = path_bytes.len();

        let response = unsafe { importBlocks(consensus, path_bytes.as_ptr(), len as i64) };
        match ConsensusFfiResponse::try_from(response)? {
            ConsensusFfiResponse::Success => Ok(()),
            other => bail!("Error during block import: {}", other),
        }
    }

    pub fn stop_importing_blocks(&self) {
        let consensus = self.consensus.load(Ordering::SeqCst);

        unsafe { stopImportingBlocks(consensus) }
    }

    /// Stream finalized blocks when they become available.
    pub fn get_account_info_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
        account_identifier: &crate::grpc2::types::account_info_request::AccountIdentifier,
    ) -> Result<([u8; 32], Vec<u8>), tonic::Status> {
        use crate::grpc2::Require;
        let (block_id_type, block_hash) =
            crate::grpc2::types::block_hash_input_to_ffi(block_hash).require_owned()?;
        let (acc_type, acc_id) =
            crate::grpc2::types::account_identifier_to_ffi(account_identifier).require_owned()?;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let mut out_hash = [0u8; 32];
        let response: ConsensusQueryResponse = unsafe {
            getAccountInfoV2(
                consensus,
                block_id_type,
                block_hash,
                acc_type,
                acc_id,
                out_hash.as_mut_ptr(),
                &mut out_data,
                copy_to_vec_callback,
            )
            .try_into()?
        };
        response.check_rpc_response()?;
        Ok((out_hash, out_data))
    }

    /// Stream finalized blocks when they become available.
    pub fn get_account_list_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let (block_id_type, block_hash) =
            crate::grpc2::types::block_hash_input_to_ffi(block_hash).require_owned()?;
        let response: ConsensusQueryResponse = unsafe {
            getAccountListV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash,
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.check_rpc_response()?;
        Ok(buf)
    }

    pub fn get_module_list_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let (block_id_type, block_hash) =
            crate::grpc2::types::block_hash_input_to_ffi(block_hash).require_owned()?;
        let response: ConsensusQueryResponse = unsafe {
            getModuleListV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash,
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.check_rpc_response()?;
        Ok(buf)
    }

    pub fn get_module_source_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
        module_ref: &crate::grpc2::types::ModuleRef,
    ) -> Result<([u8; 32], Vec<u8>), tonic::Status> {
        use crate::grpc2::Require;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let mut out_hash = [0u8; 32];
        let (block_id_type, block_hash) =
            crate::grpc2::types::block_hash_input_to_ffi(block_hash).require_owned()?;
        let response: ConsensusQueryResponse = unsafe {
            getModuleSourceV2(
                consensus,
                block_id_type,
                block_hash,
                module_ref.value.as_ptr(),
                out_hash.as_mut_ptr(),
                &mut out_data,
                copy_to_vec_callback,
            )
        }
        .try_into()?;
        response.check_rpc_response()?;
        Ok((out_hash, out_data))
    }

    pub fn get_instance_list_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let (block_id_type, block_hash) =
            crate::grpc2::types::block_hash_input_to_ffi(block_hash).require_owned()?;
        let response: ConsensusQueryResponse = unsafe {
            getInstanceListV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash,
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.check_rpc_response()?;
        Ok(buf)
    }

    /// Get information about a specific smart contract instance.
    pub fn get_instance_info_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
        address: &crate::grpc2::types::ContractAddress,
    ) -> Result<([u8; 32], Vec<u8>), tonic::Status> {
        use crate::grpc2::Require;
        let (block_id_type, block_hash) =
            crate::grpc2::types::block_hash_input_to_ffi(block_hash).require_owned()?;
        let addr_index = address.index;
        let addr_subindex = address.subindex;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let mut out_hash = [0u8; 32];
        let response: ConsensusQueryResponse = unsafe {
            getInstanceInfoV2(
                consensus,
                block_id_type,
                block_hash,
                addr_index,
                addr_subindex,
                out_hash.as_mut_ptr(),
                &mut out_data,
                copy_to_vec_callback,
            )
            .try_into()?
        };
        response.check_rpc_response()?;
        Ok((out_hash, out_data))
    }

    pub fn get_ancestors_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
        amount: u64,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let (block_id_type, block_hash) =
            crate::grpc2::types::block_hash_input_to_ffi(block_hash).require_owned()?;
        let response: ConsensusQueryResponse = unsafe {
            getAncestorsV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash,
                amount,
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.check_rpc_response()?;
        Ok(buf)
    }
}

pub enum CallbackType {
    Block = 0,
    FinalizationMessage,
    FinalizationRecord,
    CatchUpStatus,
}

impl TryFrom<u8> for CallbackType {
    type Error = anyhow::Error;

    fn try_from(byte: u8) -> anyhow::Result<Self> {
        match byte {
            0 => Ok(CallbackType::Block),
            1 => Ok(CallbackType::FinalizationMessage),
            2 => Ok(CallbackType::FinalizationRecord),
            3 => Ok(CallbackType::CatchUpStatus),
            _ => Err(anyhow!("Received invalid callback type: {}", byte)),
        }
    }
}

pub extern "C" fn on_finalization_message_catchup_out(
    peer_id: PeerIdFFI,
    data: *const u8,
    len: i64,
) {
    unsafe {
        let msg_variant = PacketType::FinalizationMessage;
        let payload = slice::from_raw_parts(data as *const u8, len as usize);
        let mut full_payload = Vec::with_capacity(1 + payload.len());
        (msg_variant as u8).serial(&mut full_payload);

        full_payload.write_all(payload).unwrap(); // infallible
        let full_payload = Arc::from(full_payload);

        let msg = ConsensusMessage::new(
            MessageType::Outbound(Some((peer_id as usize).into())),
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
    (
        $target:expr,
        $msg_type:expr,
        $genesis_index:expr,
        $msg:expr,
        $msg_length:expr,
        $omit_status:expr
    ) => {
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
            let mut full_payload = Vec::with_capacity(5 + payload.len());
            (msg_variant as u8).serial(&mut full_payload);
            ($genesis_index as u32).serial(&mut full_payload);
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

pub extern "C" fn broadcast_callback(
    msg_type: i64,
    genesis_index: u32,
    msg: *const u8,
    msg_length: i64,
) {
    trace!("Broadcast callback hit - queueing message");
    sending_callback!(None, msg_type, genesis_index, msg, msg_length, None);
}

/// Enqueue the byte array in the provided channel if possible.
/// The return value is
/// - 0 if enqueueing is successful.
/// - -1 if the channel is full.
/// - -2 if there are no more receivers. In this case the given sender is
///   dropped and the given `sender` pointer must not be used anymore.
///
/// If the msg is a null pointer then the `sender` is always dropped, and the
/// response will be `-2`.
extern "C" fn enqueue_bytearray_callback(
    sender: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    msg: *const u8,
    msg_length: i64,
) -> i32 {
    let mut sender = unsafe { Box::from_raw(sender) };
    if msg.is_null() {
        // drop sender
        return -2;
    }
    let data = unsafe { slice::from_raw_parts(msg, msg_length as usize) };
    match sender.try_send(Ok(data.to_vec())) {
        Ok(()) => {
            // Do not drop the sender.
            Box::into_raw(sender);
            0
        }
        Err(e) if e.is_full() => {
            // Do not drop the sender, we will enqueue more things.
            Box::into_raw(sender);
            -1
        }
        Err(_) => {
            // drop the sender since it is no longer necessary.
            -2
        }
    }
}

/// Copy data at the given location (provided by the `data` pointer) at the end
/// of the given vector.
extern "C" fn copy_to_vec_callback(out: *mut Vec<u8>, data: *const u8, len: i64) {
    let data = unsafe { slice::from_raw_parts(data, len as usize) };
    let out = unsafe { &mut *out };
    out.extend_from_slice(data);
}

pub extern "C" fn direct_callback(
    peer_id: u64,
    msg_type: i64,
    genesis_index: u32,
    msg: *const c_char,
    msg_length: i64,
) {
    trace!("Direct callback hit - queueing message");
    sending_callback!(
        Some((peer_id as usize).into()),
        msg_type,
        genesis_index,
        msg,
        msg_length,
        None
    );
}

pub extern "C" fn catchup_status_callback(genesis_index: u32, msg: *const u8, msg_length: i64) {
    trace!("Catch-up status callback hit - queueing message");
    // Note: this sends a catch-up status message as a broadcast. This is not ideal:
    // a catch-up status message should always be sent as a direct message, even
    // when it is sent to every peer.  However, we will rely on peers not to
    // rebroadcast catch-up status messages.
    sending_callback!(
        None,
        CallbackType::CatchUpStatus,
        genesis_index,
        msg,
        msg_length,
        Some(PeerStatus::Pending)
    );
}

/// A callback function that will append a regenesis block to the list of known
/// regenesis hashes, and raise a flag indicating that all peers should be
/// marked for catch-up.
///
/// # Safety
///
/// The first argument has to be an Arc<Regenesis> which was
/// converted into raw, as it will be casted into that type. The second argument
/// has to be a pointer to a bytestring of length 32 or null. If null this is
/// interpreted as we did not know the new genesis block since we did not
/// recognize the protocol update.
pub unsafe extern "C" fn regenesis_callback(ptr: *const Regenesis, block_hash: *const u8) {
    trace!("Regenesis callback hit");
    let arc = Arc::from_raw(ptr);
    if block_hash.is_null() {
        warn!("An unrecognized protocol update encountered. Shutting down the node's connections.");
        arc.stop_network.store(true, Ordering::Release);
    } else {
        write_or_die!(arc.blocks).push(
            BlockHash::new(std::slice::from_raw_parts(block_hash, 32))
                .expect("The slice is exactly 32 bytes so ::new must succeed."),
        );
        arc.trigger_catchup.store(true, Ordering::Release);
    }

    // The pointer must remain valid, so we use into_raw to prevent the reference
    // count from being decremented.
    let _ = Arc::into_raw(arc);
}

/// A callback to free the regenesis Arc.
///
/// # Safety
///
/// The given pointer must be an Arc<Regenesis> which was converted
/// into raw.
pub unsafe extern "C" fn free_regenesis_arc(ptr: *const Regenesis) {
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
