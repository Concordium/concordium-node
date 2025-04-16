use super::helpers::ContractStateResponse;
use crate::{
    common::p2p_peer::RemotePeerId,
    consensus_ffi::{
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
use concordium_base::{
    common::Serial,
    hashes::{BlockHash, TransactionHash},
};
use std::{
    convert::{TryFrom, TryInto},
    ffi::{CStr, CString},
    io::Write,
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

/// An opaque reference to an 'executable block' i.e., a block
/// where the metadata has been verified.
/// The value behind the reference i.e. the "execute block continuation"
/// is created in the consensus module but it's owned here on the rust side.
/// The value is being freed by the consensus side via 'executeBlock'.
#[repr(C)]
pub struct execute_block {
    private: [u8; 0],
}

/// An opaque reference to a dry-run handle, which is managed in Haskell.
#[repr(C)]
pub struct dry_run_handle {
    private: [u8; 0],
}

/// Abstracts the reference required to execute a block that has been received.
/// This wrapper type exists so we make sure that the same '*mut execute_block'
/// is not called twice as we pass ownership of 'ExecuteBlockCallback' to
/// 'executeBlock'.
#[repr(transparent)]
pub struct ExecuteBlockCallback(*mut execute_block);

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

/// A type of callback that extends the given vector with the provided data.
type CopyToVecCallback = extern "C" fn(*mut Vec<u8>, *const u8, i64);

/// The cryptographic parameters expose through ffi.
type CryptographicParameters =
    concordium_base::id::types::GlobalContext<concordium_base::id::constants::ArCurve>;

/// A type of callback that copies cryptographic parameters from one pointer to
/// another.
type CopyCryptographicParametersCallback =
    extern "C" fn(*mut Option<CryptographicParameters>, *const CryptographicParameters);

/// Context for returning V1 contract state in the
/// [`get_instance_state_v2`](ConsensusContainer::get_instance_state_v2) query.
pub struct V1ContractStateReceiver {
    state:  concordium_smart_contract_engine::v1::trie::PersistentState,
    loader: concordium_smart_contract_engine::v1::trie::LoadCallback,
}

/// A type of callback to write V1 contract state into.
type CopyV1ContractStateCallback = extern "C" fn(
    *mut Option<V1ContractStateReceiver>,
    *mut concordium_smart_contract_engine::v1::trie::PersistentState,
    concordium_smart_contract_engine::v1::trie::LoadCallback,
);

/// Context necessary for Haskell code/Consensus to send notifications on
/// important events to Rust code (i.e., RPC server, or network layer).
pub struct NotificationContext {
    /// Notification channel for newly added blocks. This is an unbounded
    /// channel since it makes the implementation simpler. This should not be a
    /// problem since the consumer of this channel is a dedicated task and
    /// blocks are not added to the tree that quickly. So there should not be
    /// much contention for this.
    pub blocks: Option<futures::channel::mpsc::UnboundedSender<Arc<[u8]>>>,
    /// Notification channel for newly finalized blocks. See
    /// [NotificationContext::blocks] documentation for why having an unbounded
    /// channel is OK here, and is unlikely to lead to resource exhaustion.
    pub finalized_blocks: Option<futures::channel::mpsc::UnboundedSender<Arc<[u8]>>>,
    /// The block height of the last finalized block.
    pub last_finalized_block_height: prometheus::core::GenericGauge<prometheus::core::AtomicU64>,
    /// Timestamp of receiving last finalized block (Unix time in milliseconds).
    pub last_finalized_block_timestamp: prometheus::IntGauge,
    /// The block height of the last arrived block.
    pub last_arrived_block_height: prometheus::core::GenericGauge<prometheus::core::AtomicU64>,
    /// Timestamp of receiving last arrived block (Unix time in milliseconds).
    pub last_arrived_block_timestamp: prometheus::IntGauge,
    /// Total number of blocks baked by the node since startup.
    pub baked_blocks: prometheus::IntCounter,
    /// Total number of finalized blocks baked by the node since startup.
    pub finalized_baked_blocks: prometheus::IntCounter,
}

/// A type of callback used to notify Rust code of important events. The
/// callback is called with
/// - the context
/// - the event type (0 for block arrived, 1 for block finalized)
/// - pointer to a byte array containing the serialized event
/// - length of the data
/// - block height of either the finalized block or arrived block
/// - byte where a value of 1 indicates the block arrived/finalized was baked by
///   this node.
///
/// The callback should not retain references to supplied data after the exit.
type NotifyCallback = unsafe extern "C" fn(*mut NotificationContext, u8, *const u8, u64, u64, u8);

pub struct NotificationHandlers {
    pub blocks:           futures::channel::mpsc::UnboundedReceiver<Arc<[u8]>>,
    pub finalized_blocks: futures::channel::mpsc::UnboundedReceiver<Arc<[u8]>>,
}

/// A type of callback used to signal the Rust code of a pending unsupported
/// protocol update. The callback is called with:
/// - the context
/// - effective time of unsupported update (milliseconds since unix epoch).
type NotifyUnsupportedUpdatesCallback =
    unsafe extern "C" fn(*mut NotifyUnsupportedUpdatesContext, u64);

pub struct NotifyUnsupportedUpdatesContext {
    /// If non-zero the value represents the effective timestamp of unsupported
    /// protocol update as milliseconds since unix epoch.
    pub unsupported_pending_protocol_version:
        prometheus::core::GenericGauge<prometheus::core::AtomicU64>,
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
        modules_cache_size: u32,
        genesis_data: *const u8,
        genesis_data_len: i64,
        private_data: *const u8,
        private_data_len: i64,
        notify_context: *mut NotificationContext,
        notify_callback: NotifyCallback,
        unsupported_update_context: *mut NotifyUnsupportedUpdatesContext,
        unsupported_update_callback: NotifyUnsupportedUpdatesCallback,
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
        modules_cache_size: u32,
        genesis_data: *const u8,
        genesis_data_len: i64,
        notify_context: *mut NotificationContext,
        notify_callback: NotifyCallback,
        unsupported_update_context: *mut NotifyUnsupportedUpdatesContext,
        unsupported_update_callback: NotifyUnsupportedUpdatesCallback,
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
    pub fn startBaker(consensus: *mut consensus_runner);
    pub fn receiveBlock(
        consensus: *mut consensus_runner,
        genesis_index: u32,
        block_data: *const u8,
        data_length: u64,
        // A mutable *pointer that will be written to
        // by the consensus layer with continuation required for
        // executing the just received block.
        execute_block_ptr_ptr: *mut *mut execute_block,
    ) -> i64;
    pub fn executeBlock(
        consensus: *mut consensus_runner,
        // Pointer to the continuation for
        // adding the block to the tree.
        execute_block_callback: ExecuteBlockCallback,
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
        out_hash: *mut u8, /* location where the hash of the transaction will be written if the
                            * transaction is successfully added. */
    ) -> i64;
    pub fn stopBaker(consensus: *mut consensus_runner);
    pub fn stopConsensus(consensus: *mut consensus_runner);

    /// Get the total number of non-finalized transactions across all accounts.
    pub fn getNumberOfNonFinalizedTransactions(consensus: *mut consensus_runner) -> u64;

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
        baker_lottery_power: *mut f64,
    ) -> u8;
    pub fn checkIfWeAreFinalizer(consensus: *mut consensus_runner) -> u8;
    pub fn checkIfRunning(consensus: *mut consensus_runner) -> u8;
    pub fn getLastFinalizedBlockHeight(consensus: *mut consensus_runner) -> u64;
    pub fn importBlocks(
        consensus: *mut consensus_runner,
        import_file_path: *const u8,
        import_file_path_len: i64,
    ) -> i64;
    pub fn stopImportingBlocks(consensus: *mut consensus_runner);

    pub fn freeByteArray(hstring: *const u8);

    // Functions related to V2 GRPC interface.

    /// Get information about a specific account in a given block.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `account_id_type` - Type of account identifier.
    /// * `account_id` - Location with the account identifier. Length must match
    ///   the corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn getAccountInfoV2(
        consensus: *mut consensus_runner,
        block_id_type: u8,
        block_id: *const u8,
        account_id_type: u8,
        account_id: *const u8,
        out_hash: *mut u8,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Get next account sequence number.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `account_address_ptr` - Pointer to account address. Must contain 32
    ///   bytes.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn getNextAccountSequenceNumberV2(
        consensus: *mut consensus_runner,
        account_address_ptr: *const u8,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Get the current consensus info.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn getConsensusInfoV2(
        consensus: *mut consensus_runner,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Get the detailed status of the consensus. If the genesis index is
    /// explicitly specified, then the status of the consensus at that
    /// genesis index is returned. Otherwise, the status of the consensus at
    /// the latest genesis index is returned.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `use_genesis_index` - Non-zero if the genesis index is explicitly
    ///   specified.
    /// * `genesis_index` - Genesis index to use if `use_genesis_index` is
    ///   non-zero.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn getConsensusDetailedStatusV2(
        consensus: *mut consensus_runner,
        use_genesis_index: u8,
        genesis_index: u32,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Get the cryptographic parameters at the end of a given block.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn getCryptographicParametersV2(
        consensus: *mut consensus_runner,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        out: *mut Option<CryptographicParameters>,
        copier: CopyCryptographicParametersCallback,
    ) -> i64;

    /// Stream a list of all modules.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getModuleListV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get the source of a smart contract module.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `module_ref` - Location with the module reference. Length must be 32
    ///   bytes.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn getModuleSourceV2(
        consensus: *mut consensus_runner,
        block_id_type: u8,
        block_id: *const u8,
        module_ref: *const u8,
        out_hash: *mut u8,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Stream a list of smart contract instances.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getInstanceListV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get an information about a specific smart contract instance.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `contract_address_index` - The contract address index to use for the
    ///   query.
    /// * `contract_address_subindex` - The contract address subindex to use for
    ///   the query.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn getInstanceInfoV2(
        consensus: *mut consensus_runner,
        block_id_type: u8,
        block_id: *const u8,
        contract_address_index: u64,
        contract_address_subindex: u64,
        out_hash: *mut u8,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Get an information about a specific smart contract instance state.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `contract_address_index` - The contract address index to use for the
    ///   query.
    /// * `contract_address_subindex` - The contract address subindex to use for
    ///   the query.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `out_v0` - Location to write the state if the instance is a V0
    ///   instance.
    /// * `copier_v0` - Callback for writting the output in case of a V0
    ///   instance.
    /// * `out_v1` - Location where to write the state if the instance is a V1
    ///   instance.
    /// * `copier_v1` - Callback for writting the output in case of a V1
    ///   instance.
    pub fn getInstanceStateV2(
        consensus: *mut consensus_runner,
        block_id_type: u8,
        block_id: *const u8,
        contract_address_index: u64,
        contract_address_subindex: u64,
        out_hash: *mut u8,
        out_v0: *mut Vec<u8>,
        copier_v0: CopyToVecCallback,
        out_v1: *mut Option<V1ContractStateReceiver>,
        copier_v1: CopyV1ContractStateCallback,
    ) -> i64;

    /// Stream a list of ancestors for the given block
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `depth` - The maximum number of ancestors to include in the response.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getAncestorsV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        depth: u64,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get the first block of a specified epoch
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `query_type` - Tag indicating the query type (specified epoch or
    ///   block).
    /// * `query_data` - Additional data to specify the epoch/block, depending
    ///   on the tag.
    /// * `out_hash` - Location to write the block hash on success.
    ///
    /// The return code is one of the following:
    ///
    /// * Success (0) - The block was found and written in `out_hash`.
    /// * NotFound (1) - The block used to query for the epoch was not found, or
    ///   the epoch is finalized but empty (only applies in consensus version
    ///   0).
    /// * FutureEpoch (3) - The query is for a future genesis index, or the
    ///   current one but the epoch contains no finalized blocks yet.
    /// * InvalidArgument (-2) - The specified epoch is for a past genesis index
    ///   and will never contain finalized blocks.
    pub fn getFirstBlockEpochV2(
        consensus: *mut consensus_runner,
        query_type: u8,
        query_data: *const u8,
        out_hash: *mut u8,
    ) -> i64;

    /// Stream a list of the winners of rounds in a specified epoch, including
    /// whether the a block in the round was included in the finalized
    /// chain.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `query_type` - Tag indicating the query type (specified epoch or
    ///   block).
    /// * `query_data` - Additional data to specify the epoch/block, depending
    ///   on the tag.
    /// * `callback` - Callback for writing to the response stream.
    ///
    /// The return code is one of the following:
    /// * Success (0) - The epoch is finalized and the winners are streamed.
    /// * NotFound (1) - The block used to query for the epoch was not found.
    /// * FutureEpoch (3) - The query is for a future genesis index, or the
    ///   current one but the epoch is not finalized yet.
    /// * InvalidArgument (-2) - The query is for a past genesis index where the
    ///   epoch was never finalized, or for a genesis index in consensus version
    ///   0.
    pub fn getWinningBakersEpochV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        query_type: u8,
        query_data: *const u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get the list of accounts in a given block and, if the block exists,
    /// enqueue them into the provided [Sender](futures::channel::mpsc::Sender).
    ///
    /// Individual account addresses are enqueued using the provided callback.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getAccountListV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get the list of tokens in a given block and, if the block exists,
    /// enqueue them into the provided [Sender](futures::channel::mpsc::Sender).
    ///
    /// Individual protocol level tokens are enqueued using the provided
    /// callback.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getTokenListV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get an information about a specific block item.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `transaction_hash` - The transaction hash to use for the query.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn getBlockItemStatusV2(
        consensus: *mut consensus_runner,
        transaction_hash: *const u8,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Run the smart contract entrypoint in a given context and in the state at
    /// the end of the given block.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `contract_index` - The contact index to invoke.
    /// * `contract_subindex` - The contact subindex to invoke.
    /// * `invoker_address_type` - Tag for whether an account or a contract
    ///   address is provided. If 0 no address is provided, if 1 the
    ///   `invoker_account_address_ptr` is 32 bytes for an account address, if 2
    ///   the `invoker_contract_index` and `invoker_contract_subindex` is used
    ///   for the contract address.
    /// * `invoker_account_address_ptr` - Pointer to the address if this is
    ///   provided. The length will depend on the value of
    ///   `invoker_address_type`.
    /// * `invoker_contract_index` - The invoker contact index. Only used if
    ///   `invoker_address_type` is 2.
    /// * `invoker_contract_subindex` - The invoker contact subindex. Only used
    ///   if `invoker_address_type` is 2.
    /// * `amount` - The amount to use for the invocation.
    /// * `receive_name_ptr` - Pointer to the entrypoint to invoke.
    /// * `receive_name_len` - Length of the bytes for the entrypoint.
    /// * `parameter_ptr` - Pointer to the parameter to invoke with.
    /// * `parameter_len` - Length of the bytes for the parameter.
    /// * `energy` - The energy to use for the invocation.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn invokeInstanceV2(
        consensus: *mut consensus_runner,
        block_id_type: u8,
        block_id: *const u8,
        contract_index: u64,
        contract_subindex: u64,
        invoker_address_type: u8,
        invoker_account_address_ptr: *const u8,
        invoker_contract_index: u64,
        invoker_contract_subindex: u64,
        amount: u64,
        receive_name_ptr: *const u8,
        receive_name_len: u32,
        parameter_ptr: *const u8,
        parameter_len: u32,
        energy: u64,
        out_hash: *mut u8,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Get information, such as height, timings, and transaction counts for the
    /// given block.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn getBlockInfoV2(
        consensus: *mut consensus_runner,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Stream a list of bakers at the end of a given block.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getBakerListV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get information about a given pool at the end of a given block.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `baker_id` - Baker id of the owner of the pool to query.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn getPoolInfoV2(
        consensus: *mut consensus_runner,
        block_id_type: u8,
        block_id: *const u8,
        baker_id: u64,
        out_hash: *mut u8,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Get information about the passive delegators at the end of a given
    /// block.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn getPassiveDelegationInfoV2(
        consensus: *mut consensus_runner,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Get a list of live blocks at a given height.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `height` - Block height, is absolute if the genesis_index is 0,
    ///   otherwise relative.
    /// * `genesis_index` - Genesis index to start from. Set to 0 to use
    ///   absolute height.
    /// * `restrict` - Whether to return results only from the specified genesis
    ///   index (1), or allow results from more recent genesis indices as well
    ///   (0).
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn getBlocksAtHeightV2(
        consensus: *mut consensus_runner,
        height: u64,
        genesis_index: u32,
        restrict: u8,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Get information related to tokenomics at the end of a given block.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn getTokenomicsInfoV2(
        consensus: *mut consensus_runner,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Get the pool delegators of a given pool at the end of a given block. The
    /// stream will end when all the delegators have been returned.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `baker_id` - Baker id of the owner of the pool to query.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getPoolDelegatorsV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        baker_id: u64,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get the reward period pool delegators of a given pool at the end of a
    /// given block. The stream will end when all the delegators have been
    /// returned.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `baker_id` - Baker id of the owner of the pool to query.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getPoolDelegatorsRewardPeriodV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        baker_id: u64,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get the passive delegators at the end of a given block. The stream will
    /// end when all the delegators have been returned.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getPassiveDelegatorsV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get the reward period passive delegators at the end of a given block.
    /// The stream will end when all the delegators have been returned.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getPassiveDelegatorsRewardPeriodV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get the current branches of blocks starting and including from the last
    /// finalized block.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn getBranchesV2(
        consensus: *mut consensus_runner,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Get information related to the baker election for a particular block.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn getElectionInfoV2(
        consensus: *mut consensus_runner,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Get the identity providers registered as of the end of a given block.
    /// The stream will end when all the identity providers have been returned.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getIdentityProvidersV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get the anonymity revokers registered as of the end of a given block.
    /// The stream will end when all the anonymity revokers have been returned.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getAnonymityRevokersV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get a list of non-finalized transaction hashes for a given account.
    /// The stream will end when all the non-finalized transaction hashes have
    /// been returned.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `account_address_ptr` - Pointer to account address. Must contain 32
    ///   bytes.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getAccountNonFinalizedTransactionsV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        account_address_ptr: *const u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get a list of items for a given block.
    /// The stream will end when all the transactions
    /// for the block have been returned
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` whether to query latest finalized or a specific block.
    /// * `block_hash_ptr` - Pointer to block hash. Must contain 32 bytes.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getBlockItemsV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_hash_ptr: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get a list of transaction events in a given block.
    /// The stream will end when all the transaction events for a given block
    /// have been returned.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getBlockTransactionEventsV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get a list of special events in a given block.
    /// The stream will end when all the special events for a given block have
    /// been returned.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getBlockSpecialEventsV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get the pending updates to chain parameters at the end of a given block.
    /// The stream will end when all the pending updates for a given block have
    /// been returned.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getBlockPendingUpdatesV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get next available sequence numbers for updating chain parameters after
    /// a given block.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn getNextUpdateSequenceNumbersV2(
        consensus: *mut consensus_runner,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Get all accounts that have scheduled releases, with the timestamp of the
    /// first pending scheduled release for that account.
    /// The stream will end when all the accounts that have scheduled releases
    /// have been returned.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getScheduledReleaseAccountsV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get all accounts that have stake in cooldown, with the timestamp of the
    /// first pending cooldown expiry for each account.
    /// The stream will end when all the accounts that have stake in cooldown
    /// have been returned.
    /// Prior to protocol version 7, the resulting stream will always be empty.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getCooldownAccountsV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get all accounts (by account index) that have stake in pre-cooldown.
    /// The stream will end when all the accounts that have stake in
    /// pre-cooldown have been returned.
    /// Prior to protocol version 7, the resulting stream will always be empty.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getPreCooldownAccountsV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get all accounts (by account index) that have stake in pre-pre-cooldown.
    /// The stream will end when all the accounts that have stake in
    /// pre-pre-cooldown have been returned.
    /// Prior to protocol version 7, the resulting stream will always be empty.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getPrePreCooldownAccountsV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get the chain parameters that are in effect in the given block.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn getBlockChainParametersV2(
        consensus: *mut consensus_runner,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Get the finalization summary of the block, i.e., whether it contains the
    /// finalization record or not, and details if so.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn getBlockFinalizationSummaryV2(
        consensus: *mut consensus_runner,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Get the slot time (in milliseconds) of the last finalized block.
    pub fn getLastFinalizedBlockSlotTimeV2(consensus: *mut consensus_runner) -> u64;

    /// Get the baker reward infos for the epoch defined by the querying block.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `stream` - Pointer to the response stream.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `callback` - Callback for writing to the response stream.
    pub fn getBakersRewardPeriodV2(
        consensus: *mut consensus_runner,
        stream: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        callback: extern "C" fn(
            *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
            *const u8,
            i64,
        ) -> i32,
    ) -> i64;

    /// Get the certificates for a given block.
    /// This is only available in consensus version 1 and returns
    /// IllegalArgument in consensus version 0.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `block_id_type` - Type of block identifier.
    /// * `block_id` - Location with the block identifier. Length must match the
    ///   corresponding type of block identifier.
    /// * `out_hash` - Location to write the block hash used in the query.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writting the output.
    pub fn getBlockCertificatesV2(
        consensus: *mut consensus_runner,
        block_id_type: u8,
        block_id: *const u8,
        out_hash: *mut u8,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Get the earliest time in which a baker wins the lottery. Returns "not
    /// found" for consensus version 0. For consensus version 1, it will
    /// return a result even if the baker ID does not correspond to a
    /// currently-valid baker.
    ///
    /// * `consensus` - Pointer to the current consensus.
    /// * `baker_id` - ID of baker to get the time for.
    /// * `out` - Location to write the output of the query.
    /// * `copier` - Callback for writing the output.
    pub fn getBakerEarliestWinTimeV2(
        consensus: *mut consensus_runner,
        baker_id: u64,
        out: *mut Vec<u8>,
        copier: CopyToVecCallback,
    ) -> i64;

    /// Start a dry-run sequence. The returned handle must be freed with a call
    /// to `dryRunEnd` once it is no longer required. (Failure to do so will
    /// leak memory.)
    ///
    /// * `consensus` - Pointer to the consensus.
    /// * `copier` - Callback for appending a bytestring to a vector.
    /// * `energy_quota` - Limit on total energy cost for operations in this
    ///   dry-run sequence.
    pub fn dryRunStart(
        consensus: *mut consensus_runner,
        copier: CopyToVecCallback,
        energy_quota: u64,
    ) -> *mut dry_run_handle;

    /// Terminate a dry-run sequence, freeing up the resources associated with
    /// the handle.
    pub fn dryRunEnd(dry_run_handle: *mut dry_run_handle);

    /// Load state from a specified block as part of a dry-run sequence.
    /// The return value is 0 on success, 1 on an internal error, and 2
    /// on out-of-energy.
    pub fn dryRunLoadBlockState(
        dry_run_handle: *mut dry_run_handle,
        block_id_type: u8,
        block_id: *const u8,
        out: *mut Vec<u8>,
    ) -> i64;

    /// Get info about a specified account as part of a dry-run sequence.
    /// The return value is 0 on success, 1 on an internal error, and 2
    /// on out-of-energy.
    pub fn dryRunGetAccountInfo(
        dry_run_handle: *mut dry_run_handle,
        account_identifier_tag: u8,
        account_identifier_data: *const u8,
        out: *mut Vec<u8>,
    ) -> i64;

    /// Get info about a specified smart contract instance as part of a dry-run
    /// sequence.
    /// The return value is 0 on success, 1 on an internal error, and 2
    /// on out-of-energy.
    pub fn dryRunGetInstanceInfo(
        dry_run_handle: *mut dry_run_handle,
        contract_index: u64,
        contract_subindex: u64,
        out: *mut Vec<u8>,
    ) -> i64;

    /// Run the smart contract entrypoint in a given context as part of a
    /// dry-run sequence.
    ///
    /// * `dry_run_handle` - Handle created with `dryRunStart`.
    /// * `contract_index` - The contact index to invoke.
    /// * `contract_subindex` - The contact subindex to invoke.
    /// * `invoker_address_type` - Tag for whether an account or a contract
    ///   address is provided. If 0 no address is provided, if 1 the
    ///   `invoker_account_address_ptr` is 32 bytes for an account address, if 2
    ///   the `invoker_contract_index` and `invoker_contract_subindex` is used
    ///   for the contract address.
    /// * `invoker_account_address_ptr` - Pointer to the address if this is
    ///   provided. The length will depend on the value of
    ///   `invoker_address_type`.
    /// * `invoker_contract_index` - The invoker contact index. Only used if
    ///   `invoker_address_type` is 2.
    /// * `invoker_contract_subindex` - The invoker contact subindex. Only used
    ///   if `invoker_address_type` is 2.
    /// * `amount` - The amount to use for the invocation.
    /// * `receive_name_ptr` - Pointer to the entrypoint to invoke.
    /// * `receive_name_len` - Length of the bytes for the entrypoint.
    /// * `parameter_ptr` - Pointer to the parameter to invoke with.
    /// * `parameter_len` - Length of the bytes for the parameter.
    /// * `energy` - The energy to use for the invocation.
    /// * `out` - Location to write the output of the query.
    ///
    /// The return value is 0 on success, 1 on an internal error, and 2
    /// on out-of-energy.
    pub fn dryRunInvokeInstance(
        dry_run_handle: *mut dry_run_handle,
        contract_index: u64,
        contract_subindex: u64,
        invoker_address_type: u8,
        invoker_account_address_ptr: *const u8,
        invoker_contract_index: u64,
        invoker_contract_subindex: u64,
        amount: u64,
        receive_name_ptr: *const u8,
        receive_name_len: u32,
        parameter_ptr: *const u8,
        parameter_len: u32,
        energy: u64,
        out: *mut Vec<u8>,
    ) -> i64;

    /// Set the current timestamp to use as part of a dry-run sequence.
    pub fn dryRunSetTimestamp(
        dry_run_handle: *mut dry_run_handle,
        new_timestamp: u64,
        out: *mut Vec<u8>,
    ) -> i64;

    /// Mint an amount to an account as part of a dry-run sequence.
    pub fn dryRunMintToAccount(
        dry_run_handle: *mut dry_run_handle,
        account_address: *const u8,
        amount: u64,
        out: *mut Vec<u8>,
    ) -> i64;

    /// Execute a transaction as part of a dry-run sequence.
    ///
    /// * `dry_run_handle` - Handle created with `dryRunStart`.
    /// * `sender_address_ptr` - Pointer to the encoded account address of the
    ///   transaction sender.
    /// * `energy` - Limit on the energy to be used by the transaction.
    /// * `payload` - Pointer to the encoded transaction payload.
    /// * `payload_length` - Length in bytes of the encoded transaction payload.
    /// * `signatures` - An array of pairs of credential ID (u8) and key ID (u8)
    ///   that are deemed to have signed the transaction.
    /// * `signature_count` - The number of signatures deemed to have signed the
    ///   transaction. The length of `signatures` in bytes must be `2 *
    ///   signature_count`.
    /// * `out` - Vector to write the output of the query.
    pub fn dryRunTransaction(
        dry_run_handle: *mut dry_run_handle,
        sender_address_ptr: *const u8,
        energy: u64,
        payload: *const u8,
        payload_length: u64,
        signatures: *const u8,
        signature_count: u64,
        out: *mut Vec<u8>,
    ) -> i64;
}

/// This is the callback invoked by consensus on newly arrived, and newly
/// finalized blocks. The callback enqueues the block in the corresponding
/// channel for further processing. The other end of the channel is owned by a
/// background task that forwards data from the channels into any currently
/// active RPC clients.
unsafe extern "C" fn notify_callback(
    notify_context: *mut NotificationContext,
    ty: u8,
    data_ptr: *const u8,
    data_len: u64,
    block_height: u64,
    home_baked: u8,
) {
    let sender = &*notify_context;
    let home_baked = home_baked == 1;
    match ty {
        0u8 => {
            sender.last_arrived_block_height.set(block_height);
            sender.last_arrived_block_timestamp.set(chrono::Utc::now().timestamp_millis());
            if home_baked {
                sender.baked_blocks.inc()
            }

            if let Some(blocks) = &sender.blocks {
                if blocks
                    .unbounded_send(std::slice::from_raw_parts(data_ptr, data_len as usize).into())
                    .is_err()
                {
                    error!("Failed to enqueue block that arrived.");
                    // do nothing. The error here should only happen if the
                    // receiver is disconnected, which means that the task
                    // forwarding events has been killed. That should never
                    // happen, and if it does, it indicates
                    // a disastrous situation.
                }
            }
        }
        1u8 => {
            sender.last_finalized_block_height.set(block_height);
            sender.last_finalized_block_timestamp.set(chrono::Utc::now().timestamp_millis());
            if home_baked {
                sender.finalized_baked_blocks.inc()
            }
            if let Some(finalized_blocks) = &sender.finalized_blocks {
                if finalized_blocks
                    .unbounded_send(std::slice::from_raw_parts(data_ptr, data_len as usize).into())
                    .is_err()
                {
                    error!("Failed to enqueue finalized block.");
                    // do nothing. The error here should only happen if the
                    // receiver is disconnected, which means that the task
                    // forwarding events has been killed. That should never
                    // happen, and if it does, it indicates
                    // a disastrous situation.
                }
            }
        }
        unexpected => {
            error!("Unexpected notification type {}. This is a bug.", unexpected);
            // do nothing
        }
    }
}

/// This is the callback invoked by consensus to signal a pending unsupported
/// protocol update. The information is exposed as a prometheus metric allowing
/// node-runners to setup alerts.
unsafe extern "C" fn unsupported_update_callback(
    context_ptr: *mut NotifyUnsupportedUpdatesContext,
    unsupported_update_pending: u64,
) {
    let context = &*context_ptr;
    context.unsupported_pending_protocol_version.set(unsupported_update_pending);
}

/// Information needed to start consensus.
pub struct StartConsensusConfig {
    /// Serialized genesis data.
    pub genesis_data:               Vec<u8>,
    /// Maximum logging level.
    pub maximum_log_level:          ConsensusLogLevel,
    /// Regenesis object.
    pub regenesis_arc:              Arc<Regenesis>,
    /// Context for notifying upon new block arrival, and new finalized blocks.
    pub notification_context:       Option<NotificationContext>,
    /// Context for when signalling a unsupported protocol update is pending.
    pub unsupported_update_context: Option<NotifyUnsupportedUpdatesContext>,
}

pub fn get_consensus_ptr(
    runtime_parameters: &ConsensusRuntimeParameters,
    start_config: StartConsensusConfig,
    private_data: Option<Vec<u8>>,
    appdata_dir: &Path,
) -> anyhow::Result<*mut consensus_runner> {
    let genesis_data_len = start_config.genesis_data.len();
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
                    runtime_parameters.modules_cache_size,
                    start_config.genesis_data.as_ptr(),
                    genesis_data_len as i64,
                    private_data_bytes.as_ptr(),
                    private_data_len as i64,
                    start_config
                        .notification_context
                        .map_or(std::ptr::null_mut(), |ctx| Box::into_raw(Box::new(ctx))),
                    notify_callback,
                    start_config
                        .unsupported_update_context
                        .map_or(std::ptr::null_mut(), |ctx| Box::into_raw(Box::new(ctx))),
                    unsupported_update_callback,
                    broadcast_callback,
                    catchup_status_callback,
                    Arc::into_raw(start_config.regenesis_arc),
                    free_regenesis_arc,
                    regenesis_callback,
                    start_config.maximum_log_level as u8,
                    on_log_emited,
                    appdata_buf.as_ptr(),
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
                        runtime_parameters.modules_cache_size,
                        start_config.genesis_data.as_ptr(),
                        genesis_data_len as i64,
                        start_config
                            .notification_context
                            .map_or(std::ptr::null_mut(), |ctx| Box::into_raw(Box::new(ctx))),
                        notify_callback,
                        start_config
                            .unsupported_update_context
                            .map_or(std::ptr::null_mut(), |ctx| Box::into_raw(Box::new(ctx))),
                        unsupported_update_callback,
                        catchup_status_callback,
                        Arc::into_raw(start_config.regenesis_arc),
                        free_regenesis_arc,
                        regenesis_callback,
                        start_config.maximum_log_level as u8,
                        on_log_emited,
                        appdata_buf.as_ptr(),
                        appdata_buf.len() as i64,
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
        11 => bail!("Block state database has incorrect version information."),
        n => bail!("Unknown error code: {}.", n),
    }
}

/// A dry-run session. This wraps the FFI operations on a dry-run handle, and
/// ensures that `dryRunEnd` is called when the `DryRun` object is dropped.
pub struct DryRun {
    handle: *mut dry_run_handle,
}

/// A dry-run handle is not bound to a particular thread, and therefore it is
/// safe for it to be `Send`.
unsafe impl Send for DryRun {}

impl Drop for DryRun {
    fn drop(&mut self) { unsafe { dryRunEnd(self.handle) } }
}

impl DryRun {
    /// Load the state of a particular block in the dry-run session, and use its
    /// timestamp as the current timestamp for the session.
    pub fn load_block_state(
        &mut self,
        request: &crate::grpc2::types::BlockHashInput,
    ) -> Result<Vec<u8>, tonic::Status> {
        use crate::grpc2::Require;
        let mut out_data: Vec<u8> = Vec::new();
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(request).require()?;
        let (block_id_type, block_id) = bhi.to_ptr();
        let res = unsafe {
            dryRunLoadBlockState(self.handle, block_id_type, block_id.as_ptr(), &mut out_data)
        };
        DryRun::check_result(res, "load block state")?;
        Ok(out_data)
    }

    /// Look up information on a particular account in the current dry-run
    /// state.
    pub fn get_account_info(
        &mut self,
        target: &crate::grpc2::types::AccountIdentifierInput,
    ) -> Result<Vec<u8>, tonic::Status> {
        use crate::grpc2::Require;
        let mut out_data: Vec<u8> = Vec::new();
        let (acc_type, acc_id) =
            crate::grpc2::types::account_identifier_to_ffi(target).require()?;
        let res = unsafe { dryRunGetAccountInfo(self.handle, acc_type, acc_id, &mut out_data) };
        DryRun::check_result(res, "get account info")?;
        Ok(out_data)
    }

    /// Look up information on a particular smart contract instance in the
    /// current dry-run state.
    pub fn get_instance_info(
        &mut self,
        target: &crate::grpc2::types::ContractAddress,
    ) -> Result<Vec<u8>, tonic::Status> {
        let mut out_data: Vec<u8> = Vec::new();
        let res = unsafe {
            dryRunGetInstanceInfo(self.handle, target.index, target.subindex, &mut out_data)
        };
        DryRun::check_result(res, "get instance info")?;
        Ok(out_data)
    }

    /// Invoke an entrypoint on a smart contract instance in the current dry-run
    /// state. No changes to the state are retained at the completion of
    /// this operation.
    pub fn invoke_instance(
        &mut self,
        request: &crate::grpc2::types::DryRunInvokeInstance,
    ) -> Result<Vec<u8>, tonic::Status> {
        use crate::grpc2::Require;
        let mut out_data: Vec<u8> = Vec::new();

        // Optional Address to ffi
        let (
            invoker_address_type,
            invoker_account_address_ptr,
            invoker_contract_index,
            invoker_contract_subindex,
        ) = if let Some(address) = &request.invoker {
            match address.r#type.as_ref().require()? {
                crate::grpc2::types::address::Type::Account(account) => {
                    (1, crate::grpc2::types::account_address_to_ffi(account).require()?, 0, 0)
                }
                crate::grpc2::types::address::Type::Contract(contract) => {
                    (2, std::ptr::null(), contract.index, contract.subindex)
                }
            }
        } else {
            (0, std::ptr::null(), 0, 0)
        };

        let amount = request.amount.as_ref().require()?.value;

        let (receive_name_ptr, receive_name_len) =
            crate::grpc2::types::receive_name_to_ffi(request.entrypoint.as_ref().require()?)
                .require()?;

        // Parameter to ffi
        let (parameter_ptr, parameter_len) = {
            let bytes = &request.parameter.as_ref().require()?.value;
            (
                bytes.as_ptr(),
                bytes.len().try_into().map_err(|_| {
                    tonic::Status::invalid_argument("Parameter exceeds maximum supported size.")
                })?,
            )
        };

        let energy = request.energy.as_ref().require()?.value;

        let contract = request.instance.as_ref().require()?;

        let res = unsafe {
            dryRunInvokeInstance(
                self.handle,
                contract.index,
                contract.subindex,
                invoker_address_type,
                invoker_account_address_ptr,
                invoker_contract_index,
                invoker_contract_subindex,
                amount,
                receive_name_ptr,
                receive_name_len,
                parameter_ptr,
                parameter_len,
                energy,
                &mut out_data,
            )
        };
        DryRun::check_result(res, "invoke instance")?;
        Ok(out_data)
    }

    /// Set the current block time for the dry-run session.
    pub fn set_timestamp(
        &mut self,
        new_timestamp: crate::grpc2::types::Timestamp,
    ) -> Result<Vec<u8>, tonic::Status> {
        let mut out_data: Vec<u8> = Vec::new();

        let res = unsafe { dryRunSetTimestamp(self.handle, new_timestamp.value, &mut out_data) };
        DryRun::check_result(res, "set timestamp")?;
        Ok(out_data)
    }

    /// Mint a specified amount and credit it to the specified account.
    pub fn mint_to_account(
        &mut self,
        mint: crate::grpc2::types::DryRunMintToAccount,
    ) -> Result<Vec<u8>, tonic::Status> {
        use crate::grpc2::Require;
        let mut out_data: Vec<u8> = Vec::new();
        let account_address =
            crate::grpc2::types::account_address_to_ffi(mint.account.as_ref().require()?)
                .require()?;

        let amount = mint.amount.require()?;
        let res = unsafe {
            dryRunMintToAccount(self.handle, account_address, amount.value, &mut out_data)
        };
        DryRun::check_result(res, "mint to account")?;
        Ok(out_data)
    }

    /// Run a transaction in the current dry-run state, updating the state if it
    /// succeeds.
    pub fn transaction(
        &mut self,
        request: crate::grpc2::types::DryRunTransaction,
    ) -> Result<Vec<u8>, tonic::Status> {
        use crate::grpc2::Require;
        let mut out_data: Vec<u8> = Vec::new();
        let energy = request.energy_amount.as_ref().require()?.value;

        let sender_address =
            crate::grpc2::types::account_address_to_ffi(request.sender.as_ref().require()?)
                .require()?;
        let encoded_payload =
            concordium_base::transactions::EncodedPayload::try_from(request.payload.require()?)?;
        let payload_bytes: Vec<u8> = encoded_payload.into();
        let signature_count = request.signatures.len();
        let mut signature_vec: Vec<u8> = Vec::with_capacity(signature_count * 2);
        for sig in request.signatures {
            signature_vec.push(
                sig.credential.try_into().map_err(|_| {
                    tonic::Status::invalid_argument("Credential index out of bounds")
                })?,
            );
            signature_vec.push(
                sig.key
                    .try_into()
                    .map_err(|_| tonic::Status::invalid_argument("Key index out of bounds"))?,
            );
        }

        let res = unsafe {
            dryRunTransaction(
                self.handle,
                sender_address,
                energy,
                payload_bytes.as_ptr(),
                payload_bytes.len() as u64,
                signature_vec.as_ptr(),
                signature_count as u64,
                &mut out_data,
            )
        };
        DryRun::check_result(res, "run transaction")?;
        Ok(out_data)
    }

    /// Convert a result code returned by a dry-run FFI into an appropriate
    /// [`tonic::Status`].
    fn check_result(result: i64, origin: &str) -> Result<(), tonic::Status> {
        match result {
            0 => Ok(()),
            1 => Err(tonic::Status::internal(format!(
                "Internal error: {origin} could not be completed"
            ))),
            2 => Err(tonic::Status::resource_exhausted("Energy quota exceeded")),
            _ => Err(tonic::Status::internal(format!(
                "Internal error: unexpected error code in {origin}"
            ))),
        }
    }
}

impl ConsensusContainer {
    pub fn receive_block(
        &self,
        genesis_index: u32,
        block: &[u8],
    ) -> (ConsensusFfiResponse, Option<ExecuteBlockCallback>) {
        let consensus = self.consensus.load(Ordering::SeqCst);

        let mut ptr_block_to_execute = std::ptr::null_mut();
        let ptr_ptr_block_to_execute = &mut ptr_block_to_execute;
        let result = unsafe {
            receiveBlock(
                consensus,
                genesis_index,
                block.as_ptr(),
                block.len() as u64,
                ptr_ptr_block_to_execute,
            )
        };
        let callback = if ptr_block_to_execute.is_null() {
            None
        } else {
            Some(ExecuteBlockCallback(ptr_block_to_execute))
        };

        (
            ConsensusFfiResponse::try_from(result)
                .unwrap_or_else(|code| panic!("Unknown FFI return code: {}", code))
                .check_consistent(),
            callback,
        )
    }

    pub fn execute_block(
        &self,
        execute_block_callback: ExecuteBlockCallback,
    ) -> ConsensusFfiResponse {
        let consensus = self.consensus.load(Ordering::SeqCst);
        let result = unsafe { executeBlock(consensus, execute_block_callback) };
        ConsensusFfiResponse::try_from(result)
            .unwrap_or_else(|code| panic!("Unknown FFI return code: {}", code))
            .check_consistent()
    }

    pub fn send_finalization(&self, genesis_index: u32, msg: &[u8]) -> ConsensusFfiResponse {
        wrap_send_data_to_c!(self, genesis_index, msg, receiveFinalizationMessage)
            .check_consistent()
    }

    pub fn send_finalization_record(&self, genesis_index: u32, rec: &[u8]) -> ConsensusFfiResponse {
        wrap_send_data_to_c!(self, genesis_index, rec, receiveFinalizationRecord).check_consistent()
    }

    /// Send a transaction to consensus. Return whether the operation succeeded
    /// or not, and if the transaction is accepted by consensus then its
    /// hash is returned.
    pub fn send_transaction(&self, data: &[u8]) -> (Option<TransactionHash>, ConsensusFfiResponse) {
        let consensus = self.consensus.load(Ordering::SeqCst);
        let len = data.len();
        let mut out_hash = [0u8; 32];
        let result = unsafe {
            receiveTransaction(consensus, data.as_ptr(), len as i64, out_hash.as_mut_ptr())
        };

        let return_code = ConsensusFfiResponse::try_from(result)
            .unwrap_or_else(|code| panic!("Unknown FFI return code: {}", code))
            .check_consistent();
        if return_code == ConsensusFfiResponse::Success {
            (Some(out_hash.into()), return_code)
        } else {
            (None, return_code)
        }
    }

    pub fn get_last_finalized_block_height(&self) -> u64 {
        let consensus = self.consensus.load(Ordering::SeqCst);
        unsafe { getLastFinalizedBlockHeight(consensus) }
    }

    /// Get the total number of non-finalized transactions across all accounts.
    pub fn number_of_non_finalized_transactions(&self) -> u64 {
        let consensus = self.consensus.load(Ordering::SeqCst);
        unsafe { getNumberOfNonFinalizedTransactions(consensus) }
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
        let consensus = self.consensus.load(Ordering::SeqCst);
        let result = unsafe {
            receiveCatchUpStatus(
                consensus,
                peer_id.into(),
                genesis_index,
                request.as_ptr(),
                request.len() as i64,
                object_limit,
                direct_callback,
            )
        };

        ConsensusFfiResponse::try_from(result)
            .unwrap_or_else(|code| panic!("Unknown FFI return code: {}", code))
            .check_consistent()
    }

    /// Gets baker status of the node along with the baker ID
    /// registered in the baker credentials used and lottery power, if
    /// available.
    ///
    /// Note: the return type does not use an Option<u64>, which would be more
    /// natural, because a weird issue on Windows caused node_info to
    /// produce the wrong result.
    pub fn in_baking_committee(&self) -> (ConsensusIsInBakingCommitteeResponse, bool, u64, f64) {
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut baker_id: u64 = 0;
        let mut has_baker_id: u8 = 0;
        let mut baker_lottery_power: f64 = 0.0;

        let result = unsafe {
            bakerStatusBestBlock(
                consensus,
                &mut baker_id,
                &mut has_baker_id,
                &mut baker_lottery_power,
            )
        };

        let status = ConsensusIsInBakingCommitteeResponse::try_from(result).unwrap_or_else(|err| {
            unreachable!("An error occured when trying to convert FFI return code: {}", err)
        });

        (status, has_baker_id != 0, baker_id, baker_lottery_power)
    }

    pub fn in_finalization_committee(&self) -> bool {
        wrap_c_bool_call!(self, checkIfWeAreFinalizer)
    }

    /// Checks if consensus is running, i.e. if consensus has been shut down,
    /// this will return false.
    pub fn is_consensus_running(&self) -> bool { wrap_c_bool_call!(self, checkIfRunning) }

    /// Import blocks from the given file path. If the file exists and the node
    /// could import all blocks from the file `Ok(())` is returned. Otherwise an
    /// error is returned.
    pub fn import_blocks(&self, import_file_path: &Path) -> anyhow::Result<()> {
        let consensus = self.consensus.load(Ordering::SeqCst);

        let path_bytes =
            import_file_path.as_os_str().to_str().context("Cannot decode path.")?.as_bytes();

        let len = path_bytes.len();

        let response = unsafe { importBlocks(consensus, path_bytes.as_ptr(), len as i64) };
        match ConsensusFfiResponse::try_from(response)?.check_consistent() {
            ConsensusFfiResponse::Success => Ok(()),
            other => bail!("Error during block import: {}", other),
        }
    }

    pub fn stop_importing_blocks(&self) {
        let consensus = self.consensus.load(Ordering::SeqCst);

        unsafe { stopImportingBlocks(consensus) }
    }

    /// Look up the account in the given block.
    /// The return value is a pair of the block hash which was used for the
    /// query, and the protobuf serialized response.
    ///
    /// If the account cannot be found then a [tonic::Status::not_found] is
    /// returned.
    pub fn get_account_info_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
        account_identifier: &crate::grpc2::types::AccountIdentifierInput,
    ) -> Result<([u8; 32], Vec<u8>), tonic::Status> {
        use crate::grpc2::Require;
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(block_hash).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let (acc_type, acc_id) =
            crate::grpc2::types::account_identifier_to_ffi(account_identifier).require()?;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let mut out_hash = [0u8; 32];
        let response: ConsensusQueryResponse = unsafe {
            getAccountInfoV2(
                consensus,
                block_id_type,
                block_hash.as_ptr(),
                acc_type,
                acc_id,
                out_hash.as_mut_ptr(),
                &mut out_data,
                copy_to_vec_callback,
            )
            .try_into()?
        };
        response.ensure_ok("account or block")?;
        Ok((out_hash, out_data))
    }

    /// Get the best guess as to what the next account sequence number should
    /// be. If all account transactions are finalized, then this information
    /// is reliable. Otherwise, this is the best guess, assuming all other
    /// transactions will be committed to blocks and eventually finalized.
    pub fn get_next_account_sequence_number_v2(
        &self,
        account_address: &crate::grpc2::types::AccountAddress,
    ) -> Result<Vec<u8>, tonic::Status> {
        use crate::grpc2::Require;
        let account_address_ptr =
            crate::grpc2::types::account_address_to_ffi(account_address).require()?;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let _response: ConsensusQueryResponse = unsafe {
            getNextAccountSequenceNumberV2(
                consensus,
                account_address_ptr,
                &mut out_data,
                copy_to_vec_callback,
            )
            .try_into()?
        };
        // The query should always return successfully, so no need to check here.
        Ok(out_data)
    }

    /// Get information of the current state of consensus.
    pub fn get_consensus_info_v2(&self) -> Result<Vec<u8>, tonic::Status> {
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let _response: ConsensusQueryResponse = unsafe {
            getConsensusInfoV2(consensus, &mut out_data, copy_to_vec_callback).try_into()?
        };
        // The query should always return successfully, so no need to check here.
        Ok(out_data)
    }

    pub fn get_consensus_detailed_status_v2(
        &self,
        genesis_index: Option<u32>,
    ) -> Result<Vec<u8>, tonic::Status> {
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let (use_genesis_index, genesis_index) = match genesis_index {
            Some(genesis_index) => (1, genesis_index),
            None => (0, 0),
        };
        let response: ConsensusQueryResponse = unsafe {
            getConsensusDetailedStatusV2(
                consensus,
                use_genesis_index,
                genesis_index,
                &mut out_data,
                copy_to_vec_callback,
            )
            .try_into()?
        };
        response.ensure_ok("genesis index")?;
        Ok(out_data)
    }

    /// Get the cryptographic parameters in a given block.
    pub fn get_cryptographic_parameters_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
    ) -> Result<([u8; 32], crate::grpc2::types::CryptographicParameters), tonic::Status> {
        use crate::grpc2::Require;
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(block_hash).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_hash = [0u8; 32];
        let mut crypto_parameter_ptr: Option<CryptographicParameters> = None;
        let response: ConsensusQueryResponse = unsafe {
            getCryptographicParametersV2(
                consensus,
                block_id_type,
                block_hash.as_ptr(),
                out_hash.as_mut_ptr(),
                &mut crypto_parameter_ptr,
                copy_cryptographic_parameters_callback,
            )
            .try_into()?
        };
        response.ensure_ok("block")?;
        let crypto_parameters = crypto_parameter_ptr
            .ok_or_else(|| tonic::Status::internal("Failed to access cryptographic parameters"))?;

        let out = crate::grpc2::types::CryptographicParameters {
            genesis_string:          crypto_parameters.genesis_string.clone(),
            bulletproof_generators:  concordium_base::common::to_bytes(
                crypto_parameters.bulletproof_generators(),
            ),
            on_chain_commitment_key: concordium_base::common::to_bytes(
                &crypto_parameters.on_chain_commitment_key,
            ),
        };
        Ok((out_hash, out))
    }

    /// Look up accounts in the given block, and return a stream of their
    /// addresses.
    ///
    /// The return value is a block hash used for the query. If the requested
    /// block does not exist a [tonic::Status::not_found] is returned.
    pub fn get_account_list_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(block_hash).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let sender_ptr = Box::into_raw(sender);
        let response: ConsensusQueryResponse = unsafe {
            getAccountListV2(
                consensus,
                sender_ptr,
                block_id_type,
                block_hash.as_ptr(),
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        if let Err(e) = response.ensure_ok("block") {
            let _ = unsafe { Box::from_raw(sender_ptr) }; // deallocate sender since it is unused by Haskell.
            Err(e)
        } else {
            Ok(buf)
        }
    }

    /// Look up tokens in the given block, and return a stream of their
    /// token ids.
    ///
    /// The return value is a block hash used for the query. If the requested
    /// block does not exist a [tonic::Status::not_found] is returned.
    pub fn get_token_list_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(block_hash).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let sender_ptr = Box::into_raw(sender);
        let response: ConsensusQueryResponse = unsafe {
            getTokenListV2(
                consensus,
                sender_ptr,
                block_id_type,
                block_hash.as_ptr(),
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        if let Err(e) = response.ensure_ok("block") {
            let _ = unsafe { Box::from_raw(sender_ptr) }; // deallocate sender since it is unused by Haskell.
            Err(e)
        } else {
            Ok(buf)
        }
    }

    /// Get a list of all smart contract modules. The stream will end
    /// when all modules that exist in the state at the end of the given
    /// block have been returned.
    pub fn get_module_list_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;

        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(block_hash).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getModuleListV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash.as_ptr(),
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok(buf)
    }

    /// Get the source of a smart contract module.
    pub fn get_module_source_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
        module_ref: &crate::grpc2::types::ModuleRef,
    ) -> Result<([u8; 32], Vec<u8>), tonic::Status> {
        use crate::grpc2::Require;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let mut out_hash = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(block_hash).require()?;
        let (block_id_type, block_id) = bhi.to_ptr();
        let module_ref_ptr = crate::grpc2::types::module_reference_to_ffi(module_ref).require()?;
        let response: ConsensusQueryResponse = unsafe {
            getModuleSourceV2(
                consensus,
                block_id_type,
                block_id.as_ptr(),
                module_ref_ptr,
                out_hash.as_mut_ptr(),
                &mut out_data,
                copy_to_vec_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("module or block")?;
        Ok((out_hash, out_data))
    }

    /// Get a list of addresses for all smart contract instances. The stream
    /// will end when all instances that exist in the state at the end of the
    /// given block has been returned.
    pub fn get_instance_list_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(block_hash).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getInstanceListV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash.as_ptr(),
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok(buf)
    }

    /// Get information about a specific smart contract instance.
    pub fn get_instance_info_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
        address: &crate::grpc2::types::ContractAddress,
    ) -> Result<([u8; 32], Vec<u8>), tonic::Status> {
        use crate::grpc2::Require;
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(block_hash).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let addr_index = address.index;
        let addr_subindex = address.subindex;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let mut out_hash = [0u8; 32];
        let response: ConsensusQueryResponse = unsafe {
            getInstanceInfoV2(
                consensus,
                block_id_type,
                block_hash.as_ptr(),
                addr_index,
                addr_subindex,
                out_hash.as_mut_ptr(),
                &mut out_data,
                copy_to_vec_callback,
            )
            .try_into()?
        };
        response.ensure_ok("block or instance")?;
        Ok((out_hash, out_data))
    }

    /// Get the entire smart contract state of the specified instance.
    pub fn get_instance_state_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
        address: &crate::grpc2::types::ContractAddress,
    ) -> Result<([u8; 32], ContractStateResponse), tonic::Status> {
        use crate::grpc2::Require;
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(block_hash).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let addr_index = address.index;
        let addr_subindex = address.subindex;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_v0_data: Vec<u8> = Vec::new();
        let mut out_v1_data = None;
        let mut out_hash = [0u8; 32];
        let response: ConsensusQueryResponse = unsafe {
            getInstanceStateV2(
                consensus,
                block_id_type,
                block_hash.as_ptr(),
                addr_index,
                addr_subindex,
                out_hash.as_mut_ptr(),
                &mut out_v0_data,
                copy_to_vec_callback,
                &mut out_v1_data,
                copy_v1_contract_state_callback,
            )
            .try_into()?
        };
        response.ensure_ok("block or instance")?;
        match out_v1_data {
            None => Ok((out_hash, ContractStateResponse::V0 {
                state: out_v0_data,
            })),
            Some(data) => Ok((out_hash, ContractStateResponse::V1 {
                state:  data.state,
                loader: data.loader,
            })),
        }
    }

    /// Get ancestors for the provided block.
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
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(block_hash).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getAncestorsV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash.as_ptr(),
                amount,
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok(buf)
    }

    pub fn get_first_block_epoch_v2(
        &self,
        query: &crate::grpc2::types::EpochRequest,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let epoch_request = crate::grpc2::types::epoch_request_to_ffi(query).require()?;
        let (query_tag, query_data) = epoch_request.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getFirstBlockEpochV2(consensus, query_tag, query_data.as_ptr(), buf.as_mut_ptr())
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok(buf)
    }

    /// Get the winning bakers for a particular epoch.
    pub fn get_winning_bakers_epoch_v2(
        &self,
        query: &crate::grpc2::types::EpochRequest,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<(), tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let epoch_request = crate::grpc2::types::epoch_request_to_ffi(query).require()?;
        let (query_tag, query_data) = epoch_request.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getWinningBakersEpochV2(
                consensus,
                Box::into_raw(sender),
                query_tag,
                query_data.as_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok(())
    }

    /// Get information about a specific transaction.
    pub fn get_block_item_status_v2(
        &self,
        transaction_hash: &crate::grpc2::types::TransactionHash,
    ) -> Result<Vec<u8>, tonic::Status> {
        use crate::grpc2::Require;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let transaction_hash_ptr =
            crate::grpc2::types::transaction_hash_to_ffi(transaction_hash).require()?;
        let response: ConsensusQueryResponse = unsafe {
            getBlockItemStatusV2(
                consensus,
                transaction_hash_ptr,
                &mut out_data,
                copy_to_vec_callback,
            )
            .try_into()?
        };
        response.ensure_ok("transaction")?;
        Ok(out_data)
    }

    /// Run the smart contract entrypoint in a given context and in the state at
    /// the end of the given block.Get status of the tokenomics at the end of a
    /// given block.
    pub fn invoke_instance_v2(
        &self,
        request: &crate::grpc2::types::InvokeInstanceRequest,
    ) -> Result<([u8; 32], Vec<u8>), tonic::Status> {
        use crate::grpc2::Require;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let mut out_hash = [0u8; 32];
        let bhi =
            crate::grpc2::types::block_hash_input_to_ffi(request.block_hash.as_ref().require()?)
                .require()?;
        let (block_id_type, block_id) = bhi.to_ptr();

        // Optional Address to ffi
        let (
            invoker_address_type,
            invoker_account_address_ptr,
            invoker_contract_index,
            invoker_contract_subindex,
        ) = if let Some(address) = &request.invoker {
            match address.r#type.as_ref().require()? {
                crate::grpc2::types::address::Type::Account(account) => {
                    (1, crate::grpc2::types::account_address_to_ffi(account).require()?, 0, 0)
                }
                crate::grpc2::types::address::Type::Contract(contract) => {
                    (2, std::ptr::null(), contract.index, contract.subindex)
                }
            }
        } else {
            (0, std::ptr::null(), 0, 0)
        };

        let amount = request.amount.as_ref().require()?.value;

        let (receive_name_ptr, receive_name_len) =
            crate::grpc2::types::receive_name_to_ffi(request.entrypoint.as_ref().require()?)
                .require()?;

        // Parameter to ffi
        let (parameter_ptr, parameter_len) = {
            let bytes = &request.parameter.as_ref().require()?.value;
            (
                bytes.as_ptr(),
                bytes.len().try_into().map_err(|_| {
                    tonic::Status::invalid_argument("Parameter exceeds maximum supported size.")
                })?,
            )
        };

        let energy = request.energy.as_ref().require()?.value;

        let contract = request.instance.as_ref().require()?;

        let response: ConsensusQueryResponse = unsafe {
            invokeInstanceV2(
                consensus,
                block_id_type,
                block_id.as_ptr(),
                contract.index,
                contract.subindex,
                invoker_address_type,
                invoker_account_address_ptr,
                invoker_contract_index,
                invoker_contract_subindex,
                amount,
                receive_name_ptr,
                receive_name_len,
                parameter_ptr,
                parameter_len,
                energy,
                out_hash.as_mut_ptr(),
                &mut out_data,
                copy_to_vec_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block or contract")?;
        Ok((out_hash, out_data))
    }

    /// Get information, such as height, timings, and transaction counts for the
    /// given block.
    pub fn get_block_info_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
    ) -> Result<([u8; 32], Vec<u8>), tonic::Status> {
        use crate::grpc2::Require;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let mut out_hash = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(block_hash).require()?;
        let (block_id_type, block_id) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getBlockInfoV2(
                consensus,
                block_id_type,
                block_id.as_ptr(),
                out_hash.as_mut_ptr(),
                &mut out_data,
                copy_to_vec_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok((out_hash, out_data))
    }

    /// Get a list bakers at the end of a given block. The stream will end when
    /// all bakers has been returned.
    pub fn get_baker_list_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(block_hash).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getBakerListV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash.as_ptr(),
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok(buf)
    }

    /// Get status information about a given pool at the end of a given block.
    pub fn get_pool_info_v2(
        &self,
        request: &crate::grpc2::types::PoolInfoRequest,
    ) -> Result<([u8; 32], Vec<u8>), tonic::Status> {
        use crate::grpc2::Require;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let mut out_hash = [0u8; 32];
        let bhi =
            crate::grpc2::types::block_hash_input_to_ffi(request.block_hash.as_ref().require()?)
                .require()?;

        let (block_id_type, block_id) = bhi.to_ptr();
        let baker_id = crate::grpc2::types::baker_id_to_ffi(request.baker.as_ref().require()?);

        let response: ConsensusQueryResponse = unsafe {
            getPoolInfoV2(
                consensus,
                block_id_type,
                block_id.as_ptr(),
                baker_id,
                out_hash.as_mut_ptr(),
                &mut out_data,
                copy_to_vec_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block or baker")?;
        Ok((out_hash, out_data))
    }

    /// Get status information about the passive delegators at the end of a
    /// given block.
    pub fn get_passive_delegation_info_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
    ) -> Result<([u8; 32], Vec<u8>), tonic::Status> {
        use crate::grpc2::Require;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let mut out_hash = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(block_hash).require()?;
        let (block_id_type, block_id) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getPassiveDelegationInfoV2(
                consensus,
                block_id_type,
                block_id.as_ptr(),
                out_hash.as_mut_ptr(),
                &mut out_data,
                copy_to_vec_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok((out_hash, out_data))
    }

    /// Get a stream of live blocks at a given height.
    pub fn get_blocks_at_height_v2(
        &self,
        height: &crate::grpc2::types::BlocksAtHeightRequest,
    ) -> Result<Vec<u8>, tonic::Status> {
        use crate::grpc2::Require;
        let consensus = self.consensus.load(Ordering::SeqCst);

        let (block_height, genesis_index, restrict) =
            crate::grpc2::types::blocks_at_height_request_to_ffi(height).require()?;

        let mut out_data: Vec<u8> = Vec::new();
        let _response: ConsensusQueryResponse = unsafe {
            getBlocksAtHeightV2(
                consensus,
                block_height,
                genesis_index,
                restrict,
                &mut out_data,
                copy_to_vec_callback,
            )
        }
        .try_into()?;
        // The query should always return successfully, so no need to check here.
        Ok(out_data)
    }

    /// Get status of the tokenomics at the end of a given block.
    pub fn get_tokenomics_info_v2(
        &self,
        block_hash: &crate::grpc2::types::BlockHashInput,
    ) -> Result<([u8; 32], Vec<u8>), tonic::Status> {
        use crate::grpc2::Require;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let mut out_hash = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(block_hash).require()?;
        let (block_id_type, block_id) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getTokenomicsInfoV2(
                consensus,
                block_id_type,
                block_id.as_ptr(),
                out_hash.as_mut_ptr(),
                &mut out_data,
                copy_to_vec_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok((out_hash, out_data))
    }

    /// Get the pool delegators of a given pool at the end of a given block.
    /// The stream will end when all the delegators have been returned.
    pub fn get_pool_delegators_v2(
        &self,
        request: &crate::grpc2::types::GetPoolDelegatorsRequest,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let bhi =
            crate::grpc2::types::block_hash_input_to_ffi(request.block_hash.as_ref().require()?)
                .require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let baker_id = crate::grpc2::types::baker_id_to_ffi(request.baker.as_ref().require()?);
        let response: ConsensusQueryResponse = unsafe {
            getPoolDelegatorsV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash.as_ptr(),
                baker_id,
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block or pool")?;
        Ok(buf)
    }

    /// Get the reward period pool delegators of a given pool at the end of a
    /// given block. The stream will end when all the delegators have been
    /// returned.
    pub fn get_pool_delegators_reward_period_v2(
        &self,
        request: &crate::grpc2::types::GetPoolDelegatorsRequest,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let bhi =
            crate::grpc2::types::block_hash_input_to_ffi(request.block_hash.as_ref().require()?)
                .require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let baker_id = crate::grpc2::types::baker_id_to_ffi(request.baker.as_ref().require()?);
        let response: ConsensusQueryResponse = unsafe {
            getPoolDelegatorsRewardPeriodV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash.as_ptr(),
                baker_id,
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block or pool")?;
        Ok(buf)
    }

    /// Get the passive delegators at the end of a given block.
    /// The stream will end when all the delegators have been returned.
    pub fn get_passive_delegators_v2(
        &self,
        request: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(request).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getPassiveDelegatorsV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash.as_ptr(),
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok(buf)
    }

    /// Get the reward period pool delegators of a given pool at the end of a
    /// given block. The stream will end when all the delegators have been
    /// returned.
    pub fn get_passive_delegators_reward_period_v2(
        &self,
        request: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(request).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getPassiveDelegatorsRewardPeriodV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash.as_ptr(),
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok(buf)
    }

    /// Get the current branches of blocks starting and including from the last
    /// finalized block.
    pub fn get_branches_v2(&self) -> Result<Vec<u8>, tonic::Status> {
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let _response: ConsensusQueryResponse =
            unsafe { getBranchesV2(consensus, &mut out_data, copy_to_vec_callback) }.try_into()?;
        Ok(out_data)
    }

    /// Get information related to the baker election for a particular block.
    pub fn get_election_info_v2(
        &self,
        request: &crate::grpc2::types::BlockHashInput,
    ) -> Result<([u8; 32], Vec<u8>), tonic::Status> {
        use crate::grpc2::Require;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let mut out_hash = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(request).require()?;
        let (block_id_type, block_id) = bhi.to_ptr();

        let response: ConsensusQueryResponse = unsafe {
            getElectionInfoV2(
                consensus,
                block_id_type,
                block_id.as_ptr(),
                out_hash.as_mut_ptr(),
                &mut out_data,
                copy_to_vec_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok((out_hash, out_data))
    }

    /// Get the identity providers registered as of the end of a given block.
    /// The stream will end when all the identity providers have been returned.
    pub fn get_identity_providers_v2(
        &self,
        request: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(request).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();

        let response: ConsensusQueryResponse = unsafe {
            getIdentityProvidersV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash.as_ptr(),
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok(buf)
    }

    /// Get the anonymity revokers registered as of the end of a given block.
    /// The stream will end when all the anonymity revokers have been returned.
    pub fn get_anonymity_revokers_v2(
        &self,
        request: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(request).require()?;

        let (block_id_type, block_hash) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getAnonymityRevokersV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash.as_ptr(),
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok(buf)
    }

    /// Get a list of non-finalized transaction hashes for a given account.
    /// The stream will end when all the non-finalized transaction hashes have
    /// been returned.
    pub fn get_account_non_finalized_transactions_v2(
        &self,
        request: &crate::grpc2::types::AccountAddress,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<(), tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let account_address_ptr = crate::grpc2::types::account_address_to_ffi(request).require()?;
        let response: ConsensusQueryResponse = unsafe {
            getAccountNonFinalizedTransactionsV2(
                consensus,
                Box::into_raw(sender),
                account_address_ptr,
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("account address")?;
        Ok(())
    }

    /// Get a list of block items in a block specified by a block hash.
    pub fn get_block_items_v2(
        &self,
        request: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let sender = Box::new(sender);
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(request).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let mut buf = [0u8; 32];
        let response: ConsensusQueryResponse = unsafe {
            getBlockItemsV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash.as_ptr(),
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok(buf)
    }

    /// Get a list of transaction events in a given block.
    /// The stream will end when all the transaction events for a given block
    /// have been returned.
    pub fn get_block_transaction_events_v2(
        &self,
        request: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(request).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getBlockTransactionEventsV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash.as_ptr(),
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok(buf)
    }

    /// Get a list of special events in a given block.
    /// The stream will end when all the special events for a given block have
    /// been returned.
    pub fn get_block_special_events_v2(
        &self,
        request: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(request).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getBlockSpecialEventsV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash.as_ptr(),
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok(buf)
    }

    /// Get the pending updates to chain parameters at the end of a given block.
    /// The stream will end when all the pending updates for a given block have
    /// been returned.
    pub fn get_block_pending_updates_v2(
        &self,
        request: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(request).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getBlockPendingUpdatesV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash.as_ptr(),
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok(buf)
    }

    /// Get next available sequence numbers for updating chain parameters after
    /// a given block.
    pub fn get_next_update_sequence_numbers_v2(
        &self,
        request: &crate::grpc2::types::BlockHashInput,
    ) -> Result<([u8; 32], Vec<u8>), tonic::Status> {
        use crate::grpc2::Require;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let mut out_hash = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(request).require()?;
        let (block_id_type, block_id) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getNextUpdateSequenceNumbersV2(
                consensus,
                block_id_type,
                block_id.as_ptr(),
                out_hash.as_mut_ptr(),
                &mut out_data,
                copy_to_vec_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok((out_hash, out_data))
    }

    /// Get accounts with scheduled releases at the end of a given block.
    /// The stream will end when all accounts with scheduled releases have been
    /// returned.
    pub fn get_scheduled_release_accounts_v2(
        &self,
        request: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(request).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getScheduledReleaseAccountsV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash.as_ptr(),
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok(buf)
    }

    /// Get accounts in cooldown at the end of a given block.
    /// The stream will end when all accounts in cooldown have been returned.
    pub fn get_cooldown_accounts_v2(
        &self,
        request: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(request).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getCooldownAccountsV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash.as_ptr(),
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok(buf)
    }

    /// Get accounts in pre-cooldown at the end of a given block.
    /// The stream will end when all accounts in pre-cooldown have been
    /// returned.
    pub fn get_pre_cooldown_accounts_v2(
        &self,
        request: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(request).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getPreCooldownAccountsV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash.as_ptr(),
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok(buf)
    }

    /// Get accounts in pre-pre-cooldown at the end of a given block.
    /// The stream will end when all accounts in pre-pre-cooldown have been
    /// returned.
    pub fn get_pre_pre_cooldown_accounts_v2(
        &self,
        request: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut buf = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(request).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getPrePreCooldownAccountsV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash.as_ptr(),
                buf.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok(buf)
    }

    /// Get chain parameters for the given block.
    pub fn get_block_chain_parameters_v2(
        &self,
        request: &crate::grpc2::types::BlockHashInput,
    ) -> Result<([u8; 32], Vec<u8>), tonic::Status> {
        use crate::grpc2::Require;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let mut out_hash = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(request).require()?;
        let (block_id_type, block_id) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getBlockChainParametersV2(
                consensus,
                block_id_type,
                block_id.as_ptr(),
                out_hash.as_mut_ptr(),
                &mut out_data,
                copy_to_vec_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok((out_hash, out_data))
    }

    /// Get chain parameters for the given block.
    pub fn get_block_finalization_summary_v2(
        &self,
        request: &crate::grpc2::types::BlockHashInput,
    ) -> Result<([u8; 32], Vec<u8>), tonic::Status> {
        use crate::grpc2::Require;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let mut out_hash = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(request).require()?;
        let (block_id_type, block_id) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getBlockFinalizationSummaryV2(
                consensus,
                block_id_type,
                block_id.as_ptr(),
                out_hash.as_mut_ptr(),
                &mut out_data,
                copy_to_vec_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok((out_hash, out_data))
    }

    /// Get the slot time (in milliseconds) of the last finalized block.
    pub fn get_last_finalized_block_slot_time_v2(
        &self,
    ) -> concordium_base::common::types::Timestamp {
        let consensus = self.consensus.load(Ordering::SeqCst);
        let millis = unsafe { getLastFinalizedBlockSlotTimeV2(consensus) };
        millis.into()
    }

    /// Get the bakers for the reward period of the block.
    /// The stream ends when all bakers have been returned.
    pub fn get_bakers_reward_period_v2(
        &self,
        request: &crate::grpc2::types::BlockHashInput,
        sender: futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    ) -> Result<[u8; 32], tonic::Status> {
        use crate::grpc2::Require;
        let sender = Box::new(sender);
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_hash = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(request).require()?;
        let (block_id_type, block_hash) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getBakersRewardPeriodV2(
                consensus,
                Box::into_raw(sender),
                block_id_type,
                block_hash.as_ptr(),
                out_hash.as_mut_ptr(),
                enqueue_bytearray_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok(out_hash)
    }

    /// Get the certificates for a block.
    pub fn get_block_certificates_v2(
        &self,
        request: &crate::grpc2::types::BlockHashInput,
    ) -> Result<([u8; 32], Vec<u8>), tonic::Status> {
        use crate::grpc2::Require;
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let mut out_hash = [0u8; 32];
        let bhi = crate::grpc2::types::block_hash_input_to_ffi(request).require()?;
        let (block_id_type, block_id) = bhi.to_ptr();
        let response: ConsensusQueryResponse = unsafe {
            getBlockCertificatesV2(
                consensus,
                block_id_type,
                block_id.as_ptr(),
                out_hash.as_mut_ptr(),
                &mut out_data,
                copy_to_vec_callback,
            )
        }
        .try_into()?;
        response.ensure_ok("block")?;
        Ok((out_hash, out_data))
    }

    pub fn get_baker_earliest_win_time_v2(
        &self,
        request: &crate::grpc2::types::BakerId,
    ) -> Result<Vec<u8>, tonic::Status> {
        let consensus = self.consensus.load(Ordering::SeqCst);
        let mut out_data: Vec<u8> = Vec::new();
        let response: ConsensusQueryResponse = unsafe {
            getBakerEarliestWinTimeV2(consensus, request.value, &mut out_data, copy_to_vec_callback)
        }
        .try_into()?;
        response.ensure_ok("baker")?;
        Ok(out_data)
    }

    /// Start a dry-run operation sequence.
    pub fn dry_run(&self, energy_quota: u64) -> DryRun {
        let consensus = self.consensus.load(Ordering::SeqCst);
        let handle = unsafe { dryRunStart(consensus, copy_to_vec_callback, energy_quota) };
        DryRun {
            handle,
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
/// If the msg is a null pointer and length is not 0 then the `sender` is always
/// dropped, and the response will be `-2`.
extern "C" fn enqueue_bytearray_callback(
    sender: *mut futures::channel::mpsc::Sender<Result<Vec<u8>, tonic::Status>>,
    msg: *const u8,
    msg_length: i64,
) -> i32 {
    let mut sender = unsafe { Box::from_raw(sender) };
    let data = if msg_length != 0 {
        if msg.is_null() {
            // drop sender
            return -2;
        } else {
            unsafe { slice::from_raw_parts(msg, msg_length as usize) }.to_vec()
        }
    } else {
        Vec::new()
    };
    match sender.try_send(Ok(data.to_vec())) {
        Ok(()) => {
            // Do not drop the sender.
            let _ = Box::into_raw(sender);
            0
        }
        Err(e) if e.is_full() => {
            // Do not drop the sender, we will enqueue more things.
            let _ = Box::into_raw(sender);
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

/// Copy cryptographic parameters to the target location provided by the
/// `source` pointer.
extern "C" fn copy_cryptographic_parameters_callback(
    target: *mut Option<CryptographicParameters>,
    source: *const CryptographicParameters,
) {
    unsafe { *target = source.as_ref().cloned() };
}

/// Store the V1 contract state and context to the given structure.
extern "C" fn copy_v1_contract_state_callback(
    out: *mut Option<V1ContractStateReceiver>,
    state: *mut concordium_smart_contract_engine::v1::trie::PersistentState,
    loader: concordium_smart_contract_engine::v1::trie::LoadCallback,
) {
    let out = unsafe { &mut *out };
    let v = V1ContractStateReceiver {
        state: unsafe { &*state }.clone(),
        loader,
    };
    *out = Some(v);
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
            BlockHash::try_from(std::slice::from_raw_parts(block_hash, 32))
                .expect("The slice is exactly 32 bytes so ::try_from must succeed."),
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
            13 => "Konsensus",
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
