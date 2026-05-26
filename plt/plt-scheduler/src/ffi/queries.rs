//! This module provides a C ABI for the Rust PLT scheduler functions.
//!
//! It is only available if the `ffi` feature is enabled.

use crate::ffi::status;
use crate::queries;
use crate::queries::{QueryLockError, QueryTokenInfoError};
use concordium_base::base::AccountIndex;
use concordium_base::common;
use concordium_base::protocol_level_locks::LockId;
use libc::size_t;
use plt_block_state::block_state::{
    ExecutionTimeBlockStateP9, ExecutionTimeBlockStateP10, ExecutionTimeBlockStateP11,
};
use plt_block_state::entity::accounts::Account;
use plt_block_state::entity::block_state::p9::BlockStateP9;
use plt_block_state::entity::block_state::p10::BlockStateP10;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::ffi::blob_store_callbacks::LoadCallback;
use plt_block_state::ffi::block_state_callbacks::{
    ExternalBlockStateQueryCallbacks, GetAccountIndexByAddressCallback,
    GetCanonicalAddressByAccountIndexCallback, GetTokenAccountStatesCallback,
    ReadTokenAccountBalanceCallback,
};
use plt_block_state::ffi::memory;
use plt_block_state::persistent::block_state::PersistentBlockState;

/// Context with no external block state (will panic if accessed).
#[derive(Debug)]
pub struct FfiQueryBlockStateTypes;

impl EntityContextTypes for FfiQueryBlockStateTypes {
    type ExternalBlockState = ExternalBlockStateQueryCallbacks;
    type Loader = LoadCallback;
}

pub type FfiQueryEntityContext = EntityContext<FfiQueryBlockStateTypes>;

/// C-binding for calling [`queries::query_plt_list`].
///
/// Returns a byte representing the result:
///
/// - [`status::FfiStatusCode::Success`]: Query succeeded
/// - [`status::FfiStatusCode::Panic`]: Execution of the query resulted in an unrecoverable error or panic.
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `read_token_account_balance_callback` External function to call reading the token balance of an account.
/// - `get_account_address_by_index_callback` External function for getting account canonical address by account index.
/// - `get_account_index_by_address_callback` External function for getting account index by account address.
/// - `get_token_account_states_callback` External function for getting token account states.
/// - `block_state` Shared pointer to a block state to use for queries.
/// - `return_data_out` Location for writing pointer to array containing return data, which is serialized tokens ids.
///   If the return value is [`status::FfiStatusCode::Success`], the data is a list of token ids.
///   The pointer written is to a uniquely owned array.
///   The caller must free the written array using `free_array_len_2` when it is no longer used.
/// - `return_data_len_out` Location for writing the length of the array whose pointer was written to `return_data_out`.
///
/// # Safety
///
/// - All callback arguments must be a valid function pointers to functions with a signature matching the
///   signature of Rust type of the function pointer.
/// - Argument `block_state` must be a non-null pointer to well-formed [`crate::block_state::BlockState`].
///   The pointer is to a shared instance, hence only valid for reading (writing only allowed through interior mutability).
/// - Argument `return_data_out` must be a non-null and valid pointer for writing
/// - Argument `return_data_len_out` must be a non-null and valid pointer for writing
#[unsafe(no_mangle)]
extern "C" fn ffi_query_plt_list(
    load_callback: LoadCallback,
    read_token_account_balance_callback: ReadTokenAccountBalanceCallback,
    get_account_index_by_address_callback: GetAccountIndexByAddressCallback,
    get_account_address_by_index_callback: GetCanonicalAddressByAccountIndexCallback,
    get_token_account_states_callback: GetTokenAccountStatesCallback,
    block_state: *const PersistentBlockState,
    return_data_out: *mut *mut u8,
    return_data_len_out: *mut size_t,
) -> status::FfiStatusCode {
    let (return_status, return_data) = status::catch_unwind(|| {
        assert!(!block_state.is_null(), "block_state is a null pointer.");
        assert!(
            !return_data_len_out.is_null(),
            "return_data_len_out is a null pointer."
        );
        assert!(
            !return_data_out.is_null(),
            "return_data_out is a null pointer."
        );
        let external = ExternalBlockStateQueryCallbacks {
            read_token_account_balance_ptr: read_token_account_balance_callback,
            get_account_address_by_index_ptr: get_account_address_by_index_callback,
            get_account_index_by_address_ptr: get_account_index_by_address_callback,
            get_token_account_states_ptr: get_token_account_states_callback,
        };
        let context = FfiQueryEntityContext {
            external,
            loader: load_callback,
        };
        let token_ids = match unsafe { &*block_state } {
            PersistentBlockState::P9(persistent) => {
                let block_state = BlockStateP9 {
                    persistent: persistent.clone(),
                };
                let exec_block_state = ExecutionTimeBlockStateP9 {
                    block_state,
                    context,
                };
                queries::query_plt_list(&exec_block_state)
            }
            PersistentBlockState::P10(persistent) => {
                let block_state = BlockStateP10 {
                    persistent: persistent.clone(),
                };
                let exec_block_state = ExecutionTimeBlockStateP10 {
                    block_state,
                    context,
                };
                queries::query_plt_list(&exec_block_state)
            }
            PersistentBlockState::P11(persistent) => {
                let block_state = BlockStateP11 {
                    persistent: persistent.clone(),
                };
                let exec_block_state = ExecutionTimeBlockStateP11 {
                    block_state,
                    context,
                };
                queries::query_plt_list(&exec_block_state)
            }
        };
        let return_data = common::to_bytes(&token_ids);
        (status::FfiStatusCode::Success, return_data)
    });
    let array = memory::alloc_array_from_vec(return_data);
    unsafe {
        *return_data_len_out = array.length;
        *return_data_out = array.array;
    }
    return_status
}

/// C-binding for calling [`queries::query_token_info`].
///
/// Returns a byte representing the result:
///
/// - [`status::FfiStatusCode::Success`]: Query succeeded
/// - [`status::FfiStatusCode::Failed`]: Token does not exist
/// - [`status::FfiStatusCode::Panic`]: Execution of the query resulted in an unrecoverable error or panic.
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `read_token_account_balance_callback` External function to call reading the token balance of an account.
/// - `get_account_address_by_index_callback` External function for getting account canonical address by account index.
/// - `get_account_index_by_address_callback` External function for getting account index by account address.
/// - `get_token_account_states_callback` External function for getting token account states.
/// - `block_state` Shared pointer to a block state to use for queries.
/// - `token_id` Shared pointer to token id UTF-8 bytes.
/// - `token_id_len` Byte length of token id UTF-8 bytes.
/// - `return_data_out` Location for writing pointer to array containing return data, which is the serialized token info.
///   If the return value is [`status::FfiStatusCode::Success`], the data is the token info.
///   If the return value is [`status::FfiStatusCode::Failed`], the data is empty (zero bytes).
///   The pointer written is to a uniquely owned array.
///   The caller must free the written array using `free_array_len_2` when it is no longer used.
/// - `return_data_len_out` Location for writing the length of the array whose pointer was written to `return_data_out`.
///
/// # Safety
///
/// - All callback arguments must be a valid function pointers to functions with a signature matching the
///   signature of Rust type of the function pointer.
/// - Argument `block_state` must be a non-null pointer to well-formed [`crate::block_state::BlockState`].
///   The pointer is to a shared instance, hence only valid for reading (writing only allowed through interior mutability).
/// - Argument `token_id` must be non-null and valid for reads for `token_id_len` many bytes.
/// - Argument `return_data_out` must be a non-null and valid pointer for writing
/// - Argument `return_data_len_out` must be a non-null and valid pointer for writing
#[unsafe(no_mangle)]
extern "C" fn ffi_query_token_info(
    load_callback: LoadCallback,
    read_token_account_balance_callback: ReadTokenAccountBalanceCallback,
    get_account_index_by_address_callback: GetAccountIndexByAddressCallback,
    get_account_address_by_index_callback: GetCanonicalAddressByAccountIndexCallback,
    get_token_account_states_callback: GetTokenAccountStatesCallback,
    block_state: *const PersistentBlockState,
    token_id: *const u8,
    token_id_len: size_t,
    return_data_out: *mut *mut u8,
    return_data_len_out: *mut size_t,
) -> status::FfiStatusCode {
    let (return_status, return_data) = status::catch_unwind(|| {
        assert!(!block_state.is_null(), "block_state is a null pointer.");
        assert!(
            !return_data_len_out.is_null(),
            "return_data_len_out is a null pointer."
        );
        assert!(
            !return_data_out.is_null(),
            "return_data_out is a null pointer."
        );
        let external = ExternalBlockStateQueryCallbacks {
            read_token_account_balance_ptr: read_token_account_balance_callback,
            get_account_address_by_index_ptr: get_account_address_by_index_callback,
            get_account_index_by_address_ptr: get_account_index_by_address_callback,
            get_token_account_states_ptr: get_token_account_states_callback,
        };
        let context = FfiQueryEntityContext {
            external,
            loader: load_callback,
        };
        let token_id_bytes = unsafe { std::slice::from_raw_parts(token_id, token_id_len) };
        let token_id = String::from_utf8(token_id_bytes.to_vec())
            .expect("Bytes for the Token ID is not a valid UTF-8 encoding")
            .try_into()
            .expect("Invalid Token ID provided");
        let token_info_res = match unsafe { &*block_state } {
            PersistentBlockState::P9(persistent) => {
                let block_state = BlockStateP9 {
                    persistent: persistent.clone(),
                };
                let exec_block_state = ExecutionTimeBlockStateP9 {
                    block_state,
                    context,
                };
                queries::query_token_info(&exec_block_state, &token_id)
            }
            PersistentBlockState::P10(persistent) => {
                let block_state = BlockStateP10 {
                    persistent: persistent.clone(),
                };
                let exec_block_state = ExecutionTimeBlockStateP10 {
                    block_state,
                    context,
                };
                queries::query_token_info(&exec_block_state, &token_id)
            }
            PersistentBlockState::P11(persistent) => {
                let block_state = BlockStateP11 {
                    persistent: persistent.clone(),
                };
                let exec_block_state = ExecutionTimeBlockStateP11 {
                    block_state,
                    context,
                };
                queries::query_token_info(&exec_block_state, &token_id)
            }
        };
        match token_info_res {
            Ok(token_info) => (
                status::FfiStatusCode::Success,
                common::to_bytes(&token_info),
            ),
            Err(QueryTokenInfoError::TokenDoesNotExist(_)) => {
                (status::FfiStatusCode::Failed, Vec::new())
            }
            Err(QueryTokenInfoError::QueryTokenModule(err)) => {
                (status::FfiStatusCode::Panic, err.to_string().into_bytes())
            }
        }
    });
    let array = memory::alloc_array_from_vec(return_data);
    unsafe {
        *return_data_len_out = array.length;
        *return_data_out = array.array;
    }
    return_status
}

/// C-binding for calling [`queries::query_token_authorizations`].
///
/// Returns a byte representing the result:
///
/// - [`status::FfiStatusCode::Success`]: Query succeeded
/// - [`status::FfiStatusCode::Failed`]: Token does not exist
/// - [`status::FfiStatusCode::Panic`]: Execution of the query resulted in an unrecoverable error or panic.
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `read_token_account_balance_callback` External function to call reading the token balance of an account.
/// - `get_account_address_by_index_callback` External function for getting account canonical address by account index.
/// - `get_account_index_by_address_callback` External function for getting account index by account address.
/// - `get_token_account_states_callback` External function for getting token account states.
/// - `block_state` Shared pointer to a block state to use for queries.
/// - `token_id` Shared pointer to token id UTF-8 bytes.
/// - `token_id_len` Byte length of token id UTF-8 bytes.
/// - `return_data_out` Location for writing pointer to array containing return data, which is the serialized token info.
///   If the return value is [`status::FfiStatusCode::Success`], the data is the token info.
///   If the return value is [`status::FfiStatusCode::Failed`], the data is empty (zero bytes).
///   The pointer written is to a uniquely owned array.
///   The caller must free the written array using `free_array_len_2` when it is no longer used.
/// - `return_data_len_out` Location for writing the length of the array whose pointer was written to `return_data_out`.
///
/// # Safety
///
/// - All callback arguments must be a valid function pointers to functions with a signature matching the
///   signature of Rust type of the function pointer.
/// - Argument `block_state` must be a non-null pointer to well-formed [`crate::block_state::BlockState`].
///   The pointer is to a shared instance, hence only valid for reading (writing only allowed through interior mutability).
/// - Argument `token_id` must be non-null and valid for reads for `token_id_len` many bytes.
/// - Argument `return_data_out` must be a non-null and valid pointer for writing
/// - Argument `return_data_len_out` must be a non-null and valid pointer for writing
#[unsafe(no_mangle)]
extern "C" fn ffi_query_token_authorizations(
    load_callback: LoadCallback,
    read_token_account_balance_callback: ReadTokenAccountBalanceCallback,
    get_account_index_by_address_callback: GetAccountIndexByAddressCallback,
    get_account_address_by_index_callback: GetCanonicalAddressByAccountIndexCallback,
    get_token_account_states_callback: GetTokenAccountStatesCallback,
    block_state: *const PersistentBlockState,
    token_id: *const u8,
    token_id_len: size_t,
    return_data_out: *mut *mut u8,
    return_data_len_out: *mut size_t,
) -> status::FfiStatusCode {
    let (return_status, return_data) = status::catch_unwind(|| {
        assert!(!block_state.is_null(), "block_state is a null pointer.");
        assert!(
            !return_data_len_out.is_null(),
            "return_data_len_out is a null pointer."
        );
        assert!(
            !return_data_out.is_null(),
            "return_data_out is a null pointer."
        );
        let external = ExternalBlockStateQueryCallbacks {
            read_token_account_balance_ptr: read_token_account_balance_callback,
            get_account_address_by_index_ptr: get_account_address_by_index_callback,
            get_account_index_by_address_ptr: get_account_index_by_address_callback,
            get_token_account_states_ptr: get_token_account_states_callback,
        };
        let context = FfiQueryEntityContext {
            external,
            loader: load_callback,
        };
        let token_id_bytes = unsafe { std::slice::from_raw_parts(token_id, token_id_len) };
        let token_id = String::from_utf8(token_id_bytes.to_vec())
            .expect("Bytes for the Token ID is not a valid UTF-8 encoding")
            .try_into()
            .expect("Invalid Token ID provided");
        let token_auths_res = match unsafe { &*block_state } {
            PersistentBlockState::P9(persistent) => {
                let block_state = BlockStateP9 {
                    persistent: persistent.clone(),
                };
                let exec_block_state = ExecutionTimeBlockStateP9 {
                    block_state,
                    context,
                };
                queries::query_token_authorizations(&exec_block_state, &token_id)
            }
            PersistentBlockState::P10(persistent) => {
                let block_state = BlockStateP10 {
                    persistent: persistent.clone(),
                };
                let exec_block_state = ExecutionTimeBlockStateP10 {
                    block_state,
                    context,
                };
                queries::query_token_authorizations(&exec_block_state, &token_id)
            }
            PersistentBlockState::P11(persistent) => {
                let block_state = BlockStateP11 {
                    persistent: persistent.clone(),
                };
                let exec_block_state = ExecutionTimeBlockStateP11 {
                    block_state,
                    context,
                };
                queries::query_token_authorizations(&exec_block_state, &token_id)
            }
        };
        match token_auths_res {
            Ok(token_auths) => (
                status::FfiStatusCode::Success,
                common::to_bytes(&token_auths),
            ),
            Err(QueryTokenInfoError::TokenDoesNotExist(_)) => {
                (status::FfiStatusCode::Failed, Vec::new())
            }
            Err(QueryTokenInfoError::QueryTokenModule(err)) => {
                (status::FfiStatusCode::Panic, err.to_string().into_bytes())
            }
        }
    });
    let array = memory::alloc_array_from_vec(return_data);
    unsafe {
        *return_data_len_out = array.length;
        *return_data_out = array.array;
    }
    return_status
}

/// C-binding for calling [`queries::query_token_account_infos`].
///
/// Returns a byte representing the result:
///
/// - [`status::FfiStatusCode::Success`]: Query succeeded
/// - [`status::FfiStatusCode::Panic`]: Execution of the query resulted in an unrecoverable error or panic.
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `read_token_account_balance_callback` External function to call reading the token balance of an account.
/// - `get_account_address_by_index_callback` External function for getting account canonical address by account index.
/// - `get_account_index_by_address_callback` External function for getting account index by account address.
/// - `get_token_account_states_callback` External function for getting token account states.
/// - `block_state` Shared pointer to a block state to use for queries.
/// - `account_index` Index of the account to find token account infos for. The account must exist.
/// - `return_data_out` Location for writing pointer to array containing return data, which is the serialized token account infos.
///   If the return value is [`status::FfiStatusCode::Success`], the data is the serialized list of token account infos.
///   The pointer written is to a uniquely owned array.
///   The caller must free the written array using `free_array_len_2` when it is no longer used.
/// - `return_data_len_out` Location for writing the length of the array whose pointer was written to `return_data_out`.
///
/// # Safety
///
/// - All callback arguments must be a valid function pointers to functions with a signature matching the
///   signature of Rust type of the function pointer.
/// - Argument `block_state` must be a non-null pointer to well-formed [`crate::block_state::PersistentBlockState`].
///   The pointer is to a shared instance, hence only valid for reading (writing only allowed through interior mutability).
/// - Argument `return_data_out` must be a non-null and valid pointer for writing
/// - Argument `return_data_len_out` must be a non-null and valid pointer for writing
#[unsafe(no_mangle)]
extern "C" fn ffi_query_token_account_infos(
    load_callback: LoadCallback,
    read_token_account_balance_callback: ReadTokenAccountBalanceCallback,
    get_account_index_by_address_callback: GetAccountIndexByAddressCallback,
    get_account_address_by_index_callback: GetCanonicalAddressByAccountIndexCallback,
    get_token_account_states_callback: GetTokenAccountStatesCallback,
    block_state: *const PersistentBlockState,
    account_index: u64,
    return_data_out: *mut *mut u8,
    return_data_len_out: *mut size_t,
) -> status::FfiStatusCode {
    let (return_status, return_data) = status::catch_unwind(|| {
        assert!(!block_state.is_null(), "block_state is a null pointer.");
        assert!(
            !return_data_len_out.is_null(),
            "return_data_len_out is a null pointer."
        );
        assert!(
            !return_data_out.is_null(),
            "return_data_out is a null pointer."
        );
        let external = ExternalBlockStateQueryCallbacks {
            read_token_account_balance_ptr: read_token_account_balance_callback,
            get_account_address_by_index_ptr: get_account_address_by_index_callback,
            get_account_index_by_address_ptr: get_account_index_by_address_callback,
            get_token_account_states_ptr: get_token_account_states_callback,
        };
        let context = FfiQueryEntityContext {
            external,
            loader: load_callback,
        };
        let token_account_infos = match unsafe { &*block_state } {
            PersistentBlockState::P9(persistent) => {
                let block_state = BlockStateP9 {
                    persistent: persistent.clone(),
                };
                let exec_block_state = ExecutionTimeBlockStateP9 {
                    block_state,
                    context,
                };
                queries::query_token_account_infos(
                    &exec_block_state,
                    Account::from_existing_account(AccountIndex::from(account_index)),
                )
            }
            PersistentBlockState::P10(persistent) => {
                let block_state = BlockStateP10 {
                    persistent: persistent.clone(),
                };
                let exec_block_state = ExecutionTimeBlockStateP10 {
                    block_state,
                    context,
                };
                queries::query_token_account_infos(
                    &exec_block_state,
                    Account::from_existing_account(AccountIndex::from(account_index)),
                )
            }
            PersistentBlockState::P11(persistent) => {
                let block_state = BlockStateP11 {
                    persistent: persistent.clone(),
                };
                let exec_block_state = ExecutionTimeBlockStateP11 {
                    block_state,
                    context,
                };
                queries::query_token_account_infos(
                    &exec_block_state,
                    Account::from_existing_account(AccountIndex::from(account_index)),
                )
            }
        };
        let return_data = common::to_bytes(&token_account_infos);
        (status::FfiStatusCode::Success, return_data)
    });
    let array = memory::alloc_array_from_vec(return_data);
    unsafe {
        *return_data_len_out = array.length;
        *return_data_out = array.array;
    }
    return_status
}

/// C-binding for calling [`queries::query_lock_list`].
///
/// Returns a byte representing the result:
///
/// - [`status::FfiStatusCode::Success`]: Query succeeded
/// - [`status::FfiStatusCode::Panic`]: Execution of the query resulted in an unrecoverable error or panic.
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `read_token_account_balance_callback` External function to call reading the token balance of an account.
/// - `get_account_address_by_index_callback` External function for getting account canonical address by account index.
/// - `get_account_index_by_address_callback` External function for getting account index by account address.
/// - `get_token_account_states_callback` External function for getting token account states.
/// - `block_state` Shared pointer to a block state to use for queries.
/// - `return_data_out` Location for writing pointer to array containing return data, which is the
///   serialized list of lock ids. The pointer written is to a uniquely owned array. The caller must
///   free the written array using `free_array_len_2` when it is no longer used.
/// - `return_data_len_out` Location for writing the length of the array whose pointer was written
///   to `return_data_out`.
///
/// # Safety
///
/// - All callback arguments must be valid function pointers to functions with a signature matching
///   the signature of the Rust type of the function pointer.
/// - Argument `block_state` must be a non-null pointer to a well-formed [`crate::block_state::PersistentBlockState`].
///   The pointer is to a shared instance, hence only valid for reading (writing only allowed through
///   interior mutability).
/// - Argument `return_data_out` must be a non-null and valid pointer for writing.
/// - Argument `return_data_len_out` must be a non-null and valid pointer for writing.
#[unsafe(no_mangle)]
extern "C" fn ffi_query_lock_list(
    load_callback: LoadCallback,
    read_token_account_balance_callback: ReadTokenAccountBalanceCallback,
    get_account_index_by_address_callback: GetAccountIndexByAddressCallback,
    get_account_address_by_index_callback: GetCanonicalAddressByAccountIndexCallback,
    get_token_account_states_callback: GetTokenAccountStatesCallback,
    block_state: *const PersistentBlockState,
    return_data_out: *mut *mut u8,
    return_data_len_out: *mut size_t,
) -> status::FfiStatusCode {
    let (return_status, return_data) = status::catch_unwind(|| {
        assert!(!block_state.is_null(), "block_state is a null pointer.");
        assert!(
            !return_data_len_out.is_null(),
            "return_data_len_out is a null pointer."
        );
        assert!(
            !return_data_out.is_null(),
            "return_data_out is a null pointer."
        );
        let external = ExternalBlockStateQueryCallbacks {
            read_token_account_balance_ptr: read_token_account_balance_callback,
            get_account_address_by_index_ptr: get_account_address_by_index_callback,
            get_account_index_by_address_ptr: get_account_index_by_address_callback,
            get_token_account_states_ptr: get_token_account_states_callback,
        };
        let context = FfiQueryEntityContext {
            external,
            loader: load_callback,
        };
        let lock_ids = match unsafe { &*block_state } {
            PersistentBlockState::P9(persistent) => {
                let block_state = BlockStateP9 {
                    persistent: persistent.clone(),
                };
                let exec_block_state = ExecutionTimeBlockStateP9 {
                    block_state,
                    context,
                };
                queries::query_lock_list(&exec_block_state)
            }
            PersistentBlockState::P10(persistent) => {
                let block_state = BlockStateP10 {
                    persistent: persistent.clone(),
                };
                let exec_block_state = ExecutionTimeBlockStateP10 {
                    block_state,
                    context,
                };
                queries::query_lock_list(&exec_block_state)
            }
            PersistentBlockState::P11(persistent) => {
                let block_state = BlockStateP11 {
                    persistent: persistent.clone(),
                };
                let exec_block_state = ExecutionTimeBlockStateP11 {
                    block_state,
                    context,
                };
                queries::query_lock_list(&exec_block_state)
            }
        };
        let return_data = common::to_bytes(&lock_ids);
        (status::FfiStatusCode::Success, return_data)
    });
    let array = memory::alloc_array_from_vec(return_data);
    unsafe {
        *return_data_len_out = array.length;
        *return_data_out = array.array;
    }
    return_status
}

/// C-binding for calling [`queries::query_lock_info`].
///
/// Returns a byte representing the result:
///
/// - [`status::FfiStatusCode::Success`]: Query succeeded
/// - [`status::FfiStatusCode::Failed`]: Lock does not exist
/// - [`status::FfiStatusCode::Panic`]: Execution of the query resulted in an unrecoverable error or panic.
///
/// # Arguments
///
/// - `load_callback` External function to call for loading bytes a reference from the blob store.
/// - `read_token_account_balance_callback` External function to call reading the token balance of an account.
/// - `get_account_address_by_index_callback` External function for getting account canonical address by account index.
/// - `get_account_index_by_address_callback` External function for getting account index by account address.
/// - `get_token_account_states_callback` External function for getting token account states.
/// - `block_state` Shared pointer to a block state to use for queries.
/// - `lock_id` Pointer to 24 bytes containing the [`LockId`] (three big-endian `u64` fields:
///   `account_index`, `sequence_number`, `creation_order`).
/// - `return_data_out` Location for writing pointer to array containing return data, which is the
///   raw CBOR-encoded `lock-info` payload. The pointer written is to a uniquely owned array; the
///   caller must free the written array using `free_array_len_2` when it is no longer used.
///   If the return value is [`status::FfiStatusCode::Failed`], the data is empty (zero bytes).
/// - `return_data_len_out` Location for writing the length of the array whose pointer was written
///   to `return_data_out`.
///
/// # Safety
///
/// - All callback arguments must be valid function pointers to functions with a signature matching
///   the signature of the Rust type of the function pointer.
/// - Argument `block_state` must be a non-null pointer to a well-formed [`crate::block_state::BlockState`].
///   The pointer is to a shared instance, hence only valid for reading (writing only allowed through
///   interior mutability).
/// - Argument `lock_id` must be non-null and valid for reads of exactly 24 bytes.
/// - Argument `return_data_out` must be a non-null and valid pointer for writing.
/// - Argument `return_data_len_out` must be a non-null and valid pointer for writing.
#[unsafe(no_mangle)]
extern "C" fn ffi_query_lock_info(
    load_callback: LoadCallback,
    read_token_account_balance_callback: ReadTokenAccountBalanceCallback,
    get_account_index_by_address_callback: GetAccountIndexByAddressCallback,
    get_account_address_by_index_callback: GetCanonicalAddressByAccountIndexCallback,
    get_token_account_states_callback: GetTokenAccountStatesCallback,
    block_state: *const PersistentBlockState,
    lock_id: *const [u8; 24],
    return_data_out: *mut *mut u8,
    return_data_len_out: *mut size_t,
) -> status::FfiStatusCode {
    let (return_status, return_data) = status::catch_unwind(|| {
        assert!(!block_state.is_null(), "block_state is a null pointer.");
        assert!(!lock_id.is_null(), "lock_id is a null pointer.");
        assert!(
            !return_data_len_out.is_null(),
            "return_data_len_out is a null pointer."
        );
        assert!(
            !return_data_out.is_null(),
            "return_data_out is a null pointer."
        );
        let external = ExternalBlockStateQueryCallbacks {
            read_token_account_balance_ptr: read_token_account_balance_callback,
            get_account_address_by_index_ptr: get_account_address_by_index_callback,
            get_account_index_by_address_ptr: get_account_index_by_address_callback,
            get_token_account_states_ptr: get_token_account_states_callback,
        };
        let context = FfiQueryEntityContext {
            external,
            loader: load_callback,
        };
        // The Haskell side serializes a `LockId` as three big-endian `u64`s, exactly 24 bytes.
        let lock_id_bytes = unsafe { lock_id.as_ref().expect("lock_id is a null pointer") };
        let lock_id: LockId = common::from_bytes_complete(lock_id_bytes)
            .expect("Bytes for the LockId could not be deserialized");
        let lock_info_res = match unsafe { &*block_state } {
            PersistentBlockState::P9(persistent) => {
                let block_state = BlockStateP9 {
                    persistent: persistent.clone(),
                };
                let exec_block_state = ExecutionTimeBlockStateP9 {
                    block_state,
                    context,
                };
                queries::query_lock_info(&exec_block_state, &lock_id)
            }
            PersistentBlockState::P10(persistent) => {
                let block_state = BlockStateP10 {
                    persistent: persistent.clone(),
                };
                let exec_block_state = ExecutionTimeBlockStateP10 {
                    block_state,
                    context,
                };
                queries::query_lock_info(&exec_block_state, &lock_id)
            }
            PersistentBlockState::P11(persistent) => {
                let block_state = BlockStateP11 {
                    persistent: persistent.clone(),
                };
                let exec_block_state = ExecutionTimeBlockStateP11 {
                    block_state,
                    context,
                };
                queries::query_lock_info(&exec_block_state, &lock_id)
            }
        };
        match lock_info_res {
            Ok(cbor_bytes) => (status::FfiStatusCode::Success, cbor_bytes.into()),
            Err(QueryLockError::LockDoesNotExist) => (status::FfiStatusCode::Failed, Vec::new()),
            Err(QueryLockError::StateInvariantViolation(message)) => {
                (status::FfiStatusCode::Panic, message.into_bytes())
            }
        }
    });
    let array = memory::alloc_array_from_vec(return_data);
    unsafe {
        *return_data_len_out = array.length;
        *return_data_out = array.array;
    }
    return_status
}
