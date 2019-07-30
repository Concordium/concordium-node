use concordium_global_state::{
    block::{BlockHash, BlockHeight, BlockPtr},
    tree::{messaging::GlobalStateResult, GlobalState},
};

use std::{mem::size_of, slice};

#[no_mangle]
pub unsafe extern "C" fn get_genesis_block_pointer(
    global_state: *const GlobalState,
) -> *const BlockPtr {
    let global_state = &*global_state;

    &*global_state.data.genesis_block_ptr as *const BlockPtr
}

#[no_mangle]
pub unsafe extern "C" fn get_genesis_data(global_state: *const GlobalState) -> *const [u8] {
    let global_state = &*global_state;

    (*global_state.data.genesis_block_ptr)
        .block
        .genesis_data()
        .as_ref() as *const [u8]
}

#[no_mangle]
pub unsafe extern "C" fn store_finalized_block(
    global_state: *mut GlobalState,
    serialized_block: *const u8,
    block_size: u64,
) {
    let global_state = &mut *global_state;
    let block = slice::from_raw_parts(serialized_block, block_size as usize);

    global_state.store_serialized_block(block);
}

#[no_mangle]
pub unsafe extern "C" fn get_finalized_block(
    global_state: *const GlobalState,
    block_hash: *const u8,
) -> *const [u8] {
    let global_state = &*global_state;
    let hash = slice::from_raw_parts(block_hash, size_of::<BlockHash>());

    let blob = match global_state.get_stored_block(&BlockHash::new(hash)) {
        GlobalStateResult::SuccessfulQuery(blob) => blob,
        _ => Box::new([]),
    };

    Box::into_raw(blob)
}

// not yet usable; we should first test the previous calls
#[no_mangle]
pub unsafe extern "C" fn get_last_finalized(global_state: *const GlobalState) -> *const BlockPtr {
    let global_state = &*global_state;

    &*global_state.data.last_finalized as *const BlockPtr
}

// ditto
#[no_mangle]
pub unsafe extern "C" fn get_last_finalized_slot(global_state: *const GlobalState) -> BlockHeight {
    let global_state = &*global_state;

    global_state.data.last_finalized.height
}
