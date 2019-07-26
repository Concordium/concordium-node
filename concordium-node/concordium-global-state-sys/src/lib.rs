use concordium_global_state::{block::BlockHash, tree::{messaging::GlobalStateResult, GlobalState}};

use std::{mem::size_of, slice};

#[no_mangle]
pub unsafe extern fn store_block(
    global_state: *mut GlobalState,
    serialized_block: *const u8,
    block_size: u64,
) {
    let global_state = &mut *global_state;
    let block = slice::from_raw_parts(
        serialized_block,
        block_size as usize,
    );

    global_state.store_serialized_block(block);
}

#[no_mangle]
pub unsafe extern fn get_block(
    global_state: *const GlobalState,
    block_hash: *const u8,
) -> *mut [u8] {
    let global_state = &*global_state;
    let hash = slice::from_raw_parts(block_hash, size_of::<BlockHash>());

    let blob = match global_state.get_stored_block(&BlockHash::new(hash)) {
        GlobalStateResult::SuccessfulQuery(blob) => blob,
        _ => Box::new([])
    };

    Box::into_raw(blob)
}

