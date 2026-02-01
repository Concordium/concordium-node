// todo remove or change this module as part of https://linear.app/concordium/issue/COR-2113/fix-rust-allocator-issue-related-to-multiple-rust-cdylibs
// - the function free_array_len exists in concordium_base, we should replace usages of the function below if plt-scheduler and wasm-chain-integration are build into one library

/// Free an array that was converted to a pointer from a vector.
/// This assumes the vector's capacity and length were the same.
#[unsafe(no_mangle)]
extern "C" fn free_array_len_2(ptr: *mut u8, len: u64) {
    unsafe {
        Vec::from_raw_parts(ptr, len as usize, len as usize);
    }
}
