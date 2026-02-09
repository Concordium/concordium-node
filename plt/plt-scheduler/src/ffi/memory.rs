// todo remove or change this module as part of https://linear.app/concordium/issue/COR-2113/fix-rust-allocator-issue-related-to-multiple-rust-cdylibs
// - the function free_array_len exists in concordium_base, we should replace usages of the function free_array_len_2 below if plt-scheduler and wasm-chain-integration are build into one library
// - the function copy_to_vec_ffi exists in concordium_base, we should replace usages of the function copy_to_vec_ffi_2 below if plt-scheduler and wasm-chain-integration are build into one library

/// Free an array that was converted to a pointer from a vector.
/// This assumes the vector's capacity and length were the same.
#[unsafe(no_mangle)]
extern "C" fn free_array_len_2(ptr: *mut u8, len: u64) {
    unsafe {
        Vec::from_raw_parts(ptr, len as usize, len as usize);
    }
}

/// Take the byte array and copy it into a vector.
/// The vector must be passed to Rust to be deallocated.
#[unsafe(no_mangle)]
extern "C" fn copy_to_vec_ffi_2(data: *const u8, len: libc::size_t) -> *mut Vec<u8> {
    Box::into_raw(Box::new(
        unsafe { std::slice::from_raw_parts(data, len) }.to_vec(),
    ))
}
