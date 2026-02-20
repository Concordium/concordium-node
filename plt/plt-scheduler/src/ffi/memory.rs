// todo remove or change this module as part of https://linear.app/concordium/issue/COR-2113/fix-rust-allocator-issue-related-to-multiple-rust-cdylibs
// - the function free_array_len exists in concordium_base, we should replace usages of the function free_array_len_2 below if plt-scheduler and wasm-chain-integration are build into one library
// - the function copy_to_vec_ffi exists in concordium_base, we should replace usages of the function copy_to_vec_ffi_2 below if plt-scheduler and wasm-chain-integration are build into one library

use libc::size_t;

/// Free an array that was converted to a pointer from a vector.
/// This assumes the vector's capacity and length were the same.
///
/// # Safety
///
/// - Argument `ptr` must be a non-null and valid unique pointer to a `Vec`.
/// - Argument `len` must be equal to the length AND capacity of the given `Vec`
#[unsafe(no_mangle)]
extern "C" fn free_array_len_2(ptr: *mut u8, len: u64) {
    println!("entry free_array_len_2"); // todo ar
    unsafe {
        Vec::from_raw_parts(ptr, len as usize, len as usize);
    }
}

/// Take the byte array and copy it into a vector.
///
/// Returns pointer to a uniquely owned [`Vec`].
/// The returned `Vec` must be deallocated passing the ownership back to the Rust code again.
#[unsafe(no_mangle)]
extern "C" fn copy_to_vec_ffi_2(data: *const u8, len: libc::size_t) -> *mut Vec<u8> {
    println!("entry copy_to_vec_ffi_2"); // todo ar
    Box::into_raw(Box::new(
        unsafe { std::slice::from_raw_parts(data, len) }.to_vec(),
    ))
}

/// Allocated array together with the array length.
/// Must be freed with [`free_array_len_2`].
pub struct ArrayWithLength {
    /// Unique pointer to first byte in array.
    pub array: *mut u8,
    ///  Length of the array
    pub length: size_t,
}

/// Allocate an array by using the given allocated `Vec` and converting it into raw parts.
/// Must be freed with [`free_array_len_2`].
pub fn alloc_array_from_vec(mut bytes: Vec<u8>) -> ArrayWithLength {
    // shrink Vec should that we know capacity and length are equal (this is important when we later free with free_array_len_2)
    bytes.shrink_to_fit();

    let (array, length, capacity) = bytes.into_raw_parts();

    // todo for now we assert that capacity is equals to the length, but we should address that this may not be the case in a better way, see https://linear.app/concordium/issue/COR-2181/address-potentially-unsafe-behaviour-cased-by-using-shrink-to-fit
    assert_eq!(
        capacity, length,
        "vec capacity not equal to length after call to shrink_to_fit"
    );

    ArrayWithLength { array, length }
}
