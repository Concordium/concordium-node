// todo remove or change this module as part of https://linear.app/concordium/issue/PSR-61/address-potentially-unsafe-behaviour-cased-by-using-shrink-to-fit

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
    unsafe {
        Vec::from_raw_parts(ptr, len as usize, len as usize);
    }
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

    // todo for now we assert that capacity is equals to the length, but we should address that this may not be the case in a better way, see https://linear.app/concordium/issue/PSR-61/address-potentially-unsafe-behaviour-cased-by-using-shrink-to-fit
    assert_eq!(
        capacity, length,
        "vec capacity not equal to length after call to shrink_to_fit"
    );

    ArrayWithLength { array, length }
}
