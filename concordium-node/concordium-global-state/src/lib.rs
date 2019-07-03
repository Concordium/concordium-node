#[macro_use]
extern crate log;

/// A debug test designed to check whether deserialization is perfectly
/// reversible. It also verifies that the input buffer is exhausted in the
/// process.
macro_rules! check_serialization {
    ($target:expr, $cursor:expr) => {
        debug_assert_eq!(
            $cursor.position(),
            $cursor.get_ref().len() as u64,
            "\n\nInvalid deserialization of {:#?}\n\nbytes: {:?}",
            $target,
            $cursor.get_ref()
        );

        debug_assert_eq!(
            &&*$target.serialize(),
            $cursor.get_ref(),
            "\n\nInvalid serialization of {:#?}",
            $target
        );
    };
}

/// A debug test designed to check whether deserialization is perfectly
/// reversible in variable-length objects.
macro_rules! check_partial_serialization {
    ($target:expr, $source:expr) => {
        debug_assert_eq!(
            &*$target.serialize(),
            $source,
            "\n\nInvalid serialization of {:#?}",
            $target
        );
    };
}

/// Reads a const-sized number of bytes into an array.
macro_rules! read_const_sized {
    ($source:expr, $size:expr) => {{
        let mut buf = [0u8; $size as usize];
        $source.read_exact(&mut buf)?;

        buf
    }};
}

/// Reads a number of bytes equal to the size of `object` into an array.
macro_rules! read_ty {
    ($source:expr, $object:ty) => {{
        let mut buf = [0u8; std::mem::size_of::<$object>()];
        $source.read_exact(&mut buf)?;

        buf
    }};
}

/// Reads a number of bytes equal to the size of `object` into an array.
macro_rules! sum_ty_lens {
    ($($t:ty),+) => {{
        let mut sum = 0;
        $(sum += size_of::<$t>();)+
        sum
    }};
}

/// Reads a known number of bytes into a boxed slice. Incurs an allocation, but
/// doesn't waste any space and the result is immutable.
macro_rules! read_sized {
    ($source:expr, $size:expr) => {{
        let mut buf = vec![0u8; $size as usize];
        $source.read_exact(&mut buf)?;

        buf.into_boxed_slice()
    }};
}

/// Reads multiple objects from into a boxed slice, checking if the target
/// length is not suspiciously long in the process.
macro_rules! read_multiple {
    ($source:expr, $list_name:expr, $elem:expr) => {{
        let count = safe_get_len!($source, $list_name);
        let mut list = Vec::with_capacity(count as usize);
        for _ in 0..count {
            let elem = $elem;
            list.push(elem);
        }

        list.into_boxed_slice()
    }};
}

/// Sequentially writes a collection of objects to the specified target using
/// the given write function.
macro_rules! write_multiple {
    ($target:expr, $list:expr, $write_function:path) => {{
        let _ = $target.write_u64::<NetworkEndian>($list.len() as u64);
        for elem in &*$list {
            let _ = $write_function($target, &*elem);
        }
    }};
}

/// Deserializes a Haskell's `Maybe` object to a specified target.
/// `1u8` is decoded as `Just X` and a `0u8` as `Nothing`.
macro_rules! read_maybe {
    ($source:expr, $read_expr:expr) => {{
        if read_const_sized!($source, 1)[0] == 1 {
            Some($read_expr)
        } else {
            None
        }
    }};
}

/// Serializes a Haskell's `Maybe` object Haskell-style to a specified target.
/// Prepends the value with `1u8` for `Just X` and a `0u8` for `Nothing`.
macro_rules! write_maybe {
    ($target:expr, $maybe:expr, $write_function:path) => {{
        if let Some(ref value) = $maybe {
            let _ = $target.write(&[1]);
            $write_function($target, &*value);
        } else {
            let _ = $target.write(&[0]);
        }
    }};
}

/// Checks whether an object intended to be used as a length is not too big
/// in order to avoid OOMs.
macro_rules! safe_get_len {
    ($source:expr, $object:expr) => {{
        let raw_len = NetworkEndian::read_u64(&read_const_sized!($source, 8)) as usize;
        failure::ensure!(
            raw_len <= ALLOCATION_LIMIT,
            "The requested size ({}) of {} exceeds the safety limit! bytes: {:?}",
            raw_len,
            $object,
            $source,
        );
        raw_len
    }};
}

macro_rules! hashed {
    ($variant:ident, $capacity:expr) => {{
        use hash_hasher::{HashBuildHasher, $variant};

        $variant::with_capacity_and_hasher($capacity, HashBuildHasher::default())
    }};
}

pub mod block;
pub mod common;
pub mod finalization;
pub mod parameters;
pub mod transaction;
pub mod tree;
