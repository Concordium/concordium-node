#[macro_use]
extern crate log;

/// A debug test designed to check whether deserialization is perfectly
/// reversible. It also verifies that the input buffer is exhausted in the
/// process.
#[allow(unused_macros)]
macro_rules! check_serialization {
    ($target:expr, $cursor:expr) => {
        if $cursor.position() != $cursor.get_ref().len() as u64 {
            return Err(failure::format_err!(
                "\n\nInvalid deserialization of {:#?}\n\nbytes: {:?}",
                $target,
                $cursor.get_ref()
            ));
        }

        if &&*$target.serialize() != $cursor.get_ref() {
            return Err(failure::format_err!(
                "\n\nInvalid serialization of {:#?}",
                $target
            ));
        }
    };
}

/// A debug test designed to check whether deserialization is perfectly
/// reversible in variable-length objects.
#[allow(unused_macros)]
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
    ($source:expr, $elem:expr, $len_size:expr, $limit:expr) => {{
        let count = safe_get_len!($source, $len_size, $limit);
        let mut list = Vec::with_capacity(count as usize);
        for _ in 0..count {
            let elem = $elem;
            list.push(elem);
        }

        list.into_boxed_slice()
    }};
}

/// Reads multiple objects from into a boxed slice, checking if the target
/// length is not suspiciously long in the process.
macro_rules! read_hashmap {
    ($source:expr, $elem:expr, $len_size:expr, $limit:expr) => {{
        let count = safe_get_len!($source, $len_size, $limit);
        let mut list = HashMap::with_capacity(count as usize);
        for _ in 0..count {
            let elem = $elem;
            list.insert(elem.0, elem.1);
        }

        list
    }};
}

/// Sequentially writes a collection of objects to the specified target using
/// the given write function.
macro_rules! write_multiple {
    ($target:expr, $list:expr, $write_function:path) => {{
        let _ = $target.write_u64::<Endianness>($list.len() as u64);
        for elem in &*$list {
            $write_function($target, &*elem)?;
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
            $target.write_u8(1)?;
            $write_function($target, &*value)?;
        } else {
            $target.write_u8(0)?;
        }
    }};
}

/// Checks whether an object intended to be used as a length is not too big
/// in order to avoid OOMs.
macro_rules! safe_get_len {
    ($source:expr, $len_size:expr, $limit:expr) => {{
        let raw_len = if $len_size == 8 {
            Endianness::read_u64(&read_const_sized!($source, 8)) as usize
        } else if $len_size == 4 {
            Endianness::read_u32(&read_const_sized!($source, 4)) as usize
        } else if $len_size == 2 {
            Endianness::read_u16(&read_const_sized!($source, 2)) as usize
        } else {
            panic!("Unexpected len size in safe_get_len!")
        };

        if raw_len <= $limit {
            raw_len
        } else {
            0
        }
    }};
}

macro_rules! hashed {
    ($variant:ident, $capacity:expr) => {{
        hash_hasher::$variant::with_capacity_and_hasher(
            $capacity,
            hash_hasher::HashBuildHasher::default(),
        )
    }};
}

pub mod block;
pub mod catch_up;
pub mod common;
pub mod finalization;
pub mod parameters;
pub mod transaction;
pub mod tree;
