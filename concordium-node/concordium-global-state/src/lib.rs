#[macro_use]
extern crate log;

// (de)serialization macros

macro_rules! check_serialization {
    ($target:expr, $cursor:expr) => {
        debug_assert_eq!(
            $cursor.position(),
            $cursor.get_ref().len() as u64,
            "Invalid deserialization of {:?}",
            $target
        );

        debug_assert_eq!(
            &&*$target.serialize(),
            $cursor.get_ref(),
            "Invalid serialization of {:?}",
            $target
        );
    };
}

macro_rules! read_const_sized {
    ($source:expr, $size:expr) => {{
        let mut buf = [0u8; $size as usize];
        $source.read_exact(&mut buf)?;

        buf
    }};
}

macro_rules! read_sized {
    ($source:expr, $size:expr) => {{
        let mut buf = vec![0u8; $size as usize];
        $source.read_exact(&mut buf)?;

        buf.into_boxed_slice()
    }};
}

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

macro_rules! write_multiple {
    ($target:expr, $list:expr, $write_function:path) => {{
        let _ = $target.write_u64::<NetworkEndian>($list.len() as u64);
        for elem in &*$list {
            let _ = $write_function($target, &*elem);
        }
    }};
}

macro_rules! write_maybe {
    ($target:expr, $maybe:expr, $write_function:ident) => {{
        if let Some(ref value) = $maybe {
            let _ = $target.write(&[1]);
            $write_function($target, value);
        } else {
            let _ = $target.write(&[0]);
        }
    }};
}

macro_rules! safe_get_len {
    ($source:expr, $object:expr) => {{
        let raw_len = NetworkEndian::read_u64(&read_const_sized!($source, 8)) as usize;
        failure::ensure!(
            raw_len <= ALLOCATION_LIMIT,
            "The requested size ({}) of \"{}\" exceeds the safety limit!",
            raw_len,
            $object,
        );
        raw_len
    }};
}


pub mod block;
pub mod common;
pub mod finalization;
pub mod parameters;
pub mod transaction;
pub mod tree;
