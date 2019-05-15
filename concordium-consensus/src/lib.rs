#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;
extern crate concordium_common;

// (de)serialization macros

macro_rules! check_serialization {
    ($target:expr, $cursor:expr) => {
        debug_assert_eq!($cursor.position(), $cursor.get_ref().len() as u64);

        debug_assert_eq!(
            &&*$target.serialize(),
            $cursor.get_ref(),
            "Invalid serialization of {:?}",
            $target
        );
    };
}

macro_rules! debug_deserialization {
    ($target:expr, $bytes:expr) => {
        info!("Deserializing an object: {} ({}B)", $target, $bytes.len());
    };
}

macro_rules! debug_serialization {
    ($object:expr) => {
        info!("Serializing an object: {:?}", $object);
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

        buf
    }};
}

// ffi macros

macro_rules! wrap_c_call_string {
    ($self:ident, $baker:ident, $c_call:expr) => {{
        let $baker = $self.runner.load(Ordering::SeqCst);
        unsafe {
            let c_string = $c_call($baker);
            let r = CStr::from_ptr(c_string).to_str().unwrap().to_owned();
            freeCStr(c_string);
            r
        }
    }};
}

macro_rules! wrap_send_data_to_c {
    ($self:ident, $peer_id:ident, $data:expr, $c_call:expr) => {{
        let baker = $self.runner.load(Ordering::SeqCst);
        let len = $data.len();
        unsafe {
            return $c_call(
                baker,
                $peer_id,
                CString::from_vec_unchecked($data.into()).as_ptr() as *const u8,
                len as i64,
            );
        };
    }};
}

macro_rules! wrap_c_call_bytes {
    ($self:ident, $c_call:expr) => {{
        let baker = $self.runner.load(Ordering::SeqCst);
        unsafe {
            let res = $c_call(baker) as *const u8;
            let raw_size = slice::from_raw_parts(res, 4);
            let mut raw_len_buf = Cursor::new(&raw_size[0..4]);
            let ret = match raw_len_buf.read_u32::<NetworkEndian>() {
                Ok(size) => slice::from_raw_parts(res, 4 + size as usize)[4..].to_owned(),
                _ => vec![],
            };
            freeCStr(res as *const i8);
            ret
        }
    }};
}

macro_rules! wrap_c_call {
    ($self:ident, $c_call:expr) => {{
        let baker = $self.runner.load(Ordering::SeqCst);
        unsafe { $c_call(baker) }
    }};
}

#[macro_use]
mod fails;
pub mod block;
pub mod common;
pub mod consensus;
pub mod ffi;
pub mod finalization;
pub mod parameters;
pub mod transaction;
pub mod tree;
