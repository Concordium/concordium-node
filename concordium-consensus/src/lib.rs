#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;

// ffi macros

macro_rules! wrap_c_call_string {
    ($self:ident, $baker:ident, $c_call:expr) => {{
        let $baker = $self.consensus.load(Ordering::SeqCst);
        unsafe {
            let c_string = $c_call($baker);
            let r = CStr::from_ptr(c_string).to_str().unwrap().to_owned();
            freeCStr(c_string);
            r
        }
    }};
}

macro_rules! wrap_send_data_to_c {
    ($self:ident, $data:expr, $c_call:expr) => {{
        let consensus = $self.consensus.load(Ordering::SeqCst);
        let len = $data.len();

        let result = unsafe {
            $c_call(
                consensus,
                CString::from_vec_unchecked($data.to_vec()).as_ptr() as *const u8,
                len as i64,
            )
        };

        ConsensusFfiResponse::try_from(result)
            .unwrap_or_else(|code| panic!("Unknown FFI return code: {}", code))
    }};
}

macro_rules! wrap_c_call_bytes {
    ($self:ident, $c_call:expr) => {{
        let consensus = $self.consensus.load(Ordering::SeqCst);

        unsafe {
            let res = $c_call(consensus) as *const u8;
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
        let consensus = $self.consensus.load(Ordering::SeqCst);
        let result = unsafe { $c_call(consensus) };

        ConsensusFfiResponse::try_from(result)
            .unwrap_or_else(|code| panic!("Unknown FFI return code: {}", code))
    }};
}

macro_rules! wrap_c_bool_call {
    ($self:ident, $c_call:expr) => {{
        let consensus = $self.consensus.load(Ordering::SeqCst);
        match unsafe { $c_call(consensus) } {
            0u8 => false,
            1u8 => true,
            code => panic!("FFI call didn't return 0 or 1 but {}", code),
        }
    }};
}

#[macro_use]
mod fails;
pub mod consensus;
pub mod ffi;
pub mod transferlog;
