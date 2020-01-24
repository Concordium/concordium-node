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

macro_rules! wrap_c_call_payload {
    ($self:ident, $c_call:expr, $prefix:expr) => {{
        let consensus = $self.consensus.load(Ordering::SeqCst);

        unsafe {
            let res = $c_call(consensus) as *const u8;
            let raw_size = slice::from_raw_parts(res, 4);
            let mut raw_len_buf = Cursor::new(&raw_size[0..4]);
            let slice = match raw_len_buf.read_u32::<NetworkEndian>() {
                Ok(size) => &slice::from_raw_parts(res, 4 + size as usize)[4..],
                _ => &[],
            };
            let mut ret = Vec::with_capacity($prefix.len() + slice.len());
            ret.extend_from_slice($prefix);
            ret.extend_from_slice(slice);
            freeCStr(res as *const i8);
            Arc::from(ret)
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

macro_rules! wrap_c_committee_call {
    ($self:ident, $c_call:expr) => {{
        let consensus = $self.consensus.load(Ordering::SeqCst);
        let result = unsafe { $c_call(consensus) };
        ConsensusIsInCommitteeResponse::try_from(result)
            .unwrap_or_else(|code| panic!("Unknown Consensus Committee FFI return code: {}", code))
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

/// Reads a const-sized number of bytes into an array.
macro_rules! read_const_sized {
    ($source:expr, $size:expr) => {{
        let mut buf = [0u8; $size as usize];
        $source.read_exact(&mut buf)?;

        buf
    }};
}

#[macro_use]
mod fails;
pub mod catch_up;
pub mod consensus;
pub mod ffi;
pub mod messaging;
pub mod transferlog;
