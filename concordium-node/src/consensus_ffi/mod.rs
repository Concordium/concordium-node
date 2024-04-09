// ffi macros

macro_rules! wrap_send_data_to_c {
    ($self:ident, $genesis_index:expr, $data:expr, $c_call:expr) => {{
        let consensus = $self.consensus.load(Ordering::SeqCst);
        let len = $data.len();

        let result = unsafe { $c_call(consensus, $genesis_index, $data.as_ptr(), len as i64) };

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

pub mod catch_up;
pub mod consensus;
pub mod ffi;
pub mod helpers;
pub mod messaging;
