// ffi macros

macro_rules! wrap_send_data_to_c {
    ($self:ident, $genesis_index:expr, $data:expr, $c_call:expr) => {{
        let consensus = $self.consensus.load(Ordering::SeqCst);
        let len = $data.len();

        #[allow(clippy::redundant_closure_call)] // allowed
        let result = unsafe { $c_call(consensus, $genesis_index, $data.as_ptr(), len as i64) };

        ConsensusFfiResponse::try_from(result)
            .unwrap_or_else(|code| panic!("Unknown FFI return code: {}", code))
    }};
}

pub mod catch_up;
pub mod consensus;
pub mod ffi;
pub mod helpers;
pub mod messaging;
