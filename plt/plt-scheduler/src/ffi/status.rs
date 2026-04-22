pub use plt_block_state::ffi::status::FfiStatusCode;

/// Helper function for wrapping calls with [`std::panic::catch_unwind`] then mapping a panic to the
/// correct status code and extracting the panic message.
///
/// # Arguments
///
/// - `function` The closure which might panic
pub fn catch_unwind<F>(function: F) -> (FfiStatusCode, Vec<u8>)
where
    F: FnOnce() -> (FfiStatusCode, Vec<u8>) + std::panic::UnwindSafe,
{
    std::panic::catch_unwind(function).unwrap_or_else(|err| {
        let data_out = if let Some(message) = err.downcast_ref::<String>() {
            message.clone().into_bytes()
        } else if let Some(message) = err.downcast_ref::<&str>() {
            message.to_string().into_bytes()
        } else {
            b"Unknown panic reason".to_vec()
        };
        (FfiStatusCode::Panic, data_out)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_catch_unwind_panics_str() {
        let (status, message) = catch_unwind(|| {
            panic!("my panic message");
        });
        assert_eq!(status, FfiStatusCode::Panic);
        assert_eq!(String::from_utf8(message).unwrap(), "my panic message");
    }

    #[test]
    fn test_catch_unwind_panics_string() {
        let (status, message) = catch_unwind(|| {
            panic!("my panic message {:?}", vec![5]);
        });
        assert_eq!(status, FfiStatusCode::Panic);
        assert_eq!(String::from_utf8(message).unwrap(), "my panic message [5]");
    }
}
