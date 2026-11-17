/// Returned status code used in FFI calls into this library.
///
/// This must match the `FFIStatusCode` type defined on the haskell side.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum FfiStatusCode {
    /// The call succeeded.
    Success = 0,
    /// The call failed gracefully.
    Failed = 1,
    /// The call resulted in a (caught) panic.
    Panic = 2,
}

/// Helper function for wrapping calls with [`std::panic::catch_unwind`] then mapping a panic to the
/// correct status code and extracting the panic message.
///
/// # Arguments
///
/// - `function` The closure which might panic
pub fn catch_unwind<F>(function: F) -> Option<String>
where
    F: std::panic::UnwindSafe + FnOnce(),
{
    if let Err(err) = std::panic::catch_unwind(function) {
        let message = if let Some(message) = err.downcast_ref::<String>() {
            message.clone()
        } else if let Some(message) = err.downcast_ref::<&str>() {
            message.to_string()
        } else {
            "Unknown panic reason".to_string()
        };
        Some(message)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_catch_unwind_panics_str() {
        let message = catch_unwind(|| {
            panic!("my panic message");
        });
        assert_eq!(message, Some("my panic message".to_string()));
    }

    #[test]
    fn test_catch_unwind_panics_string() {
        let message = catch_unwind(|| {
            panic!("my panic message {:?}", vec![5]);
        });
        assert_eq!(message, Some("my panic message [5]".to_string()));
    }
}
