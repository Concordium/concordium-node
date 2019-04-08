#[cfg(not(target_os = "windows"))]
mod unix {
    use std::io::{ErrorKind, Result};
    use vecio::Rawv;

    lazy_static! {
        // NOTE: `slice.chunks` panics if `chunk_size` is 0.
        // On modern Linux system, the limit is 1024. Back in Linux 2.0 days, this limit was
        // 16, so let's keep that minimun when `sysconf` fails.
        // We can safely ignore the reason for this syscall failling, because this is just to keep
        // application maximun buffers aligned with kernel maximun buffers. In the worst case, it
        // will means just a potential network penalty on write operations, and more `WouldBlock`
        // events than expected for an specific operation.
        pub static ref IOV_MAX :usize = std::cmp::max(
                unsafe { libc::sysconf( libc::_SC_IOV_MAX as i32) },
                16) as usize;
    }

    pub struct WriteVAdapter<'a> {
        rawv: &'a mut dyn Rawv,
    }

    impl<'a> WriteVAdapter<'a> {
        #[inline]
        pub fn new(rawv: &'a mut dyn Rawv) -> WriteVAdapter<'a> { WriteVAdapter { rawv } }
    }

    impl<'a> rustls::WriteV for WriteVAdapter<'a> {
        fn writev(&mut self, bytes: &[&[u8]]) -> Result<usize> {
            let mut total = 0;

            // Create chunked of IOV_MAX buffers.
            for chunked_bytes in bytes.chunks(*IOV_MAX) {
                match self.rawv.writev(chunked_bytes) {
                    Ok(size) => total += size,
                    Err(writev_err) => {
                        if let ErrorKind::WouldBlock = writev_err.kind() {
                            // `WouldBlock` error forces us to wait until next 'Writable' event
                            // from poll. As soon as network layer is ready, new `Writable` event
                            // will be generated and flush will be called.
                            break;
                        } else {
                            // Any other error HAS TO be exposed.
                            return Err(writev_err);
                        }
                    }
                }
            }

            Ok(total)
        }
    }
}

#[cfg(not(target_os = "windows"))]
pub use self::unix::WriteVAdapter;
