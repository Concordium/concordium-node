use failure::Fallible;
use std::io::{self, Read, Seek, SeekFrom, Write};

/// It allows to know the status of asynchronous operations.
///
/// Asynchronous operations like `write` and `flush` might not finish at its
/// first call. They return a `Readiness` object in order to know the status of
/// the operation:
///     - `Readiness::NotReady`: Operation is still on progress.
///     - `Readiness::Ready(bytes)`: Operation has finished successfully and
///       `bytes` bytes were
///     written into `output`.
///     - `Readiness::Ready(0)`: There are not pending operation in the internal
///       queue.
#[derive(Debug, Clone)]
pub enum Readiness<T> {
    Ready(T),
    NotReady,
}

impl<T> Readiness<T> {
    pub fn ready(self) -> Option<T> {
        match self {
            Readiness::Ready(v) => Some(v),
            _ => None,
        }
    }
}

pub type PayloadSize = u32;

pub const NOISE_MAX_MESSAGE_LEN: usize = 64 * 1024;
const NOISE_AUTH_TAG_LEN: usize = 16;
pub const NOISE_MAX_PAYLOAD_LEN: usize = NOISE_MAX_MESSAGE_LEN - NOISE_AUTH_TAG_LEN;

/// It tries to copy as much as possible from `input` to `output` in a
/// streaming fashion. It is used with `socket` that blocks them when
/// their output buffers are full. Written bytes are consumed from `input`.
pub fn partial_copy<R: Read + Seek, W: Write>(input: &mut R, output: &mut W) -> Fallible<usize> {
    let initial_pos = input.seek(SeekFrom::Current(0))?;

    match io::copy(input, output) {
        Ok(written) => Ok(written as usize),
        Err(err) => {
            if err.kind() == std::io::ErrorKind::WouldBlock {
                let curr_pos = input.seek(SeekFrom::Current(0))?;
                let written = curr_pos - initial_pos;
                Ok(written as usize)
            } else {
                Err(err.into())
            }
        }
    }
}

pub const PROLOGUE: &[u8] = b"CP2P";
pub const PRE_SHARED_KEY: &[u8; 32] = b"54686973206973206d79204175737472";

mod handshake_stream_sink;
pub use handshake_stream_sink::HandshakeStreamSink;

mod decrypt_stream;
pub use decrypt_stream::DecryptStream;

mod encrypt_sink;
pub use encrypt_sink::EncryptSink;

mod frame_stream;
pub use frame_stream::FrameStream;
