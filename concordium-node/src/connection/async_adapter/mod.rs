use concordium_common::UCursor;

use failure::Fallible;
use std::io::{Seek, SeekFrom, Write};

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

type PayloadSize = u32;

const CHUNK_SIZE: usize = 8_192; // 8KB

pub const MAX_NOISE_PROTOCOL_MESSAGE_LEN: usize = 65_535; // 65 kb
pub const MAX_ENCRYPTED_CHUNK: usize = MAX_NOISE_PROTOCOL_MESSAGE_LEN + 4_096;

/// It tries to copy as much as possible (or `CHUNK_SIZE`) from `input` to
/// `output`. It is used with `socket` that blocks them when their output
/// buffers are full. Written bytes are consumed from `input`.
pub fn partial_copy(input: &mut UCursor, output: &mut impl Write) -> Fallible<usize> {
    let chunk_size = std::cmp::min(CHUNK_SIZE, (input.len() - input.position()) as usize);

    let chunk = input.read_into_view(chunk_size)?;
    let written_bytes = output.write(chunk.as_slice())?;

    let offset = chunk.len() as i64 - written_bytes as i64;
    input.seek(SeekFrom::Current(offset))?;

    Ok(written_bytes)
}

/// It defines the default Noise parameters.
pub fn default_noise_params() -> snow::params::NoiseParams {
    "Noise_IKpsk2_25519_ChaChaPoly_BLAKE2b".parse().unwrap()
}

pub const PROLOGUE: &[u8] = b"CONCORDIUMP2P";
pub const PRE_SHARED_KEY: &[u8; 32] = b"54686973206973206d79204175737472";

macro_rules! map_io_error_to_fail {
    ($e:expr) => {
        $e.map_err(|io_err| {
            use crate::connection::fails::{StreamConnectionReset, StreamWouldBlock};
            use failure::Error;
            use std::io::ErrorKind;

            match io_err.kind() {
                ErrorKind::WouldBlock => Error::from(StreamWouldBlock),
                ErrorKind::ConnectionReset => Error::from(StreamConnectionReset),
                _ => Error::from_boxed_compat(Box::new(io_err)),
            }
        })
    };
}

mod handshake_stream_sink;
pub use handshake_stream_sink::HandshakeStreamSink;

mod decrypt_stream;
pub use decrypt_stream::DecryptStream;

mod encrypt_sink;
pub use encrypt_sink::EncryptSink;

mod frame_sink;
pub use frame_sink::FrameSink;

mod frame_stream;
pub use frame_stream::FrameStream;
