use concordium_common::hybrid_buf::HybridBuf;

use failure::Fallible;
use std::io::{Read, Seek, SeekFrom, Write};

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
const SNOW_MAXMSGLEN: usize = 65_535;
const SNOW_TAGLEN: usize = 16;

pub const MAX_NOISE_PROTOCOL_MESSAGE_LEN: usize = SNOW_MAXMSGLEN - SNOW_TAGLEN;

/// It tries to copy as much as possible from `input` to `output`, using chunks
/// of maximum `CHUNK_SIZE`. It is used with `socket` that blocks them when
/// their output buffers are full. Written bytes are consumed from `input`.
pub fn partial_copy(input: &mut HybridBuf, output: &mut impl Write) -> Fallible<usize> {
    let mut total_written_bytes = 0;
    let mut is_would_block = false;
    let mut chunk = [0u8; CHUNK_SIZE];

    while !is_would_block && !input.is_eof()? {
        let offset = input.position()?;

        let chunk_size = std::cmp::min(CHUNK_SIZE, (input.len()? - input.position()?) as usize);
        input.read_exact(&mut chunk[..chunk_size])?;

        match output.write(&chunk[..chunk_size]) {
            Ok(written_bytes) => {
                total_written_bytes += written_bytes;
                if written_bytes != chunk_size {
                    // Fix the offset because read data was not written completely.
                    input.seek(SeekFrom::Start(offset + written_bytes as u64))?;
                }
            }
            Err(io_err) => {
                input.seek(SeekFrom::Start(offset))?;
                is_would_block = io_err.kind() == std::io::ErrorKind::WouldBlock;
                if !is_would_block {
                    return Err(failure::Error::from(io_err));
                }
            }
        }
    }

    Ok(total_written_bytes)
}

pub const PROLOGUE: &[u8] = b"CP2P";
pub const PRE_SHARED_KEY: &[u8; 32] = b"54686973206973206d79204175737472";

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
