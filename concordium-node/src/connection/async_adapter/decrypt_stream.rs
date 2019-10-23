use crate::connection::async_adapter::{
    NOISE_AUTH_TAG_LEN, NOISE_MAX_MESSAGE_LEN, NOISE_MAX_PAYLOAD_LEN,
};
use concordium_common::hybrid_buf::HybridBuf;

use failure::Fallible;
use snow::Session;

use byteorder::{NetworkEndian, ReadBytesExt};
use std::{
    convert::From,
    io::{BufWriter, Read, Seek, SeekFrom, Write},
    mem,
};

/// It is a `stream` that decrypts data using `snow` session.
///
/// # Noise 64kb message limit
///
/// Noise protocol limits encryption/decryption to messages of 64Kb.
/// In order to support bigger payloads, we add a *chunk table*. Payload are
/// split into chunks of 64kb, and output message is prefixed with that table
/// which holds information about chunks. *Chunk table* is defined as:
///     - Number of chunks: as `unsigned of 32 bit` in `NetworkEndian`.
///     - List of the size of each chunk: as `unsigned of 32 bit` in
///       `NetworkEndian`. It is omitted if there is only one chunk.
pub struct DecryptStream {
    full_output_buffer:     BufWriter<HybridBuf>,
    encrypted_chunk_buffer: Vec<u8>,
    plaintext_chunk_buffer: Vec<u8>,
}

impl DecryptStream {
    /// Session HAS to be shared by `decrypt` stream and `encrypt` sink.
    pub fn new() -> Self {
        Self {
            full_output_buffer:     BufWriter::new(Default::default()),
            encrypted_chunk_buffer: vec![0; NOISE_MAX_MESSAGE_LEN],
            plaintext_chunk_buffer: vec![0; NOISE_MAX_PAYLOAD_LEN],
        }
    }

    /// It reads the chunk table and decodes each of them.
    ///
    /// # Return
    /// The decrypted message.
    pub fn decrypt<R: Read + Seek>(
        &mut self,
        session: &Session,
        mut input: R,
    ) -> Fallible<HybridBuf> {
        // 0. Read NONCE.
        let nonce = input.read_u64::<NetworkEndian>()?;

        // 1. Read the chunk table.
        let num_full_chunks = input.read_u32::<NetworkEndian>()? as usize;
        let last_chunk_size = input.read_u32::<NetworkEndian>()? as usize;

        for idx in 0..num_full_chunks {
            self.decrypt_chunk(session, idx, NOISE_MAX_MESSAGE_LEN, nonce, &mut input)?;
        }

        if last_chunk_size > 0 {
            self.decrypt_chunk(session, num_full_chunks, last_chunk_size, nonce, &mut input)?;
        }

        // rewind the buffer
        self.full_output_buffer.seek(SeekFrom::Start(0))?;

        Ok(mem::replace(
            &mut self.full_output_buffer.get_mut(),
            Default::default(),
        ))
    }

    fn decrypt_chunk<R: Read + Seek>(
        &mut self,
        session: &Session,
        chunk_idx: usize,
        chunk_size: usize,
        nonce: u64,
        input: &mut R,
    ) -> Fallible<()> {
        debug_assert!(chunk_size <= NOISE_MAX_MESSAGE_LEN);

        input.read_exact(&mut self.encrypted_chunk_buffer[..chunk_size])?;

        match session.read_message_with_nonce(
            nonce,
            &self.encrypted_chunk_buffer[..chunk_size],
            &mut self.plaintext_chunk_buffer[..(chunk_size - NOISE_AUTH_TAG_LEN)],
        ) {
            Ok(len) => {
                debug_assert!(
                    len <= chunk_size,
                    "Chunk {} bytes {} <= size {} fails",
                    chunk_idx,
                    len,
                    chunk_size
                );
                debug_assert!(len <= NOISE_MAX_PAYLOAD_LEN);

                self.full_output_buffer
                    .write_all(&self.plaintext_chunk_buffer[..len])?;
                Ok(())
            }
            Err(err) => {
                error!("Decryption error at chunk {}; fails: {}", chunk_idx, err);
                Err(failure::Error::from(err))
            }
        }
    }
}
