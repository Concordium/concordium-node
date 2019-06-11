use crate::connection::async_adapter::{
    MAX_NOISE_PROTOCOL_MESSAGE_LEN, SNOW_MAXMSGLEN, SNOW_TAGLEN,
};
use concordium_common::UCursor;

use failure::Fallible;
use snow::Session;

use byteorder::{NetworkEndian, ReadBytesExt};
use std::{cell::RefCell, convert::From, rc::Rc};

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
    session: Rc<RefCell<Session>>,
}

impl DecryptStream {
    /// Session HAS to be shared by `decrypt` stream and `encrypt` sink.
    pub fn new(session: Rc<RefCell<Session>>) -> Self { DecryptStream { session } }

    fn decrypt_chunk(
        &self,
        chunk_idx: usize,
        chunk_size: usize,
        nonce: u64,
        input: &mut UCursor,
        clear_message: &mut Vec<u8>,
    ) -> Fallible<()> {
        let mut clear_chunk_buffer: [u8; SNOW_MAXMSGLEN] = unsafe { std::mem::uninitialized() };
        debug_assert!(chunk_size <= SNOW_MAXMSGLEN);

        let encrypted_chunk_view = input.read_into_view(chunk_size)?;
        let input_slice = encrypted_chunk_view.as_slice();
        let mut output_slice = &mut clear_chunk_buffer[..(chunk_size - SNOW_TAGLEN)];

        match self.session.borrow_mut().read_message_with_nonce(
            nonce,
            input_slice,
            &mut output_slice,
        ) {
            Ok(bytes) => {
                debug_assert!(
                    bytes <= chunk_size,
                    "Chunk {} bytes {} <= size {} fails",
                    chunk_idx,
                    bytes,
                    chunk_size
                );
                debug_assert!(bytes <= MAX_NOISE_PROTOCOL_MESSAGE_LEN);

                clear_message.extend_from_slice(&clear_chunk_buffer[..bytes]);
                Ok(())
            }
            Err(err) => {
                error!("Decrypt error at chunk {} fails: {}", chunk_idx, err);
                Err(failure::Error::from(err))
            }
        }
    }

    /// It reads the chunk table and decodes each of them.
    ///
    /// # Return
    /// The decrypted message.
    fn decrypt(&self, mut input: UCursor) -> Fallible<UCursor> {
        // 0. Read NONCE.
        let nonce = input.read_u64::<NetworkEndian>()?;

        // 1. Read the chunk table.
        let num_full_chunks = input.read_u32::<NetworkEndian>()? as usize;
        let last_chunk_size = input.read_u32::<NetworkEndian>()? as usize;

        // 2. Load and decrypt each chunk.
        let mut clear_message = Vec::with_capacity(input.len() as usize);

        for idx in 0..num_full_chunks {
            self.decrypt_chunk(idx, SNOW_MAXMSGLEN, nonce, &mut input, &mut clear_message)?;
        }

        if last_chunk_size > 0 {
            self.decrypt_chunk(
                num_full_chunks,
                last_chunk_size,
                nonce,
                &mut input,
                &mut clear_message,
            )?;
        }

        Ok(UCursor::from(clear_message))
    }

    /// It is just a helper function to keep a coherent interface.
    #[inline]
    pub fn read(&self, input: UCursor) -> Fallible<UCursor> { self.decrypt(input) }
}
