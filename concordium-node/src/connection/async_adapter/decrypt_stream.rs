use crate::connection::async_adapter::MAX_ENCRYPTED_CHUNK;
use concordium_common::UCursor;

use failure::Fallible;
use snow::Session;

use byteorder::{NetworkEndian, ReadBytesExt};
use std::{
    convert::From,
    sync::{Arc, Mutex},
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
    session: Arc<Mutex<Session>>,
}

impl DecryptStream {
    /// Session HAS to be shared by `decrypt` stream and `encrypt` sink.
    pub fn new(session: Arc<Mutex<Session>>) -> Self { DecryptStream { session } }

    /// It reads the chunk table and decodes each of them.
    ///
    /// # Return
    /// The decrypted message.
    fn decrypt(&self, mut input: UCursor) -> Fallible<UCursor> {
        // 1. Read the chunk table.
        let num_chunks = input.read_u32::<NetworkEndian>()?;
        let chunk_sizes = if num_chunks > 1 {
            // 1.1. Read each chunk size.
            (0..num_chunks)
                .map(|_| input.read_u32::<NetworkEndian>())
                .collect::<Result<Vec<u32>, _>>()?
        } else {
            // 1.2. Only one chunk, so list is omitted.
            vec![(input.len() - input.position()) as u32]
        };

        // 2. Load and decrypt each chunk.
        let mut clear_message = Vec::with_capacity(input.len() as usize);
        let mut clear_output: [u8; MAX_ENCRYPTED_CHUNK] = unsafe { std::mem::uninitialized() };

        for size in chunk_sizes.into_iter() {
            let encrypted_chunk_view = input.read_into_view(size as usize)?;
            let bytes = safe_lock!(self.session)?
                .read_message(encrypted_chunk_view.as_slice(), &mut clear_output)?;
            clear_message.extend_from_slice(&clear_output[..bytes]);
        }

        Ok(UCursor::from(clear_message))
    }

    /// It is just a helper function to keep a coherent interface.
    #[inline]
    pub fn read(&self, input: UCursor) -> Fallible<UCursor> { self.decrypt(input) }
}
