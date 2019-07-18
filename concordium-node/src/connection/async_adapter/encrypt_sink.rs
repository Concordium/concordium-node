use crate::connection::async_adapter::{
    partial_copy, PayloadSize, Readiness, MAX_NOISE_PROTOCOL_MESSAGE_LEN, SNOW_MAXMSGLEN,
};

use concordium_common::UCursor;

use byteorder::{NetworkEndian, WriteBytesExt};
use failure::Fallible;
use rand::Rng;
use snow::Session;

use std::{
    collections::VecDeque,
    convert::From,
    io::Write,
    sync::{Arc, RwLock},
};

pub const MAX_ENCRYPTED_FRAME_IN_MEMORY: usize = 4_194_304; // 4MB

/// It is a `sink` that encrypts data using a `snow` session.
///
/// # Message queue
///
/// Each message is queued encrypted, in order to write into target output
/// `Write` (i.e. socket) once its internal buffer is free.
/// Subsequent calls to `flush` method will dump pending data into kernel
/// buffers.
///
/// # Large packets are stored on the disk
///
/// Next step after a packet encryption is send through the network
/// socket. That operation will require a lot of calls to `flush`, and it is
/// going to take a lot of time (in term of CPU time) until the full packet is
/// delivered. In order to *reduce* the memory footprint during the send
/// process, each packet larger than `MAX_ENCRYPTED_FRAME_IN_MEMORY`
/// will be transformed to a file cursor. `UCursor` uses a small buffer to
/// improve IO performance when data is stored on disk.
///
/// This memory improvement could be huge when a node is sending large packets
/// over a lot of connections.
///
/// # Noise 64kb message limit
///
/// See `DecryptStream`.
///
/// # TODO
///
/// ## Dump a large packet
///
/// This operation could take some time, so we should move it to another thread,
/// and re-queue for sending after completion.
///
/// ## Priority queue
///
/// Network packages should be prioritized.
pub struct EncryptSink {
    session:       Arc<RwLock<Session>>,
    messages:      VecDeque<UCursor>,
    written_bytes: usize,
}

impl EncryptSink {
    pub fn new(session: Arc<RwLock<Session>>) -> Self {
        EncryptSink {
            session,
            messages: VecDeque::new(),
            written_bytes: 0,
        }
    }

    /// It encrypts and enqueues new messages but it does not call flush.
    /// It should be used to add a *bunch of messages* without the overhead of
    /// flushing each of them.
    #[inline]
    pub fn write_without_flush(&mut self, input: UCursor) -> Fallible<()> {
        let encrypted = self.encrypt(input)?;
        self.messages.push_back(encrypted);
        Ok(())
    }

    /// It splits `input` into chunks (64kb max) and encrypts each of them.
    fn encrypt_chunks(&self, nonce: u64, input: &mut UCursor) -> Fallible<Vec<Vec<u8>>> {
        let expected_num_chunks = 1 + (input.len() / MAX_NOISE_PROTOCOL_MESSAGE_LEN as u64);
        let mut encrypted_chunks = Vec::with_capacity(expected_num_chunks as usize);

        let mut encrypted_output: [u8; SNOW_MAXMSGLEN] = unsafe { std::mem::uninitialized() };

        while !input.is_eof() {
            let view_size = std::cmp::min(
                MAX_NOISE_PROTOCOL_MESSAGE_LEN,
                (input.len() - input.position()) as usize,
            );
            let view = input.read_into_view(view_size)?;
            let len = write_or_die!(self.session).write_message_with_nonce(
                nonce,
                view.as_slice(),
                &mut encrypted_output,
            )?;
            encrypted_chunks.push(encrypted_output[..len].to_vec());
        }

        Ok(encrypted_chunks)
    }

    /// Frame length is:
    ///     - Size of NONCE: u64.
    ///     - Sum of each encrypted chunks.
    ///     - Size of chunk table: Number of full chunks + latest chunk size.
    fn calculate_frame_len(&self, chunks: &[Vec<u8>]) -> PayloadSize {
        let total_chunks_len: usize = chunks.iter().map(Vec::len).sum();
        let chunk_table_len = 2 * std::mem::size_of::<PayloadSize>();
        (std::mem::size_of::<u64>() + total_chunks_len + chunk_table_len) as PayloadSize
    }

    /// It encrypts `input`, and returns an encrypted data with its *chunk
    /// table* as prefix.
    fn encrypt(&mut self, mut input: UCursor) -> Fallible<UCursor> {
        let nonce = rand::thread_rng().gen::<u64>();
        let encrypted_chunks = self.encrypt_chunks(nonce, &mut input)?;

        // 1. Frame: Size of Payload + Chunk table.
        let frame_len = self.calculate_frame_len(&encrypted_chunks[..]);
        let total_len = frame_len as usize + std::mem::size_of::<PayloadSize>();
        let mut encrypted_frame = Vec::with_capacity(total_len);

        // 1.1. Add frame size.
        encrypted_frame.write_u32::<NetworkEndian>(frame_len)?;

        // 1.2. NONCE
        encrypted_frame.write_u64::<NetworkEndian>(nonce)?;

        // 2. Write Chunk index table: num of full chunks (64K) + size of latest
        // 2.1. Number of full chunks.
        let num_full_chunks = std::cmp::max(encrypted_chunks.len(), 1) - 1;
        encrypted_frame.write_u32::<NetworkEndian>(num_full_chunks as PayloadSize)?;

        // 2.2. Size of latest chunk.
        let last_chunk_size = encrypted_chunks.last().map_or(0, Vec::len) as PayloadSize;
        encrypted_frame.write_u32::<NetworkEndian>(last_chunk_size)?;

        trace!(
            "Encrypted a frame of {} bytes with chunk index: full chunks {}, latest size {}",
            frame_len,
            num_full_chunks,
            last_chunk_size
        );

        // 3. Write each encrypted chunk.
        for chunk in encrypted_chunks.into_iter() {
            encrypted_frame.extend_from_slice(&chunk);
        }
        debug_assert_eq!(encrypted_frame.len(), total_len);

        // 4. Move to disk if it is expensive in memory.
        let mut encrypted = UCursor::from(encrypted_frame);
        if encrypted.len() as usize > MAX_ENCRYPTED_FRAME_IN_MEMORY {
            encrypted.swap_to_file()?;
        }

        Ok(encrypted)
    }

    /// It encrypts `input` and queues it. It also calls `flush()` in order to
    /// start sending bytes or to keep sending any pending bytes from
    /// the previous package.
    ///
    /// # Return
    /// See `EncryptSink::flush`.
    pub fn write(&mut self, input: UCursor, output: &mut impl Write) -> Fallible<Readiness<usize>> {
        self.write_without_flush(input)?;
        self.flush(output)
    }

    /// It writes pending data into `output`.
    pub fn flush(&mut self, output: &mut impl Write) -> Fallible<Readiness<usize>> {
        if let Some(mut encrypted) = self.messages.pop_front() {
            // Get next message.
            let bytes = partial_copy(&mut encrypted, output)?;
            self.written_bytes += bytes;
            trace!(
                "Sent {} encrypted bytes ({} accumulated)",
                bytes,
                self.written_bytes
            );

            if encrypted.is_eof() {
                trace!("Sent completed with {}", self.written_bytes);

                // The message has been fully processed.
                self.written_bytes = 0;
                Ok(Readiness::Ready(encrypted.len() as usize))
            } else {
                // Message is not completed... re-queue the rest of it.
                self.messages.push_front(encrypted);
                Ok(Readiness::NotReady)
            }
        } else {
            // The message queue is empty.
            Ok(Readiness::Ready(0))
        }
    }
}
