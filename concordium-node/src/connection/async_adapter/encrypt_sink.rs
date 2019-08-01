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
    mem,
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
    full_buffer:   Vec<u8>,
    chunk_buffer:  [u8; SNOW_MAXMSGLEN],
}

impl EncryptSink {
    pub fn new(session: Arc<RwLock<Session>>) -> Self {
        EncryptSink {
            session,
            messages: VecDeque::new(),
            written_bytes: 0,
            full_buffer: Vec::with_capacity(MAX_ENCRYPTED_FRAME_IN_MEMORY),
            chunk_buffer: [0u8; SNOW_MAXMSGLEN],
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
    fn encrypt_chunks(&mut self, nonce: u64, input: &mut UCursor) -> Fallible<()> {
        self.full_buffer.clear();

        let session_reader = read_or_die!(self.session);
        while !input.is_eof() {
            let view_size = std::cmp::min(
                MAX_NOISE_PROTOCOL_MESSAGE_LEN,
                (input.len() - input.position()) as usize,
            );
            let view = input.read_into_view(view_size)?;
            let view = view.as_slice();

            let len =
                session_reader.write_message_with_nonce(nonce, view, &mut self.chunk_buffer)?;
            self.full_buffer
                .extend_from_slice(&self.chunk_buffer[..len]);
        }

        Ok(())
    }

    /// Frame length is:
    ///     - Size of NONCE: u64.
    ///     - Sum of each encrypted chunks.
    ///     - Size of chunk table: Number of full chunks + latest chunk size.
    fn calculate_frame_len(&self) -> PayloadSize {
        let total_chunks_len = self.full_buffer.len();
        let chunk_table_len = 2 * mem::size_of::<PayloadSize>();
        (mem::size_of::<u64>() + total_chunks_len + chunk_table_len) as PayloadSize
    }

    /// It encrypts `input`, and returns an encrypted data with its *chunk
    /// table* as prefix.
    fn encrypt(&mut self, mut input: UCursor) -> Fallible<UCursor> {
        let nonce = rand::thread_rng().gen::<u64>();
        self.encrypt_chunks(nonce, &mut input)?;

        // 1. Frame: Size of Payload + Chunk table.
        let frame_len = self.calculate_frame_len();
        let total_len = frame_len as usize + mem::size_of::<PayloadSize>();
        let mut encrypted_frame = Vec::with_capacity(total_len);

        // 1.1. Add frame size.
        encrypted_frame.write_u32::<NetworkEndian>(frame_len)?;

        // 1.2. NONCE
        encrypted_frame.write_u64::<NetworkEndian>(nonce)?;

        // 2. Write Chunk index table: num of full chunks (64K) + size of latest
        // 2.1. Number of full chunks.
        let num_full_chunks = self.full_buffer.len() / SNOW_MAXMSGLEN;
        encrypted_frame.write_u32::<NetworkEndian>(num_full_chunks as PayloadSize)?;

        // 2.2. Size of the last chunk.
        let last_chunk_size = self.full_buffer.len() - (num_full_chunks * SNOW_MAXMSGLEN);
        debug_assert!(last_chunk_size <= PayloadSize::max_value() as usize);
        encrypted_frame.write_u32::<NetworkEndian>(last_chunk_size as PayloadSize)?;

        trace!(
            "Encrypted a frame of {} bytes with chunk index: full chunks {}, latest size {}",
            frame_len,
            num_full_chunks,
            last_chunk_size
        );

        // 3. Write encrypted chunks.
        encrypted_frame.extend_from_slice(&self.full_buffer);
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
