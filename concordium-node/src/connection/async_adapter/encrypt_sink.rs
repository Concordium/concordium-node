use crate::connection::async_adapter::{
    partial_copy, PayloadSize, Readiness, MAX_ENCRYPTED_CHUNK, MAX_NOISE_PROTOCOL_MESSAGE_LEN,
};

use concordium_common::UCursor;

use byteorder::{NetworkEndian, WriteBytesExt};
use failure::Fallible;
use snow::Session;

use std::{
    collections::VecDeque,
    convert::From,
    io::Write,
    sync::{Arc, Mutex},
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
    session:       Arc<Mutex<Session>>,
    messages:      VecDeque<UCursor>,
    written_bytes: usize,
}

impl EncryptSink {
    pub fn new(session: Arc<Mutex<Session>>) -> Self {
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
    fn encrypt_chunks(&self, input: &mut UCursor) -> Fallible<Vec<Vec<u8>>> {
        let expected_num_chunks = 1 + (input.len() / MAX_NOISE_PROTOCOL_MESSAGE_LEN as u64);
        let mut encrypted_chunks = Vec::with_capacity(expected_num_chunks as usize);

        let mut encrypted_output: [u8; MAX_ENCRYPTED_CHUNK] = unsafe { std::mem::uninitialized() };

        while !input.is_eof() {
            let view_size = std::cmp::min(
                MAX_NOISE_PROTOCOL_MESSAGE_LEN,
                (input.len() - input.position()) as usize,
            );
            let view = input.read_into_view(view_size)?;
            let len =
                safe_lock!(self.session)?.write_message(view.as_slice(), &mut encrypted_output)?;
            encrypted_chunks.push(encrypted_output[..len].to_vec());
        }

        Ok(encrypted_chunks)
    }

    /// It encrypts `input`, and returns an encrypted data with its *chunk
    /// table* as prefix.
    fn encrypt(&mut self, mut input: UCursor) -> Fallible<UCursor> {
        let encrypted_chunks = self.encrypt_chunks(&mut input)?;

        // 1. Frame: Size of Payload + Chunk table.
        let chunk_index_len = std::mem::size_of::<PayloadSize>() * encrypted_chunks.len();
        let frame_len = encrypted_chunks
            .iter()
            .fold(chunk_index_len as PayloadSize, |sum, chunk| {
                sum + chunk.len() as PayloadSize
            });
        let mut encrypted_frame =
            Vec::with_capacity(frame_len as usize + std::mem::size_of::<PayloadSize>());

        // 1.1. Add frame size.
        encrypted_frame.write_u32::<NetworkEndian>(frame_len)?;

        // 2. Write Chunk index table
        encrypted_frame.write_u32::<NetworkEndian>(encrypted_chunks.len() as PayloadSize)?;
        if encrypted_chunks.len() > 1 {
            for chunk in encrypted_chunks.iter() {
                encrypted_frame.write_u32::<NetworkEndian>(chunk.len() as PayloadSize)?;
            }

            trace!(
                "Encrypted a frame of {} bytes with chunk index: {:?}",
                frame_len,
                encrypted_chunks
                    .iter()
                    .map(Vec::len)
                    .collect::<Vec<usize>>()
            );
        } else {
            trace!(
                "Encrypted a frame of {} bytes without chunk index",
                frame_len
            );
        }

        // 3. Write each encrypted chunk.
        for chunk in encrypted_chunks.into_iter() {
            encrypted_frame.extend_from_slice(&chunk);
        }

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

            if encrypted.is_eof() {
                // The message has been fully processed.
                self.written_bytes = 0;
                Ok(Readiness::Ready(encrypted.len() as usize))
            } else {
                // Message is not completed... re-queue the rest of it.
                self.messages.push_front(encrypted);
                self.written_bytes += bytes;
                trace!(
                    "Sent {} encrypted bytes ({} accumulated)",
                    bytes,
                    self.written_bytes
                );
                Ok(Readiness::NotReady)
            }
        } else {
            // The message queue is empty.
            Ok(Readiness::Ready(0))
        }
    }
}
