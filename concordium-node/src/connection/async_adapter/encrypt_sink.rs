use crate::connection::async_adapter::{
    partial_copy, PayloadSize, Readiness, MAX_NOISE_PROTOCOL_MESSAGE_LEN, SNOW_MAXMSGLEN,
};

use concordium_common::hybrid_buf::HybridBuf;

use byteorder::{NetworkEndian, WriteBytesExt};
use failure::Fallible;
use rand::Rng;
use snow::Session;

use std::{
    collections::VecDeque,
    convert::From,
    io::{BufWriter, Read, Seek, SeekFrom, Write},
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
/// will be transformed to a file cursor.
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
    session:                Arc<RwLock<Session>>,
    messages:               VecDeque<HybridBuf>,
    written_bytes:          usize,
    full_output_buffer:     BufWriter<HybridBuf>,
    plaintext_chunk_buffer: Vec<u8>,
    encrypted_chunk_buffer: Vec<u8>,
}

impl EncryptSink {
    pub fn new(session: Arc<RwLock<Session>>) -> Fallible<Self> {
        Ok(EncryptSink {
            session,
            messages: VecDeque::new(),
            written_bytes: 0,
            full_output_buffer: BufWriter::new(HybridBuf::with_capacity(
                MAX_ENCRYPTED_FRAME_IN_MEMORY,
            )?),
            plaintext_chunk_buffer: vec![0u8; MAX_NOISE_PROTOCOL_MESSAGE_LEN],
            encrypted_chunk_buffer: vec![0u8; SNOW_MAXMSGLEN],
        })
    }

    /// It encrypts and enqueues new messages but it does not call flush.
    /// It should be used to add a *bunch of messages* without the overhead of
    /// flushing each of them.
    #[inline]
    pub fn write_without_flush(&mut self, input: HybridBuf) -> Fallible<()> {
        let encrypted = self.encrypt(input)?;
        self.messages.push_back(encrypted);
        Ok(())
    }

    /// It splits `input` into chunks (64kb max) and encrypts each of them.
    fn encrypt_chunks(&mut self, nonce: u64, input: &mut HybridBuf) -> Fallible<usize> {
        let session_reader = read_or_die!(self.session);
        let mut written = 0;

        while !input.is_eof()? {
            let chunk_size = std::cmp::min(
                MAX_NOISE_PROTOCOL_MESSAGE_LEN,
                (input.len()? - input.position()?) as usize,
            );
            input.read_exact(&mut self.plaintext_chunk_buffer[..chunk_size])?;

            let len = session_reader.write_message_with_nonce(
                nonce,
                &self.plaintext_chunk_buffer[..chunk_size],
                &mut self.encrypted_chunk_buffer,
            )?;

            written += self
                .full_output_buffer
                .write(&self.encrypted_chunk_buffer[..len])?;
        }

        Ok(written)
    }

    /// Frame length is:
    ///     - Size of NONCE: u64.
    ///     - Size of chunk table: Number of full chunks + last chunk size.
    ///     - Sum of the lenghts of the encrypted chunks.
    fn calculate_frame_len(&self, encrypted_buffer_len: usize) -> Fallible<usize> {
        let chunk_table_len = 2 * mem::size_of::<PayloadSize>();
        Ok(mem::size_of::<u64>() + chunk_table_len + encrypted_buffer_len)
    }

    /// It encrypts `input`, and returns an encrypted data with its *chunk
    /// table* as prefix.
    fn encrypt(&mut self, mut input: HybridBuf) -> Fallible<HybridBuf> {
        let nonce = rand::thread_rng().gen::<u64>();

        // write metadata placeholders
        self.full_output_buffer.write_all(&[0u8; 4 + 8 + 4 + 4])?;

        let encrypted_len = self.encrypt_chunks(nonce, &mut input)?;

        // rewind the buffer to write the metadata
        self.full_output_buffer.seek(SeekFrom::Start(0))?;

        // 1. Frame: Size of Payload + Chunk table.
        let frame_len = self.calculate_frame_len(encrypted_len)?;
        let total_len = frame_len + mem::size_of::<PayloadSize>();

        // 1.1. Add frame size.
        self.full_output_buffer
            .write_u32::<NetworkEndian>(frame_len as PayloadSize)?;

        // 1.2. NONCE
        self.full_output_buffer.write_u64::<NetworkEndian>(nonce)?;

        // 2. Write Chunk index table: num of full chunks (64K) + size of latest
        // 2.1. Number of full chunks.
        let num_full_chunks = encrypted_len / SNOW_MAXMSGLEN;
        self.full_output_buffer
            .write_u32::<NetworkEndian>(num_full_chunks as PayloadSize)?;

        // 2.2. Size of the last chunk.
        let last_chunk_size = encrypted_len - (num_full_chunks * SNOW_MAXMSGLEN);
        debug_assert!(last_chunk_size <= PayloadSize::max_value() as usize);
        self.full_output_buffer
            .write_u32::<NetworkEndian>(last_chunk_size as PayloadSize)?;

        trace!(
            "Encrypted a frame of {} bytes with chunk index: full chunks {}, latest size {}",
            frame_len,
            num_full_chunks,
            last_chunk_size
        );

        // move to the end of the buffer
        self.full_output_buffer.seek(SeekFrom::Start(0))?;

        let ret = mem::replace(self.full_output_buffer.get_mut(), Default::default());

        debug_assert_eq!(ret.len()?, total_len as u64);

        Ok(ret)
    }

    /// It encrypts `input` and queues it. It also calls `flush()` in order to
    /// start sending bytes or to keep sending any pending bytes from
    /// the previous package.
    ///
    /// # Return
    /// See `EncryptSink::flush`.
    pub fn write(
        &mut self,
        input: HybridBuf,
        output: &mut impl Write,
    ) -> Fallible<Readiness<usize>> {
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

            if encrypted.is_eof()? {
                trace!("Sent completed with {}", self.written_bytes);

                // The message has been fully processed.
                self.written_bytes = 0;
                Ok(Readiness::Ready(encrypted.len()? as usize))
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
