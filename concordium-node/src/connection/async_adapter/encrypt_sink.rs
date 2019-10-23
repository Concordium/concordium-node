use byteorder::{NetworkEndian, WriteBytesExt};
use failure::Fallible;
use rand::Rng;
use snow::Session;

use crate::connection::async_adapter::{PayloadSize, NOISE_MAX_MESSAGE_LEN, NOISE_MAX_PAYLOAD_LEN};
use concordium_common::hybrid_buf::HybridBuf;

use std::{
    collections::VecDeque,
    io::{BufWriter, Read, Seek, SeekFrom, Write},
    mem,
};

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
    pub messages:               VecDeque<HybridBuf>,
    pub plaintext_chunk_buffer: Vec<u8>,
    pub encrypted_chunk_buffer: Vec<u8>,
    pub full_output_buffer:     BufWriter<HybridBuf>,
}

impl EncryptSink {
    pub fn new() -> Self {
        EncryptSink {
            messages:               VecDeque::new(),
            full_output_buffer:     BufWriter::new(Default::default()),
            plaintext_chunk_buffer: vec![0u8; NOISE_MAX_PAYLOAD_LEN],
            encrypted_chunk_buffer: vec![0u8; NOISE_MAX_MESSAGE_LEN],
        }
    }

    /// It encrypts and enqueues new messages but it does not call flush.
    /// It should be used to add a *bunch of messages* without the overhead of
    /// flushing each of them.
    #[inline]
    pub fn write_without_flush<R: Read + Seek>(
        &mut self,
        session: &Session,
        input: R,
    ) -> Fallible<()> {
        let encrypted = self.encrypt(session, input)?;
        self.messages.push_back(encrypted);
        Ok(())
    }

    /// It splits `input` into chunks (64kb max) and encrypts each of them.
    fn encrypt_chunks<R: Read + Seek>(
        &mut self,
        session: &Session,
        nonce: u64,
        input: &mut R,
    ) -> Fallible<usize> {
        let mut written = 0;

        let mut curr_pos = input.seek(SeekFrom::Current(0))?;
        let eof = input.seek(SeekFrom::End(0))?;
        input.seek(SeekFrom::Start(curr_pos))?;

        while curr_pos != eof {
            let chunk_size = std::cmp::min(NOISE_MAX_PAYLOAD_LEN, (eof - curr_pos) as usize);
            input.read_exact(&mut self.plaintext_chunk_buffer[..chunk_size])?;

            let len = session.write_message_with_nonce(
                nonce,
                &self.plaintext_chunk_buffer[..chunk_size],
                &mut self.encrypted_chunk_buffer,
            )?;

            written += self
                .full_output_buffer
                .write(&self.encrypted_chunk_buffer[..len])?;

            curr_pos = input.seek(SeekFrom::Current(0))?;
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
    fn encrypt<R: Read + Seek>(&mut self, session: &Session, mut input: R) -> Fallible<HybridBuf> {
        let nonce = rand::thread_rng().gen::<u64>();

        // write metadata placeholders
        self.full_output_buffer.write_all(&[0u8; 4 + 8 + 4 + 4])?;

        let encrypted_len = self.encrypt_chunks(session, nonce, &mut input)?;

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
        let num_full_chunks = encrypted_len / NOISE_MAX_MESSAGE_LEN;
        self.full_output_buffer
            .write_u32::<NetworkEndian>(num_full_chunks as PayloadSize)?;

        // 2.2. Size of the last chunk.
        let last_chunk_size = encrypted_len - (num_full_chunks * NOISE_MAX_MESSAGE_LEN);
        debug_assert!(last_chunk_size <= PayloadSize::max_value() as usize);
        self.full_output_buffer
            .write_u32::<NetworkEndian>(last_chunk_size as PayloadSize)?;

        trace!(
            "Encrypted a frame of {} bytes with chunk index: full chunks {}, latest size {}",
            frame_len,
            num_full_chunks,
            last_chunk_size
        );

        // rewind the buffer
        self.full_output_buffer.seek(SeekFrom::Start(0))?;

        let ret = mem::replace(self.full_output_buffer.get_mut(), Default::default());

        debug_assert_eq!(ret.len()?, total_len as u64);

        Ok(ret)
    }
}
