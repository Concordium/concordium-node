use byteorder::{ByteOrder, NetworkEndian, ReadBytesExt, WriteBytesExt};
use failure::{Error, Fallible};
use mio::tcp::TcpStream;
use rand::Rng;
use snow::{Keypair, Session};

use super::{
    fails::{MessageTooBigError, StreamWouldBlock},
    Connection, DeduplicationQueues,
};
use crate::network::PROTOCOL_MAX_MESSAGE_SIZE;
use concordium_common::hybrid_buf::HybridBuf;

use std::{
    collections::VecDeque,
    convert::TryFrom,
    io::{ErrorKind, Read, Seek, SeekFrom, Write},
    mem,
    pin::Pin,
    sync::Arc,
};

type PayloadSize = u32;

const PROLOGUE: &[u8] = b"CP2P";
const PRE_SHARED_KEY: &[u8; 32] = b"54686973206973206d79204175737472";
const NOISE_MAX_MESSAGE_LEN: usize = 64 * 1024 - 1; // 65535
const NOISE_AUTH_TAG_LEN: usize = 16;
const NOISE_MAX_PAYLOAD_LEN: usize = NOISE_MAX_MESSAGE_LEN - NOISE_AUTH_TAG_LEN;

/// The result of a socket operation.
#[derive(Debug, Clone)]
pub enum TcpResult<T> {
    /// For socket reads, `T` is the complete read message; for writes it's the
    /// number of written bytes
    Complete(T),
    /// Indicates that a read or write operation is incomplete and will be
    /// requeued
    Incomplete,
    /// A status dedicated to operations whose read/write result is of no
    /// interest or void
    Discarded,
    // The current read/write operation was aborted due to a `WouldBlock` error
    Aborted,
}

/// State of the *IKpsk2* handshake.
#[derive(Debug, PartialEq)]
pub enum HandshakeState {
    AwaitingPreSharedKey,
    AwaitingPublicKey,
    AwaitingMessageA,
    AwaitingMessageB,
    Complete,
}

pub struct ConnectionLowLevel {
    pub conn_ref: Option<Pin<Arc<Connection>>>,
    pub socket: TcpStream,
    keypair: Keypair,
    noise_session: Option<Session>,
    /// The buffer for reading straight from the socket
    input_buffer: [u8; NOISE_MAX_MESSAGE_LEN],
    /// The single message currently being read
    current_input: Vec<u8>,
    /// The number of bytes remaining to be appended to `current_input`
    pending_bytes: PayloadSize,
    /// A queue for outbound noise handshake messages
    handshake_queue: VecDeque<HybridBuf>,
    /// A queue for outbound messages queued during the noise handshake phase
    pending_output_queue: Vec<HybridBuf>,
    /// A queue for encrypted messages waiting to be written to the socket
    encrypted_queue: VecDeque<HybridBuf>,
    /// The current status of the noise handshake
    handshake_state: HandshakeState,
    /// A buffer for encrypted message chunks
    encrypted_chunk_buffer: Vec<u8>,
    /// A buffer for decrypted message chunks
    plaintext_chunk_buffer: Vec<u8>,
    /// A buffer for the full decrypted incoming message
    full_output_buffer: HybridBuf,
}

impl ConnectionLowLevel {
    fn conn(&self) -> &Connection {
        &self.conn_ref.as_ref().unwrap() // safe; always available
    }

    pub fn new(
        socket: TcpStream,
        keypair: Keypair,
        is_initiator: bool,
        noise_params: snow::params::NoiseParams,
    ) -> Self {
        if let Err(e) = socket.set_linger(Some(std::time::Duration::from_secs(0))) {
            error!(
                "Can't set SOLINGER to 0 for socket {:?} due to {}",
                socket, e
            );
        }

        let noise_session = if is_initiator {
            trace!("I'm the noise session initiator");
            None
        } else {
            trace!("I'm the noise session responder");
            Some(
                snow::Builder::new(noise_params)
                    .prologue(PROLOGUE)
                    .psk(2, PRE_SHARED_KEY)
                    .local_private_key(&keypair.private)
                    .build_responder()
                    .expect("Can't build a snow session!"),
            )
        };

        let mut handshake_queue = VecDeque::with_capacity(4);
        let handshake_state = if is_initiator {
            trace!("I'm sending my pre-shared static key");
            handshake_queue.push_back(create_frame(&[]).unwrap()); // infallible
            HandshakeState::AwaitingPublicKey
        } else {
            trace!("I'm awaiting the pre-shared static key");
            HandshakeState::AwaitingPreSharedKey
        };

        ConnectionLowLevel {
            conn_ref: None,
            socket,
            keypair,
            noise_session,
            current_input: Vec::with_capacity(NOISE_MAX_PAYLOAD_LEN),
            pending_bytes: 0,
            handshake_queue,
            pending_output_queue: Vec::with_capacity(4),
            encrypted_queue: VecDeque::with_capacity(16),
            handshake_state,
            input_buffer: [0u8; NOISE_MAX_MESSAGE_LEN],
            encrypted_chunk_buffer: vec![0u8; NOISE_MAX_MESSAGE_LEN],
            plaintext_chunk_buffer: vec![0; NOISE_MAX_PAYLOAD_LEN],
            full_output_buffer: Default::default(),
        }
    }

    // handshake

    fn initiator_got_public_key(&mut self, mut input: HybridBuf) -> Fallible<()> {
        trace!("I've received the peer's public key");
        let remote_public_key_vw = input.remaining_bytes()?;

        let mut session = snow::Builder::new(
            self.conn()
                .handler()
                .connection_handler
                .noise_params
                .clone(),
        )
        .prologue(PROLOGUE)
        .psk(2, PRE_SHARED_KEY)
        .local_private_key(&self.keypair.private)
        .remote_public_key(&remote_public_key_vw)
        .build_initiator()?;

        trace!("I'm sending Ikpsk2 message A");
        let msg_len = session.write_message(&[], &mut self.input_buffer)?;
        self.handshake_queue
            .push_back(create_frame(&self.input_buffer[..msg_len])?);

        self.handshake_state = HandshakeState::AwaitingMessageB;
        self.noise_session = Some(session);

        Ok(())
    }

    fn responder_got_psk(&mut self) -> Fallible<()> {
        trace!("I've received the pre-shared static key");
        trace!("I'm sending my public key");
        self.handshake_queue
            .push_back(create_frame(&self.keypair.public)?);

        // Next state
        self.handshake_state = HandshakeState::AwaitingMessageA;

        Ok(())
    }

    fn responder_got_message_a(&mut self, mut input: HybridBuf) -> Fallible<()> {
        trace!("I've received Ikpsk2 message A");
        if let Some(mut session) = self.noise_session.take() {
            let e_es_s_ss = input.remaining_bytes()?;
            session.read_message(&e_es_s_ss, &mut self.input_buffer)?;

            trace!("I'm sending Ikpsk2 message B");
            let msg_len = session.write_message(&[], &mut self.input_buffer)?;
            self.handshake_queue
                .push_back(create_frame(&self.input_buffer[..msg_len])?);

            self.noise_session = Some(session.into_stateless_transport_mode()?);
            self.handshake_state = HandshakeState::Complete;

            Ok(())
        } else {
            unreachable!("Handshake logic error");
        }
    }

    fn initiator_got_message_b(&mut self, mut input: HybridBuf) -> Fallible<()> {
        trace!("I've received Ikpsk2 message B");
        if let Some(mut session) = self.noise_session.take() {
            let e_ee_se_psk = input.remaining_bytes()?;
            session.read_message(&e_ee_se_psk, &mut self.input_buffer)?;

            self.noise_session = Some(session.into_stateless_transport_mode()?);
            self.handshake_state = HandshakeState::Complete;

            Ok(())
        } else {
            unreachable!("Handshake logic error");
        }
    }

    fn read_handshake_msg(&mut self, input: HybridBuf) -> Fallible<()> {
        use HandshakeState::*;

        match self.handshake_state {
            AwaitingPreSharedKey => self.responder_got_psk(),
            AwaitingPublicKey => self.initiator_got_public_key(input),
            AwaitingMessageA => self.responder_got_message_a(input),
            AwaitingMessageB => self.initiator_got_message_b(input),
            Complete => unreachable!("Handshake logic error"),
        }?;

        while !self.handshake_queue.is_empty() {
            self.flush_handshaker()?;
        }

        // once the noise handshake is complete, send out any pending messages
        // we might have queued during the process
        if self.handshake_state == HandshakeState::Complete {
            for frame in mem::replace(&mut self.pending_output_queue, Default::default()) {
                self.write_to_socket(frame)?;
            }
        }

        Ok(())
    }

    // input

    /// Keeps reading from the socket as long as there is data to be read.
    #[inline(always)]
    pub fn read_stream(&mut self, deduplication_queues: &mut DeduplicationQueues) -> Fallible<()> {
        loop {
            match self.read_from_socket() {
                Ok(read_result) => match read_result {
                    TcpResult::Complete(message) => {
                        self.conn().send_to_dump(&message, true);
                        if let Err(e) = self.conn().process_message(message, deduplication_queues) {
                            bail!("can't process a message: {}", e);
                        }
                    }
                    TcpResult::Discarded => {}
                    TcpResult::Incomplete | TcpResult::Aborted => return Ok(()),
                },
                Err(e) => bail!("can't read from the socket: {}", e),
            }
        }
    }

    /// Attempts to read a complete message from the socket.
    #[inline(always)]
    fn read_from_socket(&mut self) -> Fallible<TcpResult<HybridBuf>> {
        trace!("Attempting to read from the socket");
        let read_result = if self.pending_bytes == 0 {
            self.read_expected_size()
        } else {
            self.read_payload()
        };

        match read_result {
            Ok(TcpResult::Complete(payload)) => {
                trace!("A {}B message was fully read", payload.len()?);
                self.forward(payload)
            }
            Ok(TcpResult::Incomplete) => {
                trace!("The current message is incomplete");
                Ok(TcpResult::Incomplete)
            }
            Ok(_) => unreachable!(),
            Err(err) => {
                if err.downcast_ref::<StreamWouldBlock>().is_some() {
                    trace!("Further reads would be blocking; aborting");
                    Ok(TcpResult::Aborted)
                } else {
                    Err(err)
                }
            }
        }
    }

    fn forward(&mut self, input: HybridBuf) -> Fallible<TcpResult<HybridBuf>> {
        if self.handshake_state == HandshakeState::Complete {
            Ok(TcpResult::Complete(self.decrypt(input)?))
        } else {
            self.read_handshake_msg(input)?;

            Ok(TcpResult::Discarded)
        }
    }

    /// It allows packet with empty payloads.
    #[inline]
    fn pending_bytes_to_know_expected_size(&self) -> usize {
        if self.current_input.len() < std::mem::size_of::<PayloadSize>() {
            std::mem::size_of::<PayloadSize>() - self.current_input.len()
        } else {
            0
        }
    }

    /// It first reads the first 4 bytes of the message to determine its size.
    fn read_expected_size(&mut self) -> Fallible<TcpResult<HybridBuf>> {
        // only extract the bytes needed to know the size.
        let min_bytes = self.pending_bytes_to_know_expected_size();
        let read_bytes =
            map_io_error_to_fail!(self.socket.read(&mut self.input_buffer[..min_bytes]))?;

        self.current_input
            .extend_from_slice(&self.input_buffer[..read_bytes]);

        // once the number of bytes needed to read the message size is known, continue
        if self.current_input.len() == std::mem::size_of::<PayloadSize>() {
            let expected_size =
                NetworkEndian::read_u32(&self.current_input[..std::mem::size_of::<PayloadSize>()]);
            trace!("Expecting a message of {}B", expected_size);

            // remove the length from the buffer
            self.current_input.clear();

            // check if the expected size doesn't exceed the protocol limit
            if expected_size > PROTOCOL_MAX_MESSAGE_SIZE as PayloadSize {
                let error = MessageTooBigError {
                    expected_size,
                    protocol_size: PROTOCOL_MAX_MESSAGE_SIZE as PayloadSize,
                };
                return Err(Error::from(error));
            } else {
                self.pending_bytes = expected_size;
            }

            // Read data next...
            self.read_payload()
        } else {
            // We need more data to determine the message size.
            Ok(TcpResult::Incomplete)
        }
    }

    /// Once we know the message expected size, we can start to receive data.
    fn read_payload(&mut self) -> Fallible<TcpResult<HybridBuf>> {
        while self.pending_bytes > 0 {
            if self.read_intermediate()? == 0 {
                break;
            }
        }

        if self.pending_bytes == 0 {
            // Ready message
            let new_data = std::mem::replace(
                &mut self.current_input,
                Vec::with_capacity(std::mem::size_of::<PayloadSize>()),
            );

            Ok(TcpResult::Complete(HybridBuf::try_from(new_data)?))
        } else {
            Ok(TcpResult::Incomplete)
        }
    }

    fn read_intermediate(&mut self) -> Fallible<usize> {
        trace!("Reading a message ({}B remaining)", self.pending_bytes);
        let read_size = std::cmp::min(self.pending_bytes as usize, NOISE_MAX_MESSAGE_LEN);

        match self.socket.read(&mut self.input_buffer[..read_size]) {
            Ok(read_bytes) => {
                // trace!("Read {}B", read_bytes);
                self.current_input
                    .extend_from_slice(&self.input_buffer[..read_bytes]);
                self.pending_bytes -= read_bytes as PayloadSize;

                Ok(read_bytes)
            }
            Err(err) => match err.kind() {
                ErrorKind::WouldBlock => {
                    trace!("This read would be blocking; aborting");
                    Ok(0)
                }
                _ => Err(Error::from(err)),
            },
        }
    }

    /// It reads the chunk table and decrypts the chunks.
    pub fn decrypt<R: Read + Seek>(&mut self, mut input: R) -> Fallible<HybridBuf> {
        // 0. Read NONCE.
        let nonce = input.read_u64::<NetworkEndian>()?;
        trace!("Commencing decryption with nonce {:x}", nonce);

        // 1. Read the chunk table.
        let num_full_chunks = input.read_u32::<NetworkEndian>()? as usize;
        trace!("There are {} full chunks to decrypt", num_full_chunks);
        let last_chunk_size = input.read_u32::<NetworkEndian>()? as usize;
        let num_all_chunks = if last_chunk_size > 0 {
            trace!("The last chunk is {}B long", last_chunk_size);
            num_full_chunks + 1
        } else {
            trace!("All the chunks are full");
            num_full_chunks
        };

        for idx in 0..num_full_chunks {
            trace!(
                "Decrypting chunk {} ({} remaining)",
                idx,
                num_all_chunks - idx
            );
            self.decrypt_chunk(idx, NOISE_MAX_MESSAGE_LEN, nonce, &mut input)?;
        }

        if last_chunk_size > 0 {
            trace!("Decrypting the last chunk ({}B)", last_chunk_size);
            self.decrypt_chunk(num_full_chunks, last_chunk_size, nonce, &mut input)?;
        }

        // rewind the buffer
        self.full_output_buffer.seek(SeekFrom::Start(0))?;

        Ok(mem::replace(
            &mut self.full_output_buffer,
            Default::default(),
        ))
    }

    fn decrypt_chunk<R: Read + Seek>(
        &mut self,
        chunk_idx: usize,
        chunk_size: usize,
        nonce: u64,
        input: &mut R,
    ) -> Fallible<()> {
        debug_assert!(chunk_size <= NOISE_MAX_MESSAGE_LEN);

        input.read_exact(&mut self.encrypted_chunk_buffer[..chunk_size])?;

        match self
            .noise_session
            .as_ref()
            .unwrap() // infallible
            .read_message_with_nonce(
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
                error!("Decryption error: {}", err);
                Err(failure::Error::from(err))
            }
        }
    }

    // output

    #[inline(always)]
    pub fn write_to_socket(&mut self, input: HybridBuf) -> Fallible<TcpResult<usize>> {
        if self.handshake_state == HandshakeState::Complete {
            let encrypted_chunks = self.encrypt(input)?;
            for chunk in encrypted_chunks {
                self.encrypted_queue.push_back(chunk);
            }
            self.flush_encryptor()
        } else {
            self.pending_output_queue.push(input);
            Ok(TcpResult::Discarded)
        }
    }

    #[inline(always)]
    pub fn flush_socket(&mut self) -> Fallible<TcpResult<usize>> {
        if self.handshake_state == HandshakeState::Complete {
            self.flush_encryptor()
        } else {
            self.flush_handshaker()
        }
    }

    fn flush_handshaker(&mut self) -> Fallible<TcpResult<usize>> {
        let mut written_bytes = 0;

        while let Some(mut data) = self.handshake_queue.pop_front() {
            trace!("Pushing a handshake message to the socket");
            written_bytes += partial_copy(&mut data, &mut self.socket)?;

            if data.is_eof()? {
                trace!("Successfully sent a chunk of a handshake message");
            } else {
                trace!("The message was not fully written; requeuing");
                self.handshake_queue.push_front(data);
                return Ok(TcpResult::Incomplete);
            }
        }

        Ok(TcpResult::Complete(written_bytes))
    }

    #[inline(always)]
    fn flush_encryptor(&mut self) -> Fallible<TcpResult<usize>> {
        let mut written_bytes = 0;

        while let Some(mut encrypted) = self.encrypted_queue.pop_front() {
            trace!(
                "Writing a {}B message to the socket",
                encrypted.remaining_len()?
            );
            written_bytes += partial_copy(&mut encrypted, &mut self.socket)?;

            if encrypted.is_eof()? {
                trace!("Successfully written an encrypted message");
            } else {
                trace!(
                    "Incomplete write ({}B remaining); requeuing",
                    encrypted.remaining_len()?
                );
                self.encrypted_queue.push_front(encrypted);
                return Ok(TcpResult::Incomplete);
            }
        }

        Ok(TcpResult::Complete(written_bytes))
    }

    /// It splits `input` into chunks of `NOISE_MAX_PAYLOAD_LEN` and encrypts
    /// each of them.
    fn encrypt_chunks<R: Read + Seek>(
        &mut self,
        nonce: u64,
        input: &mut R,
        chunks: &mut Vec<HybridBuf>,
    ) -> Fallible<usize> {
        let mut written = 0;

        let mut curr_pos = input.seek(SeekFrom::Current(0))?;
        let eof = input.seek(SeekFrom::End(0))?;
        input.seek(SeekFrom::Start(curr_pos))?;

        let mut idx: usize = 0;

        while curr_pos != eof {
            trace!("Encrypting chunk {} ({}B remaining)", idx, eof - curr_pos);

            let chunk_size = std::cmp::min(NOISE_MAX_PAYLOAD_LEN, (eof - curr_pos) as usize);
            input.read_exact(&mut self.plaintext_chunk_buffer[..chunk_size])?;

            let len = self
                .noise_session
                .as_ref()
                .unwrap() // infallible
                .write_message_with_nonce(
                    nonce,
                    &self.plaintext_chunk_buffer[..chunk_size],
                    &mut self.encrypted_chunk_buffer,
                )?;

            let mut chunk = HybridBuf::with_capacity(len)?;
            let wrote = chunk.write(&self.encrypted_chunk_buffer[..len])?;
            chunk.rewind()?;

            chunks.push(chunk);

            // trace!("Encrypted a chunk of {} + 16B", wrote - 16);

            written += wrote;

            curr_pos = input.seek(SeekFrom::Current(0))?;
            idx += 1;
        }

        Ok(written)
    }

    /// Frame length is:
    ///     - Size of the nonce: u64.
    ///     - Size of chunk table: Number of full chunks + last chunk size.
    ///     - Sum of the lenghts of the encrypted chunks.
    fn calculate_frame_len(&self, encrypted_buffer_len: usize) -> Fallible<usize> {
        let chunk_table_len = 2 * mem::size_of::<PayloadSize>();
        Ok(mem::size_of::<u64>() + chunk_table_len + encrypted_buffer_len)
    }

    /// It encrypts `input` and returns the encrypted chunks preceded by the
    /// metadata chunk.
    pub fn encrypt<R: Read + Seek>(&mut self, mut input: R) -> Fallible<Vec<HybridBuf>> {
        let nonce = rand::thread_rng().gen::<u64>();
        trace!("Commencing encryption with nonce {:x}", nonce);

        let mut chunks = Vec::with_capacity(2);

        // create the metadata chunk
        let metadata = HybridBuf::with_capacity(4 + 8 + 4 + 4)?;
        chunks.push(metadata);

        let encrypted_len = self.encrypt_chunks(nonce, &mut input, &mut chunks)?;

        // 1. frame: size of payload + chunk table
        let frame_len = self.calculate_frame_len(encrypted_len)?;

        // 1.1. frame size
        chunks[0].write_u32::<NetworkEndian>(frame_len as PayloadSize)?;

        // 1.2. nonce
        chunks[0].write_u64::<NetworkEndian>(nonce)?;

        // 1.3. number of full chunks
        let num_full_chunks = encrypted_len / NOISE_MAX_MESSAGE_LEN;
        chunks[0].write_u32::<NetworkEndian>(num_full_chunks as PayloadSize)?;

        // 1.4. size of the last chunk
        let last_chunk_size = encrypted_len - (num_full_chunks * NOISE_MAX_MESSAGE_LEN);
        debug_assert!(last_chunk_size <= PayloadSize::max_value() as usize);
        chunks[0].write_u32::<NetworkEndian>(last_chunk_size as PayloadSize)?;

        chunks[0].rewind()?;

        trace!(
            "Encrypted a frame of {}B; full chunks: {}, last chunk size: {}",
            frame_len,
            num_full_chunks,
            last_chunk_size
        );

        Ok(chunks)
    }
}

/// It tries to copy as much as possible from `input` to `output` in a
/// streaming fashion. It is used with `socket` that blocks them when
/// their output buffers are full. Written bytes are consumed from `input`.
fn partial_copy<W: Write>(input: &mut HybridBuf, output: &mut W) -> Fallible<usize> {
    const BUF_SIZE: usize = 16 * 1024;
    let mut total_written_bytes = 0;
    let mut is_would_block = false;
    let mut chunk = [0u8; BUF_SIZE];

    while !is_would_block && !input.is_eof()? {
        let offset = input.position()?;

        let chunk_size = std::cmp::min(BUF_SIZE, (input.len()? - input.position()?) as usize);
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

/// It prefixes `data` with its length, encoded as `u32` in `NetworkEndian`.
fn create_frame(data: &[u8]) -> Fallible<HybridBuf> {
    let mut frame = Vec::with_capacity(data.len() + std::mem::size_of::<PayloadSize>());
    frame.write_u32::<NetworkEndian>(data.len() as u32)?;
    frame.extend_from_slice(data);

    into_err!(HybridBuf::try_from(frame))
}
