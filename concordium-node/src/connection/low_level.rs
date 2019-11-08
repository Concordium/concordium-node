use byteorder::{ByteOrder, NetworkEndian, WriteBytesExt};
use failure::{Error, Fallible};
use mio::tcp::TcpStream;
use snow::{Keypair, Session};

use super::{
    fails::{MessageTooBigError, StreamWouldBlock},
    Connection, DeduplicationQueues,
};
use crate::{common::counter::TOTAL_MESSAGES_SENT_COUNTER, network::PROTOCOL_MAX_MESSAGE_SIZE};
use concordium_common::hybrid_buf::HybridBuf;

use std::{
    collections::VecDeque,
    convert::TryFrom,
    io::{Cursor, ErrorKind, Read, Seek, SeekFrom, Write},
    mem,
    pin::Pin,
    sync::{atomic::Ordering, Arc},
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
enum HandshakeState {
    AwaitingPreSharedKey,
    AwaitingPublicKey,
    AwaitingMessageA,
    AwaitingMessageB,
    Complete,
}

/// The single message currently being read from the socket along with its
/// pending length.
#[derive(Default)]
struct IncomingMessage {
    pending_bytes: PayloadSize,
    message:       Vec<u8>,
}

pub struct ConnectionLowLevel {
    pub conn_ref: Option<Pin<Arc<Connection>>>,
    pub socket: TcpStream,
    keypair: Keypair,
    noise_session: Option<Session>,
    /// The input nonce
    input_nonce: u64,
    /// The output nonce
    output_nonce: u64,
    /// The buffer for reading/writing raw noise messages
    noise_msg_buffer: [u8; NOISE_MAX_MESSAGE_LEN],
    /// The single message currently being read
    incoming_msg: IncomingMessage,
    /// The current status of the noise handshake
    handshake_state: HandshakeState,
    /// A queue for messages waiting to be written to the socket
    output_queue: VecDeque<Cursor<Vec<u8>>>,
    /// A buffer for decrypted/unencrypted message chunks
    plaintext_chunk_buffer: [u8; NOISE_MAX_PAYLOAD_LEN],
}

impl ConnectionLowLevel {
    pub fn conn(&self) -> &Connection {
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

        let mut output_queue = VecDeque::with_capacity(16);

        let (noise_session, handshake_state) = if is_initiator {
            trace!("I'm the noise session initiator; sending my pre-shared static key");
            output_queue.push_back(create_frame(&[]).unwrap()); // infallible

            (None, HandshakeState::AwaitingPublicKey)
        } else {
            trace!("I'm the noise session responder; awaiting the pre-shared static key");

            (
                Some(
                    snow::Builder::new(noise_params)
                        .prologue(PROLOGUE)
                        .psk(2, PRE_SHARED_KEY)
                        .local_private_key(&keypair.private)
                        .build_responder()
                        .expect("Can't build a snow session!"),
                ),
                HandshakeState::AwaitingPreSharedKey,
            )
        };

        ConnectionLowLevel {
            conn_ref: None,
            socket,
            keypair,
            noise_session,
            input_nonce: 0,
            output_nonce: 0,
            incoming_msg: IncomingMessage::default(),
            output_queue,
            handshake_state,
            noise_msg_buffer: [0u8; NOISE_MAX_MESSAGE_LEN],
            plaintext_chunk_buffer: [0u8; NOISE_MAX_PAYLOAD_LEN],
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
        let msg_len = session.write_message(&[], &mut self.noise_msg_buffer)?;
        self.output_queue
            .push_back(create_frame(&self.noise_msg_buffer[..msg_len])?);

        self.handshake_state = HandshakeState::AwaitingMessageB;
        self.noise_session = Some(session);

        Ok(())
    }

    fn responder_got_psk(&mut self) -> Fallible<()> {
        trace!("I've received the pre-shared static key");
        trace!("I'm sending my public key");
        self.output_queue
            .push_back(create_frame(&self.keypair.public)?);

        // Next state
        self.handshake_state = HandshakeState::AwaitingMessageA;

        Ok(())
    }

    fn responder_got_message_a(&mut self, mut input: HybridBuf) -> Fallible<()> {
        trace!("I've received Ikpsk2 message A");
        if let Some(mut session) = self.noise_session.take() {
            let e_es_s_ss = input.remaining_bytes()?;
            session.read_message(&e_es_s_ss, &mut self.noise_msg_buffer)?;

            trace!("I'm sending Ikpsk2 message B");
            let msg_len = session.write_message(&[], &mut self.noise_msg_buffer)?;
            self.output_queue
                .push_back(create_frame(&self.noise_msg_buffer[..msg_len])?);

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
            session.read_message(&e_ee_se_psk, &mut self.noise_msg_buffer)?;

            self.noise_session = Some(session.into_stateless_transport_mode()?);
            self.handshake_state = HandshakeState::Complete;

            // send a high-level handshake request
            self.conn().send_handshake_request()?;

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

        self.flush_socket()?;

        Ok(())
    }

    // input

    /// Keeps reading from the socket as long as there is data to be read.
    #[inline(always)]
    pub fn read_stream(&mut self, deduplication_queues: &DeduplicationQueues) -> Fallible<()> {
        loop {
            match self.read_from_socket() {
                Ok(read_result) => match read_result {
                    TcpResult::Complete(message) => {
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
        let read_result = if self.incoming_msg.pending_bytes == 0 {
            self.read_expected_size()
        } else {
            self.read_payload()
        };

        match read_result {
            Ok(TcpResult::Complete(payload)) => {
                let len = payload.len()?;
                trace!("A {}B message was fully read", len);
                self.forward(payload, len as usize)
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

    fn forward(&mut self, input: HybridBuf, len: usize) -> Fallible<TcpResult<HybridBuf>> {
        if self.handshake_state == HandshakeState::Complete {
            Ok(TcpResult::Complete(self.decrypt(input, len)?))
        } else {
            self.read_handshake_msg(input)?;

            Ok(TcpResult::Discarded)
        }
    }

    /// Reads the number of bytes required to read the frame length
    #[inline]
    fn pending_bytes_to_know_expected_size(&self) -> usize {
        if self.incoming_msg.message.len() < std::mem::size_of::<PayloadSize>() {
            std::mem::size_of::<PayloadSize>() - self.incoming_msg.message.len()
        } else {
            0
        }
    }

    /// It first reads the first 4 bytes of the message to determine its size.
    fn read_expected_size(&mut self) -> Fallible<TcpResult<HybridBuf>> {
        // only extract the bytes needed to know the size.
        let min_bytes = self.pending_bytes_to_know_expected_size();
        let read_bytes =
            map_io_error_to_fail!(self.socket.read(&mut self.noise_msg_buffer[..min_bytes]))?;

        self.incoming_msg
            .message
            .extend_from_slice(&self.noise_msg_buffer[..read_bytes]);

        // once the number of bytes needed to read the message size is known, continue
        if self.incoming_msg.message.len() == std::mem::size_of::<PayloadSize>() {
            let expected_size = NetworkEndian::read_u32(
                &self.incoming_msg.message[..std::mem::size_of::<PayloadSize>()],
            );
            trace!("Expecting a message of {}B", expected_size);

            // remove the length from the buffer
            self.incoming_msg.message.clear();

            // check if the expected size doesn't exceed the protocol limit
            if expected_size > PROTOCOL_MAX_MESSAGE_SIZE as PayloadSize {
                let error = MessageTooBigError {
                    expected_size,
                    protocol_size: PROTOCOL_MAX_MESSAGE_SIZE as PayloadSize,
                };
                return Err(Error::from(error));
            } else {
                self.incoming_msg.pending_bytes = expected_size;
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
        while self.incoming_msg.pending_bytes > 0 {
            if self.read_intermediate()? == 0 {
                break;
            }
        }

        if self.incoming_msg.pending_bytes == 0 {
            // Ready message
            let new_data = std::mem::replace(&mut self.incoming_msg.message, Vec::new());

            Ok(TcpResult::Complete(HybridBuf::try_from(new_data)?))
        } else {
            Ok(TcpResult::Incomplete)
        }
    }

    fn read_intermediate(&mut self) -> Fallible<usize> {
        let read_size = std::cmp::min(
            self.incoming_msg.pending_bytes as usize,
            NOISE_MAX_MESSAGE_LEN,
        );

        match self.socket.read(&mut self.noise_msg_buffer[..read_size]) {
            Ok(read_bytes) => {
                self.incoming_msg
                    .message
                    .extend_from_slice(&self.noise_msg_buffer[..read_bytes]);
                self.incoming_msg.pending_bytes -= read_bytes as PayloadSize;

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
    pub fn decrypt<R: Read + Seek>(&mut self, mut input: R, len: usize) -> Fallible<HybridBuf> {
        // calculate the number of full-sized chunks
        let num_full_chunks = len / NOISE_MAX_MESSAGE_LEN;
        // calculate the number of the last, incomplete chunk (if there is one)
        let last_chunk_size = len % NOISE_MAX_MESSAGE_LEN;
        let num_all_chunks = num_full_chunks + if last_chunk_size > 0 { 1 } else { 0 };

        trace!("There are {} chunks to decrypt", num_all_chunks);

        let mut decrypted_msg =
            HybridBuf::with_capacity(NOISE_MAX_MESSAGE_LEN * num_full_chunks + last_chunk_size)?;

        // decrypt the full chunks
        for idx in 0..num_full_chunks {
            self.decrypt_chunk(idx, NOISE_MAX_MESSAGE_LEN, &mut input, &mut decrypted_msg)?;
        }

        // decrypt the incomplete chunk
        if last_chunk_size > 0 {
            self.decrypt_chunk(
                num_full_chunks,
                last_chunk_size,
                &mut input,
                &mut decrypted_msg,
            )?;
        }

        // rewind the decrypted message buffer
        decrypted_msg.rewind()?;

        // increment the input nonce
        self.input_nonce += 1;

        Ok(decrypted_msg)
    }

    fn decrypt_chunk<R: Read + Seek, W: Write>(
        &mut self,
        chunk_idx: usize,
        chunk_size: usize,
        input: &mut R,
        output: &mut W,
    ) -> Fallible<()> {
        debug_assert!(chunk_size <= NOISE_MAX_MESSAGE_LEN);

        input.read_exact(&mut self.noise_msg_buffer[..chunk_size])?;

        match self
            .noise_session
            .as_ref()
            .unwrap() // infallible
            .read_message_with_nonce(
                self.input_nonce,
                &self.noise_msg_buffer[..chunk_size],
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

                output.write_all(&self.plaintext_chunk_buffer[..len])?;
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
    pub fn write_to_socket(&mut self, input: Arc<[u8]>) -> Fallible<TcpResult<usize>> {
        TOTAL_MESSAGES_SENT_COUNTER.fetch_add(1, Ordering::Relaxed);
        self.conn()
            .stats
            .messages_sent
            .fetch_add(1, Ordering::Relaxed);
        if let Some(ref stats) = self.conn().handler().stats_export_service {
            stats.pkt_sent_inc();
        }

        if cfg!(feature = "network_dump") {
            self.conn().send_to_dump(input.clone(), false);
        }

        let encrypted_chunks = self.encrypt(&input)?;
        for chunk in encrypted_chunks {
            self.output_queue.push_back(chunk);
        }

        Ok(TcpResult::Discarded)
    }

    #[inline(always)]
    pub fn flush_socket(&mut self) -> Fallible<TcpResult<usize>> {
        let mut written_bytes = 0;

        while let Some(mut message) = self.output_queue.pop_front() {
            trace!(
                "Writing a {}B message to the socket",
                message.get_ref().len() - message.position() as usize
            );
            written_bytes +=
                partial_copy(&mut message, &mut self.noise_msg_buffer, &mut self.socket)?;

            if message.position() as usize == message.get_ref().len() {
                trace!("Successfully written a message to the socket");
            } else {
                trace!(
                    "Incomplete write ({}B remaining); requeuing",
                    message.get_ref().len() - message.position() as usize
                );
                self.output_queue.push_front(message);
                return Ok(TcpResult::Incomplete);
            }
        }

        Ok(TcpResult::Complete(written_bytes))
    }

    /// It splits `input` into chunks of `NOISE_MAX_PAYLOAD_LEN` and encrypts
    /// each of them.
    fn encrypt_chunks<R: Read + Seek>(
        &mut self,
        input: &mut R,
        chunks: &mut Vec<Cursor<Vec<u8>>>,
    ) -> Fallible<usize> {
        let mut written = 0;

        let mut curr_pos = input.seek(SeekFrom::Current(0))?;
        let eof = input.seek(SeekFrom::End(0))?;
        input.seek(SeekFrom::Start(curr_pos))?;

        while curr_pos != eof {
            let chunk_size = std::cmp::min(NOISE_MAX_PAYLOAD_LEN, (eof - curr_pos) as usize);
            input.read_exact(&mut self.plaintext_chunk_buffer[..chunk_size])?;

            let len = self
                .noise_session
                .as_ref()
                .unwrap() // infallible
                .write_message_with_nonce(
                    self.output_nonce,
                    &self.plaintext_chunk_buffer[..chunk_size],
                    &mut self.noise_msg_buffer,
                )?;

            let mut chunk = Vec::with_capacity(len);
            let wrote = chunk.write(&self.noise_msg_buffer[..len])?;

            chunks.push(Cursor::new(chunk));

            written += wrote;

            curr_pos = input.seek(SeekFrom::Current(0))?;
        }

        Ok(written)
    }

    /// It encrypts `input` and returns the encrypted chunks preceded by the
    /// length
    pub fn encrypt(&mut self, input: &[u8]) -> Fallible<Vec<Cursor<Vec<u8>>>> {
        trace!("Commencing encryption with nonce {:x}", self.output_nonce);

        let num_full_chunks = input.len() / NOISE_MAX_MESSAGE_LEN;
        let num_incomplete_chunks = if input.len() % NOISE_MAX_MESSAGE_LEN == 0 {
            0
        } else {
            1
        };
        let num_chunks = num_full_chunks + num_incomplete_chunks;

        // the extra 1 is for the message length
        let mut chunks = Vec::with_capacity(1 + num_chunks);

        // create the metadata chunk
        let metadata = Vec::with_capacity(mem::size_of::<PayloadSize>());
        chunks.push(Cursor::new(metadata));

        let encrypted_len = self.encrypt_chunks(&mut Cursor::new(input), &mut chunks)?;

        // write the message size
        chunks[0].write_u32::<NetworkEndian>(encrypted_len as PayloadSize)?;
        chunks[0].seek(SeekFrom::Start(0))?;

        trace!(
            "Encrypted a frame of {}B",
            mem::size_of::<PayloadSize>() + encrypted_len,
        );

        // increment the nonce
        self.output_nonce += 1;

        Ok(chunks)
    }
}

/// It tries to copy as much as possible from `input` to `output` in a
/// streaming fashion. It is used with `socket` that blocks them when
/// their output buffers are full. Written bytes are consumed from `input`.
fn partial_copy<W: Write>(
    input: &mut Cursor<Vec<u8>>,
    buffer: &mut [u8],
    output: &mut W,
) -> Fallible<usize> {
    let mut total_written_bytes = 0;

    while input.get_ref().len() != input.position() as usize {
        let offset = input.position();

        let chunk_size = std::cmp::min(
            NOISE_MAX_MESSAGE_LEN,
            input.get_ref().len() - offset as usize,
        );
        input.read_exact(&mut buffer[..chunk_size])?;

        match output.write(&buffer[..chunk_size]) {
            Ok(written_bytes) => {
                total_written_bytes += written_bytes;
                if written_bytes != chunk_size {
                    // Fix the offset because read data was not written completely.
                    input.seek(SeekFrom::Start(offset + written_bytes as u64))?;
                }
            }
            Err(io_err) => {
                input.seek(SeekFrom::Start(offset))?;
                match io_err.kind() {
                    std::io::ErrorKind::WouldBlock => break,
                    _ => return Err(failure::Error::from(io_err)),
                }
            }
        }
    }

    Ok(total_written_bytes)
}

/// It prefixes `data` with its length, encoded as `u32` in `NetworkEndian`.
fn create_frame(data: &[u8]) -> Fallible<Cursor<Vec<u8>>> {
    let mut frame = Vec::with_capacity(data.len() + std::mem::size_of::<PayloadSize>());
    frame.write_u32::<NetworkEndian>(data.len() as u32)?;
    frame.extend_from_slice(data);

    Ok(Cursor::new(frame))
}
