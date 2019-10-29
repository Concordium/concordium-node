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
    io::{self, BufWriter, ErrorKind, Read, Seek, SeekFrom, Write},
    mem,
    pin::Pin,
    sync::Arc,
};

type PayloadSize = u32;

const PROLOGUE: &[u8] = b"CP2P";
const PRE_SHARED_KEY: &[u8; 32] = b"54686973206973206d79204175737472";
const NOISE_MAX_MESSAGE_LEN: usize = 64 * 1024;
const NOISE_AUTH_TAG_LEN: usize = 16;
const NOISE_MAX_PAYLOAD_LEN: usize = NOISE_MAX_MESSAGE_LEN - NOISE_AUTH_TAG_LEN;

#[derive(Debug, Clone)]
pub enum Readiness<T> {
    Ready(T),
    NotReady,
}

impl<T> Readiness<T> {
    pub fn ready(self) -> Option<T> {
        match self {
            Readiness::Ready(v) => Some(v),
            _ => None,
        }
    }
}

/// State of the *IKpsk2* handshake:
/// ```ignore
/// <- s
///  A -> e,es,s,ss
///  B -> e,ee,se,psk
/// ```
#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq)]
pub enum HandshakeState {
    ResponderAwaitingRequest_S,
    InitiatorAwaiting_S,
    ResponderAwaiting_E_ES_S_SS,
    InitiatorAwaiting_E_EE_SE_PSK,
    Complete,
}

pub struct ConnectionLowLevel {
    pub conn_ref:           Option<Pin<Arc<Connection>>>,
    pub socket:             TcpStream,
    keypair:                Keypair,
    noise_session:          Option<Session>,
    input_buffer:           [u8; NOISE_MAX_MESSAGE_LEN],
    current_input:          Vec<u8>,
    handshake_queue:        VecDeque<HybridBuf>,
    pending_output_queue:   Vec<HybridBuf>,
    encrypted_queue:        VecDeque<HybridBuf>,
    handshake_state:        HandshakeState,
    encrypted_chunk_buffer: Vec<u8>,
    plaintext_chunk_buffer: Vec<u8>,
    full_output_buffer:     BufWriter<HybridBuf>,
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
            HandshakeState::InitiatorAwaiting_S
        } else {
            trace!("I'm awaiting the pre-shared static key");
            HandshakeState::ResponderAwaitingRequest_S
        };

        ConnectionLowLevel {
            conn_ref: None,
            socket,
            keypair,
            noise_session,
            current_input: Vec::with_capacity(NOISE_MAX_PAYLOAD_LEN),
            handshake_queue,
            pending_output_queue: Vec::with_capacity(4),
            encrypted_queue: VecDeque::with_capacity(16),
            handshake_state,
            input_buffer: [0u8; NOISE_MAX_MESSAGE_LEN],
            encrypted_chunk_buffer: vec![0u8; NOISE_MAX_MESSAGE_LEN],
            plaintext_chunk_buffer: vec![0; NOISE_MAX_PAYLOAD_LEN],
            full_output_buffer: BufWriter::new(Default::default()),
        }
    }

    // handshake

    fn on_initiator_get_s(&mut self, mut input: HybridBuf) -> Fallible<()> {
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

        self.handshake_state = HandshakeState::InitiatorAwaiting_E_EE_SE_PSK;
        self.noise_session = Some(session);

        Ok(())
    }

    fn on_responder_get_request_s(&mut self) -> Fallible<()> {
        trace!("I've received the pre-shared static key");
        trace!("I'm sending my public key");
        self.handshake_queue
            .push_back(create_frame(&self.keypair.public)?);

        // Next state
        self.handshake_state = HandshakeState::ResponderAwaiting_E_ES_S_SS;

        Ok(())
    }

    fn on_responder_get_e_ee_s_ss(&mut self, mut input: HybridBuf) -> Fallible<()> {
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

    fn on_initiator_get_e_ee_se_psk(&mut self, mut input: HybridBuf) -> Fallible<()> {
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

    /// It reads from `input`, and based on current internal state, interprets
    /// it.
    ///
    /// # Return
    ///
    /// As soon as handshake is done, it will return
    /// `Readiness::Ready(session)`, which contains a transport session,
    /// ready to encrypt/decrypt. Otherwise, it returns
    /// `Readiness::NotReady`.
    fn read_handshake_msg(&mut self, input: HybridBuf) -> Fallible<()> {
        use HandshakeState::*;

        match self.handshake_state {
            ResponderAwaitingRequest_S => self.on_responder_get_request_s(),
            InitiatorAwaiting_S => self.on_initiator_get_s(input),
            ResponderAwaiting_E_ES_S_SS => self.on_responder_get_e_ee_s_ss(input),
            InitiatorAwaiting_E_EE_SE_PSK => self.on_initiator_get_e_ee_se_psk(input),
            Complete => unreachable!("Handshake logic error"),
        }?;

        while !self.handshake_queue.is_empty() {
            self.flush_handshaker()?;
        }

        if self.handshake_state == HandshakeState::Complete {
            for frame in mem::replace(&mut self.pending_output_queue, Default::default()) {
                self.write_to_socket(frame)?;
            }
        }

        Ok(())
    }

    // input

    #[inline(always)]
    pub fn read_stream(&mut self, deduplication_queues: &mut DeduplicationQueues) -> Fallible<()> {
        loop {
            match self.read_from_socket() {
                Ok(readiness) => match readiness {
                    Readiness::Ready(message) => {
                        self.conn().send_to_dump(&message, true);
                        if let Err(e) = self.conn().process_message(message, deduplication_queues) {
                            bail!("can't process a message: {}", e);
                        }
                    }
                    Readiness::NotReady => return Ok(()),
                },
                Err(e) => bail!("can't read from the socket: {}", e),
            }
        }
    }

    #[inline(always)]
    fn read_from_socket(&mut self) -> Fallible<Readiness<HybridBuf>> {
        trace!("Reading from the socket");
        let payload_readiness = self.read_expected_size();

        match payload_readiness {
            Ok(Readiness::Ready(payload)) => {
                trace!("The message is complete");
                self.forward(payload)
            }
            Ok(Readiness::NotReady) => {
                trace!("The message is incomplete");
                Ok(Readiness::NotReady)
            }
            Err(err) => {
                if err.downcast_ref::<StreamWouldBlock>().is_some() {
                    trace!("Further reads would be blocking; the message is incomplete");
                    Ok(Readiness::NotReady)
                } else {
                    Err(err)
                }
            }
        }
    }

    fn forward(&mut self, input: HybridBuf) -> Fallible<Readiness<HybridBuf>> {
        if self.handshake_state == HandshakeState::Complete {
            Ok(Readiness::Ready(self.decrypt(input)?))
        } else {
            self.read_handshake_msg(input)?;

            Ok(Readiness::NotReady)
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

    /// It only reads the first 4 bytes of the message to determine its size.
    ///
    /// This operation could not be completed in just one shot due to limits of
    /// `output` buffers.
    fn read_expected_size(&mut self) -> Fallible<Readiness<HybridBuf>> {
        // Extract only the bytes needed to know the size.
        let min_bytes = self.pending_bytes_to_know_expected_size();
        let read_bytes =
            map_io_error_to_fail!(self.socket.read(&mut self.input_buffer[..min_bytes]))?;

        self.current_input
            .extend_from_slice(&self.input_buffer[..read_bytes]);

        // Load expected size
        if self.current_input.len() >= std::mem::size_of::<PayloadSize>() {
            let expected_size =
                NetworkEndian::read_u32(&self.current_input[..std::mem::size_of::<PayloadSize>()]);
            trace!("Expecting a message of {}B", expected_size);

            // Check protocol limits
            if expected_size > PROTOCOL_MAX_MESSAGE_SIZE as u32 {
                let error = MessageTooBigError {
                    expected_size,
                    protocol_size: PROTOCOL_MAX_MESSAGE_SIZE as u32,
                };
                self.current_input.clear();
                return Err(Error::from(error));
            }

            // remove the length from the buffer
            self.current_input.clear();

            // Read data next...
            self.read_payload(expected_size)
        } else {
            // We need more data to determine the message size.
            Ok(Readiness::NotReady)
        }
    }

    /// Once we know the message expected size, we can start to receive data.
    fn read_payload(&mut self, expected_size: PayloadSize) -> Fallible<Readiness<HybridBuf>> {
        trace!("Reading the message payload");
        self.read_intermediate(expected_size - self.current_input.len() as u32)?;

        if self.current_input.len() as u32 == expected_size {
            // Ready message
            let new_data = std::mem::replace(
                &mut self.current_input,
                Vec::with_capacity(std::mem::size_of::<PayloadSize>()),
            );

            Ok(Readiness::Ready(HybridBuf::try_from(new_data)?))
        } else {
            Ok(Readiness::NotReady)
        }
    }

    fn read_intermediate(&mut self, pending_bytes: PayloadSize) -> Fallible<usize> {
        let read_size = std::cmp::min(pending_bytes as usize, NOISE_MAX_MESSAGE_LEN);

        match self.socket.read(&mut self.input_buffer[..read_size]) {
            Ok(read_bytes) => {
                self.current_input
                    .extend_from_slice(&self.input_buffer[..read_bytes]);

                Ok(read_bytes)
            }
            Err(err) => match err.kind() {
                ErrorKind::WouldBlock => Ok(0),
                _ => Err(Error::from(err)),
            },
        }
    }

    /// It reads the chunk table and decodes each of them.
    ///
    /// # Return
    /// The decrypted message.
    pub fn decrypt<R: Read + Seek>(&mut self, mut input: R) -> Fallible<HybridBuf> {
        // 0. Read NONCE.
        let nonce = input.read_u64::<NetworkEndian>()?;

        // 1. Read the chunk table.
        let num_full_chunks = input.read_u32::<NetworkEndian>()? as usize;
        let last_chunk_size = input.read_u32::<NetworkEndian>()? as usize;

        for idx in 0..num_full_chunks {
            self.decrypt_chunk(idx, NOISE_MAX_MESSAGE_LEN, nonce, &mut input)?;
        }

        if last_chunk_size > 0 {
            self.decrypt_chunk(num_full_chunks, last_chunk_size, nonce, &mut input)?;
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
                error!("Decryption error at chunk {}; fails: {}", chunk_idx, err);
                Err(failure::Error::from(err))
            }
        }
    }

    // output

    /// If a channel is encrypted, `encryptor` will take care of it.
    /// Otherwise, `input` is enqueued until `encryptor` is ready
    /// (after the handshake is done).
    #[inline(always)]
    pub fn write_to_socket(&mut self, input: HybridBuf) -> Fallible<Readiness<usize>> {
        if self.handshake_state == HandshakeState::Complete {
            let encrypted = self.encrypt(input)?;
            self.encrypted_queue.push_back(encrypted);
            self.flush_encryptor()
        } else {
            self.pending_output_queue.push(input);
            Ok(Readiness::NotReady)
        }
    }

    #[inline(always)]
    pub fn flush_socket(&mut self) -> Fallible<Readiness<usize>> {
        if self.handshake_state == HandshakeState::Complete {
            self.flush_encryptor()
        } else {
            self.flush_handshaker()
        }
    }

    fn flush_handshaker(&mut self) -> Fallible<Readiness<usize>> {
        if let Some(mut data) = self.handshake_queue.pop_front() {
            trace!("Pushing a handshake message to the socket");
            let written_bytes = partial_copy(&mut data, &mut self.socket)?;

            if data.is_eof()? {
                trace!("Successfully sent a handshake message");
                Ok(Readiness::Ready(written_bytes))
            } else {
                trace!("The message was not fully written; requeuing");
                self.handshake_queue.push_front(data);
                Ok(Readiness::NotReady)
            }
        } else {
            Ok(Readiness::Ready(0))
        }
    }

    #[inline(always)]
    fn flush_encryptor(&mut self) -> Fallible<Readiness<usize>> {
        if let Some(mut encrypted) = self.encrypted_queue.pop_front() {
            trace!("Pushing an encrypted message to the socket");
            let written_bytes = partial_copy(&mut encrypted, &mut self.socket)?;

            if encrypted.is_eof()? {
                trace!("Successfully sent an encrypted message");
                Ok(Readiness::Ready(written_bytes))
            } else {
                trace!("The message was not fully written; requeuing");
                self.encrypted_queue.push_front(encrypted);
                Ok(Readiness::NotReady)
            }
        } else {
            Ok(Readiness::Ready(0))
        }
    }

    /// It splits `input` into chunks (64kb max) and encrypts each of them.
    fn encrypt_chunks<R: Read + Seek>(&mut self, nonce: u64, input: &mut R) -> Fallible<usize> {
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
                .unwrap()
                .write_message_with_nonce(
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
    pub fn encrypt<R: Read + Seek>(&mut self, mut input: R) -> Fallible<HybridBuf> {
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
        let num_full_chunks = encrypted_len / NOISE_MAX_MESSAGE_LEN;
        self.full_output_buffer
            .write_u32::<NetworkEndian>(num_full_chunks as PayloadSize)?;

        // 2.2. Size of the last chunk.
        let last_chunk_size = encrypted_len - (num_full_chunks * NOISE_MAX_MESSAGE_LEN);
        debug_assert!(last_chunk_size <= PayloadSize::max_value() as usize);
        self.full_output_buffer
            .write_u32::<NetworkEndian>(last_chunk_size as PayloadSize)?;

        trace!(
            "Encrypted a frame of {}B; full chunks: {}, last chunk size: {}",
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

/// It tries to copy as much as possible from `input` to `output` in a
/// streaming fashion. It is used with `socket` that blocks them when
/// their output buffers are full. Written bytes are consumed from `input`.
fn partial_copy<R: Read + Seek, W: Write>(input: &mut R, output: &mut W) -> Fallible<usize> {
    let initial_pos = input.seek(SeekFrom::Current(0))?;

    match io::copy(input, output) {
        Ok(written) => Ok(written as usize),
        Err(err) => {
            if err.kind() == std::io::ErrorKind::WouldBlock {
                let curr_pos = input.seek(SeekFrom::Current(0))?;
                let written = curr_pos - initial_pos;
                Ok(written as usize)
            } else {
                Err(err.into())
            }
        }
    }
}

/// It prefixes `data` with its length, encoded as `u32` in `NetworkEndian`.
fn create_frame(data: &[u8]) -> Fallible<HybridBuf> {
    let mut frame = Vec::with_capacity(data.len() + std::mem::size_of::<PayloadSize>());
    frame.write_u32::<NetworkEndian>(data.len() as u32)?;
    frame.extend_from_slice(data);

    into_err!(HybridBuf::try_from(frame))
}
