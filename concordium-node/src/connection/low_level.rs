use byteorder::{NetworkEndian, ReadBytesExt, WriteBytesExt};
use bytesize::ByteSize;
use failure::{Error, Fallible};
use mio::tcp::TcpStream;
use noiseexplorer_xx::consts::{DHLEN, MAC_LENGTH};

use super::{
    fails::{MessageTooBigError, StreamWouldBlock},
    noise_impl::{
        finalize_handshake, start_noise_session, NoiseSession, NOISE_MAX_MESSAGE_LEN,
        NOISE_MAX_PAYLOAD_LEN,
    },
    Connection, DeduplicationQueues,
};
use crate::{common::counter::TOTAL_MESSAGES_SENT_COUNTER, network::PROTOCOL_MAX_MESSAGE_SIZE};
use concordium_common::hybrid_buf::HybridBuf;

use std::{
    cmp,
    collections::VecDeque,
    io::{Cursor, ErrorKind, Read, Seek, SeekFrom, Write},
    mem,
    pin::Pin,
    sync::{atomic::Ordering, Arc},
    time::Duration,
};

type PayloadSize = u32;
const PAYLOAD_SIZE: usize = mem::size_of::<PayloadSize>();

/// The result of a socket read operation.
#[derive(Debug)]
pub enum SocketMsg {
    /// A complete message read from the socket.
    Complete(HybridBuf),
    /// The current read operation is incomplete and will be resumed later.
    Incomplete,
    /// The contents of the message are of no interest.
    Discarded,
}

/// The single message currently being read from the socket along with its
/// pending length.
#[derive(Default)]
struct IncomingMessage {
    pending_bytes: PayloadSize,
    message:       HybridBuf,
}

pub struct ConnectionLowLevel {
    pub conn_ref: Option<Pin<Arc<Connection>>>,
    pub socket: TcpStream,
    noise_session: NoiseSession,
    buffer: [u8; NOISE_MAX_MESSAGE_LEN],
    incoming_msg: IncomingMessage,
    /// A queue for messages waiting to be written to the socket
    output_queue: VecDeque<Cursor<Vec<u8>>>,
}

macro_rules! recv_xx_msg {
    ($self:ident, $data:expr, $idx:expr) => {
        let mut msg = vec![0u8; $data.len()? as usize];
        $data.read_exact(&mut msg)?;
        $self.noise_session.recv_message(&mut msg)?;
        trace!("I got message {}", $idx);
    };
}

macro_rules! send_xx_msg {
    ($self:ident, $size:expr, $idx:expr) => {
        let mut msg = vec![];
        // prepend the plaintext message length
        msg.write_u32::<NetworkEndian>($size as u32)?;
        // provide buffer space for the handshake message
        msg.append(&mut vec![0u8; $size]);
        // write the message into the buffer
        $self.noise_session.send_message(&mut msg[PAYLOAD_SIZE..])?;
        // queue and send the message
        trace!("Sending message {}", $idx);
        $self.output_queue.push_back(Cursor::new(msg));
        $self.flush_socket()?;
    };
}

impl ConnectionLowLevel {
    pub fn conn(&self) -> &Connection {
        &self.conn_ref.as_ref().unwrap() // safe; always available
    }

    pub fn new(socket: TcpStream, is_initiator: bool) -> Self {
        if let Err(e) = socket.set_linger(Some(Duration::from_secs(0))) {
            error!(
                "Can't set SOLINGER to 0 for socket {:?} due to {}",
                socket, e
            );
        }

        trace!(
            "Starting a noise session as the {}; handshake mode: XX",
            if is_initiator {
                "initiator"
            } else {
                "responder"
            }
        );

        ConnectionLowLevel {
            conn_ref: None,
            socket,
            noise_session: start_noise_session(is_initiator),
            buffer: [0; NOISE_MAX_MESSAGE_LEN],
            incoming_msg: IncomingMessage::default(),
            output_queue: VecDeque::with_capacity(16),
        }
    }

    // handshake

    pub fn initiator_send_message_a(&mut self) -> Fallible<()> {
        let pad = if cfg!(feature = "snow_noise") { 0 } else { 16 };
        send_xx_msg!(self, DHLEN + pad, "A");
        Ok(())
    }

    fn process_msg_a(&mut self, mut data: HybridBuf) -> Fallible<()> {
        recv_xx_msg!(self, data, "A");
        send_xx_msg!(self, DHLEN * 2 + MAC_LENGTH * 2, "B");
        Ok(())
    }

    fn process_msg_b(&mut self, mut data: HybridBuf) -> Fallible<()> {
        recv_xx_msg!(self, data, "B");
        send_xx_msg!(self, DHLEN + MAC_LENGTH * 2, "C");
        if cfg!(feature = "snow_noise") {
            finalize_handshake(&mut self.noise_session)?;
        }
        Ok(())
    }

    fn process_msg_c(&mut self, mut data: HybridBuf) -> Fallible<()> {
        recv_xx_msg!(self, data, "C");
        if cfg!(feature = "snow_noise") {
            finalize_handshake(&mut self.noise_session)?;
        }

        // send the high-level handshake request
        self.conn().send_handshake_request()?;
        self.flush_socket()?;

        Ok(())
    }

    fn is_post_handshake(&self) -> bool {
        if self.noise_session.is_initiator() {
            self.noise_session.get_message_count() > 1
        } else {
            self.noise_session.get_message_count() > 2
        }
    }

    // input

    /// Keeps reading from the socket as long as there is data to be read.
    #[inline(always)]
    pub fn read_stream(&mut self, deduplication_queues: &DeduplicationQueues) -> Fallible<()> {
        loop {
            match self.read_from_socket() {
                Ok(read_result) => match read_result {
                    SocketMsg::Complete(message) => {
                        if let Err(e) = self.conn().process_message(message, deduplication_queues) {
                            bail!("can't process a message: {}", e);
                        }
                    }
                    SocketMsg::Discarded => {}
                    SocketMsg::Incomplete => return Ok(()),
                },
                Err(e) => bail!("can't read from the socket: {}", e),
            }
        }
    }

    /// Attempts to read a complete message from the socket.
    #[inline(always)]
    fn read_from_socket(&mut self) -> Fallible<SocketMsg> {
        trace!("Attempting to read from the socket");
        let read_result = if self.incoming_msg.pending_bytes == 0 {
            self.read_expected_size()
        } else {
            self.read_payload()
        };

        match read_result {
            Ok(SocketMsg::Complete(msg)) => {
                if !self.is_post_handshake() {
                    match self.noise_session.get_message_count() {
                        0 if !self.noise_session.is_initiator() => self.process_msg_a(msg),
                        1 if self.noise_session.is_initiator() => self.process_msg_b(msg),
                        2 if !self.noise_session.is_initiator() => self.process_msg_c(msg),
                        _ => bail!("Invalid XX handshake"),
                    }?;
                    Ok(SocketMsg::Discarded)
                } else {
                    Ok(SocketMsg::Complete(self.decrypt(msg)?))
                }
            }
            Ok(SocketMsg::Incomplete) => {
                trace!("The current message is incomplete");
                Ok(SocketMsg::Incomplete)
            }
            Ok(_) => unreachable!(),
            Err(err) => {
                if err.downcast_ref::<StreamWouldBlock>().is_some() {
                    trace!("Further reads would be blocking; aborting");
                    Ok(SocketMsg::Incomplete)
                } else {
                    Err(err)
                }
            }
        }
    }

    /// Reads the number of bytes required to read the frame length
    #[inline]
    fn pending_bytes_to_know_expected_size(&self) -> Fallible<usize> {
        let current_len = self.incoming_msg.message.len()? as usize;

        if current_len < PAYLOAD_SIZE {
            Ok(PAYLOAD_SIZE - current_len)
        } else {
            Ok(0)
        }
    }

    /// It first reads the first 4 bytes of the message to determine its size.
    fn read_expected_size(&mut self) -> Fallible<SocketMsg> {
        // only extract the bytes needed to know the size.
        let min_bytes = self.pending_bytes_to_know_expected_size()?;
        let read_bytes = map_io_error_to_fail!(self.socket.read(&mut self.buffer[..min_bytes]))?;

        self.incoming_msg
            .message
            .write_all(&self.buffer[..read_bytes])?;

        // once the number of bytes needed to read the message size is known, continue
        if self.incoming_msg.message.len()? == PAYLOAD_SIZE as u64 {
            self.incoming_msg.message.rewind()?;
            let expected_size = self.incoming_msg.message.read_u32::<NetworkEndian>()?;

            // check if the expected size doesn't exceed the protocol limit
            if expected_size > PROTOCOL_MAX_MESSAGE_SIZE as PayloadSize {
                let error = MessageTooBigError {
                    expected_size,
                    protocol_size: PROTOCOL_MAX_MESSAGE_SIZE as PayloadSize,
                };
                return Err(Error::from(error));
            } else {
                trace!(
                    "Expecting a {} message",
                    ByteSize(expected_size as u64).to_string_as(true)
                );
                self.incoming_msg.pending_bytes = expected_size;
            }

            // remove the length from the buffer
            mem::replace(
                &mut self.incoming_msg.message,
                HybridBuf::with_capacity(expected_size as usize)?,
            );

            // Read data next...
            self.read_payload()
        } else {
            // We need more data to determine the message size.
            Ok(SocketMsg::Incomplete)
        }
    }

    /// Once we know the message expected size, we can start to receive data.
    fn read_payload(&mut self) -> Fallible<SocketMsg> {
        while self.incoming_msg.pending_bytes > 0 {
            if self.read_intermediate()? == 0 {
                break;
            }
        }

        if self.incoming_msg.pending_bytes == 0 {
            trace!("The message was fully read");
            self.incoming_msg.message.rewind()?;
            let new_data = mem::replace(
                &mut self.incoming_msg.message,
                HybridBuf::with_capacity(PAYLOAD_SIZE)?,
            );

            Ok(SocketMsg::Complete(new_data))
        } else {
            Ok(SocketMsg::Incomplete)
        }
    }

    fn read_intermediate(&mut self) -> Fallible<usize> {
        let read_size = cmp::min(
            self.incoming_msg.pending_bytes as usize,
            NOISE_MAX_MESSAGE_LEN,
        );

        match self.socket.read(&mut self.buffer[..read_size]) {
            Ok(read_bytes) => {
                self.incoming_msg
                    .message
                    .write_all(&self.buffer[..read_bytes])?;
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

    fn decrypt(&mut self, mut input: HybridBuf) -> Fallible<HybridBuf> {
        // calculate the number of full-sized chunks
        let len = input.len()? as usize;
        let num_full_chunks = len / NOISE_MAX_MESSAGE_LEN;
        // calculate the number of the last, incomplete chunk (if there is one)
        let last_chunk_size = len % NOISE_MAX_MESSAGE_LEN;

        let mut decrypted_msg =
            HybridBuf::with_capacity(NOISE_MAX_MESSAGE_LEN * num_full_chunks + last_chunk_size)?;

        // decrypt the full chunks
        for _ in 0..num_full_chunks {
            self.decrypt_chunk(NOISE_MAX_MESSAGE_LEN, &mut input, &mut decrypted_msg)?;
        }

        // decrypt the incomplete chunk
        if last_chunk_size > 0 {
            self.decrypt_chunk(last_chunk_size, &mut input, &mut decrypted_msg)?;
        }

        decrypted_msg.rewind()?;

        Ok(decrypted_msg)
    }

    fn decrypt_chunk<R: Read + Seek, W: Write>(
        &mut self,
        chunk_size: usize,
        input: &mut R,
        output: &mut W,
    ) -> Fallible<()> {
        debug_assert!(chunk_size <= NOISE_MAX_MESSAGE_LEN);

        input.read_exact(&mut self.buffer[..chunk_size])?;

        if let Err(err) = self
            .noise_session
            .recv_message(&mut self.buffer[..chunk_size])
        {
            error!("Decryption error: {}", err);
            Err(err.into())
        } else {
            output.write_all(&self.buffer[..chunk_size - MAC_LENGTH])?;
            Ok(())
        }
    }

    // output

    #[inline(always)]
    pub fn write_to_socket(&mut self, input: Arc<[u8]>) -> Fallible<()> {
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

        self.encrypt_and_enqueue(&input)
    }

    #[inline(always)]
    pub fn flush_socket(&mut self) -> Fallible<()> {
        while let Some(mut message) = self.output_queue.pop_front() {
            trace!(
                "Writing a {} message to the socket",
                ByteSize(message.get_ref().len() as u64 - message.position()).to_string_as(true)
            );
            partial_copy(&mut message, &mut self.buffer, &mut self.socket)?;

            if message.position() as usize == message.get_ref().len() {
                trace!("Successfully written a message to the socket");
            } else {
                trace!(
                    "Incomplete write ({}B remaining); requeuing",
                    message.get_ref().len() - message.position() as usize
                );
                self.output_queue.push_front(message);
                break;
            }
        }

        Ok(())
    }

    /// It encrypts `input` and enqueues the encrypted chunks preceded by the
    /// length for later sending
    fn encrypt_and_enqueue(&mut self, input: &[u8]) -> Fallible<()> {
        let num_full_chunks = input.len() / NOISE_MAX_PAYLOAD_LEN;
        let last_chunk_len = input.len() % NOISE_MAX_PAYLOAD_LEN + MAC_LENGTH;
        let full_msg_len = num_full_chunks * NOISE_MAX_MESSAGE_LEN + last_chunk_len;

        // write the length in plaintext
        let mut payload_len = Cursor::new(Vec::with_capacity(PAYLOAD_SIZE));
        payload_len.write_u32::<NetworkEndian>(full_msg_len as PayloadSize)?;
        self.output_queue.push_back(payload_len);

        let mut input = Cursor::new(input);
        let eof = input.get_ref().len() as u64;

        if full_msg_len <= NOISE_MAX_PAYLOAD_LEN - PAYLOAD_SIZE {
            self.encrypt_chunk(&mut input, true)?;
        } else {
            while input.position() != eof {
                self.encrypt_chunk(&mut input, false)?;
            }
        }

        Ok(())
    }

    fn encrypt_chunk(&mut self, input: &mut Cursor<&[u8]>, squeeze: bool) -> Fallible<()> {
        let remaining_len = input.get_ref().len() - input.position() as usize;
        let chunk_size = cmp::min(NOISE_MAX_PAYLOAD_LEN, remaining_len);
        input.read_exact(&mut self.buffer[..chunk_size])?;
        let encrypted_len = chunk_size + MAC_LENGTH;

        self.noise_session
            .send_message(&mut self.buffer[..encrypted_len])?;

        let mut chunk = if squeeze {
            self.output_queue.pop_back().unwrap() // infallible
        } else {
            if let Some(ref mut len_chunk) = self.output_queue.back_mut() {
                len_chunk.seek(SeekFrom::Start(0))?;
            }
            Cursor::new(Vec::with_capacity(encrypted_len))
        };

        chunk.write_all(&self.buffer[..encrypted_len])?;
        chunk.seek(SeekFrom::Start(0))?;
        self.output_queue.push_back(chunk);

        Ok(())
    }
}

/// It tries to copy as much as possible from `input` to `output` in
/// chunks. It is used with `socket` that blocks them when their
/// output buffers are full. Written bytes are consumed from `input`.
fn partial_copy<W: Write>(
    input: &mut Cursor<Vec<u8>>,
    buffer: &mut [u8],
    output: &mut W,
) -> Fallible<()> {
    while input.get_ref().len() != input.position() as usize {
        let offset = input.position();

        let chunk_size = cmp::min(
            NOISE_MAX_MESSAGE_LEN,
            input.get_ref().len() - offset as usize,
        );
        input.read_exact(&mut buffer[..chunk_size])?;

        match output.write(&buffer[..chunk_size]) {
            Ok(written_bytes) => {
                if written_bytes != chunk_size {
                    // Fix the offset because read data was not written completely.
                    input.seek(SeekFrom::Start(offset + written_bytes as u64))?;
                }
            }
            Err(io_err) => {
                input.seek(SeekFrom::Start(offset))?;
                match io_err.kind() {
                    ErrorKind::WouldBlock => break,
                    _ => return Err(failure::Error::from(io_err)),
                }
            }
        }
    }

    Ok(())
}
