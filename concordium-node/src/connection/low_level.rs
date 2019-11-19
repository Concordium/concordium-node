use byteorder::{NetworkEndian, WriteBytesExt};
use bytesize::ByteSize;
use failure::Fallible;
use mio::tcp::TcpStream;
use noiseexplorer_xx::consts::{DHLEN, MAC_LENGTH};

use super::{
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
    convert::TryInto,
    io::{Cursor, ErrorKind, Read, Write},
    mem,
    pin::Pin,
    sync::{atomic::Ordering, Arc},
    time::Duration,
};

type PayloadSize = u32;
const PAYLOAD_SIZE: usize = mem::size_of::<PayloadSize>();
const MAX_MESSAGE_LEN: usize = PAYLOAD_SIZE + NOISE_MAX_MESSAGE_LEN;
const WRITE_QUEUE_ALLOC: usize = 4 * 1024 * 1024;

/// The single message currently being read from the socket along with its
/// pending length.
#[derive(Default)]
struct IncomingMessage {
    size_bytes:    Vec<u8>,
    pending_bytes: PayloadSize,
    message:       HybridBuf,
}

impl IncomingMessage {
    fn is_size_known(&mut self) -> Fallible<bool> {
        if self.pending_bytes != 0 {
            Ok(true)
        } else if self.size_bytes.len() == PAYLOAD_SIZE {
            let ready_size = mem::replace(&mut self.size_bytes, Vec::with_capacity(PAYLOAD_SIZE));
            let expected_size = PayloadSize::from_be_bytes((&ready_size[..]).try_into().unwrap());

            // check if the expected size doesn't exceed the protocol limit
            if expected_size > PROTOCOL_MAX_MESSAGE_SIZE as PayloadSize {
                bail!(
                    "expected message size ({}) exceeds the maximum protocol size ({})",
                    ByteSize(expected_size as u64).to_string_as(true),
                    ByteSize(PROTOCOL_MAX_MESSAGE_SIZE as u64).to_string_as(true)
                );
            }

            trace!(
                "Expecting a {} message",
                ByteSize(expected_size as u64).to_string_as(true)
            );
            self.pending_bytes = expected_size;

            Ok(true)
        } else {
            Ok(false)
        }
    }
}

pub struct ConnectionLowLevel {
    pub conn_ref: Option<Pin<Arc<Connection>>>,
    pub socket: TcpStream,
    noise_session: NoiseSession,
    buffer: [u8; MAX_MESSAGE_LEN],
    backup_buffer: Option<Vec<u8>>, // FIXME: change to an array
    incoming_msg: IncomingMessage,
    /// A queue for bytes waiting to be written to the socket
    output_queue: VecDeque<u8>,
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
        $self.output_queue.extend(msg);
        $self.flush_socket()?;
    };
}

macro_rules! handle_io_read {
    ($result:expr, $default:expr) => {
        match $result {
            Ok(num_bytes) => {
                trace!(
                    "Read {} from the socket",
                    ByteSize(num_bytes as u64).to_string_as(true)
                );
                num_bytes
            }
            Err(e) if e.kind() == ErrorKind::WouldBlock => return Ok($default),
            Err(e) => return Err(e.into()),
        }
    };
}

impl ConnectionLowLevel {
    pub fn conn(&self) -> &Connection {
        &self.conn_ref.as_ref().unwrap() // safe; always available
    }

    pub fn new(socket: TcpStream, is_initiator: bool) -> Self {
        if let Err(e) = socket.set_linger(Some(Duration::from_secs(0))) {
            error!("Can't set SOLINGER for socket {:?}: {}", socket, e);
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
            buffer: [0; MAX_MESSAGE_LEN],
            backup_buffer: None,
            incoming_msg: IncomingMessage::default(),
            output_queue: VecDeque::with_capacity(WRITE_QUEUE_ALLOC),
        }
    }

    // the XX handshake

    pub fn send_handshake_message_a(&mut self) -> Fallible<()> {
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

    #[inline]
    /// Checks whether the low-level noise handshake is complete.
    fn is_post_handshake(&self) -> bool {
        if self.noise_session.is_initiator() {
            self.noise_session.get_message_count() > 1
        } else {
            self.noise_session.get_message_count() > 2
        }
    }

    // input

    /// Keeps reading from the socket as long as there is data to be read
    /// and the operation is not blocking.
    #[inline]
    pub fn read_stream(&mut self, deduplication_queues: &DeduplicationQueues) -> Fallible<()> {
        loop {
            match self.read_from_socket() {
                Ok(Some(message)) => self.conn().process_message(message, deduplication_queues)?,
                Ok(None) => return Ok(()), // this read would be blocking or it was a handshake
                Err(e) => bail!("Can't read from the socket: {}", e),
            }
        }
    }

    /// Attempts to read a complete message from the socket.
    #[inline]
    fn read_from_socket(&mut self) -> Fallible<Option<HybridBuf>> {
        let mut read_bytes = if let Some(backup_buffer) = self.backup_buffer.take() {
            self.buffer[..backup_buffer.len()].copy_from_slice(&backup_buffer);
            backup_buffer.len()
        } else {
            handle_io_read!(self.socket.read(&mut self.buffer[..MAX_MESSAGE_LEN]), None)
        };

        if !self.incoming_msg.is_size_known()? {
            let offset = self.incoming_msg.size_bytes.len() as usize;
            let read_size = cmp::min(read_bytes, PAYLOAD_SIZE - offset);
            let written = self
                .incoming_msg
                .size_bytes
                .write(&self.buffer[offset..][..read_size])?;
            self.buffer.rotate_left(written);
            read_bytes -= written;
        }

        // check if we can know the size of the message
        if self.incoming_msg.is_size_known()? {
            let expected_size = self.incoming_msg.pending_bytes;

            // pre-allocate if we've not been reading the message yet
            if self.incoming_msg.message.is_empty()? {
                self.incoming_msg.message = HybridBuf::with_capacity(expected_size as usize)?;
            }

            let to_read = cmp::min(self.incoming_msg.pending_bytes as usize, read_bytes);
            self.incoming_msg
                .message
                .write_all(&self.buffer[..to_read])?;
            self.incoming_msg.pending_bytes -= to_read as PayloadSize;

            if read_bytes > to_read {
                self.backup_buffer = Some(self.buffer[to_read..][..read_bytes - to_read].to_vec());
            }

            // read the actual message
            match self.read_payload()? {
                Some(msg) => {
                    if !self.is_post_handshake() {
                        match self.noise_session.get_message_count() {
                            0 if !self.noise_session.is_initiator() => self.process_msg_a(msg),
                            1 if self.noise_session.is_initiator() => self.process_msg_b(msg),
                            2 if !self.noise_session.is_initiator() => self.process_msg_c(msg),
                            _ => bail!("invalid XX handshake"),
                        }?;
                        Ok(None)
                    } else {
                        Ok(Some(self.decrypt(msg)?))
                    }
                }
                None => Ok(None),
            }
        } else {
            // We need more data to determine the message size.
            Ok(None)
        }
    }

    /// Read data from the socket until the expected incoming message
    /// size is reached.
    #[inline]
    fn read_payload(&mut self) -> Fallible<Option<HybridBuf>> {
        if self.backup_buffer.is_none() {
            while self.incoming_msg.pending_bytes > 0 {
                if self.read_intermediate()? == 0 {
                    break;
                }
            }
        }

        if self.incoming_msg.pending_bytes == 0 {
            trace!("The message was fully read");
            self.incoming_msg.message.rewind()?;
            let message =
                mem::replace(&mut self.incoming_msg.message, HybridBuf::with_capacity(0)?);

            Ok(Some(message))
        } else {
            Ok(None)
        }
    }

    /// Collects at most `NOISE_MAX_MESSAGE_LEN` from the socket.
    #[inline]
    fn read_intermediate(&mut self) -> Fallible<usize> {
        let read_size = cmp::min(
            self.incoming_msg.pending_bytes as usize,
            NOISE_MAX_MESSAGE_LEN,
        );

        let read_bytes = handle_io_read!(self.socket.read(&mut self.buffer[..read_size]), 0);

        self.incoming_msg
            .message
            .write_all(&self.buffer[..read_bytes])?;
        self.incoming_msg.pending_bytes -= read_bytes as PayloadSize;

        Ok(read_bytes)
    }

    /// Decrypt a full message read from the socket.
    #[inline]
    fn decrypt(&mut self, mut input: HybridBuf) -> Fallible<HybridBuf> {
        // calculate the number of full-sized chunks
        let len = input.len()? as usize;
        let num_full_chunks = len / NOISE_MAX_MESSAGE_LEN;
        // calculate the number of the last, incomplete chunk (if there is one)
        let last_chunk_size = len % NOISE_MAX_MESSAGE_LEN;
        let num_all_chunks = num_full_chunks + if last_chunk_size > 0 { 1 } else { 0 };

        let mut decrypted_msg =
            HybridBuf::with_capacity(NOISE_MAX_PAYLOAD_LEN * num_full_chunks + last_chunk_size)?;

        // decrypt the chunks
        for _ in 0..num_all_chunks {
            self.decrypt_chunk(&mut input, &mut decrypted_msg)?;
        }

        decrypted_msg.rewind()?;

        Ok(decrypted_msg)
    }

    /// Decrypt a single chunk of the received encrypted message.
    #[inline]
    fn decrypt_chunk<W: Write>(&mut self, input: &mut HybridBuf, output: &mut W) -> Fallible<()> {
        let read_size = cmp::min(NOISE_MAX_MESSAGE_LEN, input.remaining_len()? as usize);
        input.read_exact(&mut self.buffer[..read_size])?;

        if let Err(err) = self
            .noise_session
            .recv_message(&mut self.buffer[..read_size])
        {
            error!("decryption error: {}", err);
            Err(err.into())
        } else {
            output.write_all(&self.buffer[..read_size - MAC_LENGTH])?;
            Ok(())
        }
    }

    // output

    /// Enqueue a message to be written to the socket.
    #[inline]
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

    /// Writes enequeued messages to the socket until the queue is exhausted
    /// or the write would be blocking.
    #[inline]
    pub fn flush_socket(&mut self) -> Fallible<()> {
        loop {
            if self.output_queue.is_empty() {
                break;
            }

            let write_size = cmp::min(NOISE_MAX_MESSAGE_LEN, self.output_queue.len());

            for (i, &byte) in self.output_queue.iter().take(write_size).enumerate() {
                self.buffer[i] = byte;
            }

            let written = match self.socket.write(&self.buffer[..write_size]) {
                Ok(num_bytes) => num_bytes,
                Err(e) if e.kind() == ErrorKind::WouldBlock => break,
                Err(e) => return Err(e.into()),
            };

            trace!(
                "Written {} to the socket",
                ByteSize(written as u64).to_string_as(true)
            );

            if written == write_size {
                self.output_queue.drain(..write_size);
            } else {
                self.output_queue.drain(..written);
                break;
            }
        }

        Ok(())
    }

    /// It encrypts `input` and enqueues the encrypted chunks preceded by the
    /// length for later sending.
    #[inline]
    fn encrypt_and_enqueue(&mut self, input: &[u8]) -> Fallible<()> {
        let num_full_chunks = input.len() / NOISE_MAX_PAYLOAD_LEN;
        let last_chunk_len = input.len() % NOISE_MAX_PAYLOAD_LEN + MAC_LENGTH;
        let full_msg_len = (num_full_chunks * NOISE_MAX_MESSAGE_LEN + last_chunk_len) as u32;

        self.output_queue.extend(&full_msg_len.to_be_bytes());

        let mut input = Cursor::new(input);
        let eof = input.get_ref().len() as u64;

        while input.position() != eof {
            self.encrypt_chunk(&mut input)?;
        }

        Ok(())
    }

    /// Produces and enqueues a single noise message from `input`, potentially
    /// squeezing it with the previously enqueued chunk.
    #[inline]
    fn encrypt_chunk(&mut self, input: &mut Cursor<&[u8]>) -> Fallible<()> {
        let remaining_len = input.get_ref().len() - input.position() as usize;
        let chunk_size = cmp::min(NOISE_MAX_PAYLOAD_LEN, remaining_len);
        input.read_exact(&mut self.buffer[..chunk_size])?;
        let encrypted_len = chunk_size + MAC_LENGTH;

        self.noise_session
            .send_message(&mut self.buffer[..encrypted_len])?;

        self.output_queue.extend(&self.buffer[..encrypted_len]);

        Ok(())
    }
}
