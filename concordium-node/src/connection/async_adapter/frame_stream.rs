use crate::{
    common::PeerType,
    connection::{
        async_adapter::{DecryptStream, HandshakeStreamSink, PayloadSize, NOISE_MAX_MESSAGE_LEN},
        fails::{MessageTooBigError, StreamWouldBlock, UnwantedMessageError},
        Readiness,
    },
    network::{ProtocolMessageType, PROTOCOL_HEADER_LEN, PROTOCOL_MAX_MESSAGE_SIZE},
};

use byteorder::{ByteOrder, NetworkEndian};
use concordium_common::hybrid_buf::HybridBuf;
use failure::{Error, Fallible};

use std::{
    convert::{From, TryFrom},
    io::{ErrorKind, Read},
    sync::{Arc, RwLock},
};

/// A stream to receive and decrypt messages over an asynchronous `Read`.
///
/// # Frame format
///
/// The frame is composed of two fields:
///     - *Frame size* prefix (`PayloadSize` in `NetworkEndian` representation):
///       It is the size in bytes of the payload.
///     - *Payload*.
///
/// # Asynchronous
///
/// Packets are going to be received in chunks due to limitation of the network
/// layer and the handshake protocol itself. In order to receive a
/// completed message, you need to call `read` function several times (using the
/// `mio` event-lop).
///
/// As soon as the message is completed and decrypted, it will return
/// `Readiness::Ready(message)`. Otherwise, a `Readiness::NotReady` is returned.
///
/// # Protocol Maximum Message Size
///
/// Any protocol message is limited to 256MB, and it will generate a
/// `MessageTooBigError` if it exceeds that constraint.
///
/// # Package Early Validation.
///
/// It also validates the kind of the received packet, in order to discard any
/// unexpected message. Right now, the validation covers the following
/// constraints:
///     - In a Bootstrapper Node, any *direct* or *broadcast* message is invalid
///       and generates an `UnwantedMessageError`.
///
/// # Encryption
///
/// It establishes a `Noise IKpsk2` handshake.
///
/// # TODO
///
/// ## Intermediate buffer
///
/// We should be able to load from `Read` directly into our message vector
/// instead of using an intermediate buffer

pub struct FrameStream {
    handshaker: Option<Arc<RwLock<HandshakeStreamSink>>>,
    decryptor:  Option<DecryptStream>,
    peer_type:  PeerType,
    buffer:     [u8; NOISE_MAX_MESSAGE_LEN],

    message:       Vec<u8>,
    expected_size: PayloadSize,
    is_validated:  bool,
}

impl FrameStream {
    /// It uses `peer_type` to emit an early-invalidation upon unexpected
    /// packages.
    pub fn new(peer_type: PeerType, handshaker: Arc<RwLock<HandshakeStreamSink>>) -> Self {
        FrameStream {
            handshaker: Some(handshaker),
            decryptor: None,
            peer_type,
            buffer: [0u8; NOISE_MAX_MESSAGE_LEN],
            message: Vec::with_capacity(std::mem::size_of::<PayloadSize>()),
            expected_size: 0,
            is_validated: false,
        }
    }

    /// It allows packet with empty payloads.
    #[inline]
    fn pending_bytes_to_know_expected_size(&self) -> usize {
        if self.message.len() < std::mem::size_of::<PayloadSize>() {
            std::mem::size_of::<PayloadSize>() - self.message.len()
        } else {
            0
        }
    }

    /// It only reads the first 4 bytes of the message to determine its size.
    ///
    /// This operation could not be completed in just one shot due to limits of
    /// `output` buffers.
    fn read_expected_size(&mut self, input: &mut impl Read) -> Fallible<Readiness<HybridBuf>> {
        // Extract only the bytes needed to know the size.
        let min_bytes = self.pending_bytes_to_know_expected_size();
        let read_bytes = map_io_error_to_fail!(input.read(&mut self.buffer[..min_bytes]))?;

        self.message.extend_from_slice(&self.buffer[..read_bytes]);

        // Load expected size
        if self.message.len() >= std::mem::size_of::<PayloadSize>() {
            // Update target: ignore first 4 bytes on data.
            self.expected_size =
                NetworkEndian::read_u32(&self.message[..std::mem::size_of::<PayloadSize>()]);
            self.message.clear();
            trace!("Expected message size is {}", self.expected_size);

            // Check protocol limits
            if self.expected_size > PROTOCOL_MAX_MESSAGE_SIZE as u32 {
                let error = MessageTooBigError {
                    message_size:  self.expected_size,
                    protocol_size: PROTOCOL_MAX_MESSAGE_SIZE as u32,
                };
                self.clear();
                return Err(Error::from(error));
            }

            // Pre-allocate some space. We don't allocate the expected size in order to
            // reduce the memory foot-print during transmission and the impact
            // of invalid packages.
            let pre_alloc_size = std::cmp::min(NOISE_MAX_MESSAGE_LEN, self.expected_size as usize);
            self.message.reserve(pre_alloc_size);

            // Read data next...
            self.read_payload(input)
        } else {
            // We need more data to determine the message size.
            Ok(Readiness::NotReady)
        }
    }

    /// Once we know the message expected size, we can start to receive data.
    fn read_payload(&mut self, input: &mut impl Read) -> Fallible<Readiness<HybridBuf>> {
        // Read no more than expected, directly into our buffer
        while self.read_intermediate(input)? >= NOISE_MAX_MESSAGE_LEN {
            // Validation only on encrypted channels.
            if self.decryptor.is_some() && !self.is_validated {
                match self.validate() {
                    Readiness::Ready(true) => self.is_validated = true,
                    Readiness::Ready(false) => {
                        self.clear();
                        return Err(Error::from(UnwantedMessageError {
                            message: "Packet is not valid for this node".to_owned(),
                        }));
                    }
                    _ => {}
                }
            }
        }

        if self.expected_size == self.message.len() as u32 {
            // Ready message
            let new_data = std::mem::replace(
                &mut self.message,
                Vec::with_capacity(std::mem::size_of::<PayloadSize>()),
            );
            self.clear();

            Ok(Readiness::Ready(HybridBuf::try_from(new_data)?))
        } else {
            Ok(Readiness::NotReady)
        }
    }

    /// It clears the internal state, in order to be able to receive new
    /// packets.
    fn clear(&mut self) {
        self.is_validated = false;
        self.expected_size = 0;
        self.message.clear();
    }

    /// It invalidates any `Packet` or unknown message when we are in
    /// Bootstrapper mode.
    fn validate(&mut self) -> Readiness<bool> {
        if self.peer_type == PeerType::Bootstrapper {
            if self.message.len() > PROTOCOL_HEADER_LEN {
                let protocol_type =
                    ProtocolMessageType::try_from(self.message[PROTOCOL_HEADER_LEN])
                        .unwrap_or(ProtocolMessageType::Packet);
                match protocol_type {
                    ProtocolMessageType::Packet => Readiness::Ready(false),
                    _ => Readiness::Ready(true),
                }
            } else {
                // The message doesn't have enough data yet.
                Readiness::NotReady
            }
        } else {
            // Any message is valid for non-boostrapper node.
            Readiness::Ready(true)
        }
    }

    fn read_intermediate(&mut self, input: &mut impl Read) -> Fallible<usize> {
        let pending_bytes = self.expected_size - self.message.len() as u32;
        let max_buff = std::cmp::min(pending_bytes as usize, NOISE_MAX_MESSAGE_LEN);

        match input.read(&mut self.buffer[..max_buff]) {
            Ok(read_bytes) => {
                trace!(
                    "Appended {} bytes to current message of {} bytes, and an expected size of {} \
                     bytes",
                    read_bytes,
                    self.message.len(),
                    self.expected_size
                );
                self.message.extend_from_slice(&self.buffer[..read_bytes]);

                Ok(read_bytes)
            }
            Err(err) => match err.kind() {
                ErrorKind::WouldBlock => Ok(0),
                _ => Err(Error::from(err)),
            },
        }
    }

    /// It forwards the payload based on the current state, into:
    ///     a) `Handshaker` stream, if handshake is not yet completed.
    ///     b) `Decryptor` stream, to decrypt payload.
    fn forward(&mut self, input: HybridBuf) -> Fallible<Readiness<HybridBuf>> {
        if let Some(ref mut decryptor) = self.decryptor {
            return Ok(Readiness::Ready(decryptor.read(input)?));
        } else if let Some(handshaker) = self.handshaker.take() {
            // 1. Still in handshake phase.
            let session_rediness = write_or_die!(handshaker).read(input)?;
            match session_rediness {
                // 1.1. Create decryptor using session.
                Readiness::Ready(session) => self.decryptor = Some(DecryptStream::new(session)),
                // 1.2. Keep waiting on handshaker.
                Readiness::NotReady => self.handshaker = Some(handshaker),
            }
        }

        Ok(Readiness::NotReady)
    }

    /// It tries to fully read a message from `input`.
    pub fn read(&mut self, input: &mut impl Read) -> Fallible<Readiness<HybridBuf>> {
        let payload_readiness = if self.expected_size == 0 {
            self.read_expected_size(input)
        } else {
            self.read_payload(input)
        };

        match payload_readiness {
            Ok(Readiness::Ready(payload)) => self.forward(payload),
            Ok(Readiness::NotReady) => Ok(Readiness::NotReady),
            Err(err) => {
                if err.downcast_ref::<StreamWouldBlock>().is_some() {
                    Ok(Readiness::NotReady)
                } else {
                    Err(err)
                }
            }
        }
    }

    #[cfg(test)]
    pub fn validate_packet_type(&mut self, msg: &[u8]) -> Readiness<bool> {
        self.message.clear();
        self.message.extend_from_slice(msg);
        self.validate()
    }
}
