use crate::connection::async_adapter::PayloadSize;

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
    pub message:       Vec<u8>,
    pub expected_size: PayloadSize,
    pub is_validated:  bool,
}

impl FrameStream {
    /// It uses `peer_type` to emit an early-invalidation upon unexpected
    /// packages.
    pub fn new() -> Self {
        FrameStream {
            message:       Vec::with_capacity(std::mem::size_of::<PayloadSize>()),
            expected_size: 0,
            is_validated:  false,
        }
    }

    /// It clears the internal state, in order to be able to receive new
    /// packets.
    pub fn clear_input(&mut self) {
        self.is_validated = false;
        self.expected_size = 0;
        self.message.clear();
    }

    #[cfg(test)]
    pub fn validate_packet_type(&mut self, msg: &[u8]) -> Readiness<bool> {
        self.message.clear();
        self.message.extend_from_slice(msg);
        self.validate()
    }
}
