use crate::connection::async_adapter::{EncryptSink, HandshakeStreamSink, Readiness};

use concordium_common::hybrid_buf::HybridBuf;

use failure::Fallible;

use std::{
    io::Write,
    sync::{Arc, RwLock},
};

/// A sink to send and encrypt messages over an asynchronous `Write`.
///
/// # Asynchronous
///
/// It is designed to be integrated into a `mio` event log. This is a kind of
/// an asynchronous call because packets usually are not sent in a single call.
/// The number of needed calls (to complete an operation) depends of the output.
/// In this case, output is a socket, and it is limited by its internal buffer
/// size.
///
/// Asynchronous integration is based on two key points.
///
/// ## Send API
///
/// Asynchronous send API is composed of two 2 functions:
///  - `write`. It enqueues a message in order for it to be sent in future.
///  - `flush`. It should be called each time the output `Write` buffers are
///    ready to be written to.
///
/// # Encryption
///
/// It establishes a `Noise IKpsk2` handshake to encrypt data before start
/// sending any data.
pub struct FrameSink {
    frame_queue: Vec<HybridBuf>,
    handshaker:  Option<Arc<RwLock<HandshakeStreamSink>>>,
    encryptor:   Option<EncryptSink>,
}

impl FrameSink {
    pub fn new(handshaker: Arc<RwLock<HandshakeStreamSink>>) -> Self {
        FrameSink {
            frame_queue: Vec::with_capacity(16),
            handshaker:  Some(handshaker),
            encryptor:   None,
        }
    }

    /// If a channel is encrypted, `self.encryptor` will take care of it.
    /// Otherwise, `input` is enqueued until `encryptor` is ready
    /// (after the handshake is done).
    ///
    /// # Return
    /// see `FrameSink::flush`
    pub fn write(
        &mut self,
        input: HybridBuf,
        output: &mut impl Write,
    ) -> Fallible<Readiness<usize>> {
        if let Some(ref mut encryptor) = self.encryptor {
            encryptor.write(input, output)
        } else {
            trace!(
                "New frame ({} bytes) was put into a {} frame queue",
                input.len().unwrap_or(0),
                self.frame_queue.len()
            );
            self.frame_queue.push(input);
            self.flush(output)
        }
    }

    pub fn flush(&mut self, output: &mut impl Write) -> Fallible<Readiness<usize>> {
        // 1. Still in handshake phase.
        if let Some(handshaker) = self.handshaker.take() {
            let session_opt = {
                // Ensure that handshake `send` queue is empty, before move on
                let mut handshaker_locker = write_or_die!(handshaker);
                match handshaker_locker.flush(output)? {
                    Readiness::Ready(0) => handshaker_locker.transport_session(),
                    _ => None,
                }
            };

            match session_opt {
                Some(session) => {
                    // Create an encryptor and enqueue pending messages.
                    let mut encryptor = EncryptSink::new(session);

                    for frame in std::mem::replace(&mut self.frame_queue, Default::default()) {
                        encryptor.write_without_flush(frame)?;
                    }

                    // Move to next phase: encryption.
                    self.encryptor = Some(encryptor);
                }
                None => self.handshaker = Some(handshaker),
            }
        }

        // 2. Encryption
        if let Some(ref mut encryptor) = self.encryptor {
            encryptor.flush(output)
        } else {
            Ok(Readiness::NotReady)
        }
    }
}
