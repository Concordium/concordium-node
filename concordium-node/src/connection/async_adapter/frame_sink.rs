use crate::connection::{
    async_adapter::{EncryptSink, HandshakeStreamSink, Readiness},
    MessageSendingPriority,
};

use concordium_common::UCursor;

use failure::Fallible;

use std::{cell::RefCell, collections::VecDeque, io::Write, rc::Rc};

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
    frame_queue: VecDeque<UCursor>,
    handshaker:  Option<Rc<RefCell<HandshakeStreamSink>>>,
    encryptor:   Option<EncryptSink>,
}

impl FrameSink {
    pub fn new(handshaker: Rc<RefCell<HandshakeStreamSink>>) -> Self {
        FrameSink {
            frame_queue: VecDeque::new(),
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
        input: UCursor,
        output: &mut impl Write,
        priority: MessageSendingPriority,
    ) -> Fallible<Readiness<usize>> {
        if let Some(ref mut encryptor) = self.encryptor {
            encryptor.write(input, output)
        } else {
            trace!(
                "New frame ({} bytes) was put into a {} frame queue",
                input.len(),
                self.frame_queue.len()
            );
            match priority {
                MessageSendingPriority::High => self.frame_queue.push_front(input),
                MessageSendingPriority::Normal => self.frame_queue.push_back(input),
            }
            self.flush(output)
        }
    }

    pub fn flush(&mut self, output: &mut impl Write) -> Fallible<Readiness<usize>> {
        // 1. Still in handshake phase.
        if let Some(handshaker) = self.handshaker.take() {
            let session_opt = {
                // Ensure that handshake `send` queue is empty, before move on
                let mut handshaker_locker = handshaker.borrow_mut();
                match handshaker_locker.flush(output)? {
                    Readiness::Ready(0) => handshaker_locker.transport_session(),
                    _ => None,
                }
            };

            match session_opt {
                Some(session) => {
                    // Create an encryptor and enqueue pending messages.
                    let mut encryptor = EncryptSink::new(session);

                    let clear_frames = std::mem::replace(&mut self.frame_queue, VecDeque::new());
                    for frame in clear_frames.into_iter() {
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
