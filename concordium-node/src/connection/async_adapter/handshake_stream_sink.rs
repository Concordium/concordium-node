use crate::connection::async_adapter::{
    partial_copy, PayloadSize, Readiness, PRE_SHARED_KEY, PROLOGUE,
};

use concordium_common::hybrid_buf::HybridBuf;

use byteorder::{NetworkEndian, WriteBytesExt};
use failure::Fallible;
use snow::{Keypair, Session};

use std::{
    collections::VecDeque,
    convert::From,
    io::Write,
    sync::{Arc, RwLock},
};

const MAX_BUFFER_SIZE: usize = 4_096;

/// State of the *IKpsk2* handshake:
/// ```ignore
/// <- s
///  A -> e,es,s,ss
///  B -> e,ee,se,psk
/// ```
#[allow(non_camel_case_types)]
enum HandshakeStreamSinkState {
    ResponderAwaitingRequest_S,
    InitiatorAwaiting_S,
    ResponderAwaiting_E_ES_S_SS,
    InitiatorAwaiting_E_EE_SE_PSK,
}
type TransportSession = Arc<RwLock<Session>>;
type AsyncResultSession = Fallible<Readiness<TransportSession>>;

/// It implements a Handshake stream/sink for Noise *IKpks2*.
/// It should be shared by the encrypt sink and the decrypt stream.
pub struct HandshakeStreamSink {
    // Common
    keypair:           Keypair,
    state:             HandshakeStreamSinkState,
    noise_session:     Option<Session>,
    transport_session: Option<TransportSession>,
    noise_params:      snow::params::NoiseParams,
    buffer:            [u8; MAX_BUFFER_SIZE],

    // Sink
    send_queue:         VecDeque<HybridBuf>,
    last_written_bytes: usize,
}

/// It prefixes `data` with its length, encoded as `u32` in `NetworkEndian`.
fn create_frame(data: &[u8]) -> Fallible<HybridBuf> {
    let mut frame = Vec::with_capacity(data.len() + std::mem::size_of::<PayloadSize>());
    frame.write_u32::<NetworkEndian>(data.len() as u32)?;
    frame.extend_from_slice(data);

    Ok(HybridBuf::from(frame))
}

impl HandshakeStreamSink {
    pub fn new(
        noise_params: snow::params::NoiseParams,
        keypair: Keypair,
        is_initiator: bool,
    ) -> Self {
        let mut send_queue = VecDeque::new();

        let (state, noise_session) = if is_initiator {
            // Start with an empty payload.
            send_queue.push_back(create_frame(&[]).unwrap());

            // The initiator needs remote public key to create its session.
            (HandshakeStreamSinkState::InitiatorAwaiting_S, None)
        } else {
            // The responder does NOT need any key from initiator, so let's create its
            // session.
            let session = snow::Builder::new(noise_params.clone())
                .prologue(PROLOGUE)
                .psk(2, PRE_SHARED_KEY)
                .local_private_key(&keypair.private)
                .build_responder()
                .unwrap();
            (
                HandshakeStreamSinkState::ResponderAwaitingRequest_S,
                Some(session),
            )
        };

        HandshakeStreamSink {
            keypair,
            state,
            noise_session,
            transport_session: None,
            noise_params,
            buffer: [0u8; MAX_BUFFER_SIZE],
            send_queue,
            last_written_bytes: 0,
        }
    }

    /// It returns the transport session if the handshake was done successfully.
    pub fn transport_session(&self) -> Option<TransportSession> {
        self.transport_session.as_ref().map(Arc::clone)
    }

    /// It transforms handshake session into a transport session and stores it.
    fn set_transport_mode(&mut self, transport_session: TransportSession) -> AsyncResultSession {
        self.transport_session = Some(Arc::clone(&transport_session));
        Ok(Readiness::Ready(transport_session))
    }

    fn on_responder_get_request_s(&mut self) -> AsyncResultSession {
        // Send the public key
        self.send_queue
            .push_back(create_frame(&self.keypair.public)?);
        trace!(
            "Responder sends its public key ({} bytes): <- s",
            self.keypair.public.len()
        );

        // Next state
        self.state = HandshakeStreamSinkState::ResponderAwaiting_E_ES_S_SS;

        Ok(Readiness::NotReady)
    }

    fn on_responder_get_e_ee_s_ss(&mut self, mut input: HybridBuf) -> AsyncResultSession {
        // Received: A -> e,es,s,ss
        if let Some(mut session) = self.noise_session.take() {
            let e_es_s_ss = input.remaining_bytes()?;
            session.read_message(e_es_s_ss.as_slice(), &mut self.buffer)?;

            trace!(
                "Responder has received ({} bytes): A -> e,es,s,ss",
                e_es_s_ss.len()
            );

            // Send: B -> e,ee,se,psk
            let buf_len = session.write_message(&[], &mut self.buffer)?;
            self.send_queue
                .push_back(create_frame(&self.buffer[..buf_len])?);
            trace!("Responder sends ({} bytes):B -> e,ee,se,psk", buf_len);

            // Transport session is ready
            self.set_transport_mode(Arc::new(RwLock::new(
                session.into_stateless_transport_mode()?,
            )))
        } else {
            unreachable!("Noise Session (no transport) is none");
        }
    }

    fn on_initiator_get_s(&mut self, mut input: HybridBuf) -> AsyncResultSession {
        // Received: <- s
        let remote_public_key_vw = input.remaining_bytes()?;
        trace!(
            "Initiator has received ({} bytes): <- s",
            remote_public_key_vw.len()
        );

        let mut session = snow::Builder::new(self.noise_params.clone())
            .prologue(PROLOGUE)
            .psk(2, PRE_SHARED_KEY)
            .local_private_key(&self.keypair.private)
            .remote_public_key(remote_public_key_vw.as_slice())
            .build_initiator()?;

        // Send: A -> e,es,s,ss
        let buf_len = session.write_message(&[], &mut self.buffer)?;
        self.send_queue
            .push_back(create_frame(&self.buffer[..buf_len])?);
        trace!("Initiator sends ({} bytes): A -> e,es,s,ss", buf_len);

        self.state = HandshakeStreamSinkState::InitiatorAwaiting_E_EE_SE_PSK;
        self.noise_session = Some(session);
        Ok(Readiness::NotReady)
    }

    fn on_initiator_get_e_ee_se_psk(&mut self, mut input: HybridBuf) -> AsyncResultSession {
        // B -> e,ee,se,psk
        if let Some(mut session) = self.noise_session.take() {
            let e_ee_se_psk = input.remaining_bytes()?;
            trace!(
                "Initiator has received ({} bytes): B -> e,ee,se,psk",
                e_ee_se_psk.len()
            );

            session.read_message(e_ee_se_psk.as_slice(), &mut self.buffer)?;

            // Transport session is ready.
            self.set_transport_mode(Arc::new(RwLock::new(
                session.into_stateless_transport_mode()?,
            )))
        } else {
            unreachable!("Noise Session (no transport) is none");
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
    pub fn read(&mut self, input: HybridBuf) -> AsyncResultSession {
        match self.state {
            HandshakeStreamSinkState::ResponderAwaitingRequest_S => {
                self.on_responder_get_request_s()
            }
            HandshakeStreamSinkState::ResponderAwaiting_E_ES_S_SS => {
                self.on_responder_get_e_ee_s_ss(input)
            }
            HandshakeStreamSinkState::InitiatorAwaiting_S => self.on_initiator_get_s(input),
            HandshakeStreamSinkState::InitiatorAwaiting_E_EE_SE_PSK => {
                self.on_initiator_get_e_ee_se_psk(input)
            }
        }
    }

    pub fn write(
        &mut self,
        input: HybridBuf,
        output: &mut impl Write,
    ) -> Fallible<Readiness<usize>> {
        self.send_queue.push_back(input);
        self.flush(output)
    }

    pub fn flush(&mut self, output: &mut impl Write) -> Fallible<Readiness<usize>> {
        if let Some(mut data) = self.send_queue.pop_front() {
            let written_bytes = self.last_written_bytes + partial_copy(&mut data, output)?;

            // Requeue data if it is not eof.
            if !data.is_eof()? {
                self.last_written_bytes = written_bytes;
                self.send_queue.push_front(data);
                Ok(Readiness::NotReady)
            } else {
                self.last_written_bytes = 0;
                Ok(Readiness::Ready(written_bytes))
            }
        } else {
            Ok(Readiness::Ready(0))
        }
    }
}
