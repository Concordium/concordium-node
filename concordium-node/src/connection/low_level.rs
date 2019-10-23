use failure::Fallible;
use mio::tcp::TcpStream;
use snow::Keypair;

use super::{
    async_adapter::{FrameSink, FrameStream, HandshakeStreamSink, Readiness},
    Connection, DeduplicationQueues,
};
use crate::common::p2p_peer::PeerType;
use concordium_common::hybrid_buf::HybridBuf;

use std::{
    pin::Pin,
    sync::{Arc, RwLock},
};

pub struct ConnectionLowLevel {
    pub conn_ref:   Option<Pin<Arc<Connection>>>,
    pub socket:     TcpStream,
    message_sink:   FrameSink,
    message_stream: FrameStream,
}

impl ConnectionLowLevel {
    fn conn(&self) -> &Connection {
        &self.conn_ref.as_ref().unwrap() // safe; always available
    }

    pub fn new(
        peer_type: PeerType,
        socket: TcpStream,
        key_pair: Keypair,
        is_initiator: bool,
        noise_params: snow::params::NoiseParams,
    ) -> Self {
        let handshaker = Arc::new(RwLock::new(HandshakeStreamSink::new(
            noise_params.clone(),
            key_pair,
            is_initiator,
        )));

        if let Err(e) = socket.set_linger(Some(std::time::Duration::from_secs(0))) {
            error!(
                "Can't set SOLINGER to 0 for socket {:?} due to {}",
                socket, e
            );
        }

        ConnectionLowLevel {
            conn_ref: None,
            socket,
            message_sink: FrameSink::new(Arc::clone(&handshaker)),
            message_stream: FrameStream::new(peer_type, handshaker),
        }
    }

    #[inline(always)]
    pub fn read_stream(&mut self, deduplication_queues: &mut DeduplicationQueues) -> Fallible<()> {
        loop {
            let read_result = self.message_stream.read(&mut self.socket);
            match read_result {
                Ok(readiness) => match readiness {
                    Readiness::Ready(message) => {
                        self.conn().send_to_dump(&message, true);
                        if let Err(e) = self.conn().process_message(message, deduplication_queues) {
                            bail!("Can't read the stream: {}", e);
                        }
                    }
                    Readiness::NotReady => break,
                },
                Err(e) => bail!("Can't read the stream: {}", e),
            }
        }

        Ok(())
    }

    #[inline(always)]
    pub fn flush_sink(&mut self) -> Fallible<Readiness<usize>> {
        self.message_sink.flush(&mut self.socket)
    }

    #[inline(always)]
    pub fn write_to_sink(&mut self, input: HybridBuf) -> Fallible<Readiness<usize>> {
        self.message_sink.write(input, &mut self.socket)
    }
}
