use failure::Fallible;
use mio::{net::TcpStream, Event};

use crate::connection::{
    async_adapter::Readiness, fails, Connection, FrameSink, FrameStream, MessageSendingPriority,
};
use concordium_common::hybrid_buf::HybridBuf;

use std::{
    net::Shutdown,
    pin::Pin,
    sync::{atomic::Ordering, Arc},
};

pub struct ConnectionPrivate {
    pub conn_ref:       Option<Pin<Arc<Connection>>>,
    pub socket:         TcpStream,
    pub message_sink:   FrameSink,
    pub message_stream: FrameStream,
}

impl ConnectionPrivate {
    pub fn conn(&self) -> &Pin<Arc<Connection>> {
        self.conn_ref.as_ref().unwrap() // safe; always available
    }

    pub fn shutdown(&mut self) -> Fallible<()> {
        map_io_error_to_fail!(self.socket.shutdown(Shutdown::Both))
    }

    pub fn read_from_stream(&mut self, ev: &Event) -> Fallible<Vec<HybridBuf>> {
        let mut messages = vec![];
        let ev_readiness = ev.readiness();

        // 1. Try to read messages from `socket`.
        if ev_readiness.is_readable() {
            loop {
                let read_result = self.message_stream.read(&mut self.socket);
                match read_result {
                    Ok(readiness) => match readiness {
                        Readiness::Ready(message) => {
                            self.conn().send_to_dump(&message, true);
                            messages.push(message)
                        }
                        Readiness::NotReady => break,
                    },
                    Err(err) => {
                        let token_id = usize::from(self.conn().token);

                        if err.downcast_ref::<fails::UnwantedMessageError>().is_none()
                            && err.downcast_ref::<fails::MessageTooBigError>().is_none()
                        {
                            warn!(
                                "Protocol error, connection {} is dropped: {}",
                                token_id, err
                            );
                        } else {
                            error!("Message stream error on connection {}: {:?}", token_id, err);
                        }

                        // In this case, we have to drop this connection, so we can avoid
                        // writing any data.
                        self.conn().close();
                        break;
                    }
                }
            }
        }

        // 2. Write pending data into `socket` or shut the connection down
        if !self.conn().is_closing.load(Ordering::SeqCst) {
            self.message_sink.flush(&mut self.socket)?;
        } else {
            self.shutdown()?;
        }

        Ok(messages)
    }

    pub fn write_to_sink(
        &mut self,
        input: HybridBuf,
        priority: MessageSendingPriority,
    ) -> Fallible<Readiness<usize>> {
        self.message_sink.write(input, &mut self.socket, priority)
    }
}

impl Drop for ConnectionPrivate {
    fn drop(&mut self) {
        debug!(
            "Closing connection {} ({}:{})",
            usize::from(self.conn().token),
            self.conn().remote_addr().ip(),
            self.conn().remote_addr().port()
        );

        let attempt_shutdown = |shutdown_call: Fallible<()>| -> Fallible<()> {
            use crate::connection::fails::PeerTerminatedConnection;
            match shutdown_call {
                Err(err) => {
                    if err.downcast_ref::<PeerTerminatedConnection>().is_some() {
                        Ok(())
                    } else {
                        Err(err)
                    }
                }
                _ => Ok(()),
            }
        };

        let result = {
            // Deregister connection from the poll and shut down the socket
            attempt_shutdown(self.conn().deregister(&self.conn().handler().poll))
                .and_then(|_| attempt_shutdown(self.shutdown()))
        };

        if let Err(e) = result {
            error!(
                "Connection {} couldn't be closed: {:?}",
                usize::from(self.conn().token),
                e
            );
            return;
        }

        // Report number of peers to stats export engine
        if let Some(ref service) = self.conn().handler().stats_export_service {
            if self.conn().is_post_handshake() {
                service.peers_dec();
            }
        }
    }
}
