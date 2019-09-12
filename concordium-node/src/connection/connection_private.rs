use std::{
    collections::HashSet,
    net::{Shutdown, SocketAddr},
    pin::Pin,
    sync::{
        atomic::{AtomicU64, Ordering},
        Arc, RwLock,
    },
};

use chrono::prelude::*;
use failure::Fallible;
use mio::{net::TcpStream, Event, Poll, PollOpt, Ready};

use crate::{
    common::{
        counter::TOTAL_MESSAGES_SENT_COUNTER, get_current_stamp, P2PNodeId, PeerStats, PeerType,
        RemotePeer,
    },
    connection::{
        fails, Connection, ConnectionStatus, FrameSink, FrameStream, MessageSendingPriority,
        Readiness,
    },
    dumper::DumpItem,
    network::NetworkId,
};
use concordium_common::hybrid_buf::HybridBuf;

/// It is just a helper struct to facilitate sharing information with
/// message handlers, which are set up from _inside_ `Connection`.
/// In this way, all closures only need two arguments:
///     - This structure as a shared object, like `Rc< RefCell<...>>`
///     - The input message.
pub struct ConnectionPrivate {
    pub conn_ref:            Option<Pin<Arc<Connection>>>,
    pub remote_peer:         RemotePeer,
    pub remote_end_networks: HashSet<NetworkId>,
    pub local_end_networks:  Arc<RwLock<HashSet<NetworkId>>>,

    // Socket and Sink/Stream
    pub socket:         TcpStream,
    pub message_sink:   FrameSink,
    pub message_stream: FrameStream,
    pub status:         ConnectionStatus,

    // Stats
    pub last_seen:   AtomicU64,
    pub failed_pkts: u32,

    // Time
    pub sent_handshake:        Arc<AtomicU64>,
    pub sent_ping:             Arc<AtomicU64>,
    pub last_latency_measured: Arc<AtomicU64>,
}

impl ConnectionPrivate {
    pub fn conn(&self) -> &Pin<Arc<Connection>> {
        self.conn_ref.as_ref().unwrap() // safe; always available
    }

    pub fn update_last_seen(&self) {
        if self.conn().handler().peer_type() != PeerType::Bootstrapper {
            self.last_seen.store(get_current_stamp(), Ordering::SeqCst);
        }
    }

    pub fn last_seen(&self) -> u64 { self.last_seen.load(Ordering::SeqCst) }

    #[inline]
    pub fn add_remote_end_network(&mut self, network: NetworkId) {
        self.remote_end_networks.insert(network);
    }

    #[inline]
    pub fn add_remote_end_networks(&mut self, networks: &HashSet<NetworkId>) {
        self.remote_end_networks.extend(networks.iter())
    }

    pub fn remove_remote_end_network(&mut self, network: NetworkId) {
        self.remote_end_networks.remove(&network);
    }

    pub fn set_measured_ping_sent(&self) {
        self.sent_ping.store(get_current_stamp(), Ordering::SeqCst)
    }

    pub fn remote_peer(&self) -> RemotePeer { self.remote_peer }

    pub fn promote_to_post_handshake(&mut self, id: P2PNodeId, addr: SocketAddr) -> Fallible<()> {
        self.status = ConnectionStatus::PostHandshake;
        self.remote_peer = self.remote_peer.promote_to_post_handshake(id, addr)?;

        // register peer's stats in the P2PNode
        let remote_peer_stats = PeerStats::new(
            id.as_raw(),
            addr,
            self.remote_peer.peer_type(),
            Arc::clone(&self.conn().messages_sent),
            Arc::clone(&self.conn().messages_received),
            Arc::clone(&self.last_latency_measured),
        );
        write_or_die!(self.conn().handler().active_peer_stats)
            .insert(id.as_raw(), remote_peer_stats);

        Ok(())
    }

    /// It registers this connection into `poll`.
    /// This allows us to receive notifications once `socket` is able to read
    /// or/and write.
    #[inline]
    pub fn register(&self, poll: &Poll) -> Fallible<()> {
        into_err!(poll.register(
            &self.socket,
            self.conn().token,
            Ready::readable() | Ready::writable(),
            PollOpt::edge()
        ))
    }

    #[inline]
    pub fn deregister(&self, poll: &Poll) -> Fallible<()> {
        map_io_error_to_fail!(poll.deregister(&self.socket))
    }

    /// It shuts `socket` down.
    #[inline]
    pub fn shutdown(&mut self) -> Fallible<()> {
        map_io_error_to_fail!(self.socket.shutdown(Shutdown::Both))
    }

    /// This function is called when `poll` indicates that `socket` is ready to
    /// write or/and read.
    ///
    /// # Return
    /// A vector of read messages. If message cannot be completed in one read,
    /// an empty vector will be returned.
    pub fn ready(&mut self, ev: &Event) -> Fallible<Vec<HybridBuf>> {
        let mut messages = vec![];
        let ev_readiness = ev.readiness();

        // 1. Try to read messages from `socket`.
        if ev_readiness.is_readable() {
            loop {
                let read_result = self.message_stream.read(&mut self.socket);
                match read_result {
                    Ok(readiness) => match readiness {
                        Readiness::Ready(message) => {
                            self.send_to_dump(&message, true);
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

                        // In this case, we have to drop this connection, so we can avoid to
                        // write any data.
                        self.status = ConnectionStatus::Closing;
                        break;
                    }
                }
            }
        }

        // 2. Write pending data into `socket`.
        if self.status != ConnectionStatus::Closing {
            self.message_sink.flush(&mut self.socket)?;
        }

        // 3. Check closing...
        if self.status == ConnectionStatus::Closing {
            self.shutdown()?;
        }

        Ok(messages)
    }

    /// It sends `input` through `socket`.
    /// This functions returns (almost) immediately, because it does NOT wait
    /// for real write. Function `ConnectionPrivate::ready` will make ensure to
    /// write chunks of the message
    #[inline]
    pub fn async_send(
        &mut self,
        input: HybridBuf,
        priority: MessageSendingPriority,
    ) -> Fallible<Readiness<usize>> {
        TOTAL_MESSAGES_SENT_COUNTER.fetch_add(1, Ordering::Relaxed);
        if let Some(ref stats) = self.conn().handler().stats_export_service {
            stats.pkt_sent_inc();
        }

        self.send_to_dump(&input, false);
        self.message_sink.write(input, &mut self.socket, priority)
    }

    fn send_to_dump(&self, buf: &HybridBuf, inbound: bool) {
        if let Some(ref sender) = self.conn().handler().connection_handler.log_dumper {
            let di = DumpItem::new(
                Utc::now(),
                inbound,
                self.remote_peer(),
                self.remote_peer().addr().ip(),
                buf.clone(),
            );
            let _ = sender.send(di);
        }
    }

    #[cfg(test)]
    pub fn validate_packet_type(&mut self, msg: &[u8]) -> Readiness<bool> {
        self.message_stream.validate_packet_type(msg)
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
                .and_then(|_| attempt_shutdown(self.conn().shutdown()))
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
