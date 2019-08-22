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
use snow::Keypair;

use crate::{
    common::{get_current_stamp, P2PNodeId, P2PPeer, PeerType, RemotePeer},
    connection::{
        fails, Connection, ConnectionStatus, FrameSink, FrameStream, HandshakeStreamSink,
        MessageSendingPriority, Readiness,
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
    socket:         TcpStream,
    message_sink:   FrameSink,
    message_stream: FrameStream,
    pub status:     ConnectionStatus,

    // Stats
    pub last_seen:   AtomicU64,
    pub failed_pkts: u32,

    // Time
    pub sent_handshake:        Arc<AtomicU64>,
    pub sent_ping:             Arc<AtomicU64>,
    pub last_latency_measured: u64,
}

impl ConnectionPrivate {
    pub fn conn(&self) -> &Pin<Arc<Connection>> {
        self.conn_ref.as_ref().unwrap() // safe; always available
    }

    pub fn update_last_seen(&self) {
        if self.conn().local_peer().peer_type() != PeerType::Bootstrapper {
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
            self.status = ConnectionStatus::Closed;
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
        self.send_to_dump(&input, false);
        self.message_sink.write(input, &mut self.socket, priority)
    }

    fn send_to_dump(&self, buf: &HybridBuf, inbound: bool) {
        if let Some(ref sender) = self.conn().handler().log_dumper {
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

#[derive(Default)]
pub struct ConnectionPrivateBuilder {
    pub local_peer:         Option<P2PPeer>,
    pub remote_peer:        Option<RemotePeer>,
    pub local_end_networks: Option<Arc<RwLock<HashSet<NetworkId>>>>,

    // Sessions
    pub socket:       Option<TcpStream>,
    pub key_pair:     Option<Keypair>,
    pub is_initiator: bool,

    pub noise_params: Option<snow::params::NoiseParams>,
}

impl ConnectionPrivateBuilder {
    pub fn build(self) -> Fallible<ConnectionPrivate> {
        let u64_max_value: u64 = u64::max_value();

        if let (
            Some(local_peer),
            Some(remote_peer),
            Some(local_end_networks),
            Some(socket),
            Some(key_pair),
            Some(noise_params),
        ) = (
            self.local_peer,
            self.remote_peer,
            self.local_end_networks,
            self.socket,
            self.key_pair,
            self.noise_params,
        ) {
            let peer_type = local_peer.peer_type();
            let handshaker = Arc::new(RwLock::new(HandshakeStreamSink::new(
                noise_params.clone(),
                key_pair,
                self.is_initiator,
            )));

            Ok(ConnectionPrivate {
                conn_ref: None,
                remote_peer,
                remote_end_networks: HashSet::new(),
                local_end_networks,
                socket,
                message_sink: FrameSink::new(Arc::clone(&handshaker)),
                message_stream: FrameStream::new(peer_type, handshaker),
                status: ConnectionStatus::PreHandshake,
                last_seen: AtomicU64::new(get_current_stamp()),
                failed_pkts: 0,
                sent_handshake: Arc::new(AtomicU64::new(u64_max_value)),
                sent_ping: Arc::new(AtomicU64::new(u64_max_value)),
                last_latency_measured: u64_max_value,
            })
        } else {
            Err(failure::Error::from(fails::MissingFieldsConnectionBuilder))
        }
    }

    pub fn set_as_initiator(mut self, value: bool) -> Self {
        self.is_initiator = value;
        self
    }

    pub fn set_socket(mut self, socket: TcpStream) -> Self {
        self.socket = Some(socket);
        self
    }

    pub fn set_local_peer(mut self, p: P2PPeer) -> ConnectionPrivateBuilder {
        self.local_peer = Some(p);
        self
    }

    pub fn set_remote_peer(mut self, p: RemotePeer) -> ConnectionPrivateBuilder {
        self.remote_peer = Some(p);
        self
    }

    pub fn set_local_end_networks(
        mut self,
        local_end_nets: Arc<RwLock<HashSet<NetworkId>>>,
    ) -> ConnectionPrivateBuilder {
        self.local_end_networks = Some(local_end_nets);
        self
    }

    pub fn set_key_pair(mut self, kp: Keypair) -> ConnectionPrivateBuilder {
        self.key_pair = Some(kp);
        self
    }

    pub fn set_noise_params(
        mut self,
        params: snow::params::NoiseParams,
    ) -> ConnectionPrivateBuilder {
        self.noise_params = Some(params);
        self
    }
}
