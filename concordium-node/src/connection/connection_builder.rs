use crate::{
    common::{get_current_stamp, P2PPeer, RemotePeer},
    connection::{
        connection_private::ConnectionPrivate, fails::MissingFieldsConnectionBuilder, Connection,
        FrameSink, FrameStream, HandshakeStreamSink,
    },
    p2p::P2PNode,
};

use failure::Fallible;
use mio::{net::TcpStream, Token};
use snow::Keypair;

use std::{
    pin::Pin,
    sync::{atomic::AtomicU64, Arc, RwLock},
};

#[derive(Default)]
pub struct ConnectionBuilder {
    handler_ref:       Option<Pin<Arc<P2PNode>>>,
    token:             Option<Token>,
    remote_peer:       Option<RemotePeer>,
    key_pair:          Option<Keypair>,
    is_initiator:      bool,
    priv_conn_builder: ConnectionPrivateBuilder,
    noise_params:      Option<snow::params::NoiseParams>,
}

impl ConnectionBuilder {
    pub fn build(self) -> Fallible<Connection> {
        let curr_stamp = get_current_stamp();

        if let (
            Some(handler_ref),
            Some(key_pair),
            Some(token),
            Some(remote_peer),
            Some(noise_params),
        ) = (
            self.handler_ref,
            self.key_pair,
            self.token,
            self.remote_peer,
            self.noise_params,
        ) {
            let priv_conn = self
                .priv_conn_builder
                .set_as_initiator(self.is_initiator)
                .set_key_pair(key_pair)
                .set_noise_params(noise_params.clone())
                .build()?;

            let conn = Connection {
                handler_ref,
                token,
                remote_peer,
                dptr: Arc::new(RwLock::new(priv_conn)),
                remote_end_networks: Default::default(),
                is_post_handshake: Default::default(),
                is_closed: Default::default(),
                messages_received: Default::default(),
                messages_sent: Default::default(),
                last_ping_sent: Arc::new(AtomicU64::new(curr_stamp)),
                sent_handshake: Default::default(),
                sent_ping: Default::default(),
                last_latency_measured: Default::default(),
                last_seen: Arc::new(AtomicU64::new(curr_stamp)),
                failed_pkts: Default::default(),
            };

            write_or_die!(conn.dptr).conn_ref = Some(Arc::pin(conn.clone()));

            Ok(conn)
        } else {
            Err(failure::Error::from(MissingFieldsConnectionBuilder))
        }
    }

    pub fn set_handler_ref(mut self, handler_ref: Pin<Arc<P2PNode>>) -> ConnectionBuilder {
        self.handler_ref = Some(handler_ref);
        self
    }

    pub fn set_as_initiator(mut self, value: bool) -> Self {
        self.is_initiator = value;
        self
    }

    pub fn set_key_pair(mut self, key_pair: Keypair) -> Self {
        self.key_pair = Some(key_pair);
        self
    }

    pub fn set_token(mut self, t: Token) -> ConnectionBuilder {
        self.token = Some(t);
        self
    }

    pub fn set_local_peer(mut self, p: P2PPeer) -> ConnectionBuilder {
        self.priv_conn_builder = self.priv_conn_builder.set_local_peer(p);
        self
    }

    pub fn set_remote_peer(mut self, peer: RemotePeer) -> ConnectionBuilder {
        self.remote_peer = Some(peer);
        self
    }

    pub fn set_socket(mut self, s: TcpStream) -> ConnectionBuilder {
        self.priv_conn_builder = self.priv_conn_builder.set_socket(s);
        self
    }

    pub fn set_noise_params(mut self, params: snow::params::NoiseParams) -> ConnectionBuilder {
        self.noise_params = Some(params);
        self
    }
}

#[derive(Default)]
pub struct ConnectionPrivateBuilder {
    pub local_peer: Option<P2PPeer>,

    // Sessions
    pub socket:       Option<TcpStream>,
    pub key_pair:     Option<Keypair>,
    pub is_initiator: bool,

    pub noise_params: Option<snow::params::NoiseParams>,
}

impl ConnectionPrivateBuilder {
    pub fn build(self) -> Fallible<ConnectionPrivate> {
        if let (Some(local_peer), Some(socket), Some(key_pair), Some(noise_params)) = (
            self.local_peer,
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
                socket,
                message_sink: FrameSink::new(Arc::clone(&handshaker)),
                message_stream: FrameStream::new(peer_type, handshaker),
            })
        } else {
            Err(failure::Error::from(MissingFieldsConnectionBuilder))
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
