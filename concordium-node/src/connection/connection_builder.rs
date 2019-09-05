use crate::{
    common::{get_current_stamp, P2PPeer, RemotePeer},
    connection::{
        connection_private::ConnectionPrivate, fails::MissingFieldsConnectionBuilder,
        network_handler::message_processor::MessageProcessor, Connection, ConnectionStatus,
        FrameSink, FrameStream, HandshakeStreamSink,
    },
    network::NetworkId,
    p2p::P2PNode,
};

use failure::Fallible;
use mio::{net::TcpStream, Token};
use snow::Keypair;

use std::{
    collections::HashSet,
    pin::Pin,
    sync::{atomic::AtomicU64, Arc, RwLock},
};

#[derive(Default)]
pub struct ConnectionBuilder {
    handler_ref:       Option<Pin<Arc<P2PNode>>>,
    key_pair:          Option<Keypair>,
    token:             Option<Token>,
    is_initiator:      bool,
    priv_conn_builder: ConnectionPrivateBuilder,
    noise_params:      Option<snow::params::NoiseParams>,
}

impl ConnectionBuilder {
    pub fn build(self) -> Fallible<Connection> {
        let curr_stamp = get_current_stamp();

        if let (Some(handler_ref), Some(key_pair), Some(token), Some(noise_params)) = (
            self.handler_ref,
            self.key_pair,
            self.token,
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
                messages_received: Default::default(),
                messages_sent: Default::default(),
                last_ping_sent: Arc::new(AtomicU64::new(curr_stamp)),
                token,
                dptr: Arc::new(RwLock::new(priv_conn)),
                pre_handshake_message_processor: MessageProcessor::new(),
                post_handshake_message_processor: MessageProcessor::new(),
                common_message_processor: MessageProcessor::new(),
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

    pub fn set_remote_peer(mut self, p: RemotePeer) -> ConnectionBuilder {
        self.priv_conn_builder = self.priv_conn_builder.set_remote_peer(p);
        self
    }

    pub fn set_local_end_networks(
        mut self,
        local_end_nets: Arc<RwLock<HashSet<NetworkId>>>,
    ) -> ConnectionBuilder {
        self.priv_conn_builder = self
            .priv_conn_builder
            .set_local_end_networks(local_end_nets);
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
                failed_pkts: Default::default(),
                sent_handshake: Default::default(),
                sent_ping: Default::default(),
                last_latency_measured: Default::default(),
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
