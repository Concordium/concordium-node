use crate::{
    common::{get_current_stamp, P2PPeer, RemotePeer},
    connection::{
        connection_private::ConnectionPrivateBuilder, fails::MissingFieldsConnectionBuilder,
        network_handler::message_processor::MessageProcessor, Connection,
    },
    network::NetworkId,
    p2p::noise_protocol_handler::NoiseProtocolHandler,
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
    handler_ref:       Option<Pin<Arc<NoiseProtocolHandler>>>,
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
                messages_received: Arc::new(AtomicU64::new(0)),
                messages_sent: Arc::new(AtomicU64::new(0)),
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

    pub fn set_handler_ref(
        mut self,
        handler_ref: Pin<Arc<NoiseProtocolHandler>>,
    ) -> ConnectionBuilder {
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
