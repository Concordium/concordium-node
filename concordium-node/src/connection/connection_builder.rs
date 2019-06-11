use crate::{
    common::{get_current_stamp, NetworkRawRequest, P2PPeer, RemotePeer},
    connection::{
        connection_private::ConnectionPrivateBuilder, fails::MissingFieldsConnectionBuilder,
        network_handler::MessageHandler, p2p_event::P2PEvent, Connection,
    },
    dumper::DumpItem,
    network::{Buckets, NetworkId},
    stats_export_service::StatsExportService,
};

use failure::Fallible;
use mio::{net::TcpStream, Token};
use snow::Keypair;

use std::{
    cell::RefCell,
    collections::HashSet,
    rc::Rc,
    sync::{
        mpsc::{channel, Sender},
        Arc, RwLock,
    },
};

#[derive(Default)]
pub struct ConnectionBuilder {
    key_pair:                Option<Keypair>,
    token:                   Option<Token>,
    blind_trusted_broadcast: Option<bool>,
    log_dumper:              Option<Sender<DumpItem>>,
    is_initiator:            bool,
    network_request_sender:  Option<Sender<NetworkRawRequest>>,
    priv_conn_builder:       ConnectionPrivateBuilder,
}

impl ConnectionBuilder {
    pub fn build(self) -> Fallible<Connection> {
        let curr_stamp = get_current_stamp();

        if let (Some(key_pair), Some(token), Some(blind_trusted_broadcast)) =
            (self.key_pair, self.token, self.blind_trusted_broadcast)
        {
            let sender = self.network_request_sender.unwrap_or_else(|| {
                // Create a dummy sender.
                let (s, _) = channel();
                s
            });

            let priv_conn = self
                .priv_conn_builder
                .set_token(token)
                .set_as_initiator(self.is_initiator)
                .set_key_pair(key_pair)
                .set_log_dumper(self.log_dumper)
                .build()?;

            let mut lself = Connection {
                messages_received: 0,
                messages_sent: 0,
                last_ping_sent: curr_stamp,
                network_request_sender: sender,
                dptr: Rc::new(RefCell::new(priv_conn)),
                pre_handshake_message_handler: MessageHandler::new(),
                post_handshake_message_handler: MessageHandler::new(),
                common_message_handler: Rc::new(RefCell::new(MessageHandler::new())),
                blind_trusted_broadcast,
            };

            lself.setup_pre_handshake();
            lself.setup_post_handshake();

            Ok(lself)
        } else {
            Err(failure::Error::from(MissingFieldsConnectionBuilder))
        }
    }

    pub fn set_network_request_sender(mut self, sender: Option<Sender<NetworkRawRequest>>) -> Self {
        self.network_request_sender = sender;
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

    pub fn set_buckets(mut self, buckets: Arc<RwLock<Buckets>>) -> ConnectionBuilder {
        self.priv_conn_builder = self.priv_conn_builder.set_buckets(buckets);
        self
    }

    pub fn set_socket(mut self, s: TcpStream) -> ConnectionBuilder {
        self.priv_conn_builder = self.priv_conn_builder.set_socket(s);
        self
    }

    pub fn set_stats_export_service(
        mut self,
        stats_export_service: Option<Arc<RwLock<StatsExportService>>>,
    ) -> ConnectionBuilder {
        self.priv_conn_builder = self
            .priv_conn_builder
            .set_stats_export_service(stats_export_service);
        self
    }

    pub fn set_event_log(mut self, el: Option<Sender<P2PEvent>>) -> ConnectionBuilder {
        self.priv_conn_builder = self.priv_conn_builder.set_event_log(el);
        self
    }

    pub fn set_blind_trusted_broadcast(mut self, btb: bool) -> ConnectionBuilder {
        self.blind_trusted_broadcast = Some(btb);
        self.priv_conn_builder = self.priv_conn_builder.set_blind_trusted_broadcast(btb);
        self
    }

    pub fn set_log_dumper(mut self, log_dumper: Option<Sender<DumpItem>>) -> ConnectionBuilder {
        self.log_dumper = log_dumper;
        self
    }
}
