use p2p::{P2PNode};
use common::{P2PNodeId,NetworkMessage,NetworkPacket};
use std::cell::RefCell;
use proto::*;
use futures::future::Future;
use std::net::IpAddr;
use std::str::FromStr;
use grpcio::{ServerBuilder,Environment};
use std::sync::{Mutex,Arc};
use grpcio;
use std::sync::mpsc;

#[derive(Clone)]
pub struct RpcServerImpl {
    node: RefCell<P2PNode>,
    listen_port: u16,
    listen_addr: String,
    access_token: Option<String>,
    server: Option<Arc<Mutex<grpcio::Server>>>,
    subscription_queue_out:RefCell<Option<Arc<Mutex<mpsc::Receiver<NetworkMessage>>>>>,
    subscription_queue_in:RefCell<Option<mpsc::Sender<NetworkMessage>>>,
}

impl RpcServerImpl {
    pub fn new(node: P2PNode, listen_addr: String, listen_port: u16, access_token: Option<String>) -> Self {
        RpcServerImpl {
            node: RefCell::new(node),
            listen_addr: listen_addr,
            listen_port: listen_port,
            access_token: access_token,
            server: None,
            subscription_queue_out: RefCell::new(None),
            subscription_queue_in: RefCell::new(None),
        }
    }

    pub fn queue_message(&self, msg: &NetworkMessage) {
        if let Some(ref mut sender) = *self.subscription_queue_in.borrow_mut() {
            sender.send(msg.clone()).unwrap();
        }
    }

    fn start_subscription(&self) {
        let (sender, receiver ) = mpsc::channel::<NetworkMessage>();
        *self.subscription_queue_in.borrow_mut() = Some(sender);
        *self.subscription_queue_out.borrow_mut() = Some(Arc::new(Mutex::new(receiver)));
    }

    fn stop_subscription(&self) {
        *self.subscription_queue_in.borrow_mut() = None;
        *self.subscription_queue_out.borrow_mut() = None;
    }

    pub fn start_server(&mut self) {
        let self_clone = self.clone();
            let env = Arc::new(Environment::new(1));
            let service = create_p2_p(self_clone.clone());
            info!("RPC started on {}:{}",self_clone.listen_addr, self_clone.listen_port);
             let mut server = ServerBuilder::new(env)
                .register_service(service)
                .bind(self_clone.listen_addr, self_clone.listen_port)
                .build()
                .unwrap();
            server.start();
            self.server = Some(Arc::new(Mutex::new(server)));
    }

    pub fn stop_server(&mut self) {
        if let Some(ref mut server) = self.server {
            &server.lock().unwrap().shutdown().wait().unwrap();
        }
    }
}

impl P2P for RpcServerImpl {
    fn peer_connect(&self, ctx: ::grpcio::RpcContext, req: PeerConnectRequest, sink: ::grpcio::UnarySink<SuccessResponse>) {
        let mut r: SuccessResponse = SuccessResponse::new();
        if req.has_ip() && req.has_port() {
            let ip = IpAddr::from_str(req.get_ip().get_value()).unwrap();
            let port = req.get_port().get_value() as u16;
            self.node.borrow_mut().connect(ip, port);
            r.set_value( true );
        } else {
            r.set_value(false);
        } 
        let f = sink.success(r) 
            .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
       ctx.spawn(f);
    }

    fn peer_version(&self, ctx: ::grpcio::RpcContext, _req: Empty, sink: ::grpcio::UnarySink<StringResponse>) {
        let mut r: StringResponse = StringResponse::new();
        r.set_value(self.node.borrow_mut().get_version());
        let f = sink.success(r) 
            .map_err(move |e| error!("failed to reply {:?}: {:?}", _req, e));
       ctx.spawn(f);
    }

    fn peer_uptime(&self, ctx: ::grpcio::RpcContext, _req: Empty, sink: ::grpcio::UnarySink<NumberResponse>) {
        let mut r: NumberResponse = NumberResponse::new();
        r.set_value(self.node.borrow_mut().get_uptime() as u64);
        let f = sink.success(r) 
            .map_err(move |e| error!("failed to reply {:?}: {:?}", _req, e));
       ctx.spawn(f);
    }

    fn peer_total_received(&self, ctx: ::grpcio::RpcContext, _req: Empty, sink: ::grpcio::UnarySink<NumberResponse>) {
        let mut r: NumberResponse = NumberResponse::new();
        r.set_value(self.node.borrow_mut().get_total_received() as u64);
        let f = sink.success(r) 
            .map_err(move |e| error!("failed to reply {:?}: {:?}", _req, e));
       ctx.spawn(f);
    }

    fn peer_total_sent(&self, ctx: ::grpcio::RpcContext, _req: Empty, sink: ::grpcio::UnarySink<NumberResponse>) {
        let mut r: NumberResponse = NumberResponse::new();
        r.set_value(self.node.borrow_mut().get_total_sent() as u64);
        let f = sink.success(r) 
            .map_err(move |e| error!("failed to reply {:?}: {:?}", _req, e));
       ctx.spawn(f);
    }

    fn send_message(&self, ctx: ::grpcio::RpcContext, req: SendMessageRequest, sink: ::grpcio::UnarySink<SuccessResponse>) {
        let mut r: SuccessResponse = SuccessResponse::new();
        if req.has_node_id() && req.has_message() && req.has_broadcast() && !req.get_broadcast().get_value() {
            let id = P2PNodeId::from_string(req.get_node_id().get_value().to_string());
            info!("Sending direct message to: {:064x}", id.get_id());
            self.node.borrow_mut().send_message(Some(id), req.get_message().get_value(), false);
            r.set_value(true);
        } else if req.has_message() && req.has_broadcast() && req.get_broadcast().get_value() {
            info!("Sending broadcast message");
            self.node.borrow_mut().send_message(None, req.get_message().get_value(), true);
            r.set_value(true);
        } else {
            r.set_value(false);
        }
        let f = sink.success(r) 
            .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
       ctx.spawn(f);
    }

    fn peer_stats(&self, ctx: ::grpcio::RpcContext, _req: Empty, sink: ::grpcio::UnarySink<PeerStatsResponse>) {
        let data:Vec<_> = self.node.borrow_mut().get_peer_stats().iter()
            .map(|x| {
                let mut peer_resp = PeerStatsResponse_PeerStats::new();
                peer_resp.set_node_id(x.id.clone());
                peer_resp.set_packets_sent(x.sent);
                peer_resp.set_packets_received(x.received);
                peer_resp
            })
            .collect();
        let mut resp = PeerStatsResponse::new();
        resp.set_peerstats(::protobuf::RepeatedField::from_vec(data));
        let f = sink.success(resp) 
            .map_err(move |e| error!("failed to reply {:?}: {:?}", _req , e));
       ctx.spawn(f);
    }

    fn peer_list(&self, ctx: ::grpcio::RpcContext, _req: Empty, sink: ::grpcio::UnarySink<PeerListResponse>) {
        let data:Vec<_> = self.node.borrow_mut().get_nodes().unwrap().iter()
            .map(|x| {
                let mut peer_resp = PeerListResponse_Peer::new();
                peer_resp.set_node_id(format!("{:064x}",x.id().get_id()));
                peer_resp.set_ip(x.ip().to_string());
                peer_resp.set_port(x.port() as u32);
                peer_resp
            })
            .collect();
        let mut resp = PeerListResponse::new();
        resp.set_peer(::protobuf::RepeatedField::from_vec(data));
        let f = sink.success(resp) 
            .map_err(move |e| error!("failed to reply {:?}: {:?}", _req , e));
       ctx.spawn(f);
    }

    fn subscription_start(&self, ctx: ::grpcio::RpcContext, _req: Empty, sink: ::grpcio::UnarySink<SuccessResponse>) {
        self.start_subscription();
        let mut r: SuccessResponse = SuccessResponse::new();
        r.set_value(true);
        let f = sink.success(r) 
            .map_err(move |e| error!("failed to reply {:?}: {:?}", _req , e));
       ctx.spawn(f);
    }

    fn subscription_stop(&self, ctx: ::grpcio::RpcContext, _req: Empty, sink: ::grpcio::UnarySink<SuccessResponse>) {
        self.stop_subscription();
        let mut r: SuccessResponse = SuccessResponse::new();
        r.set_value(true);
        let f = sink.success(r) 
            .map_err(move |e| error!("failed to reply {:?}: {:?}", _req , e));
       ctx.spawn(f);
    }

    fn subscription_poll(&self, ctx: ::grpcio::RpcContext, _req: Empty, sink: ::grpcio::UnarySink<P2PNetworkMessage>) {
        let mut r:P2PNetworkMessage = P2PNetworkMessage::new();
        if let Some(ref mut receiver) = *self.subscription_queue_out.borrow_mut() {
            match receiver.lock().unwrap().try_recv() {
                Ok(NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(sender, _, msg),sent, received)) => {
                    let mut i_msg = MessageDirect::new();
                    i_msg.set_data(msg);
                    r.set_message_direct(i_msg);
                    r.set_sent_at(sent.unwrap());
                    r.set_received_at(received.unwrap());
                    r.set_sender(format!("{:064x}", sender.id().get_id()));
                },
                Ok(NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(sender, msg),sent, received)) => {
                    let mut i_msg = MessageBroadcast::new();
                    i_msg.set_data(msg);
                    r.set_message_broadcast(i_msg);
                    r.set_sent_at(sent.unwrap());
                    r.set_received_at(received.unwrap());
                    r.set_sender(format!("{:064x}", sender.id().get_id()));
                },
                _ => {
                    r.set_message_none(MessageNone::new())
                }
            }
        } else {
            r.set_message_none(MessageNone::new())
        }
        let f = sink.success(r) 
            .map_err(move |e| error!("failed to reply {:?}: {:?}", _req , e));
       ctx.spawn(f);
    }
}