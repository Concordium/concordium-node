use common::{NetworkMessage, NetworkPacket, P2PNodeId, P2PPeer};
use db::P2PDB;
use futures::future::Future;
use grpcio;
use grpcio::{Environment, ServerBuilder};
use p2p::P2PNode;
use proto::*;
use std::boxed::Box;
use std::cell::RefCell;
use std::net::IpAddr;
use std::str::FromStr;
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use errors::*;
use errors::ErrorKind::{ QueueingError, ProcessControlError };

#[derive(Clone)]
pub struct RpcServerImpl {
    node: RefCell<P2PNode>,
    listen_port: u16,
    listen_addr: String,
    access_token: String,
    server: Option<Arc<Mutex<grpcio::Server>>>,
    subscription_queue_out: RefCell<Option<Arc<Mutex<mpsc::Receiver<Box<NetworkMessage>>>>>>,
    subscription_queue_in: RefCell<Option<mpsc::Sender<Box<NetworkMessage>>>>,
    db: Option<P2PDB>,
}

impl RpcServerImpl {
    pub fn new(node: P2PNode,
               db: Option<P2PDB>,
               listen_addr: String,
               listen_port: u16,
               access_token: String)
               -> Self {
        RpcServerImpl { node: RefCell::new(node),
                        listen_addr: listen_addr,
                        listen_port: listen_port,
                        access_token: access_token,
                        server: None,
                        subscription_queue_out: RefCell::new(None),
                        subscription_queue_in: RefCell::new(None),
                        db: db, }
    }

    pub fn queue_message(&self, msg: &NetworkMessage) -> ResultExtWrapper<()> {
        if let Some(ref mut sender) = *self.subscription_queue_in.borrow_mut() {
            sender.send(box msg.clone()).chain_err(|| QueueingError("Can't queue message for Rpc retrieval queue".to_string()))?;
        }
        Ok(())
    }

    fn start_subscription(&self) {
        let (sender, receiver) = mpsc::channel::<Box<NetworkMessage>>();
        *self.subscription_queue_in.borrow_mut() = Some(sender);
        *self.subscription_queue_out.borrow_mut() = Some(Arc::new(Mutex::new(receiver)));
    }

    fn stop_subscription(&self) {
        *self.subscription_queue_in.borrow_mut() = None;
        *self.subscription_queue_out.borrow_mut() = None;
    }

    pub fn start_server(&mut self) -> ResultExtWrapper<()> {
        let self_clone = self.clone();
        let env = Arc::new(Environment::new(1));
        let service = create_p2_p(self_clone.clone());
        info!("RPC started on {}:{}",
              self_clone.listen_addr, self_clone.listen_port);
        let mut server =
            ServerBuilder::new(env).register_service(service)
                                   .bind(self_clone.listen_addr, self_clone.listen_port)
                                   .build()
                                   .chain_err(|| ProcessControlError("can't start server".to_string()))?;
        server.start();
        self.server = Some(Arc::new(Mutex::new(server)));
        Ok(())
    }

    pub fn stop_server(&mut self) -> ResultExtWrapper<()>{
        if let Some(ref mut server) = self.server {
            &server.lock().unwrap().shutdown().wait().chain_err(|| ProcessControlError("can't stop process".to_string()))?;
        }
        Ok(())
    }
}

macro_rules! authenticate{
    ($ctx:expr,$req:expr,$sink:expr,$access_token: expr, $inner:block) => {
         match $ctx.request_headers().iter().find(|&val| val.0 == "authentication" ) {
             Some(val) => {
                 if String::from_utf8(val.1.to_vec()).unwrap() == *$access_token {
                    $inner
                 } else {
                     let f = $sink.fail(::grpcio::RpcStatus::new(::grpcio::RpcStatusCode::Unauthenticated, Some("Missing or incorrect token provided".to_string())))
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", $req, e));
                    $ctx.spawn(f);
                 }
             },
             _ => {
                let f = $sink.fail(::grpcio::RpcStatus::new(::grpcio::RpcStatusCode::Unauthenticated, Some("Missing or incorrect token provided".to_string())))
                    .map_err(move |e| error!("failed to reply {:?}: {:?}", $req, e));
                $ctx.spawn(f);
             },
         }
    };
}

impl P2P for RpcServerImpl {
    fn peer_connect(&self,
                    ctx: ::grpcio::RpcContext,
                    req: PeerConnectRequest,
                    sink: ::grpcio::UnarySink<SuccessResponse>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();
            if req.has_ip() && req.has_port() {
                let ip = IpAddr::from_str(req.get_ip().get_value()).unwrap();
                let port = req.get_port().get_value() as u16;
                self.node.borrow_mut().connect(ip, port);
                r.set_value(true);
            } else {
                r.set_value(false);
            }
            let f = sink.success(r)
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn peer_version(&self,
                    ctx: ::grpcio::RpcContext,
                    req: Empty,
                    sink: ::grpcio::UnarySink<StringResponse>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: StringResponse = StringResponse::new();
            r.set_value(self.node.borrow_mut().get_version());
            let f = sink.success(r)
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn peer_uptime(&self,
                   ctx: ::grpcio::RpcContext,
                   req: Empty,
                   sink: ::grpcio::UnarySink<NumberResponse>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: NumberResponse = NumberResponse::new();
            r.set_value(self.node.borrow_mut().get_uptime() as u64);
            let f = sink.success(r)
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn peer_total_received(&self,
                           ctx: ::grpcio::RpcContext,
                           req: Empty,
                           sink: ::grpcio::UnarySink<NumberResponse>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: NumberResponse = NumberResponse::new();
            r.set_value(self.node.borrow_mut().get_total_received() as u64);
            let f = sink.success(r)
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn peer_total_sent(&self,
                       ctx: ::grpcio::RpcContext,
                       req: Empty,
                       sink: ::grpcio::UnarySink<NumberResponse>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: NumberResponse = NumberResponse::new();
            r.set_value(self.node.borrow_mut().get_total_sent() as u64);
            let f = sink.success(r)
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn send_message(&self,
                    ctx: ::grpcio::RpcContext,
                    req: SendMessageRequest,
                    sink: ::grpcio::UnarySink<SuccessResponse>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();
            if req.has_node_id()
               && req.has_message()
               && req.has_broadcast()
               && !req.get_broadcast().get_value()
            {
                let id =
                    P2PNodeId::from_string(&req.get_node_id().get_value().to_string()).unwrap();
                info!("Sending direct message to: {:064x}", id.get_id());
                self.node
                    .borrow_mut()
                    .send_message(Some(id), req.get_message().get_value(), false);
                r.set_value(true);
            } else if req.has_message() && req.has_broadcast() && req.get_broadcast().get_value() {
                info!("Sending broadcast message");
                self.node
                    .borrow_mut()
                    .send_message(None, req.get_message().get_value(), true);
                r.set_value(true);
            } else {
                r.set_value(false);
            }
            let f = sink.success(r)
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn peer_stats(&self,
                  ctx: ::grpcio::RpcContext,
                  req: Empty,
                  sink: ::grpcio::UnarySink<PeerStatsResponse>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let data: Vec<_> = self.node
                                   .borrow_mut()
                                   .get_peer_stats()
                                   .iter()
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
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn peer_list(&self,
                 ctx: ::grpcio::RpcContext,
                 req: Empty,
                 sink: ::grpcio::UnarySink<PeerListResponse>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let data: Vec<_> =
                self.node
                    .borrow_mut()
                    .get_nodes()
                    .unwrap()
                    .iter()
                    .map(|x| {
                             let mut peer_resp = PeerElement::new();
                             let mut node_id = ::protobuf::well_known_types::StringValue::new();
                             node_id.set_value(format!("{:064x}", x.id().get_id()));
                             peer_resp.set_node_id(node_id);
                             let mut ip = ::protobuf::well_known_types::StringValue::new();
                             ip.set_value(x.ip().to_string());
                             peer_resp.set_ip(ip);
                             let mut port = ::protobuf::well_known_types::Int32Value::new();
                             port.set_value(x.port() as i32);
                             peer_resp.set_port(port);
                             peer_resp
                         })
                    .collect();
            let mut resp = PeerListResponse::new();
            resp.set_peer(::protobuf::RepeatedField::from_vec(data));
            let f = sink.success(resp)
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn subscription_start(&self,
                          ctx: ::grpcio::RpcContext,
                          req: Empty,
                          sink: ::grpcio::UnarySink<SuccessResponse>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            self.start_subscription();
            let mut r: SuccessResponse = SuccessResponse::new();
            r.set_value(true);
            let f = sink.success(r)
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn subscription_stop(&self,
                         ctx: ::grpcio::RpcContext,
                         req: Empty,
                         sink: ::grpcio::UnarySink<SuccessResponse>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            self.stop_subscription();
            let mut r: SuccessResponse = SuccessResponse::new();
            r.set_value(true);
            let f = sink.success(r)
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn subscription_poll(&self,
                         ctx: ::grpcio::RpcContext,
                         req: Empty,
                         sink: ::grpcio::UnarySink<P2PNetworkMessage>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: P2PNetworkMessage = P2PNetworkMessage::new();
            if let Some(ref mut receiver) = *self.subscription_queue_out.borrow_mut() {
                match receiver.lock().unwrap().try_recv() {
                    Ok(box NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(sender,
                                                                                      _,
                                                                                      msg),
                                                         sent,
                                                         received)) => {
                        let mut i_msg = MessageDirect::new();
                        i_msg.set_data(msg);
                        r.set_message_direct(i_msg);
                        r.set_sent_at(sent.unwrap());
                        r.set_received_at(received.unwrap());
                        r.set_sender(format!("{:064x}", sender.id().get_id()));
                    }
                    Ok(box NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(sender,
                                                                                           msg),
                                                         sent,
                                                         received)) => {
                        let mut i_msg = MessageBroadcast::new();
                        i_msg.set_data(msg);
                        r.set_message_broadcast(i_msg);
                        r.set_sent_at(sent.unwrap());
                        r.set_received_at(received.unwrap());
                        r.set_sender(format!("{:064x}", sender.id().get_id()));
                    }
                    _ => r.set_message_none(MessageNone::new()),
                }
            } else {
                r.set_message_none(MessageNone::new())
            }
            let f = sink.success(r)
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn ban_node(&self,
                ctx: ::grpcio::RpcContext,
                req: PeerElement,
                sink: ::grpcio::UnarySink<SuccessResponse>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();
            if req.has_node_id() && req.has_ip() && req.has_port() {
                let req_id = req.get_node_id().get_value().to_string();
                let node_id = P2PNodeId::from_string(&req_id);
                let ip = IpAddr::from_str(&req.get_ip().get_value().to_string());
                let port = req.get_port().get_value() as u16;
                if node_id.is_ok() && ip.is_ok() {
                    let mut node = self.node.borrow_mut();
                    let peer = P2PPeer::from(node_id.unwrap(), ip.unwrap(), port);
                    if node.ban_node(peer.clone()) {
                        let db_done = if let Some(ref db) = self.db {
                            db.insert_ban(peer.id().to_string(),
                                          format!("{}", peer.ip()),
                                          peer.port())
                        } else {
                            true
                        };
                        if db_done {
                            r.set_value(node.send_ban(peer.clone()).is_ok());
                        } else {
                            node.unban_node(peer.clone());
                            r.set_value(false);
                        }
                    }
                } else {
                    r.set_value(false);
                }
            } else {
                r.set_value(false);
            }
            let f = sink.success(r)
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn unban_node(&self,
                  ctx: ::grpcio::RpcContext,
                  req: PeerElement,
                  sink: ::grpcio::UnarySink<SuccessResponse>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();
            if req.has_node_id() && req.has_ip() && req.has_port() {
                let req_id = req.get_node_id().get_value().to_string();
                let node_id = P2PNodeId::from_string(&req_id);
                let ip = IpAddr::from_str(&req.get_ip().get_value().to_string());
                let port = req.get_port().get_value() as u16;
                if node_id.is_ok() && ip.is_ok() {
                    let mut node = self.node.borrow_mut();
                    let peer = P2PPeer::from(node_id.unwrap(), ip.unwrap(), port);
                    if node.unban_node(peer.clone()) {
                        let db_done = if let Some(ref db) = self.db {
                            db.delete_ban(peer.id().to_string(),
                                          format!("{}", peer.ip()),
                                          peer.port())
                        } else {
                            true
                        };
                        if db_done {
                            r.set_value(node.send_unban(peer.clone()).is_ok());
                        } else {
                            node.ban_node(peer.clone());
                            r.set_value(false);
                        }
                    }
                } else {
                    r.set_value(false);
                }
            } else {
                r.set_value(false);
            }
            let f = sink.success(r)
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }
}
