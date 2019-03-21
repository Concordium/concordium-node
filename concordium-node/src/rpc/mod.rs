mod fails;

use crate::common::{ConnectionType, P2PNodeId, P2PPeer};
use crate::network::{ NetworkMessage, NetworkPacket };
use crate::db::P2PDB;
use crate::failure::Fallible;
use crate::fails as global_fails;
use futures::future::Future;
use ::grpcio;
use ::grpcio::{Environment, ServerBuilder};
use crate::p2p::P2PNode;
use crate::proto::*;
use std::boxed::Box;
use std::cell::RefCell;
use std::net::IpAddr;
use std::str::FromStr;
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use std::time::{ SystemTime , UNIX_EPOCH};
use byteorder::{BigEndian, WriteBytesExt};
use atomic_counter::AtomicCounter;
use consensus_sys::consensus::ConsensusContainer;
use crate::common::counter::{ TOTAL_MESSAGES_RECEIVED_COUNTER, TOTAL_MESSAGES_SENT_COUNTER };

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
    consensus: Option<ConsensusContainer>,
}

impl RpcServerImpl {
    pub fn new(node: P2PNode,
               db: Option<P2PDB>,
               consensus: Option<ConsensusContainer>,
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
                        db: db,
                        consensus:  consensus.clone(),}
    }

    pub fn queue_message(&self, msg: &NetworkMessage) -> Fallible<()> {
        if let Some(ref mut sender) = *self.subscription_queue_in.borrow_mut() {
            sender.send(box msg.clone())
                .map_err(|_| fails::QueueingError{})?;
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

    pub fn start_server(&mut self) -> Fallible<()> {
        let self_clone = self.clone();
        let env = Arc::new(Environment::new(1));
        let service = create_p2_p(self_clone.clone());
        info!("RPC started on {}:{}",
              self_clone.listen_addr, self_clone.listen_port);
        let mut server = ServerBuilder::new(env).register_service(service)
            .bind(self_clone.listen_addr, self_clone.listen_port)
            .build()
            .map_err(|_| fails::ServerBuildError{})?;
        server.start();
        self.server = Some(Arc::new(Mutex::new(server)));
        Ok(())
    }

    pub fn stop_server(&mut self) -> Fallible<()> {
        if let Some(ref mut server) = self.server {
            server.lock().map_err(global_fails::PoisonError::from)?
                .shutdown() .wait().map_err(fails::GeneralRpcError::from)?;
        }
        Ok(())
    }

     fn send_message_with_error(&self,
                                   req: &SendMessageRequest) -> Fallible<SuccessResponse> {
            let mut r: SuccessResponse = SuccessResponse::new();
            if req.has_node_id()
                && req.has_message()
                && req.has_broadcast()
                && !req.get_broadcast().get_value()
                && req.has_network_id()
            {
                let id = P2PNodeId::from_string(&req.get_node_id().get_value().to_string())?;

                info!("Sending direct message to: {:064x}", id.get_id());
                            r.set_value(self.node
                                        .borrow_mut()
                                        .send_message(Some(id),
                                                      req.get_network_id().get_value() as u16,
                                                      None,
                                                      req.get_message().get_value(),
                                                      false)
                                        .map_err(|e| error!("{}", e))
                                        .is_ok());
            } else if req.has_message() && req.has_broadcast() && req.get_broadcast().get_value() {
                        info!("Sending broadcast message");
                        r.set_value(self.node
                                    .borrow_mut()
                                    .send_message(None,
                                                  req.get_network_id().get_value() as u16,
                                                  None,
                                                  req.get_message().get_value(),
                                                  true)
                                    .map_err(|e| error!("{}", e))
                                    .is_ok());
            } else {
                r.set_value(false);
            }
            Ok(r)
        }
}

macro_rules! authenticate {
    ($ctx:expr,$req:expr,$sink:expr,$access_token: expr, $inner:block) => {
        match $ctx.request_headers().iter().find(|&val| val.0 == "authentication") {
            Some(val) => {
                match String::from_utf8(val.1.to_vec()) {
                    Ok(at) => {
                        if at == *$access_token {
                            $inner
                        } else {
                            let f = $sink.fail(::grpcio::RpcStatus::new(::grpcio::RpcStatusCode::Unauthenticated, Some("Missing or incorrect token provided".to_string()))).map_err(move |e| error!("failed to reply {:?}: {:?}", $req, e));
                            $ctx.spawn(f);
                        }
                    }
                    Err(e) => {
                        let f = $sink.fail(::grpcio::RpcStatus::new(::grpcio::RpcStatusCode::InvalidArgument, Some(e.to_string()))).map_err(move |e| error!("failed to reply {:?}: {:?}", $req, e));
                        $ctx.spawn(f);
                    }
                };
            }
            _ => {
                let f = $sink.fail(::grpcio::RpcStatus::new(::grpcio::RpcStatusCode::Unauthenticated, Some("Missing or incorrect token provided".to_string()))).map_err(move |e| error!("failed to reply {:?}: {:?}", $req, e));
                $ctx.spawn(f);
            }
        };
    }
}

impl P2P for RpcServerImpl {
    fn peer_connect(&self,
                    ctx: ::grpcio::RpcContext,
                    req: PeerConnectRequest,
                    sink: ::grpcio::UnarySink<SuccessResponse>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();
            if req.has_ip() && req.has_port() {
                let ip = IpAddr::from_str(req.get_ip().get_value()).expect(&format!("incorrect IP in peer connect request, current value: {:?}", req.get_ip().get_value()).to_string());
                let port = req.get_port().get_value() as u16;
                r.set_value(self.node
                                .borrow_mut()
                                .connect(ConnectionType::Node, ip, port, None)
                                .map_err(|e| error!("{}", e))
                                .is_ok());
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
            r.set_value( TOTAL_MESSAGES_RECEIVED_COUNTER.get() as u64);
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
            r.set_value( TOTAL_MESSAGES_SENT_COUNTER.get() as u64);
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

            let f = match self.send_message_with_error(&req) {
                Ok(r) => sink.success(r),
                Err(e) => sink.fail(grpcio::RpcStatus::new(grpcio::RpcStatusCode::InvalidArgument, e.name().map(|x| String::from_str(x).expect("Unwrapping of an error name failed"))))
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn join_network(&self,
                    ctx: ::grpcio::RpcContext,
                    req: NetworkChangeRequest,
                    sink: ::grpcio::UnarySink<SuccessResponse>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();
            if req.has_network_id()
               && req.get_network_id().get_value() > 0
               && req.get_network_id().get_value() < 100_000
            {
                info!("Attempting to join network {}",
                      req.get_network_id().get_value());
                r.set_value(self.node
                                .borrow_mut()
                                .send_joinnetwork(req.get_network_id().get_value() as u16)
                                .map_err(|e| error!("{}", e))
                                .is_ok());
            } else {
                r.set_value(false);
            }
            let f = sink.success(r)
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn leave_network(&self,
                     ctx: ::grpcio::RpcContext,
                     req: NetworkChangeRequest,
                     sink: ::grpcio::UnarySink<SuccessResponse>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();
            if req.has_network_id()
               && req.get_network_id().get_value() > 0
               && req.get_network_id().get_value() < 100_000
            {
                info!("Attempting to leave network {}",
                      req.get_network_id().get_value());
                r.set_value(self.node
                                .borrow_mut()
                                .send_leavenetwork(req.get_network_id().get_value() as u16)
                                .map_err(|e| error!("{}", e))
                                .is_ok());
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
            let f = match self.node.borrow_mut().get_peer_stats(&vec![]) {
                Ok(data) => {
                    let data: Vec<_> =
                        data
                        .iter()
                        .map(|x| {
                            let mut peer_resp = PeerStatsResponse_PeerStats::new();
                            peer_resp.set_node_id(x.id.clone());
                            peer_resp.set_packets_sent(x.sent);
                            peer_resp.set_packets_received(x.received);
                            x.measured_latency().map_or_else(
                                || {} ,
                                |val| peer_resp.set_measured_latency(val)
                            );
                            peer_resp
                        })
                        .collect();
                    let mut resp = PeerStatsResponse::new();
                    resp.set_peerstats(::protobuf::RepeatedField::from_vec(data));
                    sink.success(resp)
                },
                Err(e) => {
                    sink.fail(grpcio::RpcStatus::new(grpcio::RpcStatusCode::Aborted, Some(e.name().expect("Couldn't extract error name").to_string())))
                }
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn peer_list(&self,
                 ctx: ::grpcio::RpcContext,
                 req: Empty,
                 sink: ::grpcio::UnarySink<PeerListResponse>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let borrowed_node = self.node.borrow_mut();
            let f = match borrowed_node.get_peer_stats(&vec![]) {
                Ok(data) => {
                    let data: Vec<_> =
                        data
                        .iter()
                        .map(|x| {
                            let mut peer_resp = PeerElement::new();
                            let mut node_id = ::protobuf::well_known_types::StringValue::new();
                            node_id.set_value(format!("{}", x.id()));
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
                    let node_type = match &format!("{:?}", borrowed_node.get_node_mode())[..] {
                        "NormalMode" | "NormalPrivateMode" => {
                            "Normal"
                        }
                        "BootstrapperMode" | "BootstrapperPrivateMode" => {
                            "Bootstrapper"
                        }
                        _ => {
                            panic!()
                        }
                    };
                    resp.set_node_type(node_type.to_string());
                    resp.set_peer(::protobuf::RepeatedField::from_vec(data));
                    sink.success(resp)
                },
                Err(e) => {
                    sink.fail(grpcio::RpcStatus::new(grpcio::RpcStatusCode::Aborted, Some(e.name().expect("Couldn't extract error name").to_string())))
                }
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn node_info(&self,
                 ctx: ::grpcio::RpcContext,
                 req: Empty,
                 sink: ::grpcio::UnarySink<NodeInfoResponse>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut resp = NodeInfoResponse::new();
            let mut node_id = ::protobuf::well_known_types::StringValue::new();
            node_id.set_value(format!("{}", self.node.borrow().get_own_id().to_string()));
            resp.set_node_id(node_id);
            let curtime = SystemTime::now().duration_since( UNIX_EPOCH ).expect("Time went backwards").as_secs();
            resp.set_current_localtime(curtime);
            // TODO: use enums for matching
            let node_type = match &format!("{:?}", self.node.borrow().get_node_mode())[..] {
                "NormalMode" | "NormalPrivateMode" => {
                    "Normal"
                }
                "BootstrapperMode" | "BootstrapperPrivateMode" => {
                    "Bootstrapper"
                }
                _ => {
                    panic!()
                }
            };
            resp.set_node_type(node_type.to_string());
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
                match receiver.lock() {
                    Err(_) => {r.set_message_none(MessageNone::new()); } ,
                    Ok(rec) => {
                        match rec.try_recv() {
                            Ok(box NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(sender,
                                                                                              msgid,
                                                                                              _,
                                                                                              network_id,
                                                                                              msg),
                                                                 sent,
                                                                 received)) => {
                                let mut i_msg = MessageDirect::new();
                                i_msg.set_data(msg);
                                r.set_message_direct(i_msg);
                                sent.map_or_else(
                                    || {},
                                    |sent| {r.set_sent_at(sent);}
                                );
                                received.map_or_else(
                                    || {},
                                    |val| {r.set_received_at(val);}
                                );
                                r.set_network_id(network_id as u32);
                                r.set_message_id(msgid);
                                r.set_sender(format!("{:064x}", sender.id().get_id()));
                            }
                            Ok(box NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(sender,
                                                                                                   msg_id,
                                                                                                   network_id,
                                                                                                   msg),
                                                                 sent,
                                                                 received)) => {
                                let mut i_msg = MessageBroadcast::new();
                                i_msg.set_data(msg);
                                r.set_message_broadcast(i_msg);
                                sent.map_or_else(
                                    || {},
                                    |sent| {r.set_sent_at(sent);}
                                );
                                received.map_or_else(
                                    || {},
                                    |val| {r.set_received_at(val);}
                                );
                                r.set_network_id(network_id as u32);
                                r.set_message_id(msg_id);
                                r.set_sender(format!("{:064x}", sender.id().get_id()));
                            }
                            _ => r.set_message_none(MessageNone::new()),
                        }
                    }
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
            let f = if req.has_node_id() && req.has_ip() && req.has_port() {
                let req_id = req.get_node_id().get_value().to_string();
                let node_id = P2PNodeId::from_string(&req_id);
                let ip = IpAddr::from_str(&req.get_ip().get_value().to_string());
                let port = req.get_port().get_value() as u16;
                if node_id.is_ok() && ip.is_ok() {
                    let mut node = self.node.borrow_mut();
                    let peer =
                        P2PPeer::from(ConnectionType::Node, node_id.unwrap(), ip.unwrap(), port);
                    if node.ban_node(peer.clone()).is_ok() {
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
                            node.unban_node(peer.clone())
                                .map_err(|e| error!("{}", e))
                                .ok();
                            r.set_value(false);
                        }
                    }
                     sink.success(r)
                } else {
                    sink.fail(grpcio::RpcStatus::new(grpcio::RpcStatusCode::InvalidArgument,
                                                     Some("invalid node_id or ip".to_string())))
                }
            } else {
                sink.fail(grpcio::RpcStatus::new(grpcio::RpcStatusCode::InvalidArgument,
                                                 Some("missing node_id, ip or port".to_string())))
            };
           let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }

    fn po_c_send_transaction(&self,
                  ctx: ::grpcio::RpcContext,
                  req: PoCSendTransactionMessage,
                  sink: ::grpcio::UnarySink<SuccessResponse>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();
            if let Some(ref consensus) = self.consensus {
                if consensus.send_transaction(&req.get_message_content().to_string()) == 0 as i64 {
                    let mut out_bytes = vec![];
                    match out_bytes.write_u16::<BigEndian>(1 as u16) {
                        Ok(_) => {
                            out_bytes.extend(req.get_message_content().as_bytes());
                            match self.node.borrow_mut().send_message(None, req.get_network_id() as u16, None, &out_bytes , true)  {
                                Ok(_) => {
                                    info!("Transmitted transaction to network");
                                    r.set_value(true);
                                }
                                Err(_) => {
                                    error!("Couldn't transmit transaction to network");
                                    r.set_value(false);
                                }
                            }
                        }
                        _ => r.set_value(false),
                    }
                } else {
                    r.set_value(false);
                }
            }
            let f = sink.success(r)
                        .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        } );
    }

    fn get_best_block_info(&self,
        ctx: ::grpcio::RpcContext,
        req: Empty,
        sink: ::grpcio::UnarySink<BestBlockInfoMessage> ) {
            authenticate!(ctx, req, sink, &self.access_token, {
                if let Some(ref consensus) = self.consensus {
                    match consensus.get_best_block_info() {
                        Some(ref res) => {
                            let mut r: BestBlockInfoMessage = BestBlockInfoMessage::new();
                            r.set_best_block_info(res.clone());
                            let f = sink.success(r)
                                .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
                            ctx.spawn(f);
                        }
                        _ => {
                            let f = sink.fail(
                                    ::grpcio::RpcStatus::new(::grpcio::RpcStatusCode::Internal, None))
                                .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
                            ctx.spawn(f);
                        }
                    }
                }
            });
        }

    fn unban_node(&self,
                  ctx: ::grpcio::RpcContext,
                  req: PeerElement,
                  sink: ::grpcio::UnarySink<SuccessResponse>) {
        authenticate!(ctx, req, sink, &self.access_token, {
            let mut r: SuccessResponse = SuccessResponse::new();
            let f = if req.has_node_id() && req.has_ip() && req.has_port() {
                let req_id = req.get_node_id().get_value().to_string();
                let node_id = P2PNodeId::from_string(&req_id);
                let ip = IpAddr::from_str(&req.get_ip().get_value().to_string());
                let port = req.get_port().get_value() as u16;
                if node_id.is_ok() && ip.is_ok() {
                    let mut node = self.node.borrow_mut();
                    let peer =
                        P2PPeer::from(ConnectionType::Node, node_id.unwrap(), ip.unwrap(), port);
                    if node.unban_node(peer.clone()).is_ok() {
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
                            node.ban_node(peer.clone())
                                .map_err(|e| error!("{}", e))
                                .ok();
                            r.set_value(false);
                        }
                    }
                    sink.success(r)
                } else {
                      sink.fail(grpcio::RpcStatus::new(grpcio::RpcStatusCode::InvalidArgument,
                                                     Some("invalid node_id or ip".to_string())))
                }
            } else {
                sink.fail(grpcio::RpcStatus::new(grpcio::RpcStatusCode::InvalidArgument,
                                                 Some("missing node_id, ip or port".to_string())))
            };
            let f = f.map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
            ctx.spawn(f);
        });
    }
}
