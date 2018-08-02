use tarpc::sync::server;
use p2p::{P2PNode, PeerStatistic};
use common::{P2PNodeId, P2PPeer};
use std::cell::RefCell;
use tarpc::util::Never;
use std::thread;
use proto::*;
use futures::future::Future;
use std::net::IpAddr;
use std::str::FromStr;
use grpcio::{ChannelBuilder, EnvBuilder, Environment,
             RpcContext, ServerBuilder, UnarySink};
use std::sync::{Mutex,Arc};
use grpcio;

service! {
    rpc peer_connect(ip: String, port: u16) -> bool;
    rpc peer_list() -> Vec<P2PPeerText>;
    rpc peer_uptime() -> i64;
    rpc peer_total_sent() -> u64;
    rpc peer_total_received() -> u64;
    rpc peer_stats() -> Vec<PeerStatistic>;
    rpc send_message(id: Option<String>, msg: String, broadcast: bool) -> bool;
    rpc get_version() -> String;
}

#[derive(Clone)]
pub struct RpcServer {
    node: RefCell<P2PNode>,
    listen_port: u16,
    listen_addr: String,
    access_token: Option<String>,
}

impl RpcServer {
    pub fn new(node: P2PNode, listen_addr: String, listen_port: u16, access_token: Option<String>) -> Self {
        RpcServer {
            node: RefCell::new(node),
            listen_addr: listen_addr,
            listen_port: listen_port,
            access_token: access_token,
        }
    }

    pub fn spawn(&mut self) -> thread::JoinHandle<()> {
        let self_clone = self.clone();
        thread::spawn(move || {
            let th2 = self_clone.node.borrow_mut().spawn();
            let listen_str = format!("{}:{}", self_clone.listen_addr, self_clone.listen_port);
            let handle = match self_clone.listen(listen_str, server::Options::default()) {
                Ok(x) => x,
                Err(_) => panic!("Couldn't start RPC service!"),
            };

            handle.run();
            th2.join().unwrap();
        })
    }
}

#[derive(Serialize, Deserialize)]
pub struct P2PPeerText {
    ip: String,
    port: u16,
    id: String,
}

impl P2PPeerText {
    pub fn from(peer: &P2PPeer) -> P2PPeerText {
        P2PPeerText {
            ip: format!("{}", peer.ip()),
            port: peer.port(),
            id: peer.id().to_string(),
        }
    }
}

impl SyncService for RpcServer {
    fn peer_connect(&self, ip: String, port: u16) -> Result<bool, Never> {
        info!("Connecting to IP: {} and port: {}!", ip, port);
        self.node.borrow_mut().connect(ip.parse().unwrap(), port);
        Ok(true)
    }

    fn peer_list(&self) -> Result<Vec<P2PPeerText>, Never> {
        Ok(
            self.node.borrow_mut()
                .get_nodes()
                .iter()
                .map(|ref x| P2PPeerText::from(x))
                .collect::<Vec<P2PPeerText>>()
        )
    }

    fn peer_uptime(&self) -> Result<i64, Never> {
        Ok(self.node.borrow_mut().get_uptime())
    }

    fn peer_total_sent(&self) -> Result<u64, Never> {
        Ok(self.node.borrow_mut().get_total_sent())
    }

    fn peer_total_received(&self) -> Result<u64, Never> {
        Ok(self.node.borrow_mut().get_total_received())
    }

    fn peer_stats(&self) -> Result<Vec<PeerStatistic>, Never> {
        Ok(self.node.borrow_mut().get_peer_stats())
    }


    fn send_message(&self, id: Option<String>, msg: String, broadcast: bool) -> Result<bool, Never> {
        info!("Sending message to ID: {:?} with contents: {}. Broadcast? {}", id,msg, broadcast);
        let id = match id {
            Some(x) => Some(P2PNodeId::from_string(x)),
            None => None,
        };

        self.node.borrow_mut().send_message(id, msg.as_bytes(), broadcast);
        Ok(true)
    }

    fn get_version(&self) -> Result<String,Never> {
        Ok(self.node.borrow_mut().get_version())
    }
}

#[derive(Clone)]
struct RpcServerImpl {
    node: RefCell<P2PNode>,
    listen_port: u16,
    listen_addr: String,
    access_token: Option<String>,
    server: Option<Arc<Mutex<grpcio::Server>>>,
}

impl RpcServerImpl {
    pub fn new(node: P2PNode, listen_addr: String, listen_port: u16, access_token: Option<String>) -> Self {
        RpcServerImpl {
            node: RefCell::new(node),
            listen_addr: listen_addr,
            listen_port: listen_port,
            access_token: access_token,
            server: None,
        }
    }

    pub fn start_server(&mut self) {
        let self_clone = self.clone();
            let env = Arc::new(Environment::new(1));
            let service = create_p2_p(self_clone.clone());
            info!("RPC started on port {}",self_clone.listen_port);
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

    fn peer_version(&self, ctx: ::grpcio::RpcContext, req: Empty, sink: ::grpcio::UnarySink<StringResponse>) {
        let mut r: StringResponse = StringResponse::new();
        r.set_value(self.node.borrow().get_version());
        let f = sink.success(r) 
            .map_err(move |e| error!("failed to reply {:?}: {:?}", req, e));
       ctx.spawn(f);
    }

}

#[cfg(test)]
mod tests {
    use std::sync::mpsc;
    use std::{thread,time};
    use p2p::*;
    use common::{NetworkPacket,NetworkMessage, NetworkRequest};
    use rpc::RpcServerImpl;
    use env_logger;
    use std::sync::Arc;
    use grpcio::{ChannelBuilder, EnvBuilder, Environment,
             RpcContext, ServerBuilder, UnarySink};
    use ::proto::*;

    #[test]
    pub fn test_grpc_version() {
        let (pkt_in,pkt_out) = mpsc::channel();

        let (sender, receiver) = mpsc::channel();
            let _guard = thread::spawn(move|| {
                loop {
                    if let Ok(msg) = receiver.recv() {
                        match msg {
                            P2PEvent::ConnectEvent(ip, port) => info!("Received connection from {}:{}", ip, port),
                            P2PEvent::DisconnectEvent(msg) => info!("Received disconnect for {}", msg),
                            P2PEvent::ReceivedMessageEvent(node_id) => info!("Received message from {:?}", node_id),
                            P2PEvent::SentMessageEvent(node_id) => info!("Sent message to {:?}", node_id),
                            P2PEvent::InitiatingConnection(ip,port) => info!("Initiating connection to {}:{}", ip, port),
                        }
                    }
                }
            });
        let node = P2PNode::new(None, 8888, pkt_in, Some(sender));

        let mut _node_self_clone = node.clone();

        let _guard_pkt = thread::spawn(move|| {
            loop {
                if let Ok(msg) = pkt_out.recv() {
                    match msg {
                        NetworkMessage::NetworkPacket(NetworkPacket::DirectMessage(_,_, msg),_,_) => info!( "DirectMessage with {:?} received", msg),
                        NetworkMessage::NetworkPacket(NetworkPacket::BroadcastedMessage(_,msg),_,_) => { 
                            info!("BroadcastedMessage with {:?} received", msg);
                            _node_self_clone.send_message(None,&msg,true);
                        },
                        NetworkMessage::NetworkRequest(NetworkRequest::BanNode(_, x),_,_)  => info!("Ban node request for {:x}", x.get_id()),
                        NetworkMessage::NetworkRequest(NetworkRequest::UnbanNode(_, x), _, _) => info!("Unban node requets for {:x}", x.get_id()), 
                        _ => {}
                    }
                }
            }
        });

        env_logger::init();

        let mut rpc_serv = RpcServerImpl::new(node.clone(), "127.0.0.1".to_string(), 10000, None);
        rpc_serv.start_server();

        let env = Arc::new(EnvBuilder::new().build());
        let ch = ChannelBuilder::new(env).connect("127.0.0.1:10000");

        let client = P2PClient::new(ch);
        
        let reply = client.peer_version(&Empty::new()).expect("rpc");

        assert_eq!(reply.get_value(), env!("CARGO_PKG_VERSION").to_string());

        rpc_serv.stop_server();

    }
}