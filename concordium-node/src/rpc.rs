use tarpc::sync::server;
use p2p::{P2PNode};
use common::{P2PNodeId, P2PPeer};
use std::cell::RefCell;
use tarpc::util::Never;
use std::thread;

service! {
    rpc peer_connect(ip: String, port: u16) -> bool;
    rpc peer_list() -> Vec<P2PPeerText>;
    rpc peer_uptime() -> i64;
    rpc peer_total_sent() -> u64;
    rpc peer_total_received() -> u64;
    rpc send_message(id: Option<String>, msg: String, broadcast: bool) -> bool;
    rpc get_version() -> String;
}

#[derive(Clone)]
pub struct RpcServer {
    node: RefCell<P2PNode>,
    listen_port: u16,
    listen_addr: String,
}

impl RpcServer {
    pub fn new(node: P2PNode, listen_addr: String, listen_port: u16) -> Self {
        RpcServer {
            node: RefCell::new(node),
            listen_addr: listen_addr,
            listen_port: listen_port,
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