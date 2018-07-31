use tarpc::sync::server;
use p2p::{P2PNode};
use common::{P2PNodeId};
use std::cell::RefCell;
use tarpc::util::Never;
use std::thread;

service! {
    rpc peer_connect(ip: String, port: u16) -> bool;
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

impl SyncService for RpcServer {
    fn peer_connect(&self, ip: String, port: u16) -> Result<bool, Never> {
        info!("Connecting to IP: {} and port: {}!", ip, port);
        self.node.borrow_mut().connect(ip.parse().unwrap(), port);
        Ok(true)
    }

    fn send_message(&self, id: Option<String>, msg: String, broadcast: bool) -> Result<bool, Never> {
        info!("Sending message to ID: {:?} with contents: {}. Broadcast? {}", id, msg, broadcast);
        let id = match id {
            Some(x) => Some(P2PNodeId::from_string(x)),
            None => None,
        };

        self.node.borrow_mut().send_message(id, msg, broadcast);
        Ok(true)
    }

    fn get_version(&self) -> Result<String,Never> {
        Ok(self.node.borrow_mut().get_version())
    }
}