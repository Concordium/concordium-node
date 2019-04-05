#![feature(box_syntax, box_patterns)]
#[macro_use]
extern crate log;
#[cfg(not(target_os = "windows"))]
extern crate grpciounix as grpcio;
#[cfg(target_os = "windows")]
extern crate grpciowin as grpcio;

#[cfg(test)]
mod tests {
    use ::grpcio::{ChannelBuilder, EnvBuilder, RpcStatusCode};
    use p2p_client::{
        connection::{P2PEvent, P2PNodeMode},
        db::P2PDB,
        network::NetworkMessage,
        p2p::p2p_node::P2PNode,
        proto::*,
        rpc::RpcServerImpl,
    };
    use std::{
        sync::{
            atomic::{AtomicUsize, Ordering},
            mpsc, Arc,
        },
        thread,
        time::{SystemTime, UNIX_EPOCH},
    };

    static PORT_OFFSET: AtomicUsize = AtomicUsize::new(0);
    static PORT_START_NODE: u16 = 8888;
    static PORT_START_RPC: u16 = 10002;

    fn next_port_offset_node(slot_size: usize) -> u16 {
        PORT_OFFSET.fetch_add(slot_size, Ordering::SeqCst) as u16 + PORT_START_NODE
    }

    fn next_port_offset_rpc(slot_size: usize) -> u16 {
        PORT_OFFSET.fetch_add(slot_size, Ordering::SeqCst) as u16 + PORT_START_RPC
    }

    // Creates P2PClient, RpcServImpl and CallOption instances.
    // The intended use is for spawning nodes for testing gRPC api.
    // The port number is safe as it uses a AtomicUsize for respecting the order.
    macro_rules! create_node_rpc_call_option_mode {
        ($c:ident, $r:ident, $co:ident, $nt:ident, $id:expr) => {
            let (pkt_in, _pkt_out) = mpsc::channel::<Arc<NetworkMessage>>();

            let (sender, receiver) = mpsc::channel();
            let _guard = thread::spawn(move || loop {
                if let Ok(msg) = receiver.recv() {
                    match msg {
                        P2PEvent::ConnectEvent(ip, port) => {
                            info!("Received connection from {}:{}", ip, port)
                        }
                        P2PEvent::DisconnectEvent(msg) => info!("Received disconnect for {}", msg),
                        P2PEvent::ReceivedMessageEvent(node_id) => {
                            info!("Received message from {:?}", node_id)
                        }
                        P2PEvent::SentMessageEvent(node_id) => {
                            info!("Sent message to {:?}", node_id)
                        }
                        P2PEvent::InitiatingConnection(ip, port) => {
                            info!("Initiating connection to {}:{}", ip, port)
                        }
                        P2PEvent::JoinedNetwork(peer, network_id) => {
                            info!(
                                "Peer {} joined network {}",
                                peer.id().to_string(),
                                network_id
                            );
                        }
                        P2PEvent::LeftNetwork(peer, network_id) => {
                            info!("Peer {} left network {}", peer.id().to_string(), network_id);
                        }
                    }
                }
            });
            let node_type = match &$nt[..] {
                "NormalMode" => P2PNodeMode::NormalMode,
                "BootstrapperMode" => P2PNodeMode::BootstrapperMode,
                _ => panic!(),
            };

            let node = P2PNode::new(
                $id,
                Some("127.0.0.1".to_string()),
                next_port_offset_node(1),
                None,
                None,
                pkt_in,
                Some(sender),
                node_type,
                None,
                vec![],
                100,
                true,
            );

            let rpc_port = next_port_offset_rpc(1);
            let mut $r = RpcServerImpl::new(
                node,
                P2PDB::default(),
                None,
                "127.0.0.1".to_string(),
                rpc_port,
                "rpcadmin".to_string(),
            );
            $r.start_server().expect("rpc");

            let env = Arc::new(EnvBuilder::new().build());
            let ch = ChannelBuilder::new(env).connect(&format!("127.0.0.1:{}", rpc_port));

            let $c = P2PClient::new(ch);

            let mut req_meta_builder = ::grpcio::MetadataBuilder::new();
            req_meta_builder
                .add_str("Authentication", "rpcadmin")
                .unwrap();
            let meta_data = req_meta_builder.build();

            let $co = ::grpcio::CallOption::default().headers(meta_data);
        };
    }

    #[test]
    pub fn test_grpc_version() {
        let node_type = "NormalMode".to_string();
        create_node_rpc_call_option_mode!(client, rpc_serv, call_options, node_type, None);
        let reply = client
            .peer_version_opt(&Empty::new(), call_options)
            .expect("rpc");

        assert_eq!(reply.get_value(), env!("CARGO_PKG_VERSION").to_string());

        rpc_serv.stop_server().expect("rpc");
    }

    #[test]
    pub fn test_grpc_noauth() {
        let node_type = "NormalMode".to_string();
        create_node_rpc_call_option_mode!(client, rpc_serv, _call_options, node_type, None);
        match client.peer_version(&Empty::new()) {
            Err(::grpcio::Error::RpcFailure(ref x)) => {
                assert_eq!(x.status, RpcStatusCode::Unauthenticated)
            }
            _ => panic!("Wrong rejection"),
        }

        rpc_serv.stop_server().expect("rpc");
    }

    // Tests that PeerList call effectively returns the correct P2PNodeMode
    #[test]
    pub fn test_grpc_peer_list_node_type() {
        let modes = [P2PNodeMode::NormalMode, P2PNodeMode::BootstrapperMode];

        for m in modes.into_iter().map(|x| format!("{:?}", x)) {
            info!("testing mode: {}", m);
            grpc_peer_list_node_type_str(m);
        }
    }

    fn grpc_peer_list_node_type_str(node_type: String) {
        create_node_rpc_call_option_mode!(client, rpc_serv, call_options, node_type, None);
        let reply = client
            .peer_list_opt(&Empty::new(), call_options)
            .expect("rpc");
        let node_type = match node_type.as_str() {
            "NormalMode" => "Normal",
            "BootstrapperMode" => "Bootstrapper",
            _ => panic!(),
        };
        assert_eq!(reply.node_type, node_type);

        rpc_serv.stop_server().expect("rpc");
    }

    #[test]
    pub fn test_grpc_node_info() {
        let node_type = "NormalMode";
        // the she bytestring below is the first 32 characters of an empty SHA256
        let id = "Cc0Td01Pk/mKDVjJfsQ3rP7P2J0/i3qRAk+2sQz0MtY=".to_string();
        create_node_rpc_call_option_mode!(
            client,
            rpc_serv,
            call_options,
            node_type,
            Some(id.clone())
        );
        let instant1 = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time")
            .as_secs();
        let reply = client
            .node_info_opt(&Empty::new(), call_options)
            .expect("rpc");
        let instant2 = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time")
            .as_secs();
        assert!((reply.current_localtime >= instant1) && (reply.current_localtime <= instant2));
        assert_eq!(reply.node_type, "Normal");
        // As the ID gets transformed into a BigUint and then transformed back
        // num_bigint and format! always output the value in lower-case.
        assert_eq!(reply.node_id.unwrap().get_value(), id);
    }
}
