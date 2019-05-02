#[macro_use]
extern crate log;
#[cfg(not(target_os = "windows"))]
extern crate grpciounix as grpcio;
#[cfg(target_os = "windows")]
extern crate grpciowin as grpcio;

#[cfg(test)]
mod tests {
    use grpcio::{ChannelBuilder, EnvBuilder, RpcStatusCode};
    use p2p_client::{
        common::{
            functor::{FilterFunctor, Functorable},
            PeerType,
        },
        configuration::Config,
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
                    info!("{}", msg);
                }
            });
            let peer_type = match &$nt[..] {
                "Node" => PeerType::Node,
                "Bootstrapper" => PeerType::Bootstrapper,
                _ => panic!(),
            };

            let mut config = Config::new(
                Some("127.0.0.1".to_owned()),
                next_port_offset_node(1),
                vec![],
                100,
            );

            let node = P2PNode::new(
                $id,
                &config,
                pkt_in,
                Some(sender),
                peer_type,
                None,
                Arc::new(FilterFunctor::new("Broadcasting_checks")),
            );

            let rpc_port = next_port_offset_rpc(1);
            config.cli.rpc.rpc_server_port = rpc_port;
            config.cli.rpc.rpc_server_addr = "127.0.0.1".to_owned();
            config.cli.rpc.rpc_server_token = "rpcadmin".to_owned();
            let mut $r = RpcServerImpl::new(node, P2PDB::default(), None, &config.cli.rpc);
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
        let peer_type = "Node".to_string();
        create_node_rpc_call_option_mode!(client, rpc_serv, call_options, peer_type, None);
        let reply = client
            .peer_version_opt(&Empty::new(), call_options)
            .expect("rpc");

        assert_eq!(reply.get_value(), env!("CARGO_PKG_VERSION").to_string());

        rpc_serv.stop_server().expect("rpc");
    }

    #[test]
    pub fn test_grpc_noauth() {
        let peer_type = "Node".to_string();
        create_node_rpc_call_option_mode!(client, rpc_serv, _call_options, peer_type, None);
        match client.peer_version(&Empty::new()) {
            Err(::grpcio::Error::RpcFailure(ref x)) => {
                assert_eq!(x.status, RpcStatusCode::Unauthenticated)
            }
            _ => panic!("Wrong rejection"),
        }

        rpc_serv.stop_server().expect("rpc");
    }

    // Tests that PeerList call effectively returns the correct PeerType
    #[test]
    pub fn test_grpc_peer_list_node_type() {
        let types = [PeerType::Node, PeerType::Bootstrapper];

        for m in types.into_iter().map(|x| format!("{:?}", x)) {
            info!("testing mode: {}", m);
            grpc_peer_list_node_type_str(m);
        }
    }

    fn grpc_peer_list_node_type_str(peer_type: String) {
        create_node_rpc_call_option_mode!(client, rpc_serv, call_options, peer_type, None);
        let reply = client
            .peer_list_opt(&Empty::new(), call_options)
            .expect("rpc");
        let peer_type = match peer_type.as_str() {
            "Node" => "Node",
            "Bootstrapper" => "Bootstrapper",
            _ => panic!(),
        };
        assert_eq!(reply.peer_type, peer_type);

        rpc_serv.stop_server().expect("rpc");
    }

    #[test]
    pub fn test_grpc_node_info() {
        let peer_type = "Node";
        let id = "000000002dd2b6ed";
        create_node_rpc_call_option_mode!(
            client,
            rpc_serv,
            call_options,
            peer_type,
            Some(id.to_owned())
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
        assert_eq!(reply.peer_type, "Node");
        assert_eq!(reply.node_id.unwrap().get_value(), id);
    }
}
