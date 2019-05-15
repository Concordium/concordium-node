#[macro_use]
extern crate log;
#[cfg(not(target_os = "windows"))]
extern crate grpciounix as grpcio;
#[cfg(target_os = "windows")]
extern crate grpciowin as grpcio;

#[cfg(test)]
mod tests {
    use concordium_common::{
        functor::{FilterFunctor, Functorable},
        spawn_or_die,
    };
    use concordium_consensus::{consensus::*, ffi::*};
    use grpcio::{ChannelBuilder, EnvBuilder};
    use p2p_client::{
        common::PeerType, configuration::Config, db::P2PDB, network::NetworkMessage,
        p2p::p2p_node::P2PNode, proto::*, rpc::RpcServerImpl,
    };
    use std::sync::{
        atomic::{AtomicUsize, Ordering},
        mpsc, Arc,
    };

    static PORT_OFFSET: AtomicUsize = AtomicUsize::new(0);

    /// It returns next port available and it ensures that next `slot_size`
    /// ports will be available too.
    ///
    /// # Arguments
    /// * `slot_size` - Size of blocked ports. It
    ///
    /// # Example
    /// ```
    /// let port_range_1 = next_port_offset(10); // It will return 0, you can use from 0..9
    /// let port_range_2 = next_port_offset(20); // It will return 10, you can use from 10..19
    /// let port_range_3 = next_port_offset(100); // It will return 30, you can use from 20..129
    /// let port_range_4 = next_port_offset(130);
    /// ```
    fn next_port_offset(slot_size: usize) -> u16 {
        PORT_OFFSET.fetch_add(slot_size, Ordering::SeqCst) as u16
    }

    #[test]
    pub fn test_consensus_tests() {
        start_haskell();
        test_grpc_consensus();
        stop_haskell();
    }

    pub fn test_grpc_consensus() {
        let port_node = next_port_offset(2);

        let (pkt_in, _pkt_out) = mpsc::channel::<Arc<NetworkMessage>>();

        let (genesis_data, private_data) = ConsensusContainer::generate_data(0, 1)
            .unwrap_or_else(|_| panic!("Couldn't read haskell data"));
        let mut consensus_container = ConsensusContainer::new(genesis_data);
        &consensus_container.start_baker(0, private_data[&0].clone());

        let (sender, receiver) = mpsc::channel();
        let _guard = spawn_or_die!("Log loop", move || loop {
            if let Ok(msg) = receiver.recv() {
                info!("{}", msg);
            }
        });

        let mut config = Config::new(Some("127.0.0.1".to_owned()), 18888 + port_node, vec![], 100);
        config.cli.rpc.rpc_server_port = 11000 + port_node;
        config.cli.rpc.rpc_server_addr = "127.0.0.1".to_owned();
        config.cli.rpc.rpc_server_token = "rpcadmin".to_owned();

        let node = P2PNode::new(
            None,
            &config,
            pkt_in,
            Some(sender),
            PeerType::Node,
            None,
            Arc::new(FilterFunctor::new("Broadcasting_checks")),
        );

        let mut rpc_serv = RpcServerImpl::new(
            node,
            P2PDB::default(),
            Some(consensus_container.clone()),
            &config.cli.rpc,
        );
        rpc_serv.start_server().expect("rpc");

        let env = Arc::new(EnvBuilder::new().build());
        let ch = ChannelBuilder::new(env).connect(&format!("127.0.0.1:{}", 11000 + port_node));

        let client = P2PClient::new(ch);

        let mut req_meta_builder = ::grpcio::MetadataBuilder::new();
        req_meta_builder
            .add_str("Authentication", "rpcadmin")
            .unwrap();
        let meta_data = req_meta_builder.build();

        let call_options = ::grpcio::CallOption::default().headers(meta_data.clone());
        match client.get_last_final_account_list_opt(&Empty::new(), call_options.clone()) {
            Ok(ref res) => assert!(!res.payload.is_empty()),
            _ => panic!("Didn't get respones back from sending query"),
        }

        let mut req_bytes = AccountAddress::new();
        req_bytes.set_payload(vec![1, 3, 3, 7]);
        match client.get_last_final_account_info_opt(&req_bytes, call_options) {
            Ok(ref res) => assert!(!res.payload.is_empty()),
            _ => panic!("Didn't get respones back from sending query"),
        }
        consensus_container.stop_baker(0);
    }
}
