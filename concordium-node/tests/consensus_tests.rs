#![feature(box_syntax, box_patterns)]
#[macro_use]
extern crate log;
#[cfg(not(target_os = "windows"))]
extern crate grpciounix as grpcio;
#[cfg(target_os = "windows")]
extern crate grpciowin as grpcio;

#[cfg(test)]
mod tests {
    use ::grpcio::{ChannelBuilder, EnvBuilder};
    use p2p_client::network::NetworkMessage;
    use p2p_client::connection::{ P2PEvent, P2PNodeMode };
    use p2p_client::p2p::p2p_node::{ P2PNode };
    use p2p_client::proto::*;
    use p2p_client::rpc::RpcServerImpl;
    use std::sync::mpsc;
    use std::sync::Arc;
    use std::thread;
    use consensus_sys::consensus::*;
    use std::sync::atomic::{ AtomicUsize, Ordering};

    static PORT_OFFSET: AtomicUsize = AtomicUsize::new(0);

    /// It returns next port available and it ensures that next `slot_size` ports will be
    /// available too.
    ///
    /// # Arguments
    /// * `slot_size` - Size of blocked ports. It
    ///
    /// # Example
    /// ```
    /// let port_range_1 = next_port_offset( 10);   // It will return 0, you can use from 0..9
    /// let port_range_2 = next_port_offset( 20);   // It will return 10, you can use from 10..19
    /// let port_range_3 = next_port_offset( 100);  // It will return 30, you can use from 20..129
    /// let port_range_4 = next_port_offset( 130);
    /// ```
    fn next_port_offset( slot_size: usize) -> u16 {
        PORT_OFFSET.fetch_add( slot_size, Ordering::SeqCst) as u16
    }

    #[test]
    pub fn test_consensus_tests() {
        ConsensusContainer::start_haskell();
        test_grpc_consensus();
        ConsensusContainer::stop_haskell();
    }

    pub fn test_grpc_consensus() {
        let port_node = next_port_offset( 2);

        let (pkt_in, _pkt_out) = mpsc::channel::<Arc<NetworkMessage>>();

        let (genesis_data, private_data) = ConsensusContainer::generate_data(0, 1)
            .unwrap_or_else(|_| panic!("Couldn't read haskell data"));
        let mut consensus_container = ConsensusContainer::new(genesis_data);
        &consensus_container.start_baker(0, private_data[&0].clone());

        let (sender, receiver) = mpsc::channel();
        let _guard =
            thread::spawn(move || {
                              loop {
                                  if let Ok(msg) = receiver.recv() {
                                      match msg {
                                          P2PEvent::ConnectEvent(ip, port) => {
                                              info!("Received connection from {}:{}", ip, port)
                                          }
                                          P2PEvent::DisconnectEvent(msg) => {
                                              info!("Received disconnect for {}", msg)
                                          }
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
                                              info!("Peer {} joined network {}",
                                                    peer.id().to_string(),
                                                    network_id);
                                          }
                                          P2PEvent::LeftNetwork(peer, network_id) => {
                                              info!("Peer {} left network {}",
                                                    peer.id().to_string(),
                                                    network_id);
                                          }
                                      }
                                  }
                              }
                          });

        let node = P2PNode::new(None,
                                Some("127.0.0.1".to_string()),
                                18888+port_node,
                                None,
                                None,
                                pkt_in,
                                Some(sender),
                                P2PNodeMode::NormalPrivateMode,
                                None,
                                vec![],
                                100,
                                true);

        let mut rpc_serv = RpcServerImpl::new(node,
                                              None,
                                              Some(consensus_container.clone()),
                                              "127.0.0.1".to_string(),
                                              11000+port_node,
                                              "rpcadmin".to_string());
        rpc_serv.start_server().expect("rpc");

        let env = Arc::new(EnvBuilder::new().build());
        let ch = ChannelBuilder::new(env).connect(&format!("127.0.0.1:{}", 11000+port_node));

        let client = P2PClient::new(ch);

        let mut req_meta_builder = ::grpcio::MetadataBuilder::new();
        req_meta_builder.add_str("Authentication", "rpcadmin")
                        .unwrap();
        let meta_data = req_meta_builder.build();

        let call_options = ::grpcio::CallOption::default().headers(meta_data.clone());
        match client.get_last_final_account_list_opt(&Empty::new(), call_options) {
            Ok(ref res) => assert!(res.payload.len()> 0),
            _ => panic!("Didn't get respones back from sending query"),
        }
        consensus_container.stop_baker(0);
    }
}
