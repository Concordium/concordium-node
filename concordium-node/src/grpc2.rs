use std::marker::PhantomData;

use prost::bytes::BufMut;

pub mod types {
    include!(concat!(env!("OUT_DIR"), "/concordium.types.rs"));
}

pub mod service {
    include!(concat!(env!("OUT_DIR"), "/concordium_v2.Queries.rs"));
}

#[derive(Default)]
pub struct RawCodec<D>(PhantomData<D>);

impl<D: prost::Message + Default + 'static> tonic::codec::Codec for RawCodec<D> {
    type Decode = D;
    type Decoder = RawDecoder<D>;
    type Encode = Vec<u8>;
    type Encoder = RawEncoder;

    fn encoder(&mut self) -> Self::Encoder { RawEncoder }

    fn decoder(&mut self) -> Self::Decoder { RawDecoder::default() }
}

pub struct RawEncoder;

impl tonic::codec::Encoder for RawEncoder {
    type Error = tonic::Status;
    type Item = Vec<u8>;

    fn encode(
        &mut self,
        item: Self::Item,
        dst: &mut tonic::codec::EncodeBuf<'_>,
    ) -> Result<(), Self::Error> {
        dst.reserve(item.len());
        dst.put_slice(&item);
        Ok(())
    }
}

pub struct RawDecoder<D>(PhantomData<D>);

impl<D> Default for RawDecoder<D> {
    fn default() -> Self { RawDecoder(Default::default()) }
}

impl<D: prost::Message + Default> tonic::codec::Decoder for RawDecoder<D> {
    type Error = tonic::Status;
    type Item = D;

    fn decode(
        &mut self,
        src: &mut tonic::codec::DecodeBuf<'_>,
    ) -> Result<Option<Self::Item>, Self::Error> {
        D::decode(src).map(Option::Some).map_err(|e| tonic::Status::internal(e.to_string()))
    }
}

pub mod server {
    use std::{net::SocketAddr, sync::Arc};

    use futures::StreamExt;
    use tonic::async_trait;

    use crate::{consensus_ffi::{consensus::ConsensusContainer, helpers::ConsensusFfiResponse}, p2p::P2PNode};

    use super::*;
    /// The object used to initiate a gRPC server.
    #[derive(Clone)]
    pub struct RpcServerImpl {
        node:        Arc<P2PNode>,
        listen_addr: SocketAddr,
        consensus:   ConsensusContainer,
    }

    impl RpcServerImpl {
        /// Creates a new RPC server object.
        pub fn new(node: Arc<P2PNode>, consensus: ConsensusContainer) -> anyhow::Result<Self> {
            let listen_addr = SocketAddr::from(([0, 0, 0, 0], 10001));

            Ok(RpcServerImpl {
                node: Arc::clone(&node),
                listen_addr,
                consensus,
            })
        }

        /// Starts the gRPC server, which will shutdown when the provided future
        /// is ready. When shutting down, the server will stop accepting
        /// new requests and shutdown once all the in-progress requests
        /// are completed.
        pub async fn start_server(&mut self) -> anyhow::Result<()> {
            let server = tonic::transport::Server::builder()
                .add_service(service::queries_server::QueriesServer::new(self.clone()));
            server.serve(self.listen_addr).await.map_err(|e| e.into())
        }
    }

    #[async_trait]
    impl service::queries_server::Queries for RpcServerImpl {
        ///Server streaming response type for the FinalizedBlocks method.
        type FinalizedBlocksStream = futures::stream::Map<futures::channel::mpsc::Receiver<Vec<u8>>, fn(Vec<u8>) -> Result<Vec<u8>, tonic::Status>>;

        async fn finalized_blocks(
            &self,
            _request: tonic::Request<crate::grpc2::types::Empty>,
        ) -> Result<tonic::Response<Self::FinalizedBlocksStream>, tonic::Status> {
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            let consensus_response = self.consensus.stream_finalized_blocks(sender);
            match consensus_response {
                ConsensusFfiResponse::Success => {
                    let receiver_fixed = receiver.map(Result::Ok as fn(_) -> _);
                    Ok(tonic::Response::new(receiver_fixed))
                }
                e => {
                    error!("Cannot respond with finalized blocks: {}", e);
                    Err(tonic::Status::resource_exhausted("Cannot create response."))
                }
            }
        }
    }
}
