use std::marker::PhantomData;

use prost::bytes::BufMut;

pub mod types {
    use self::account_info_request::AccountIdentifier;

    include!(concat!(env!("OUT_DIR"), "/concordium.v2.rs"));

    pub(crate) fn account_identifier_to_ffi(
        account_identifier: &AccountIdentifier,
    ) -> Option<(u8, *const u8)> {
        match account_identifier {
            AccountIdentifier::Address(addr) if addr.value.len() == 32 => {
                Some((0u8, addr.value.as_ptr()))
            }
            AccountIdentifier::CredId(cred_id) if cred_id.value.len() == 48 => {
                Some((1u8, cred_id.value.as_ptr()))
            }
            AccountIdentifier::AccountIndex(ai) => {
                Some((2u8, (&ai.value) as *const u64 as *const u8))
            }
            _ => None,
        }
    }

    pub(crate) fn block_hash_input_to_ffi(bhi: &BlockHashInput) -> Option<(u8, *const u8)> {
        use block_hash_input::BlockHashInput::*;
        match bhi.block_hash_input.as_ref()? {
            Best(_) => Some((0, std::ptr::null())),
            LastFinal(_) => Some((1, std::ptr::null())),
            Given(bh) if bh.value.len() == 32 => Some((2, bh.value.as_ptr())),
            _ => None,
        }
    }
}

pub mod service {
    include!(concat!(env!("OUT_DIR"), "/concordium.v2.Queries.rs"));
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

    use crate::{
        consensus_ffi::{consensus::ConsensusContainer, helpers::ConsensusFfiResponse},
        p2p::P2PNode,
    };

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
        type FinalizedBlocksStream = futures::stream::Map<
            futures::channel::mpsc::Receiver<Vec<u8>>,
            fn(Vec<u8>) -> Result<Vec<u8>, tonic::Status>,
        >;

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

        async fn get_account_info(
            &self,
            request: tonic::Request<crate::grpc2::types::AccountInfoRequest>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            let request = request.get_ref();
            let block_hash = request.block_hash.require()?;
            let account_identifier = request.account_identifier.require()?;
            let response = self.consensus.get_account_info_v2(block_hash, account_identifier)?;
            Ok(tonic::Response::new(response))
        }
    }
}

pub(crate) trait Require<E> {
    type A;
    fn require(&self) -> Result<&Self::A, E>;
    fn require_owned(self) -> Result<Self::A, E>;
}

impl<A> Require<tonic::Status> for Option<A> {
    type A = A;

    fn require(&self) -> Result<&Self::A, tonic::Status> {
        match self {
            Some(v) => Ok(v),
            None => Err(tonic::Status::invalid_argument("missing field")),
        }
    }

    fn require_owned(self) -> Result<Self::A, tonic::Status> {
        match self {
            Some(v) => Ok(v),
            None => Err(tonic::Status::invalid_argument("missing field")),
        }
    }
}
