use anyhow::Context;
use prost::bytes::BufMut;
use std::{
    convert::{TryFrom, TryInto},
    marker::PhantomData,
    path::Path,
};

/// Types generated from the types.proto file, together
/// with some auxiliary definitions that help passing values through the FFI
/// boundary.
pub mod types {
    // Tell clippy to allow large enum variants in the generated code.
    #![allow(clippy::large_enum_variant)]

    include!(concat!(env!("OUT_DIR"), "/concordium.v2.rs"));

    /// Convert an account address to a pointer to the content. The length of
    /// the content is checked to be 32 bytes.
    ///
    /// # Safety
    /// The caller **must** ensure that the pointer is not used after the
    /// reference to the supplied `account_identifier` is no longer
    /// retained.
    pub(crate) fn account_address_to_ffi(address: &AccountAddress) -> Option<*const u8> {
        if address.value.len() == 32 {
            Some(address.value.as_ptr())
        } else {
            None
        }
    }

    /// Convert an account identifier to a pair of a tag and a pointer to the
    /// content. The length of the content is determined by the tag, which is
    /// either 0 for the address, 1 for the credential registration ID, and 2
    /// for the account index.
    ///
    /// # Safety
    /// The caller **must** ensure that the pointer is not used after the
    /// reference to the supplied `account_identifier` is no longer
    /// retained.
    pub(crate) fn account_identifier_to_ffi(
        account_identifier: &AccountIdentifierInput,
    ) -> Option<(u8, *const u8)> {
        use account_identifier_input::AccountIdentifierInput::*;
        match account_identifier.account_identifier_input.as_ref()? {
            Address(addr) if addr.value.len() == 32 => Some((0u8, addr.value.as_ptr())),
            CredId(cred_id) if cred_id.value.len() == 48 => Some((1u8, cred_id.value.as_ptr())),
            AccountIndex(ai) => Some((2u8, (&ai.value) as *const u64 as *const u8)),
            _ => None,
        }
    }

    /// Convert the [BlockHashInput] to a pair of a tag and pointer to the
    /// content. The tag is 0 for "Best" block, 1 for "LastFinal" block, and
    /// 2 for a specific block given by a hash. If the tag is 0 or 1 then
    /// there is no additional data, and the content pointer is `null`.
    ///
    /// # Safety
    /// The caller **must** ensure that the pointer is not used after the
    /// reference to the supplied `bhi` is no longer retained.
    pub(crate) fn block_hash_input_to_ffi(bhi: &BlockHashInput) -> Option<(u8, *const u8)> {
        use block_hash_input::BlockHashInput::*;
        match bhi.block_hash_input.as_ref()? {
            Best(_) => Some((0, std::ptr::null())),
            LastFinal(_) => Some((1, std::ptr::null())),
            Given(bh) if bh.value.len() == 32 => Some((2, bh.value.as_ptr())),
            _ => None,
        }
    }

    /// Convert [ModuleRef] to a pointer to the content. The length of the
    /// content is checked to be 32 bytes.
    ///
    /// # Safety
    /// The caller **must** ensure that the pointer is not used after the
    /// reference to the supplied `module_ref` is no longer retained.
    pub(crate) fn module_reference_to_ffi(module_ref: &ModuleRef) -> Option<*const u8> {
        if module_ref.value.len() == 32 {
            Some(module_ref.value.as_ptr())
        } else {
            None
        }
    }

    /// Convert [TransactionHash] to a pointer to the content. The length of the
    /// content is checked to be 32 bytes.
    ///
    /// # Safety
    /// The caller **must** ensure that the pointer is not used after the
    /// reference to the supplied `transaction_hash` is no longer retained.
    pub(crate) fn transaction_hash_to_ffi(transaction_hash: &TransactionHash) -> Option<*const u8> {
        if transaction_hash.value.len() == 32 {
            Some(transaction_hash.value.as_ptr())
        } else {
            None
        }
    }
}

/// The service generated from the configuration in the `build.rs` file.
pub mod service {
    include!(concat!(env!("OUT_DIR"), "/concordium.v2.Queries.rs"));
}

/// Service configuration, listing which endpoints are enabled.
/// If the endpoint is not listed in the configuration file it will be disabled.
/// This is what the `#[serde(default)]` annotations achieve.
#[derive(serde::Deserialize)]
struct ServiceConfig {
    #[serde(default)]
    get_finalized_blocks: bool,
    #[serde(default)]
    get_blocks:           bool,
    #[serde(default)]
    get_account_list:     bool,
    #[serde(default)]
    get_account_info:     bool,
}

impl ServiceConfig {
    pub const fn new_all_enabled() -> Self {
        Self {
            get_finalized_blocks: true,
            get_blocks:           true,
            get_account_list:     true,
            get_account_info:     true,
        }
    }

    pub fn from_file(source: &Path) -> anyhow::Result<ServiceConfig> {
        let config: ServiceConfig = toml::from_slice(
            &std::fs::read(source).context("Unable to read the endpoints configuration file.")?,
        )
        .context("Unable to parse the endpoints configuration file.")?;
        Ok(config)
    }
}

/// The "codec" used by [tonic] to encode proto messages.
/// Normally [tonic] works with [prost] to encode messages. However that
/// requires that we have all the data modelled in Rust. Our case is that the
/// data that is encoded most of the time lives on the consensus/Haskell side of
/// the FFI boundary. Thus it is natural to encode it there and only pass the
/// byte array through FFI.. This presents a challenge since the grpc server is
/// ultimately in Rust for various reasons. We solve the issue by using
/// [tonic]'s support for custom [codecs](https://docs.rs/tonic/latest/tonic/codec/index.html).
///
/// In particular we define this "RawCodec" which knows how to encode messages
/// that are already encoded, i.e., messages that are `AsRef<[u8]>`.
///
/// **NB: This codec does not know how to decode anything, and any attempt to do
/// so will lead to an RPC error.**
pub struct RawCodec<E, D>(PhantomData<E>, PhantomData<D>);
impl<E, D> Default for RawCodec<E, D> {
    fn default() -> Self { Self(Default::default(), Default::default()) }
}

impl<E: AsRef<[u8]> + Send + Sync + 'static, D: prost::Message + Default + 'static>
    tonic::codec::Codec for RawCodec<E, D>
{
    type Decode = D;
    type Decoder = RawDecoder<D>;
    type Encode = E;
    type Encoder = RawEncoder<E>;

    fn encoder(&mut self) -> Self::Encoder { RawEncoder::default() }

    fn decoder(&mut self) -> Self::Decoder { RawDecoder::default() }
}

pub struct RawEncoder<E>(PhantomData<E>);

impl<E> Default for RawEncoder<E> {
    fn default() -> Self { Self(Default::default()) }
}

impl<E: AsRef<[u8]>> tonic::codec::Encoder for RawEncoder<E> {
    type Error = tonic::Status;
    type Item = E;

    fn encode(
        &mut self,
        item: Self::Item,
        dst: &mut tonic::codec::EncodeBuf<'_>,
    ) -> Result<(), Self::Error> {
        dst.reserve(item.as_ref().len());
        dst.put_slice(item.as_ref());
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

/// The implementation of the GRPC2 server.
pub mod server {
    use crate::{
        configuration::GRPC2Config,
        consensus_ffi::{
            consensus::{ConsensusContainer, CALLBACK_QUEUE},
            ffi::NotificationHandlers,
            helpers::{ConsensusFfiResponse, PacketType},
            messaging::{ConsensusMessage, MessageType},
        },
        p2p::P2PNode,
    };
    use anyhow::Context;
    use byteorder::{BigEndian, WriteBytesExt};
    use crypto_common::{types::MAX_MEMO_SIZE, Version};
    use futures::{FutureExt, StreamExt};
    use std::{
        io::Write,
        sync::{Arc, Mutex},
    };
    use tonic::{async_trait, transport::ServerTlsConfig};

    use super::*;

    /// An updatable list of listeners for events generated by the node.
    /// These are specifically for events that are generated naturally during
    /// consensus operations. Currently that is "block arrived" and "block
    /// finalized".
    type Clients = Arc<Mutex<Vec<tokio::sync::mpsc::Sender<Result<Arc<[u8]>, tonic::Status>>>>>;

    /// The type that implements the service that responds to queries.
    struct RpcServerImpl {
        /// Configuration of enabled endpoints.
        service_config: ServiceConfig,
        /// Reference to the node to support network and node status related
        /// queries.
        #[allow(dead_code)] // this will be used by network queries.
        node: Arc<P2PNode>,
        /// Reference to consensus to support consensus queries.
        consensus: ConsensusContainer,
        /// The list of active clients listening for new block arrivals.
        blocks_channels: Clients,
        /// The list of active clients listening for new finalized blocks.
        finalized_blocks_channels: Clients,
    }

    /// An administrative structure that collects objects needed to manage the
    /// the GRPC2 server.
    pub struct GRPC2Server {
        /// A handle to the actual server task. This is used to shut down the
        /// server upon node shutdown if it does not want to shut down
        /// itself.
        task:                   tokio::task::JoinHandle<Result<(), tonic::transport::Error>>,
        /// A one-shot channel used to send an interrupt to the server, asking
        /// it to stop.
        shutdown_sender:        tokio::sync::oneshot::Sender<()>,
        /// The handles to background tasks that relay messages from the queue
        /// which is written to by consensus, to the receivers for any
        /// existing clients. There is a task for relaying blocks, and one for
        /// relaying finalized blocks.
        blocks_relay:           tokio::task::JoinHandle<()>,
        finalized_blocks_relay: tokio::task::JoinHandle<()>,
    }

    impl GRPC2Server {
        /// Creates a new RPC server if the configuration demands it.
        /// Otherwise returns `Ok(None)`. If the server needs to, but cannot be
        /// created an error is returned.
        pub fn new(
            node: &Arc<P2PNode>,
            consensus: &ConsensusContainer,
            config: &GRPC2Config,
            notification_handlers: NotificationHandlers,
        ) -> anyhow::Result<Option<Self>> {
            if let Some(listen_addr) = config.listen_addr {
                // the error in the following line should never happen since the command-line
                // parser should already make sure that when the address is defined, so is the
                // port.
                let listen_port = config.listen_port.context("Missing GRPC port")?;

                let service_config = if let Some(ref source) = config.endpoint_config {
                    ServiceConfig::from_file(source)?
                } else {
                    ServiceConfig::new_all_enabled()
                };

                let identity = match (&config.x509_cert, &config.cert_private_key) {
                    (None, None) => None,
                    (None, Some(_)) => {
                        anyhow::bail!("Private key supplied, but not the certificate.")
                    }
                    (Some(_), None) => {
                        anyhow::bail!("Certificate supplied, but not the private key.")
                    }
                    (Some(cert_path), Some(key_path)) => {
                        let cert =
                            std::fs::read(cert_path).context("Unable to read certificate.")?;
                        let key = std::fs::read(key_path).context("Unable to read key.")?;
                        let identity = tonic::transport::Identity::from_pem(cert, key);
                        Some(identity)
                    }
                };
                let server = RpcServerImpl {
                    service_config,
                    node: Arc::clone(node),
                    consensus: consensus.clone(),
                    blocks_channels: Arc::new(Mutex::new(Vec::new())),
                    finalized_blocks_channels: Arc::new(Mutex::new(Vec::new())),
                };

                let NotificationHandlers {
                    mut blocks,
                    mut finalized_blocks,
                } = notification_handlers;

                let blocks_channel = server.blocks_channels.clone();
                let blocks_relay = tokio::spawn(async move {
                    while let Some(v) = blocks.next().await {
                        match blocks_channel.lock() {
                            Ok(mut senders) => senders.retain(|sender| {
                                if let Err(e) = sender.try_send(Ok(v.clone())) {
                                    match e {
                                        tokio::sync::mpsc::error::TrySendError::Full(_) => {
                                            // if the client is too slow they will just not get all
                                            // the blocks. If they miss some they can query.
                                            // Retaining old blocks would complicate the server and
                                            // go beyond the simple semantics that the current API
                                            // has.
                                            // We still retain the client for the next block though.
                                            true
                                        }
                                        tokio::sync::mpsc::error::TrySendError::Closed(_) => false,
                                    }
                                } else {
                                    true
                                }
                            }),
                            Err(e) => {
                                error!("Could not acquire lock to the list of receivers: {}.", e)
                            }
                        }
                    }
                });

                let finalized_blocks_channel = server.finalized_blocks_channels.clone();
                let finalized_blocks_relay = tokio::spawn(async move {
                    while let Some(v) = finalized_blocks.next().await {
                        match finalized_blocks_channel.lock() {
                            Ok(mut senders) => senders.retain(|sender| {
                                if let Err(e) = sender.try_send(Ok(v.clone())) {
                                    match e {
                                        tokio::sync::mpsc::error::TrySendError::Full(_) => {
                                            // if the client is too slow they will just not get all
                                            // the blocks. If they miss some they can query.
                                            // Retaining old blocks would complicate the server and
                                            // go beyond
                                            // the simple semantics that the current API has.
                                            // We still retain the client for the next block though.
                                            true
                                        }
                                        tokio::sync::mpsc::error::TrySendError::Closed(_) => false,
                                    }
                                } else {
                                    true
                                }
                            }),
                            Err(e) => {
                                error!("Could not acquire lock to the list of receivers: {}.", e)
                            }
                        }
                    }
                });
                let service = service::queries_server::QueriesServer::new(server);
                let log_layer = tower_http::trace::TraceLayer::new_for_grpc();
                let mut builder = tonic::transport::Server::builder().layer(log_layer);
                if let Some(identity) = identity {
                    builder = builder
                        .tls_config(ServerTlsConfig::new().identity(identity))
                        .context("Unable to configure TLS.")?;
                } else {
                    // if TLS is not enabled and we want grpc-web we need to explicitly
                    // enable http1 support.
                    // If TLS is enabled this is not necessary because TLS supports protocol
                    // negotiation.
                    if config.enable_grpc_web {
                        builder = builder.accept_http1(true);
                    }
                }

                let (shutdown_sender, shutdown_receiver) = tokio::sync::oneshot::channel::<()>();

                let router = if config.enable_grpc_web {
                    builder.add_service(tonic_web::enable(service))
                } else {
                    builder.add_service(service)
                };

                let task = tokio::spawn(async move {
                    router
                        .serve_with_shutdown(
                            std::net::SocketAddr::new(listen_addr, listen_port),
                            shutdown_receiver.map(|_| ()),
                        )
                        .await
                });
                Ok(Some(Self {
                    task,
                    shutdown_sender,
                    blocks_relay,
                    finalized_blocks_relay,
                }))
            } else {
                Ok(None)
            }
        }

        /// Stop the server and any associated tasks.
        /// If the server does not stop on its own, the server task will be
        /// terminated after at most 10s.
        pub async fn shutdown(self) {
            if self.shutdown_sender.send(()).is_err() {
                error!("Could not stop the GRPC2 server correctly. Forcing shutdown.");
                self.task.abort();
            }
            self.blocks_relay.abort();
            self.finalized_blocks_relay.abort();
            // Force the rpc server to shut down in at most 10 seconds.
            let timeout_duration = std::time::Duration::from_secs(10);
            match tokio::time::timeout(timeout_duration, self.task).await {
                Ok(res) => {
                    if let Err(err) = res {
                        if err.is_cancelled() {
                            info!("GRPC2 server was successfully stopped.");
                        } else if err.is_panic() {
                            error!("GRPC2 server panicked: {}", err);
                        }
                    }
                }
                Err(timed_out) => {
                    warn!("RPC server was forcefully shut down due to: {}", timed_out);
                }
            }
        }
    }

    #[async_trait]
    impl service::queries_server::Queries for RpcServerImpl {
        /// Return type for the 'GetAccountList' method.
        type GetAccountListStream =
            futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'GetAncestors' method.
        type GetAncestorsStream = futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'Blocks' method.
        type GetBlocksStream =
            tokio_stream::wrappers::ReceiverStream<Result<Arc<[u8]>, tonic::Status>>;
        /// Return type for the 'FinalizedBlocks' method.
        type GetFinalizedBlocksStream =
            tokio_stream::wrappers::ReceiverStream<Result<Arc<[u8]>, tonic::Status>>;
        /// Return type for the 'GetInstanceList' method.
        type GetInstanceListStream =
            futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'GetModuleList' method.
        type GetModuleListStream = futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;

        async fn get_blocks(
            &self,
            _request: tonic::Request<crate::grpc2::types::Empty>,
        ) -> Result<tonic::Response<Self::GetBlocksStream>, tonic::Status> {
            if !self.service_config.get_blocks {
                return Err(tonic::Status::unimplemented("`GetBlocks` is not enabled."));
            }
            let (sender, receiver) = tokio::sync::mpsc::channel(100);
            match self.blocks_channels.lock() {
                Ok(mut fbs) => {
                    fbs.push(sender);
                }
                Err(e) => {
                    error!("Could not acquire lock: {}", e);
                    return Err(tonic::Status::internal("Could not enqueue request."));
                }
            }
            Ok(tonic::Response::new(tokio_stream::wrappers::ReceiverStream::new(receiver)))
        }

        async fn get_finalized_blocks(
            &self,
            _request: tonic::Request<crate::grpc2::types::Empty>,
        ) -> Result<tonic::Response<Self::GetFinalizedBlocksStream>, tonic::Status> {
            if !self.service_config.get_finalized_blocks {
                return Err(tonic::Status::unimplemented("`GetFinalizedBlocks` is not enabled."));
            }
            let (sender, receiver) = tokio::sync::mpsc::channel(100);
            match self.finalized_blocks_channels.lock() {
                Ok(mut fbs) => {
                    fbs.push(sender);
                }
                Err(e) => {
                    error!("Could not acquire lock: {}", e);
                    return Err(tonic::Status::internal("Could not enqueue request."));
                }
            }
            Ok(tonic::Response::new(tokio_stream::wrappers::ReceiverStream::new(receiver)))
        }

        async fn get_account_info(
            &self,
            request: tonic::Request<crate::grpc2::types::AccountInfoRequest>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if !self.service_config.get_account_info {
                return Err(tonic::Status::unimplemented("`GetAccountInfo` is not enabled."));
            }
            let request = request.get_ref();
            let block_hash = request.block_hash.as_ref().require()?;
            let account_identifier = request.account_identifier.as_ref().require()?;
            let (hash, response) =
                self.consensus.get_account_info_v2(block_hash, account_identifier)?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_account_list(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetAccountListStream>, tonic::Status> {
            if !self.service_config.get_account_list {
                return Err(tonic::Status::unimplemented("`GetAccountList` is not enabled."));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            let hash = self.consensus.get_account_list_v2(request.get_ref(), sender)?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_module_list(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetModuleListStream>, tonic::Status> {
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            let hash = self.consensus.get_module_list_v2(request.get_ref(), sender)?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_module_source(
            &self,
            request: tonic::Request<crate::grpc2::types::ModuleSourceRequest>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            let request = request.get_ref();
            let block_hash = request.block_hash.as_ref().require()?;
            let module_ref = request.module_ref.as_ref().require()?;
            let (hash, response) = self.consensus.get_module_source_v2(block_hash, module_ref)?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_instance_list(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetInstanceListStream>, tonic::Status> {
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            let hash = self.consensus.get_instance_list_v2(request.get_ref(), sender)?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_instance_info(
            &self,
            request: tonic::Request<crate::grpc2::types::InstanceInfoRequest>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            let request = request.get_ref();
            let block_hash = request.block_hash.as_ref().require()?;
            let contract_address = request.address.as_ref().require()?;
            let (hash, response) =
                self.consensus.get_instance_info_v2(block_hash, contract_address)?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_next_account_sequence_number(
            &self,
            request: tonic::Request<crate::grpc2::types::AccountAddress>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            let response = self.consensus.get_next_account_sequence_number_v2(request.get_ref())?;
            Ok(tonic::Response::new(response))
        }

        async fn get_consensus_info(
            &self,
            _request: tonic::Request<crate::grpc2::types::Empty>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            let response = self.consensus.get_consensus_info_v2()?;
            Ok(tonic::Response::new(response))
        }

        async fn get_ancestors(
            &self,
            request: tonic::Request<crate::grpc2::types::AncestorsRequest>,
        ) -> Result<tonic::Response<Self::GetAncestorsStream>, tonic::Status> {
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            let request = request.get_ref();
            let block_hash = request.block_hash.as_ref().require()?;
            let amount = request.amount;
            let hash = self.consensus.get_ancestors_v2(block_hash, amount, sender)?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_block_item_status(
            &self,
            request: tonic::Request<crate::grpc2::types::TransactionHash>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            let response = self.consensus.get_block_item_status_v2(request.get_ref())?;
            Ok(tonic::Response::new(response))
        }

        async fn send_block_item(
            &self,
            request: tonic::Request<crate::grpc2::types::SendBlockItemRequest>,
        ) -> Result<tonic::Response<crate::grpc2::types::TransactionHash>, tonic::Status> {
            use ConsensusFfiResponse::*;

            if self.node.is_network_stopped() {
                return Err(tonic::Status::failed_precondition(
                    "The network is stopped due to unrecognized protocol update.",
                ));
            }

            let (transaction, transaction_hash) =
                serial_and_hash_send_block_item_request(request.into_inner())?;
            if transaction.len() > crate::configuration::PROTOCOL_MAX_TRANSACTION_SIZE {
                warn!("Received a transaction that exceeds maximum transaction size.");
                return Err(tonic::Status::invalid_argument(
                    "Transaction size exceeds maximum allowed size.",
                ));
            }
            let consensus_result = self.consensus.send_transaction(&transaction);

            let result = if consensus_result == Success {
                let mut payload = Vec::with_capacity(1 + transaction.len());
                payload.write_u8(PacketType::Transaction as u8)?;
                payload.write_all(&transaction)?;

                CALLBACK_QUEUE.send_out_message(ConsensusMessage::new(
                    MessageType::Outbound(None),
                    PacketType::Transaction,
                    Arc::from(payload),
                    vec![],
                    None,
                ))
            } else {
                Err(consensus_result.into())
            };

            let mk_err_response = |code, error| Err(tonic::Status::new(code, error));
            let mk_err_invalid_argument_response =
                |error| mk_err_response(tonic::Code::InvalidArgument, error);

            match (result, consensus_result) {
                (Ok(_), Success) => {
                    Ok(tonic::Response::new(crate::grpc2::types::TransactionHash {
                        value: transaction_hash,
                    }))
                }
                (Err(e), Success) => {
                    warn!("Couldn't put a transaction in the outbound queue due to {:?}", e);
                    Err(tonic::Status::new(
                        tonic::Code::Internal,
                        format!("Couldn't put a transaction in the outbound queue due to {:?}", e),
                    ))
                }
                // the wildcard is always Err as only 'Success' responses from the consensus are
                // being retransmitted. In other words Ok(_) implies consensus_result == Success
                (_, DuplicateEntry) => {
                    mk_err_response(tonic::Code::AlreadyExists, DuplicateEntry.to_string())
                }
                (_, ConsensusShutDown) => {
                    warn!(
                        "Consensus didn't accept a transaction via RPC due to {:?}",
                        ConsensusShutDown.to_string()
                    );
                    mk_err_invalid_argument_response(ConsensusShutDown.to_string())
                }
                (_, consensus_error) => {
                    mk_err_invalid_argument_response(consensus_error.to_string())
                }
            }
        }
    }

    /// Try to serial and hash a [`types::SendBlockItemRequest`].
    /// Returns a pair of bytearrays (serialized_request, transaction_hash).
    fn serial_and_hash_send_block_item_request(
        request: types::SendBlockItemRequest,
    ) -> Result<(Vec<u8>, Vec<u8>), tonic::Status> {
        let version_length;
        let mut out = std::io::Cursor::new(Vec::new());
        // The version prefix.
        out.write(&crypto_common::to_bytes(&Version {
            value: 0,
        }))?;
        version_length = out.position();
        match request.block_item_type.require()? {
            types::send_block_item_request::BlockItemType::AccountTransaction(v) => {
                // Put the tag for account transaction.
                out.write_u8(0)?;
                // Put the signatures
                let cred_map = v.signature.require()?;
                out.write_u8(try_into_u8(cred_map.signatures.len(), "Length of SignatureMap")?)?;
                for (cred, sig_map) in cred_map.signatures.into_iter() {
                    out.write_u8(try_into_u8(cred, "CredentialIndex")?)?;
                    out.write_u8(try_into_u8(
                        sig_map.signatures.len(),
                        "Length of AccountSignatureMap",
                    )?)?;
                    for (k, sig) in sig_map.signatures.into_iter() {
                        out.write_u8(try_into_u8(k, "KeyIndex")?)?;
                        out.write_u16::<BigEndian>(try_into_u16(
                            sig.value.len(),
                            "Length of Signature",
                        )?)?;
                        out.write(&sig.value)?;
                    }
                }
                // Put the header.
                let header = v.header.require()?;
                serialize_account_address(header.sender.require()?, &mut out)?;
                out.write_u64::<BigEndian>(header.sequence_number.require()?.value)?;
                out.write_u64::<BigEndian>(header.energy_amount.require()?.value)?;
                // Construct payload, so we can serialize its size. The actual payload is
                // included later.
                let payload_bytes = {
                    let mut pl_cursor = std::io::Cursor::new(Vec::new());
                    match v.payload.require()? {
                        types::account_transaction::Payload::RawPayload(bytes) => {
                            pl_cursor = std::io::Cursor::new(bytes);
                        }
                        types::account_transaction::Payload::Transfer(p) => match p.memo {
                            None => {
                                pl_cursor.write_u8(3)?;
                                serialize_account_address(p.receiver.require()?, &mut pl_cursor)?;
                                pl_cursor.write_u64::<BigEndian>(p.amount.require()?.value)?;
                            }
                            Some(memo) => {
                                pl_cursor.write_u8(22)?;
                                serialize_account_address(p.receiver.require()?, &mut pl_cursor)?;
                                serialize_memo(memo, &mut pl_cursor)?;
                                pl_cursor.write_u64::<BigEndian>(p.amount.require()?.value)?;
                            }
                        },
                        types::account_transaction::Payload::DeployModule(p) => {
                            serialize_versioned_wasm_module(p, &mut pl_cursor)?;
                        }
                        types::account_transaction::Payload::InitContract(p) => {
                            pl_cursor.write_u64::<BigEndian>(p.amount.require()?.value)?;
                            serialize_module_ref(p.module_ref.require()?, &mut pl_cursor)?;
                            serialize_init_name(p.init_name.require()?, &mut pl_cursor)?;
                            serialize_parameter(p.parameter.require()?, &mut pl_cursor)?;
                        }
                        types::account_transaction::Payload::UpdateContract(p) => {
                            pl_cursor.write_u64::<BigEndian>(p.amount.require()?.value)?;
                            let addr = p.address.require()?;
                            pl_cursor.write_u64::<BigEndian>(addr.index)?;
                            pl_cursor.write_u64::<BigEndian>(addr.subindex)?;
                            serialize_receive_name(p.receive_name.require()?, &mut pl_cursor)?;
                            serialize_parameter(p.parameter.require()?, &mut pl_cursor)?;
                        }
                        types::account_transaction::Payload::RegisterData(p) => {
                            const MAX_REGISTERED_DATA_SIZE: usize = 256;
                            if p.value.len() > MAX_REGISTERED_DATA_SIZE {
                                return Err(tonic::Status::invalid_argument(format!(
                                    "Invalid RegisteredData. Has length {}, which exceeds the \
                                     limit of {} bytes.",
                                    p.value.len(),
                                    MAX_REGISTERED_DATA_SIZE
                                )));
                            }
                            pl_cursor.write(&p.value)?;
                        }
                    }
                    pl_cursor.into_inner()
                };
                out.write_u32::<BigEndian>(try_into_u32(payload_bytes.len(), "Payloadsize")?)?;
                out.write_u64::<BigEndian>(header.expiry.require()?.value)?;
                // Put the payload.
                out.write(&payload_bytes)?;
            }
            types::send_block_item_request::BlockItemType::CredentialDeployment(v) => {
                // Put the tag for credential deployment.
                out.write_u8(1)?;
                out.write_u64::<BigEndian>(v.message_expiry.require()?.value)?;
                match v.payload.require()? {
                    types::credential_deployment::Payload::RawPayload(p) => out.write(&p)?,
                };
            }
            types::send_block_item_request::BlockItemType::UpdateInstruction(v) => {
                // Put the tag for update instruction.
                out.write_u8(2)?;
                // Signature
                let signatures = v.signatures.require()?.signatures;
                out.write_u16::<BigEndian>(try_into_u16(
                    signatures.len(),
                    "Length of SignatureMap",
                )?)?;
                for (k, sig) in signatures.into_iter() {
                    out.write_u16::<BigEndian>(try_into_u16(k, "UpdateKeyIndex")?)?;
                    out.write_u16::<BigEndian>(try_into_u16(
                        sig.value.len(),
                        "Length of Signature",
                    )?)?;
                    out.write(&sig.value)?;
                }
                // Header
                let header = v.header.require()?;
                out.write_u64::<BigEndian>(header.sequence_number.require()?.value)?;
                out.write_u64::<BigEndian>(header.effective_time.require()?.value)?;
                out.write_u64::<BigEndian>(header.timeout.require()?.value)?;
                // Payload size + payload
                match v.payload.require()? {
                    types::update_instruction::Payload::RawPayload(p) => {
                        out.write_u32::<BigEndian>(try_into_u32(p.len(), "PayloadSize")?)?;
                        out.write(&p)?;
                    }
                };
            }
        }
        let out = out.into_inner();
        let hash = {
            use sha2::{Digest, Sha256};
            let mut hasher = Sha256::new();
            // The transaction hash is computed on the serialized output but without the
            // version prefix.
            hasher.update(&out[version_length as usize..]);
            hasher.finalize().to_vec()
        };
        Ok((out, hash))
    }

    /// Checks that the account address is 32 bytes and then writes it to the
    /// provided buffer.
    fn serialize_account_address<W: Write>(
        address: types::AccountAddress,
        out: &mut W,
    ) -> Result<(), tonic::Status> {
        if address.value.len() == 32 {
            out.write(&address.value)?;
            Ok(())
        } else {
            Err(tonic::Status::invalid_argument("Invalid AccountAddress. Must be 32 bytes long."))
        }
    }

    /// Tries to convert the value into a `u32`. If it fails, a `tonic::Status`
    /// is returned with an appropriate error, which includes the `name`
    /// parameter.
    fn try_into_u32<T: TryInto<u32>>(t: T, name: &str) -> Result<u32, tonic::Status> {
        t.try_into().map_err(|_| {
            tonic::Status::invalid_argument(format!("{} could not be converted into an u32.", name))
        })
    }

    /// Tries to convert the value into a `u8`. If it fails, a `tonic::Status`
    /// is returned with an appropriate error, which includes the `name`
    /// parameter.
    fn try_into_u16<T: TryInto<u16>>(t: T, name: &str) -> Result<u16, tonic::Status> {
        t.try_into().map_err(|_| {
            tonic::Status::invalid_argument(format!("{} could not be converted into an u16.", name))
        })
    }

    /// Tries to convert the value into a `u8`. If it fails, a `tonic::Status`
    /// is returned with an appropriate error, which includes the `name`
    /// parameter.
    fn try_into_u8<T: TryInto<u8>>(t: T, name: &str) -> Result<u8, tonic::Status> {
        t.try_into().map_err(|_| {
            tonic::Status::invalid_argument(format!("{} could not be converted into an u8.", name))
        })
    }

    /// Checks that the memo is valid and writes it into `out`.
    fn serialize_memo<W: Write>(memo: types::Memo, out: &mut W) -> Result<(), tonic::Status> {
        if memo.value.len() <= MAX_MEMO_SIZE {
            out.write(&memo.value)?;
            Ok(())
        } else {
            Err(tonic::Status::invalid_argument(format!(
                "Invalid Memo. Must be less than {} bytes long.",
                MAX_MEMO_SIZE
            )))
        }
    }

    /// Checks that the module ref is valid and writes it into `out`.
    fn serialize_module_ref<W: Write>(
        module_ref: types::ModuleRef,
        out: &mut W,
    ) -> Result<(), tonic::Status> {
        if module_ref.value.len() != 32 {
            return Err(tonic::Status::internal("Invalid ModuleRef. Must be 32 bytes long"));
        } else {
            out.write(&module_ref.value)?;
            Ok(())
        }
    }

    /// Checks that the wasm module is valid and writes it into `out`.
    fn serialize_versioned_wasm_module<W: Write>(
        module: types::VersionedModuleSource,
        out: &mut W,
    ) -> Result<(), tonic::Status> {
        // TODO: Move these constants to base.
        const MAX_WASM_MODULE_V0_SIZE: usize = 65536;
        const MAX_WASM_MODULE_V1_SIZE: usize = 65536 * 8;
        let (version, src, max_size) = match module.module.require()? {
            types::versioned_module_source::Module::V0(src) => {
                (0, src.value, MAX_WASM_MODULE_V0_SIZE)
            }
            types::versioned_module_source::Module::V1(src) => {
                (1, src.value, MAX_WASM_MODULE_V1_SIZE)
            }
        };

        if src.len() <= max_size {
            out.write_u32::<BigEndian>(version)?;
            out.write(&src)?;
            Ok(())
        } else {
            Err(tonic::Status::invalid_argument(format!(
                "Invalid contract module. Must be less than {} bytes long.",
                max_size
            )))
        }
    }

    /// Checks that the parameter is valid and writes it into `out`.
    fn serialize_parameter<W: Write>(
        parameter: types::Parameter,
        out: &mut W,
    ) -> Result<(), tonic::Status> {
        // TODO: Move constant to base.
        const MAX_PARAMETER_SIZE: u32 = 1024;

        if parameter.value.len() <= MAX_PARAMETER_SIZE as usize {
            out.write(&parameter.value)?;
            Ok(())
        } else {
            Err(tonic::Status::invalid_argument(format!(
                "Invalid Parameter. Must be less than {} bytes long.",
                MAX_PARAMETER_SIZE
            )))
        }
    }

    /// Checks that the init name is valid and writes it into `out`.
    fn serialize_init_name<W: Write>(
        init_name: types::InitName,
        out: &mut W,
    ) -> Result<(), tonic::Status> {
        match concordium_contracts_common::ContractName::is_valid_contract_name(&init_name.value) {
            Ok(()) => {
                out.write_u16::<BigEndian>(try_into_u16(
                    init_name.value.len(),
                    "Length of InitName",
                )?)?;
                out.write(init_name.value.as_bytes())?;
                Ok(())
            }
            Err(e) => Err(tonic::Status::invalid_argument(format!("Invalid InitName: {:?}", e))),
        }
    }

    /// Checks that the receive name is valid and writes it into `out`.
    fn serialize_receive_name<W: Write>(
        receive_name: types::ReceiveName,
        out: &mut W,
    ) -> Result<(), tonic::Status> {
        match concordium_contracts_common::ReceiveName::is_valid_receive_name(&receive_name.value) {
            Ok(()) => {
                out.write_u16::<BigEndian>(try_into_u16(
                    receive_name.value.len(),
                    "Length of ReceiveName",
                )?)?;
                out.write(receive_name.value.as_bytes())?;
                Ok(())
            }
            Err(e) => Err(tonic::Status::invalid_argument(format!("Invalid ReceiveName: {:?}", e))),
        }
    }
}

/// Add a block hash to the metadata of a response. Used for returning the block
/// hash.
fn add_hash<T>(response: &mut tonic::Response<T>, hash: [u8; 32]) -> Result<(), tonic::Status> {
    let value = tonic::metadata::MetadataValue::try_from(hex::encode(&hash))
        .map_err(|_| tonic::Status::internal("Cannot add metadata hash."))?;
    response.metadata_mut().insert("blockhash", value);
    Ok(())
}

/// A helper trait to make it simpler to require specific fields when parsing a
/// protobuf message by allowing us to use method calling syntax and
/// constructing responses that match the calling context, allowing us to use
/// the `?` syntax.
///
/// The main reason for needing this is that in proto3 all non-primitive fields
/// are optional, so it is up to the application to validate inputs if they are
/// required.
pub(crate) trait Require<E> {
    type A;
    fn require(self) -> Result<Self::A, E>;
}

impl<A> Require<tonic::Status> for Option<A> {
    type A = A;

    fn require(self) -> Result<Self::A, tonic::Status> {
        match self {
            Some(v) => Ok(v),
            None => Err(tonic::Status::invalid_argument("missing field")),
        }
    }
}
