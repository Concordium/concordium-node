use anyhow::Context;
use prost::bytes::BufMut;
use std::{convert::TryFrom, marker::PhantomData, path::Path};

/// Maximum allowed energy to use in the `invoke_instance` request.
/// This is to make sure that there are no conversion errors to interpreter
/// energy.
const MAX_ALLOWED_INVOKE_ENERGY: u64 = 100_000_000_000;

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

    pub(crate) fn receive_name_to_ffi(receive_name: &ReceiveName) -> Option<(*const u8, u32)> {
        let string = &receive_name.value;
        let len = string.len();
        if string.is_ascii() && len <= 100 {
            Some((string.as_ptr(), len as u32))
        } else {
            None
        }
    }

    /// Convert [BakerId] to a u64.
    pub(crate) fn baker_id_to_ffi(baker_id: &BakerId) -> u64 { baker_id.value }

    /// Convert the [BlocksAtHeightRequest] to a triple of a block height,
    /// genesis_index and a boolean. If the genesis_index is 0, the height
    /// is treated as an absolute block height otherwise it is treated as
    /// relative. Setting the boolean to true will restrict to only return
    /// blocks within the specified genesis_index.
    pub(crate) fn blocks_at_height_request_to_ffi(
        height: &BlocksAtHeightRequest,
    ) -> Option<(u64, u32, u8)> {
        use blocks_at_height_request::BlocksAtHeight::*;
        match height.blocks_at_height.as_ref()? {
            Absolute(h) => {
                let height = h.height.as_ref()?.value;
                Some((height, 0, 0))
            }
            Relative(h) => {
                let height = h.height.as_ref()?.value;
                let genesis_index = h.genesis_index.as_ref()?.value;
                let restrict = if h.restrict {
                    1
                } else {
                    0
                };
                Some((height, genesis_index, restrict))
            }
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
            consensus::ConsensusContainer, ffi::NotificationHandlers,
            helpers::ContractStateResponse,
        },
        p2p::P2PNode,
    };
    use anyhow::Context;
    use futures::{FutureExt, StreamExt};
    use std::sync::{Arc, Mutex};
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
        /// Return type for the 'GetAccountNonFinalizedTransactions' method.
        type GetAccountNonFinalizedTransactionsStream =
            futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'GetAncestors' method.
        type GetAncestorsStream = futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'GetAnonymityRevokers' method.
        type GetAnonymityRevokersStream =
            futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'GetBakerList' method.
        type GetBakerListStream = futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'Blocks' method.
        type GetBlocksStream =
            tokio_stream::wrappers::ReceiverStream<Result<Arc<[u8]>, tonic::Status>>;
        /// Return type for the 'FinalizedBlocks' method.
        type GetFinalizedBlocksStream =
            tokio_stream::wrappers::ReceiverStream<Result<Arc<[u8]>, tonic::Status>>;
        /// Return type for the 'GetIdentityProviders' method.
        type GetIdentityProvidersStream =
            futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'GetInstanceList' method.
        type GetInstanceListStream =
            futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'GetInstanceState' method.
        type GetInstanceStateStream = tokio_stream::wrappers::ReceiverStream<
            Result<types::InstanceStateKvPair, tonic::Status>,
        >;
        /// Return type for the 'GetModuleList' method.
        type GetModuleListStream = futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'GetPassiveDelegatorsRewardPeriod' method.
        type GetPassiveDelegatorsRewardPeriodStream =
            futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'GetPassiveDelegators' method.
        type GetPassiveDelegatorsStream =
            futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'GetPoolDelegatorsRewardPeriod' method.
        type GetPoolDelegatorsRewardPeriodStream =
            futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'GetPoolDelegators' method.
        type GetPoolDelegatorsStream =
            futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;

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

        async fn get_instance_state(
            &self,
            request: tonic::Request<crate::grpc2::types::InstanceInfoRequest>,
        ) -> Result<tonic::Response<Self::GetInstanceStateStream>, tonic::Status> {
            let request = request.get_ref();
            let block_hash = request.block_hash.as_ref().require()?;
            let contract_address = request.address.as_ref().require()?;
            let (hash, response) =
                self.consensus.get_instance_state_v2(block_hash, contract_address)?;
            match response {
                ContractStateResponse::V0 {
                    state,
                } => {
                    // We need to return the same type in both branches, so we create a silly
                    // little channel to which we will only send one value.
                    let (sender, receiver) = tokio::sync::mpsc::channel(1);
                    let _sender = tokio::spawn(async move {
                        let msg = types::InstanceStateKvPair {
                            key:   Vec::new(),
                            value: state,
                        };
                        let _ = sender.send(Ok(msg)).await;
                        // The error only happens if the receiver has been
                        // dropped already (e.g., connection closed),
                        // so we do not have to handle it. We just stop sending.
                    });
                    let mut response =
                        tonic::Response::new(tokio_stream::wrappers::ReceiverStream::new(receiver));
                    add_hash(&mut response, hash)?;
                    Ok(response)
                }
                ContractStateResponse::V1 {
                    state,
                    mut loader,
                } => {
                    // Create a buffer of size 10 to send messages. It is not clear what the optimal
                    // size would be, or even what optimal means. I choose 10 here to have some
                    // buffering to account for variance in networking and
                    // scheduling of the sender task. 10 is also small enough so that not too many
                    // values linger in memory while waiting to be sent.
                    let (sender, receiver) = tokio::sync::mpsc::channel(10);
                    let _sender = tokio::spawn(async move {
                        let iter = state.into_iterator(&mut loader);
                        for (key, value) in iter {
                            let msg = types::InstanceStateKvPair {
                                key,
                                value,
                            };
                            if sender.send(Ok(msg)).await.is_err() {
                                // the receiver has been dropped, so we stop
                                break;
                            }
                        }
                    });
                    let mut response =
                        tonic::Response::new(tokio_stream::wrappers::ReceiverStream::new(receiver));
                    add_hash(&mut response, hash)?;
                    Ok(response)
                }
            }
        }

        async fn instance_state_lookup(
            &self,
            request: tonic::Request<types::InstanceStateLookupRequest>,
        ) -> Result<tonic::Response<types::InstanceStateValueAtKey>, tonic::Status> {
            let request = request.get_ref();
            let block_hash = request.block_hash.as_ref().require()?;
            let contract_address = request.address.as_ref().require()?;
            // this is cheap since we only lookup the tree root in the V1 case, and V0
            // lookup always involves the entire state anyhow.
            let (hash, response) =
                self.consensus.get_instance_state_v2(block_hash, contract_address)?;
            match response {
                ContractStateResponse::V0 {
                    state,
                } => {
                    let mut response = tonic::Response::new(types::InstanceStateValueAtKey {
                        value: state,
                    });
                    add_hash(&mut response, hash)?;
                    Ok(response)
                }
                ContractStateResponse::V1 {
                    state,
                    mut loader,
                } => {
                    let value = state.lookup(&mut loader, &request.key);
                    if let Some(value) = value {
                        let mut response = tonic::Response::new(types::InstanceStateValueAtKey {
                            value,
                        });
                        add_hash(&mut response, hash)?;
                        Ok(response)
                    } else {
                        Err(tonic::Status::not_found("Key not found."))
                    }
                }
            }
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

        async fn invoke_instance(
            &self,
            request: tonic::Request<crate::grpc2::types::InvokeInstanceRequest>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if request
                .get_ref()
                .energy
                .as_ref()
                .map_or(false, |x| x.value <= MAX_ALLOWED_INVOKE_ENERGY)
            {
                let (hash, response) = self.consensus.invoke_instance_v2(request.get_ref())?;
                let mut response = tonic::Response::new(response);
                add_hash(&mut response, hash)?;
                Ok(response)
            } else {
                Err(tonic::Status::internal(format!(
                    "`energy` must be supplied and be less than {}",
                    MAX_ALLOWED_INVOKE_ENERGY
                )))
            }
        }

        async fn get_cryptographic_parameters(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<crate::grpc2::types::CryptographicParameters>, tonic::Status>
        {
            let (hash, response) =
                self.consensus.get_cryptographic_parameters_v2(request.get_ref())?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_block_info(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            let (hash, response) = self.consensus.get_block_info_v2(request.get_ref())?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_baker_list(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetBakerListStream>, tonic::Status> {
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            let hash = self.consensus.get_baker_list_v2(request.get_ref(), sender)?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_pool_info(
            &self,
            request: tonic::Request<crate::grpc2::types::PoolInfoRequest>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            let (hash, response) = self.consensus.get_pool_info_v2(request.get_ref())?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_passive_delegation_info(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            let (hash, response) =
                self.consensus.get_passive_delegation_info_v2(request.get_ref())?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_blocks_at_height(
            &self,
            request: tonic::Request<crate::grpc2::types::BlocksAtHeightRequest>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            let data = self.consensus.get_blocks_at_height_v2(request.get_ref())?;
            let response = tonic::Response::new(data);
            Ok(response)
        }

        async fn get_tokenomics_info(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            let (hash, response) = self.consensus.get_tokenomics_info_v2(request.get_ref())?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_pool_delegators(
            &self,
            request: tonic::Request<crate::grpc2::types::GetPoolDelegatorsRequest>,
        ) -> Result<tonic::Response<Self::GetPoolDelegatorsStream>, tonic::Status> {
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            let hash = self.consensus.get_pool_delegators_v2(request.get_ref(), sender)?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_pool_delegators_reward_period(
            &self,
            request: tonic::Request<crate::grpc2::types::GetPoolDelegatorsRequest>,
        ) -> Result<tonic::Response<Self::GetPoolDelegatorsRewardPeriodStream>, tonic::Status>
        {
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            let hash =
                self.consensus.get_pool_delegators_reward_period_v2(request.get_ref(), sender)?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_passive_delegators(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetPassiveDelegatorsStream>, tonic::Status> {
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            let hash = self.consensus.get_passive_delegators_v2(request.get_ref(), sender)?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_passive_delegators_reward_period(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetPassiveDelegatorsRewardPeriodStream>, tonic::Status>
        {
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            let hash = self
                .consensus
                .get_passive_delegators_reward_period_v2(request.get_ref(), sender)?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_branches(
            &self,
            _request: tonic::Request<crate::grpc2::types::Empty>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            Ok(tonic::Response::new(self.consensus.get_branches_v2()?))
        }

        async fn get_election_info(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            let (hash, response) = self.consensus.get_election_info_v2(request.get_ref())?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_identity_providers(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetIdentityProvidersStream>, tonic::Status> {
            let (sender, receiver) = futures::channel::mpsc::channel(10);
            let hash = self.consensus.get_identity_providers_v2(request.get_ref(), sender)?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_anonymity_revokers(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetAnonymityRevokersStream>, tonic::Status> {
            let (sender, receiver) = futures::channel::mpsc::channel(10);
            let hash = self.consensus.get_anonymity_revokers_v2(request.get_ref(), sender)?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_account_non_finalized_transactions(
            &self,
            request: tonic::Request<crate::grpc2::types::AccountAddress>,
        ) -> Result<tonic::Response<Self::GetAccountNonFinalizedTransactionsStream>, tonic::Status>
        {
            let (sender, receiver) = futures::channel::mpsc::channel(10);
            self.consensus.get_account_non_finalized_transactions_v2(request.get_ref(), sender)?;
            let response = tonic::Response::new(receiver);
            Ok(response)
        }

        #[cfg(feature = "network_dump")]
        async fn dump_start(
            &self,
            request: tonic::Request<crate::grpc2::types::DumpRequest>,
        ) -> Result<tonic::Response<crate::grpc2::types::BoolResponse>, tonic::Status> {
            let file_path = request.get_ref().file.to_owned();
            let result = self
                .node
                .activate_dump(
                    if file_path.is_empty() {
                        "dump"
                    } else {
                        &file_path
                    },
                    request.get_ref().raw,
                )
                .is_ok();
            Ok(tonic::Response::new(crate::grpc2::types::BoolResponse {
                value: result,
            }))
        }

        #[cfg(not(feature = "network_dump"))]
        async fn dump_start(
            &self,
            _request: tonic::Request<crate::grpc2::types::DumpRequest>,
        ) -> Result<tonic::Response<crate::grpc2::types::BoolResponse>, tonic::Status> {
            Err(tonic::Status::failed_precondition("Feature \"network_dump\" is not active"))
        }

        #[cfg(feature = "network_dump")]
        async fn dump_stop(
            &self,
            _request: tonic::Request<crate::grpc2::types::Empty>,
        ) -> Result<tonic::Response<crate::grpc2::types::BoolResponse>, tonic::Status> {
            Ok(tonic::Response::new(crate::grpc2::types::BoolResponse {
                value: self.node.stop_dump().is_ok(),
            }))
        }

        #[cfg(not(feature = "network_dump"))]
        async fn dump_stop(
            &self,
            _request: tonic::Request<crate::grpc2::types::Empty>,
        ) -> Result<tonic::Response<crate::grpc2::types::BoolResponse>, tonic::Status> {
            Err(tonic::Status::failed_precondition("Feature \"network_dump\" is not active"))
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
