use crate::stats_export_service::StatsExportService;
use anyhow::Context;
use hyper::server::conn::AddrStream;
use prometheus::core::{Atomic, AtomicU64, GenericGauge};
use prost::bytes::BufMut;
use std::{
    convert::{TryFrom, TryInto},
    marker::PhantomData,
    path::Path,
    sync::Arc,
};
use tokio::sync::{OwnedSemaphorePermit, Semaphore};
use tonic::transport::server::{Connected, TcpIncoming};

/// Types generated from the types.proto file, together
/// with some auxiliary definitions that help passing values through the FFI
/// boundary.
pub mod types {
    // Tell clippy to allow large enum variants in the generated code.
    #![allow(clippy::large_enum_variant)]

    use crate::configuration::PROTOCOL_MAX_TRANSACTION_SIZE;

    use super::Require;
    use concordium_base::{common::Versioned, transactions::PayloadLike};
    use std::convert::{TryFrom, TryInto};

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
    /// An auxiliary type used to bridge the FFI when passing [`BlockHashInput`]
    /// to the Haskell part of the codebase.
    pub(crate) enum BlockHashInputFFI<'a> {
        /// Current best block.
        Best,
        /// Last finalized block.
        LastFinal,
        /// Pointer to 32 bytes for the block hash.
        Given(&'a [u8]),
        /// Absolute block height encoded as 8 bytes big endian.
        Absolute([u8; 8]),
        /// Block height relative to a genesis index, 8 bytes for the relative
        /// height (big endian), 4 bytes for the genesis index (big endian) and
        /// 1 byte where a value of 1 signals to restrict the result within the
        /// same genesis index.
        Relative([u8; 13]),
    }

    impl<'a> BlockHashInputFFI<'a> {
        /// Convert the [`BlockHashInputFFI`] to a pair of a tag and pointer to
        /// the content. The tag is 0 for "Best" block, 1 for
        /// "LastFinal" block, 2 for a specific block given by a hash, 3
        /// for block given by absolute height, and 4 for block given by
        /// relative height.
        pub fn to_ptr(&'a self) -> (u8, &'a [u8]) {
            match self {
                BlockHashInputFFI::Best => (0u8, &[]),
                BlockHashInputFFI::LastFinal => (1u8, &[]),
                BlockHashInputFFI::Given(ptr) => (2u8, *ptr),
                BlockHashInputFFI::Absolute(data) => (3u8, &data[..]),
                BlockHashInputFFI::Relative(data) => (4u8, &data[..]),
            }
        }
    }

    /// Construct a representation of a [`BlockHashInput`] which can be passed
    /// through FFI. Returns `None` if failing due to missing or invalids fields
    /// in the input.
    pub(crate) fn block_hash_input_to_ffi(bhi: &BlockHashInput) -> Option<BlockHashInputFFI> {
        use block_hash_input::BlockHashInput::*;
        match bhi.block_hash_input.as_ref()? {
            Best(_) => Some(BlockHashInputFFI::Best),
            LastFinal(_) => Some(BlockHashInputFFI::LastFinal),
            Given(bh) if bh.value.len() == 32 => Some(BlockHashInputFFI::Given(&bh.value[..])),
            AbsoluteHeight(abh) => Some(BlockHashInputFFI::Absolute(abh.value.to_be_bytes())),
            RelativeHeight(rel) => {
                let mut bytes = [0u8; 13];
                bytes[0..8].copy_from_slice(&rel.height.as_ref()?.value.to_be_bytes());
                bytes[8..12].copy_from_slice(&rel.genesis_index.as_ref()?.value.to_be_bytes());
                bytes[12] = if rel.restrict {
                    1
                } else {
                    0
                };
                Some(BlockHashInputFFI::Relative(bytes))
            }
            _ => None,
        }
    }

    pub(crate) enum EpochRequestFFI<'a> {
        /// Epoch number (8 bytes) and genesis index (4 bytes) of the specified
        /// epoch.
        Specified([u8; 12]),
        /// The epoch is specified as the epoch of a block.
        OfBlock(BlockHashInputFFI<'a>),
    }

    impl<'a> EpochRequestFFI<'a> {
        /// Convert the [`EpochRequestFFI`] to a pair of a tag and pointer to
        /// the content. The tag is 0 for "Best" block, 1 for
        /// "LastFinal" block, 2 for a specific block given by a hash, 3
        /// for block given by absolute height, 4 for block given by
        /// relative height, and 5 for a specified epoch.
        pub fn to_ptr(&'a self) -> (u8, &'a [u8]) {
            match self {
                EpochRequestFFI::Specified(data) => (5u8, &data[..]),
                EpochRequestFFI::OfBlock(bhi) => bhi.to_ptr(),
            }
        }
    }

    /// Construct a representation of an [`EpochRequest`] which can be passed
    /// through FFI. Returns `None` if failing due to missing or invalids fields
    /// in the input.
    pub(crate) fn epoch_request_to_ffi(epoch_req: &EpochRequest) -> Option<EpochRequestFFI> {
        use epoch_request::EpochRequestInput::*;
        match epoch_req.epoch_request_input.as_ref()? {
            RelativeEpoch(epoch) => {
                let mut bytes = [0u8; 12];
                bytes[0..8].copy_from_slice(&epoch.epoch.as_ref()?.value.to_be_bytes());
                bytes[8..12].copy_from_slice(&epoch.genesis_index.as_ref()?.value.to_be_bytes());
                Some(EpochRequestFFI::Specified(bytes))
            }
            BlockHash(bhi) => block_hash_input_to_ffi(bhi).map(EpochRequestFFI::OfBlock),
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

    /// Convert [`TransactionHash`] to a pointer to the content. The length of
    /// the content is checked to be 32 bytes.
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

    /// Convert [`BakerId`] to a u64.
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

    impl From<Amount> for concordium_base::common::types::Amount {
        fn from(n: Amount) -> Self { Self::from_micro_ccd(n.value) }
    }

    impl TryFrom<ModuleRef> for concordium_base::contracts_common::ModuleReference {
        type Error = tonic::Status;

        fn try_from(value: ModuleRef) -> Result<Self, Self::Error> {
            match value.value.try_into() {
                Ok(mod_ref) => Ok(Self::new(mod_ref)),
                Err(_) => {
                    Err(tonic::Status::invalid_argument("Unexpected module reference format."))
                }
            }
        }
    }

    impl TryFrom<InitName> for concordium_base::smart_contracts::OwnedContractName {
        type Error = tonic::Status;

        fn try_from(value: InitName) -> Result<Self, Self::Error> {
            Self::new(value.value).map_err(|e| {
                tonic::Status::invalid_argument(format!("Invalid contract init name: {}", e))
            })
        }
    }

    impl TryFrom<ReceiveName> for concordium_base::smart_contracts::OwnedReceiveName {
        type Error = tonic::Status;

        fn try_from(value: ReceiveName) -> Result<Self, Self::Error> {
            Self::new(value.value).map_err(|e| {
                tonic::Status::invalid_argument(format!("Invalid contract receive name: {}", e))
            })
        }
    }

    impl TryFrom<Parameter> for concordium_base::smart_contracts::OwnedParameter {
        type Error = tonic::Status;

        fn try_from(value: Parameter) -> Result<Self, Self::Error> {
            Self::try_from(value.value)
                .map_err(|e| tonic::Status::invalid_argument(format!("Invalid parameter: {}", e)))
        }
    }

    impl From<ContractAddress> for concordium_base::contracts_common::ContractAddress {
        fn from(value: ContractAddress) -> Self { Self::new(value.index, value.subindex) }
    }

    impl TryFrom<Memo> for concordium_base::transactions::Memo {
        type Error = tonic::Status;

        fn try_from(value: Memo) -> Result<Self, Self::Error> {
            value.value.try_into().map_err(|_| {
                tonic::Status::invalid_argument("Memo is invalid because it is too big.")
            })
        }
    }

    impl TryFrom<RegisteredData> for concordium_base::transactions::RegisteredData {
        type Error = tonic::Status;

        fn try_from(value: RegisteredData) -> Result<Self, Self::Error> {
            value.value.try_into().map_err(|e| {
                tonic::Status::invalid_argument(format!("Invalid register data payload: {}", e))
            })
        }
    }

    impl TryFrom<AccountTransactionPayload> for concordium_base::transactions::EncodedPayload {
        type Error = tonic::Status;

        fn try_from(value: AccountTransactionPayload) -> Result<Self, Self::Error> {
            match value.payload.require()? {
                account_transaction_payload::Payload::RawPayload(rp) => {
                    Self::try_from(rp).map_err(|_| {
                        tonic::Status::invalid_argument("Payload size exceeds maximum allowed.")
                    })
                }
                account_transaction_payload::Payload::DeployModule(dm) => {
                    let module = match dm.module.require()? {
                        versioned_module_source::Module::V0(source) => {
                            concordium_base::smart_contracts::WasmModule {
                                version: concordium_base::smart_contracts::WasmVersion::V0,
                                source:  source.value.into(),
                            }
                        }
                        versioned_module_source::Module::V1(source) => {
                            concordium_base::smart_contracts::WasmModule {
                                version: concordium_base::smart_contracts::WasmVersion::V1,
                                source:  source.value.into(),
                            }
                        }
                    };
                    Ok(concordium_base::transactions::Payload::DeployModule {
                        module,
                    }
                    .encode())
                }
                account_transaction_payload::Payload::InitContract(ic) => {
                    let payload = concordium_base::transactions::InitContractPayload {
                        amount:    ic.amount.require()?.into(),
                        mod_ref:   ic.module_ref.require()?.try_into()?,
                        init_name: ic.init_name.require()?.try_into()?,
                        param:     ic.parameter.require()?.try_into()?,
                    };
                    Ok(concordium_base::transactions::Payload::InitContract {
                        payload,
                    }
                    .encode())
                }
                account_transaction_payload::Payload::UpdateContract(uc) => {
                    let payload = concordium_base::transactions::UpdateContractPayload {
                        amount:       uc.amount.require()?.into(),
                        address:      uc.address.require()?.into(),
                        receive_name: uc.receive_name.require()?.try_into()?,
                        message:      uc.parameter.require()?.try_into()?,
                    };
                    Ok(concordium_base::transactions::Payload::Update {
                        payload,
                    }
                    .encode())
                }
                account_transaction_payload::Payload::Transfer(t) => {
                    let payload = concordium_base::transactions::Payload::Transfer {
                        to_address: t.receiver.require()?.try_into()?,
                        amount:     t.amount.require()?.into(),
                    };
                    Ok(payload.encode())
                }
                account_transaction_payload::Payload::TransferWithMemo(t) => {
                    let payload = concordium_base::transactions::Payload::TransferWithMemo {
                        to_address: t.receiver.require()?.try_into()?,
                        amount:     t.amount.require()?.into(),
                        memo:       t.memo.require()?.try_into()?,
                    };
                    Ok(payload.encode())
                }
                account_transaction_payload::Payload::RegisterData(t) => {
                    let payload = concordium_base::transactions::Payload::RegisterData {
                        data: t.try_into()?,
                    };
                    Ok(payload.encode())
                }
            }
        }
    }

    impl TryFrom<AccountAddress> for concordium_base::id::types::AccountAddress {
        type Error = tonic::Status;

        fn try_from(value: AccountAddress) -> Result<Self, Self::Error> {
            match value.value.try_into() {
                Ok(addr) => Ok(Self(addr)),
                Err(_) => {
                    Err(tonic::Status::invalid_argument("Unexpected account address format."))
                }
            }
        }
    }

    impl From<SequenceNumber> for concordium_base::base::Nonce {
        #[inline]
        fn from(n: SequenceNumber) -> Self {
            Self {
                nonce: n.value,
            }
        }
    }

    impl From<UpdateSequenceNumber> for concordium_base::base::UpdateSequenceNumber {
        #[inline]
        fn from(n: UpdateSequenceNumber) -> Self { n.value.into() }
    }

    impl From<Energy> for concordium_base::base::Energy {
        #[inline]
        fn from(value: Energy) -> Self {
            Self {
                energy: value.value,
            }
        }
    }

    impl From<TransactionTime> for concordium_base::common::types::TransactionTime {
        #[inline]
        fn from(value: TransactionTime) -> Self {
            Self {
                seconds: value.value,
            }
        }
    }

    impl TryFrom<PreAccountTransaction>
        for (
            concordium_base::transactions::TransactionHeader,
            concordium_base::transactions::EncodedPayload,
        )
    {
        type Error = tonic::Status;

        fn try_from(value: PreAccountTransaction) -> Result<Self, Self::Error> {
            let header = value.header.require()?;
            let payload = value.payload.require()?;
            let sender = header.sender.require()?.try_into()?;
            let nonce = header.sequence_number.require()?.into();
            let energy_amount = header.energy_amount.require()?.into();
            let expiry = header.expiry.require()?.into();
            let payload: concordium_base::transactions::EncodedPayload = payload.try_into()?;
            let payload_size = payload.size();
            let header = concordium_base::transactions::TransactionHeader {
                sender,
                nonce,
                energy_amount,
                payload_size,
                expiry,
            };
            Ok((header, payload))
        }
    }

    impl TryFrom<Signature> for concordium_base::common::types::Signature {
        type Error = tonic::Status;

        fn try_from(value: Signature) -> Result<Self, Self::Error> {
            if value.value.len() <= usize::from(u16::MAX) {
                Ok(Self {
                    sig: value.value,
                })
            } else {
                Err(tonic::Status::invalid_argument("Signature is too large."))
            }
        }
    }

    impl TryFrom<AccountTransactionSignature> for concordium_base::common::types::TransactionSignature {
        type Error = tonic::Status;

        fn try_from(value: AccountTransactionSignature) -> Result<Self, Self::Error> {
            let signatures = value
                .signatures
                .into_iter()
                .map(|(ci, m)| {
                    let ci = u8::try_from(ci).map_err(|_| {
                        tonic::Status::invalid_argument("Invalid credential index.")
                    })?;
                    let cred_sigs = m
                        .signatures
                        .into_iter()
                        .map(|(ki, sig)| {
                            let ki = u8::try_from(ki).map_err(|_| {
                                tonic::Status::invalid_argument("Invalid key index.")
                            })?;
                            let sig = sig.try_into()?;
                            Ok::<_, tonic::Status>((ki.into(), sig))
                        })
                        .collect::<Result<_, _>>()?;
                    Ok::<_, tonic::Status>((ci.into(), cred_sigs))
                })
                .collect::<Result<_, _>>()?;
            Ok(Self {
                signatures,
            })
        }
    }

    impl TryFrom<SignatureMap> for concordium_base::updates::UpdateInstructionSignature {
        type Error = tonic::Status;

        fn try_from(value: SignatureMap) -> Result<Self, Self::Error> {
            let signatures = value
                .signatures
                .into_iter()
                .map(|(k, sig)| {
                    let k = u16::try_from(k).map_err(|_| {
                        tonic::Status::invalid_argument("Update key index too large.")
                    })?;
                    let sig = sig.try_into()?;
                    Ok::<_, tonic::Status>((k.into(), sig))
                })
                .collect::<Result<_, _>>()?;
            Ok(Self {
                signatures,
            })
        }
    }

    impl SendBlockItemRequest {
        /// Return the Versioned block item serialized in the V0 format.
        pub(crate) fn get_v0_format(self) -> Result<Vec<u8>, tonic::Status> {
            match self.block_item.require()? {
                send_block_item_request::BlockItem::AccountTransaction(at) => {
                    let pat = PreAccountTransaction {
                        header:  at.header,
                        payload: at.payload,
                    };
                    let (header, payload) = pat.try_into()?;
                    let signature = at.signature.require()?.try_into()?;
                    let at = concordium_base::transactions::AccountTransaction {
                        signature,
                        header,
                        payload,
                    };
                    Ok(concordium_base::common::to_bytes(&Versioned::new(
                        0.into(),
                        concordium_base::transactions::BlockItem::AccountTransaction(at),
                    )))
                }
                send_block_item_request::BlockItem::CredentialDeployment(cd) => {
                    // this variant is a bit hacky since we don't want to deserialize the credential
                    // at this point that is expensive and wasteful. So we
                    // directly construct the serialization of the V0
                    // block item.
                    let message_expiry: concordium_base::common::types::TransactionTime =
                        cd.message_expiry.require()?.into();
                    let ac = cd.payload.require()?;
                    // first serialize the version prefix, the tag of credential deployment, and the
                    // expiry
                    let mut data = concordium_base::common::to_bytes(&Versioned::new(
                        0.into(),
                        (1u8, message_expiry), /* 1 is the tag of the credential deployment
                                                * variant. */
                    ));
                    // then append the actual credential. This works because the actual credential
                    // is the last item to be serialized.
                    match ac {
                        credential_deployment::Payload::RawPayload(rp) => {
                            data.extend_from_slice(&rp);
                            Ok(data)
                        }
                    }
                }
                send_block_item_request::BlockItem::UpdateInstruction(ui) => {
                    use concordium_base::common::Serial;
                    let header = ui.header.require()?;
                    let update_instruction_payload::Payload::RawPayload(payload) =
                        ui.payload.require()?.payload.require()?;
                    if payload.len() > PROTOCOL_MAX_TRANSACTION_SIZE {
                        return Err(tonic::Status::invalid_argument("Update payload too large."));
                    }
                    let header = concordium_base::updates::UpdateHeader {
                        seq_number:     header.sequence_number.require()?.into(),
                        effective_time: header.effective_time.require()?.into(),
                        timeout:        header.timeout.require()?.into(),
                        payload_size:   (payload.len() as u32).into(), /* as is safe since we
                                                                        * checked size above. */
                    };
                    let signatures: concordium_base::updates::UpdateInstructionSignature =
                        ui.signatures.require()?.try_into()?;

                    let mut data = concordium_base::common::to_bytes(
                        &Versioned::new(0.into(), (2u8, header)), /* 2 is the tag of the
                                                                   * credential deployment
                                                                   * variant. */
                    );
                    data.extend_from_slice(&payload);
                    signatures.serial(&mut data);
                    Ok(data)
                }
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
#[derive(Debug, serde::Deserialize)]
struct ServiceConfig {
    #[serde(default)]
    get_finalized_blocks: bool,
    #[serde(default)]
    get_blocks: bool,
    #[serde(default)]
    get_account_list: bool,
    #[serde(default)]
    get_account_info: bool,
    #[serde(default)]
    get_module_list: bool,
    #[serde(default)]
    get_module_source: bool,
    #[serde(default)]
    get_instance_list: bool,
    #[serde(default)]
    get_instance_info: bool,
    #[serde(default)]
    get_instance_state: bool,
    #[serde(default)]
    instance_state_lookup: bool,
    #[serde(default)]
    get_next_account_sequence_number: bool,
    #[serde(default)]
    get_consensus_info: bool,
    #[serde(default)]
    get_consensus_detailed_status: bool,
    #[serde(default)]
    get_ancestors: bool,
    #[serde(default)]
    get_block_item_status: bool,
    #[serde(default)]
    invoke_instance: bool,
    #[serde(default)]
    get_cryptographic_parameters: bool,
    #[serde(default)]
    get_block_info: bool,
    #[serde(default)]
    get_baker_list: bool,
    #[serde(default)]
    get_pool_info: bool,
    #[serde(default)]
    get_passive_delegation_info: bool,
    #[serde(default)]
    get_blocks_at_height: bool,
    #[serde(default)]
    get_tokenomics_info: bool,
    #[serde(default)]
    get_pool_delegators: bool,
    #[serde(default)]
    get_pool_delegators_reward_period: bool,
    #[serde(default)]
    get_passive_delegators: bool,
    #[serde(default)]
    get_passive_delegators_reward_period: bool,
    #[serde(default)]
    get_branches: bool,
    #[serde(default)]
    get_election_info: bool,
    #[serde(default)]
    get_identity_providers: bool,
    #[serde(default)]
    get_anonymity_revokers: bool,
    #[serde(default)]
    get_account_non_finalized_transactions: bool,
    #[serde(default)]
    get_block_transaction_events: bool,
    #[serde(default)]
    get_block_special_events: bool,
    #[serde(default)]
    get_block_pending_updates: bool,
    #[serde(default)]
    get_next_update_sequence_numbers: bool,
    #[serde(default)]
    get_scheduled_release_accounts: bool,
    #[serde(default)]
    get_cooldown_accounts: bool,
    #[serde(default)]
    get_pre_cooldown_accounts: bool,
    #[serde(default)]
    get_pre_pre_cooldown_accounts: bool,
    #[serde(default)]
    get_block_chain_parameters: bool,
    #[serde(default)]
    get_block_finalization_summary: bool,
    #[serde(default)]
    shutdown: bool,
    #[serde(default)]
    peer_connect: bool,
    #[serde(default)]
    peer_disconnect: bool,
    #[serde(default)]
    get_banned_peers: bool,
    #[serde(default)]
    ban_peer: bool,
    #[serde(default)]
    unban_peer: bool,
    #[serde(default)]
    dump_start: bool,
    #[serde(default)]
    dump_stop: bool,
    #[serde(default)]
    get_peers_info: bool,
    #[serde(default)]
    get_node_info: bool,
    #[serde(default)]
    send_block_item: bool,
    #[serde(default)]
    get_account_transaction_sign_hash: bool,
    #[serde(default)]
    get_block_items: bool,
    #[serde(default)]
    get_bakers_reward_period: bool,
    #[serde(default)]
    get_block_certificates: bool,
    #[serde(default)]
    get_baker_earliest_win_time: bool,
    #[serde(default)]
    get_first_block_epoch: bool,
    #[serde(default)]
    get_winning_bakers_epoch: bool,
    #[serde(default)]
    dry_run: bool,
}

impl ServiceConfig {
    pub const fn new_all_enabled() -> Self {
        Self {
            get_finalized_blocks: true,
            get_blocks: true,
            get_account_list: true,
            get_account_info: true,
            get_module_list: true,
            get_module_source: true,
            get_instance_list: true,
            get_instance_info: true,
            get_instance_state: true,
            instance_state_lookup: true,
            get_next_account_sequence_number: true,
            get_consensus_info: true,
            get_consensus_detailed_status: true,
            get_ancestors: true,
            get_block_item_status: true,
            invoke_instance: true,
            get_cryptographic_parameters: true,
            get_block_info: true,
            get_baker_list: true,
            get_pool_info: true,
            get_passive_delegation_info: true,
            get_blocks_at_height: true,
            get_tokenomics_info: true,
            get_pool_delegators: true,
            get_pool_delegators_reward_period: true,
            get_passive_delegators: true,
            get_passive_delegators_reward_period: true,
            get_branches: true,
            get_election_info: true,
            get_identity_providers: true,
            get_anonymity_revokers: true,
            get_account_non_finalized_transactions: true,
            get_block_transaction_events: true,
            get_block_special_events: true,
            get_block_pending_updates: true,
            get_next_update_sequence_numbers: true,
            get_scheduled_release_accounts: true,
            get_cooldown_accounts: true,
            get_pre_cooldown_accounts: true,
            get_pre_pre_cooldown_accounts: true,
            get_block_chain_parameters: true,
            get_block_finalization_summary: true,
            shutdown: true,
            peer_connect: true,
            peer_disconnect: true,
            get_banned_peers: true,
            ban_peer: true,
            unban_peer: true,
            dump_start: true,
            dump_stop: true,
            get_peers_info: true,
            get_node_info: true,
            send_block_item: true,
            get_account_transaction_sign_hash: true,
            get_block_items: true,
            get_bakers_reward_period: true,
            get_block_certificates: true,
            get_baker_earliest_win_time: true,
            get_first_block_epoch: true,
            get_winning_bakers_epoch: true,
            dry_run: true,
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
            consensus::{ConsensusContainer, ConsensusType, CALLBACK_QUEUE},
            ffi::NotificationHandlers,
            helpers::{
                ConsensusFfiResponse, ConsensusIsInBakingCommitteeResponse, ContractStateResponse,
                PacketType,
            },
            messaging::{ConsensusMessage, MessageType},
        },
        health,
        p2p::P2PNode,
    };
    use anyhow::Context;
    use byteorder::WriteBytesExt;
    use futures::{Future, FutureExt, StreamExt};
    use std::{
        io::Write,
        net::SocketAddr,
        sync::{Arc, Mutex},
    };
    use tokio_util::sync::PollSemaphore;
    use tonic::{
        async_trait,
        transport::{server::TcpIncoming, ServerTlsConfig},
    };

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
        /// Maximum amount of energy allowed for the `InvokeInstance` endpoint.
        invoke_max_energy: u64,
        /// Reference to the node to support network and node status related
        /// queries.
        node: Arc<P2PNode>,
        /// Reference to consensus to support consensus queries.
        consensus: ConsensusContainer,
        /// The list of active clients listening for new block arrivals.
        blocks_channels: Clients,
        /// The list of active clients listening for new finalized blocks.
        finalized_blocks_channels: Clients,
        /// The maximum energy allowed to be used in a dry run invocation.
        dry_run_max_energy: u64,
        /// The timeout for a dry run invocation to complete.
        dry_run_timeout: tokio::time::Duration,
        /// Semaphore limiting the concurrent dry run sessions allowed.
        dry_run_semaphore: Option<Arc<tokio::sync::Semaphore>>,
        /// A thread pool where all queries are processed. We have a dedicated
        /// thread pool since the queries are not async in the sense that they
        /// call into Haskell code which can block indefinitely due to
        /// locking, or take a long time. The guidelines for tokio
        /// are that there should be no more than 100us between two await
        /// points, and most queries that go into Haskell are much above
        /// that. In cases of high load this means that even the operations of
        /// the gRPC server, such as accepting new connections, are interrupted
        /// because all tasks are occupied.
        ///
        /// To alleviate the load on the async runtime we instead spawn a thread
        /// pool with a limited number of threads. All computation that
        /// goes into Haskell code is running in a thread spawned in
        /// this thread pool so that the async task which is spawned by tonic to
        /// handle the request is instead blocked at an `await` point,
        /// which means the server can make progress scheduling other
        /// tasks.
        ///
        /// An added benefit of using a thread pool compared to using `tokio`'s
        /// blocking threads is that we have precise control on how many
        /// resources we use for queries compared to other operations of
        /// consensus.
        thread_pool: rayon::ThreadPool,
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
            error_sender: tokio::sync::broadcast::Sender<()>,
        ) -> anyhow::Result<Option<Self>> {
            if let Some(listen_addr) = config.listen_addr {
                // the error in the following line should never happen since the command-line
                // parser should already make sure that when the address is defined, so is the
                // port.
                let listen_port = config.listen_port.context("Missing GRPC port")?;

                log::info!("Starting GRPC V2 server listening on {listen_addr}:{listen_port}");

                let service_config = if let Some(ref source) = config.endpoint_config {
                    ServiceConfig::from_file(source)?
                } else {
                    ServiceConfig::new_all_enabled()
                };
                debug!("GRPC endpoints enabled: {:#?}", service_config);

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
                let num_threads = config.max_threads.unwrap_or_else(num_cpus::get);
                let server =
                    RpcServerImpl {
                        service_config,
                        invoke_max_energy: config.invoke_max_energy,
                        node: Arc::clone(node),
                        consensus: consensus.clone(),
                        blocks_channels: Arc::new(Mutex::new(Vec::new())),
                        finalized_blocks_channels: Arc::new(Mutex::new(Vec::new())),
                        dry_run_max_energy: config.invoke_max_energy,
                        dry_run_timeout: tokio::time::Duration::from_secs(config.dry_run_timeout),
                        dry_run_semaphore: config
                            .dry_run_concurrency
                            .map(|n| Arc::new(tokio::sync::Semaphore::new(n))),
                        thread_pool: rayon::ThreadPoolBuilder::new()
                            .num_threads(num_threads)
                            .build()
                            .context("Unable to create thread pool for handling gRPC requests.")?,
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
                let log_layer = {
                    #[derive(Default, Copy, Clone)]
                    struct OnFailure;
                    impl tower_http::trace::OnFailure<tower_http::classify::GrpcFailureClass> for OnFailure {
                        fn on_failure(
                            &mut self,
                            failure: tower_http::classify::GrpcFailureClass,
                            _latency: std::time::Duration,
                            _span: &tracing::Span,
                        ) {
                            debug!("gRPC request failed {}", failure);
                        }
                    }
                    tower_http::trace::TraceLayer::new_for_grpc().on_failure(OnFailure)
                };
                let stats_layer = StatsLayer {
                    stats: node.stats.clone(),
                };
                let in_flight_request_layer = tower_http::metrics::InFlightRequestsLayer::new(
                    node.stats.grpc_in_flight_requests_counter.clone(),
                );
                // Construct a server.
                // We apply a number of layers to limit the service. Note that layers apply "top
                // down", so for example the timeout layer applies on a request
                // before, e.g., the log layer.
                let mut builder = tonic::transport::Server::builder()
                    .concurrency_limit_per_connection(config.max_concurrent_requests_per_connection)
                    .max_concurrent_streams(config.max_concurrent_streams)
                    .timeout(std::time::Duration::from_secs(config.request_timeout))
                    .http2_keepalive_interval(
                        config.keepalive_interval.map(std::time::Duration::from_secs),
                    )
                    .http2_keepalive_timeout(Some(std::time::Duration::from_secs(
                        config.keepalive_timeout,
                    )))
                    // Note: the in-flight request layer applies first here. Since we are using a load-shed
                    // layer just below this corresponds very directly to the number of requests being actually handled.
                    // The technical reason for this is that we cannot really stack the in flight requests layer
                    // below the stats layer since we want to transform some `Err` responses in the stats layer
                    // to Ok responses with a meaningful gRPC status code,
                    // but since the in flight request layer adds a guard to count in-flight requests this would
                    // mean we'd have to construct such a guard in the response, which is not possible.
                    .layer(in_flight_request_layer)
                    .layer(stats_layer)
                    .layer(tower::load_shed::LoadShedLayer::new())
                    .layer(tower::limit::ConcurrencyLimitLayer::new(config.max_concurrent_requests))
                    .layer(log_layer);
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

                let router = {
                    // add the health service with reflection.
                    // The naming of the reflection service here (queries_descriptor) must match
                    // the naming chosen in the build.rs file.
                    let reflection_service = tonic_reflection::server::Builder::configure()
                        .register_encoded_file_descriptor_set(health::concordium::HEALTH_DESCRIPTOR)
                        .register_encoded_file_descriptor_set(
                            health::grpc_health_v1::HEALTH_DESCRIPTOR,
                        )
                        .build()
                        .context("Unable to start the GRPC2 reflection service.")?;

                    let health_service = health::HealthServiceImpl {
                        consensus: consensus.clone(),
                        node: node.clone(),
                        health_max_finalization_delay: config.health_max_finalized_delay,
                        health_min_peers: config.health_min_peers,
                    };

                    if config.enable_grpc_web {
                        router
                            .add_service(tonic_web::enable(
                                health::concordium::health_server::HealthServer::new(
                                    health_service.clone(),
                                ),
                            ))
                            .add_service(tonic_web::enable(
                                health::grpc_health_v1::health_server::HealthServer::new(
                                    health_service,
                                ),
                            ))
                            .add_service(tonic_web::enable(reflection_service))
                    } else {
                        router
                            .add_service(health::concordium::health_server::HealthServer::new(
                                health_service.clone(),
                            ))
                            .add_service(health::grpc_health_v1::health_server::HealthServer::new(
                                health_service,
                            ))
                            .add_service(reflection_service)
                    }
                };

                let stream = TcpIncoming::new(
                    std::net::SocketAddr::new(listen_addr, listen_port),
                    true,
                    config.tcp_keepalive.map(std::time::Duration::from_secs),
                )
                .map_err(|e| anyhow::anyhow!("Error creating listener: {e}"))?;
                let incoming = ConnStreamWithTicket {
                    stream,
                    grpc_connected_clients: node.stats.grpc_connected_clients.clone(),
                    semaphore: PollSemaphore::new(Arc::new(Semaphore::new(config.max_connections))),
                };
                let task = tokio::spawn(async move {
                    let result = router
                        .serve_with_incoming_shutdown(incoming, shutdown_receiver.map(|_| ()))
                        .await;
                    if let Err(ref err) = result {
                        // Log an error and notify main thread that an error occured.
                        error!("A runtime error occurred in the GRPC2 server: {}", err);
                        if error_sender.send(()).is_err() {
                            error!("An error occurred while trying to signal the main node thread.")
                        }
                    }
                    result
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

        /// Query whether the server task thread is still running.
        pub fn is_finished(&self) -> bool { self.task.is_finished() }

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

    impl RpcServerImpl {
        /// Run a computation in thread pool dedicated for running
        /// long-running computations. The gRPC server uses tokio tasks
        /// for handling requests, and these tasks must be lightweight in the
        /// sense that they should only do a small amount of work between
        /// `await` points, since task scheduling is cooperative.
        ///
        /// Consensus queries are not like that, a lot of them potentially block
        /// on the global lock, or take more time even when they don't block due
        /// to heavy computation. This interfers badly with the tonic server's
        /// handling of other tasks.
        ///
        /// This method takes such a computation and schedules it to run on one
        /// of the dedicated threads. in the background. It then
        /// `await`s the result so that tokio's async scheduler can schedule
        /// other tasks on its own workers.
        async fn run_blocking<R: Send + Sync + 'static>(
            &self,
            f: impl FnOnce(&ConsensusContainer) -> tonic::Result<R> + Send + 'static,
        ) -> tonic::Result<R> {
            let consensus = self.consensus.clone();
            let (sender, receiver) = tokio::sync::oneshot::channel();
            self.thread_pool.spawn(move || {
                if sender.send(f(&consensus)).is_err() {
                    // This error only happens if the `receiver` was dropped. And the receiver is
                    // only dropped if the async task is dropped at the await point.
                    // This can happen if the client kills the connection or request while
                    // waiting for the response.
                    trace!("Request was cancelled by the client.");
                }
            });
            receiver.await.map_err(|e| {
                let msg = format!("Unable to join blocking task: {e}");
                error!("{}", msg);
                tonic::Status::internal(msg)
            })?
        }
    }

    #[async_trait]
    impl service::queries_server::Queries for RpcServerImpl {
        /// Return type for the 'DryRun' method.
        type DryRunStream = std::pin::Pin<Box<DryRunStream>>;
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
        /// Return type for the 'GetBakersRewardPeriod' method.
        type GetBakersRewardPeriodStream =
            futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for `GetBlockItems`.
        type GetBlockItemsStream = futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'GetBlockPendingUpdates' method.
        type GetBlockPendingUpdatesStream =
            futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'GetBlockSpecialEvents' method.
        type GetBlockSpecialEventsStream =
            futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'GetBlockTransactionEvents' method.
        type GetBlockTransactionEventsStream =
            futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'Blocks' method.
        type GetBlocksStream =
            tokio_stream::wrappers::ReceiverStream<Result<Arc<[u8]>, tonic::Status>>;
        /// Return type for the 'GetCooldownAccounts' method.
        type GetCooldownAccountsStream =
            futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
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
        /// Return type for the 'GetPreCooldownAccounts' method.
        type GetPreCooldownAccountsStream =
            futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'GetPrePreCooldowneleaseAccounts' method.
        type GetPrePreCooldownAccountsStream =
            futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'GetScheduledReleaseAccounts' method.
        type GetScheduledReleaseAccountsStream =
            futures::channel::mpsc::Receiver<Result<Vec<u8>, tonic::Status>>;
        /// Return type for the 'GetWinningBakersEpoch' method.
        type GetWinningBakersEpochStream =
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
            let (hash, response) = self
                .run_blocking(move |consensus| {
                    let request = request.get_ref();
                    let block_hash = request.block_hash.as_ref().require()?;
                    let account_identifier = request.account_identifier.as_ref().require()?;
                    consensus.get_account_info_v2(block_hash, account_identifier)
                })
                .await?;

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
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_account_list_v2(request.get_ref(), sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_module_list(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetModuleListStream>, tonic::Status> {
            if !self.service_config.get_module_list {
                return Err(tonic::Status::unimplemented("`GetModuleList` is not enabled."));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_module_list_v2(request.get_ref(), sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_module_source(
            &self,
            request: tonic::Request<crate::grpc2::types::ModuleSourceRequest>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if !self.service_config.get_module_source {
                return Err(tonic::Status::unimplemented("`GetModuleSource` is not enabled."));
            }
            let (hash, response) = self
                .run_blocking(move |consensus| {
                    let request = request.get_ref();
                    let block_hash = request.block_hash.as_ref().require()?;
                    let module_ref = request.module_ref.as_ref().require()?;
                    consensus.get_module_source_v2(block_hash, module_ref)
                })
                .await?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_instance_list(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetInstanceListStream>, tonic::Status> {
            if !self.service_config.get_instance_list {
                return Err(tonic::Status::unimplemented("`GetInstanceList` is not enabled."));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_instance_list_v2(request.get_ref(), sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_instance_info(
            &self,
            request: tonic::Request<crate::grpc2::types::InstanceInfoRequest>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if !self.service_config.get_instance_info {
                return Err(tonic::Status::unimplemented("`GetInstanceInfo` is not enabled."));
            }
            let (hash, response) = self
                .run_blocking(move |consensus| {
                    let request = request.get_ref();
                    let block_hash = request.block_hash.as_ref().require()?;
                    let contract_address = request.address.as_ref().require()?;
                    consensus.get_instance_info_v2(block_hash, contract_address)
                })
                .await?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_instance_state(
            &self,
            request: tonic::Request<crate::grpc2::types::InstanceInfoRequest>,
        ) -> Result<tonic::Response<Self::GetInstanceStateStream>, tonic::Status> {
            if !self.service_config.get_instance_state {
                return Err(tonic::Status::unimplemented("`GetInstanceState` is not enabled."));
            }
            let (hash, response) = self
                .run_blocking(move |consensus| {
                    let request = request.get_ref();
                    let block_hash = request.block_hash.as_ref().require()?;
                    let contract_address = request.address.as_ref().require()?;
                    consensus.get_instance_state_v2(block_hash, contract_address)
                })
                .await?;
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
            if !self.service_config.instance_state_lookup {
                return Err(tonic::Status::unimplemented("`InstanceStateLookup` is not enabled."));
            }
            // this is cheap since we only lookup the tree root in the V1 case, and V0
            // lookup always involves the entire state anyhow.
            let request = request.into_inner();
            let block_hash = request.block_hash.require()?;
            let contract_address = request.address.require()?;
            let (hash, response) = self
                .run_blocking(move |consensus| {
                    consensus.get_instance_state_v2(&block_hash, &contract_address)
                })
                .await?;
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
            if !self.service_config.get_next_account_sequence_number {
                return Err(tonic::Status::unimplemented(
                    "`GetNextAccountSequenceNumber` is not enabled.",
                ));
            }
            let response = self
                .run_blocking(move |consensus| {
                    consensus.get_next_account_sequence_number_v2(request.get_ref())
                })
                .await?;
            Ok(tonic::Response::new(response))
        }

        async fn get_consensus_info(
            &self,
            _request: tonic::Request<crate::grpc2::types::Empty>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if !self.service_config.get_consensus_info {
                return Err(tonic::Status::unimplemented("`GetConsensusInfo` is not enabled."));
            }
            let response =
                self.run_blocking(move |consensus| consensus.get_consensus_info_v2()).await?;
            Ok(tonic::Response::new(response))
        }

        async fn get_consensus_detailed_status(
            &self,
            request: tonic::Request<crate::grpc2::types::ConsensusDetailedStatusQuery>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if !self.service_config.get_consensus_detailed_status {
                return Err(tonic::Status::unimplemented(
                    "`GetConsensusDetailedStatus` is not enabled.",
                ));
            }
            let response = self
                .run_blocking(move |consensus| {
                    let request = request.get_ref().genesis_index.as_ref().map(|x| x.value);
                    consensus.get_consensus_detailed_status_v2(request)
                })
                .await?;
            Ok(tonic::Response::new(response))
        }

        async fn get_ancestors(
            &self,
            request: tonic::Request<crate::grpc2::types::AncestorsRequest>,
        ) -> Result<tonic::Response<Self::GetAncestorsStream>, tonic::Status> {
            if !self.service_config.get_ancestors {
                return Err(tonic::Status::unimplemented("`GetAncestors` is not enabled."));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            let hash = self
                .run_blocking(move |consensus| {
                    let request = request.get_ref();
                    let block_hash = request.block_hash.as_ref().require()?;
                    let amount = request.amount;
                    consensus.get_ancestors_v2(block_hash, amount, sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_block_item_status(
            &self,
            request: tonic::Request<crate::grpc2::types::TransactionHash>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if !self.service_config.get_block_item_status {
                return Err(tonic::Status::unimplemented("`GetBlockItemStatus` is not enabled."));
            }
            let response = self
                .run_blocking(move |consensus| {
                    consensus.get_block_item_status_v2(request.get_ref())
                })
                .await?;
            Ok(tonic::Response::new(response))
        }

        async fn invoke_instance(
            &self,
            mut request: tonic::Request<crate::grpc2::types::InvokeInstanceRequest>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if !self.service_config.invoke_instance {
                return Err(tonic::Status::unimplemented("`InvokeInstance` is not enabled."));
            }
            let mut max_energy = self.invoke_max_energy;
            if let Some(nrg) = request.get_ref().energy.as_ref() {
                max_energy = std::cmp::min(max_energy, nrg.value);
            }
            request.get_mut().energy = Some(crate::grpc2::types::Energy {
                value: max_energy,
            });
            let (hash, response) = self
                .run_blocking(move |consensus| consensus.invoke_instance_v2(request.get_ref()))
                .await?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_cryptographic_parameters(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<crate::grpc2::types::CryptographicParameters>, tonic::Status>
        {
            if !self.service_config.get_cryptographic_parameters {
                return Err(tonic::Status::unimplemented(
                    "`GetCryptographicParameters` is not enabled.",
                ));
            }
            let (hash, response) = self
                .run_blocking(move |consensus| {
                    consensus.get_cryptographic_parameters_v2(request.get_ref())
                })
                .await?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_block_info(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if !self.service_config.get_block_info {
                return Err(tonic::Status::unimplemented("`GetBlockInfo` is not enabled."));
            }
            let (hash, response) = self
                .run_blocking(move |consensus| consensus.get_block_info_v2(request.get_ref()))
                .await?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_baker_list(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetBakerListStream>, tonic::Status> {
            if !self.service_config.get_baker_list {
                return Err(tonic::Status::unimplemented("`GetBakerList` is not enabled."));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_baker_list_v2(request.get_ref(), sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_pool_info(
            &self,
            request: tonic::Request<crate::grpc2::types::PoolInfoRequest>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if !self.service_config.get_pool_info {
                return Err(tonic::Status::unimplemented("`GetPoolInfo` is not enabled."));
            }
            let (hash, response) = self
                .run_blocking(move |consensus| consensus.get_pool_info_v2(request.get_ref()))
                .await?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_passive_delegation_info(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if !self.service_config.get_passive_delegation_info {
                return Err(tonic::Status::unimplemented(
                    "`GetPassiveDelegationInfo` is not enabled.",
                ));
            }
            let (hash, response) = self
                .run_blocking(move |consensus| {
                    consensus.get_passive_delegation_info_v2(request.get_ref())
                })
                .await?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_blocks_at_height(
            &self,
            request: tonic::Request<crate::grpc2::types::BlocksAtHeightRequest>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if !self.service_config.get_blocks_at_height {
                return Err(tonic::Status::unimplemented("`GetBlocksAtHeight` is not enabled."));
            }
            let data = self
                .run_blocking(move |consensus| consensus.get_blocks_at_height_v2(request.get_ref()))
                .await?;
            let response = tonic::Response::new(data);
            Ok(response)
        }

        async fn get_tokenomics_info(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if !self.service_config.get_tokenomics_info {
                return Err(tonic::Status::unimplemented("`GetTokenomicsInfo` is not enabled."));
            }
            let (hash, response) = self
                .run_blocking(move |consensus| consensus.get_tokenomics_info_v2(request.get_ref()))
                .await?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_pool_delegators(
            &self,
            request: tonic::Request<crate::grpc2::types::GetPoolDelegatorsRequest>,
        ) -> Result<tonic::Response<Self::GetPoolDelegatorsStream>, tonic::Status> {
            if !self.service_config.get_pool_delegators {
                return Err(tonic::Status::unimplemented("`GetPoolDelegators` is not enabled."));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_pool_delegators_v2(request.get_ref(), sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_pool_delegators_reward_period(
            &self,
            request: tonic::Request<crate::grpc2::types::GetPoolDelegatorsRequest>,
        ) -> Result<tonic::Response<Self::GetPoolDelegatorsRewardPeriodStream>, tonic::Status>
        {
            if !self.service_config.get_pool_delegators_reward_period {
                return Err(tonic::Status::unimplemented(
                    "`GetPoolDelegatorsRewardPeriod` is not enabled.",
                ));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_pool_delegators_reward_period_v2(request.get_ref(), sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_passive_delegators(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetPassiveDelegatorsStream>, tonic::Status> {
            if !self.service_config.get_passive_delegators {
                return Err(tonic::Status::unimplemented("`GetPassiveDelegators` is not enabled."));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_passive_delegators_v2(request.get_ref(), sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_passive_delegators_reward_period(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetPassiveDelegatorsRewardPeriodStream>, tonic::Status>
        {
            if !self.service_config.get_passive_delegators_reward_period {
                return Err(tonic::Status::unimplemented(
                    "`GetPassiveDelegatorsRewardPeriod` is not enabled.",
                ));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_passive_delegators_reward_period_v2(request.get_ref(), sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_branches(
            &self,
            _request: tonic::Request<crate::grpc2::types::Empty>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if !self.service_config.get_branches {
                return Err(tonic::Status::unimplemented("`GetBranches` is not enabled."));
            }
            Ok(tonic::Response::new(
                self.run_blocking(move |consensus| consensus.get_branches_v2()).await?,
            ))
        }

        async fn get_election_info(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if !self.service_config.get_election_info {
                return Err(tonic::Status::unimplemented("`GetElectionInfo` is not enabled."));
            }
            let (hash, response) = self
                .run_blocking(move |consensus| consensus.get_election_info_v2(request.get_ref()))
                .await?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_identity_providers(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetIdentityProvidersStream>, tonic::Status> {
            if !self.service_config.get_identity_providers {
                return Err(tonic::Status::unimplemented("`GetIdentityProviders` is not enabled."));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(10);
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_identity_providers_v2(request.get_ref(), sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_anonymity_revokers(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetAnonymityRevokersStream>, tonic::Status> {
            if !self.service_config.get_anonymity_revokers {
                return Err(tonic::Status::unimplemented("`GetAnonymityRevokers` is not enabled."));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(10);
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_anonymity_revokers_v2(request.get_ref(), sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_account_non_finalized_transactions(
            &self,
            request: tonic::Request<crate::grpc2::types::AccountAddress>,
        ) -> Result<tonic::Response<Self::GetAccountNonFinalizedTransactionsStream>, tonic::Status>
        {
            if !self.service_config.get_account_non_finalized_transactions {
                return Err(tonic::Status::unimplemented(
                    "`GetAccountNonFinalizedTransactions` is not enabled.",
                ));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(10);
            self.run_blocking(move |consensus| {
                consensus.get_account_non_finalized_transactions_v2(request.get_ref(), sender)
            })
            .await?;
            let response = tonic::Response::new(receiver);
            Ok(response)
        }

        async fn get_block_transaction_events(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetBlockTransactionEventsStream>, tonic::Status> {
            if !self.service_config.get_block_transaction_events {
                return Err(tonic::Status::unimplemented(
                    "`GetBlockTransactionEvents` is not enabled.",
                ));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(10);
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_block_transaction_events_v2(request.get_ref(), sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_block_special_events(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetBlockSpecialEventsStream>, tonic::Status> {
            if !self.service_config.get_block_special_events {
                return Err(tonic::Status::unimplemented(
                    "`GetBlockSpecialEvents` is not enabled.",
                ));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(10);
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_block_special_events_v2(request.get_ref(), sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_block_pending_updates(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetBlockPendingUpdatesStream>, tonic::Status> {
            if !self.service_config.get_block_pending_updates {
                return Err(tonic::Status::unimplemented(
                    "`GetBlockPendingUpdates` is not enabled.",
                ));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(10);
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_block_pending_updates_v2(request.get_ref(), sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_next_update_sequence_numbers(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if !self.service_config.get_next_update_sequence_numbers {
                return Err(tonic::Status::unimplemented(
                    "`GetNextUpdateSequenceNumber` is not enabled.",
                ));
            }
            let (hash, response) = self
                .run_blocking(move |consensus| {
                    consensus.get_next_update_sequence_numbers_v2(request.get_ref())
                })
                .await?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_scheduled_release_accounts(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetScheduledReleaseAccountsStream>, tonic::Status>
        {
            if !self.service_config.get_scheduled_release_accounts {
                return Err(tonic::Status::unimplemented(
                    "`GetScheduledReleaseAccounts` is not enabled.",
                ));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(10);
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_scheduled_release_accounts_v2(request.get_ref(), sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_cooldown_accounts(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetCooldownAccountsStream>, tonic::Status> {
            if !self.service_config.get_cooldown_accounts {
                return Err(tonic::Status::unimplemented("`GetCooldownAccounts` is not enabled."));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(10);
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_cooldown_accounts_v2(request.get_ref(), sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_pre_cooldown_accounts(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetPreCooldownAccountsStream>, tonic::Status> {
            if !self.service_config.get_pre_cooldown_accounts {
                return Err(tonic::Status::unimplemented(
                    "`GetPreCooldownAccounts` is not enabled.",
                ));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(10);
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_pre_cooldown_accounts_v2(request.get_ref(), sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_pre_pre_cooldown_accounts(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetPrePreCooldownAccountsStream>, tonic::Status> {
            if !self.service_config.get_pre_pre_cooldown_accounts {
                return Err(tonic::Status::unimplemented(
                    "`GetPrePreCooldownAccounts` is not enabled.",
                ));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(10);
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_pre_pre_cooldown_accounts_v2(request.get_ref(), sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_block_chain_parameters(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if !self.service_config.get_block_chain_parameters {
                return Err(tonic::Status::unimplemented(
                    "`GetBlockChainParameters` is not enabled.",
                ));
            }
            let (hash, response) = self
                .run_blocking(move |consensus| {
                    consensus.get_block_chain_parameters_v2(request.get_ref())
                })
                .await?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_block_finalization_summary(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if !self.service_config.get_block_finalization_summary {
                return Err(tonic::Status::unimplemented(
                    "`GetBlockFinalizationSummary` is not enabled.",
                ));
            }
            let (hash, response) = self
                .run_blocking(move |consensus| {
                    consensus.get_block_finalization_summary_v2(request.get_ref())
                })
                .await?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_bakers_reward_period(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetBakersRewardPeriodStream>, tonic::Status> {
            if !self.service_config.get_bakers_reward_period {
                return Err(tonic::Status::unimplemented(
                    "`GetBakersRewardPeriod` is not enabled.",
                ));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(10);
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_bakers_reward_period_v2(request.get_ref(), sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_baker_earliest_win_time(
            &self,
            request: tonic::Request<crate::grpc2::types::BakerId>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if !self.service_config.get_baker_earliest_win_time {
                return Err(tonic::Status::unimplemented(
                    "`GetBakerEarliestWinTime` is not enabled.",
                ));
            }
            let response = self
                .run_blocking(move |consensus| {
                    consensus.get_baker_earliest_win_time_v2(request.get_ref())
                })
                .await?;
            let response = tonic::Response::new(response);
            Ok(response)
        }

        async fn shutdown(
            &self,
            _request: tonic::Request<crate::grpc2::types::Empty>,
        ) -> Result<tonic::Response<crate::grpc2::types::Empty>, tonic::Status> {
            if !self.service_config.shutdown {
                return Err(tonic::Status::unimplemented("`Shutdown` is not enabled."));
            }
            match self.node.close() {
                Ok(_) => Ok(tonic::Response::new(crate::grpc2::types::Empty {})),
                Err(e) => Err(tonic::Status::internal(format!("Unable to shutdown server {}.", e))),
            }
        }

        async fn peer_connect(
            &self,
            request: tonic::Request<crate::grpc2::types::IpSocketAddress>,
        ) -> Result<tonic::Response<crate::grpc2::types::Empty>, tonic::Status> {
            if !self.service_config.peer_connect {
                return Err(tonic::Status::unimplemented("`PeerConnect` is not enabled."));
            }
            if self.node.is_network_stopped() {
                Err(tonic::Status::failed_precondition(
                    "The network is stopped due to unrecognized protocol update.",
                ))
            } else {
                let peer_connect = request.into_inner();
                if let Ok(ip) = peer_connect.ip.require()?.value.parse::<std::net::IpAddr>() {
                    let addr = SocketAddr::new(ip, peer_connect.port.require()?.value as u16);
                    self.node.register_conn_change(crate::connection::ConnChange::NewConn {
                        addr,
                        peer_type: crate::common::PeerType::Node,
                        given: true,
                    });
                    Ok(tonic::Response::new(crate::grpc2::types::Empty {}))
                } else {
                    Err(tonic::Status::invalid_argument("Invalid IP address."))
                }
            }
        }

        async fn peer_disconnect(
            &self,
            request: tonic::Request<crate::grpc2::types::IpSocketAddress>,
        ) -> Result<tonic::Response<crate::grpc2::types::Empty>, tonic::Status> {
            if !self.service_config.peer_disconnect {
                return Err(tonic::Status::unimplemented("`PeerDisconnect` is not enabled."));
            }
            if self.node.is_network_stopped() {
                Err(tonic::Status::failed_precondition(
                    "The network is stopped due to unrecognized protocol update.",
                ))
            } else {
                let peer_connect = request.into_inner();
                if let Ok(ip) = peer_connect.ip.require()?.value.parse::<std::net::IpAddr>() {
                    let addr = SocketAddr::new(ip, peer_connect.port.require()?.value as u16);
                    if self.node.drop_addr(addr) {
                        Ok(tonic::Response::new(crate::grpc2::types::Empty {}))
                    } else {
                        Err(tonic::Status::not_found("The peer was not found."))
                    }
                } else {
                    Err(tonic::Status::invalid_argument("Invalid IP address."))
                }
            }
        }

        async fn get_banned_peers(
            &self,
            _request: tonic::Request<crate::grpc2::types::Empty>,
        ) -> Result<tonic::Response<crate::grpc2::types::BannedPeers>, tonic::Status> {
            if !self.service_config.get_banned_peers {
                return Err(tonic::Status::unimplemented("`GetBannedPeers` is not enabled."));
            }
            if let Ok(banned_peers) = self.node.get_banlist() {
                let peers = banned_peers
                    .into_iter()
                    .map(|banned_peer| {
                        let ip_address = match banned_peer {
                            crate::p2p::bans::PersistedBanId::Ip(addr) => addr.to_string(),
                        };
                        crate::grpc2::types::BannedPeer {
                            ip_address: Some(crate::grpc2::types::IpAddress {
                                value: ip_address,
                            }),
                        }
                    })
                    .collect();
                Ok(tonic::Response::new(crate::grpc2::types::BannedPeers {
                    peers,
                }))
            } else {
                Err(tonic::Status::internal("Could not load banned peers."))
            }
        }

        async fn ban_peer(
            &self,
            request: tonic::Request<crate::grpc2::types::PeerToBan>,
        ) -> Result<tonic::Response<crate::grpc2::types::Empty>, tonic::Status> {
            if !self.service_config.ban_peer {
                return Err(tonic::Status::unimplemented("`BanPeer` is not enabled."));
            }
            let ip = request.into_inner().ip_address.require()?;
            match ip.value.parse::<std::net::IpAddr>() {
                Ok(ip_addr) => match self.node.drop_by_ip_and_ban(ip_addr) {
                    Ok(_) => Ok(tonic::Response::new(crate::grpc2::types::Empty {})),
                    Err(e) => Err(tonic::Status::internal(format!("Could not ban peer {}.", e))),
                },
                Err(e) => Err(tonic::Status::invalid_argument(format!(
                    "Invalid IP address provided {}",
                    e
                ))),
            }
        }

        async fn unban_peer(
            &self,
            request: tonic::Request<crate::grpc2::types::BannedPeer>,
        ) -> Result<tonic::Response<crate::grpc2::types::Empty>, tonic::Status> {
            if !self.service_config.unban_peer {
                return Err(tonic::Status::unimplemented("`UnbanPeer` is not enabled."));
            }
            match request.into_inner().ip_address.require()?.value.parse::<std::net::IpAddr>() {
                Ok(ip_addr) => {
                    let banned_id = crate::p2p::bans::PersistedBanId::Ip(ip_addr);
                    match self.node.unban_node(banned_id) {
                        Ok(_) => Ok(tonic::Response::new(crate::grpc2::types::Empty {})),
                        Err(e) => {
                            Err(tonic::Status::internal(format!("Could not unban peer {}.", e)))
                        }
                    }
                }
                Err(e) => {
                    Err(tonic::Status::invalid_argument(format!("Invalid IP address {}.", e)))
                }
            }
        }

        #[cfg(feature = "network_dump")]
        async fn dump_start(
            &self,
            request: tonic::Request<crate::grpc2::types::DumpRequest>,
        ) -> Result<tonic::Response<crate::grpc2::types::Empty>, tonic::Status> {
            if !self.service_config.dump_start {
                return Err(tonic::Status::unimplemented("`DumpStart` is not enabled."));
            }
            let file_path = request.get_ref().file.to_owned();
            if file_path.is_empty() {
                Err(tonic::Status::invalid_argument("The supplied path must be non-empty"))
            } else {
                match self.node.activate_dump(&file_path, request.get_ref().raw) {
                    Ok(_) => Ok(tonic::Response::new(crate::grpc2::types::Empty {})),
                    Err(e) => {
                        Err(tonic::Status::internal(format!("Could not start network dump {}", e)))
                    }
                }
            }
        }

        #[cfg(not(feature = "network_dump"))]
        async fn dump_start(
            &self,
            _request: tonic::Request<crate::grpc2::types::DumpRequest>,
        ) -> Result<tonic::Response<crate::grpc2::types::Empty>, tonic::Status> {
            if !self.service_config.dump_start {
                return Err(tonic::Status::unimplemented("`DumpStart` is not enabled."));
            }
            Err(tonic::Status::failed_precondition("Feature \"network_dump\" is not active."))
        }

        #[cfg(feature = "network_dump")]
        async fn dump_stop(
            &self,
            _request: tonic::Request<crate::grpc2::types::Empty>,
        ) -> Result<tonic::Response<crate::grpc2::types::Empty>, tonic::Status> {
            if !self.service_config.dump_stop {
                return Err(tonic::Status::unimplemented("`DumpStop` is not enabled."));
            }
            match self.node.stop_dump() {
                Ok(_) => Ok(tonic::Response::new(crate::grpc2::types::Empty {})),
                Err(e) => {
                    Err(tonic::Status::internal(format!("Could not stop network dump {}", e)))
                }
            }
        }

        #[cfg(not(feature = "network_dump"))]
        async fn dump_stop(
            &self,
            _request: tonic::Request<crate::grpc2::types::Empty>,
        ) -> Result<tonic::Response<crate::grpc2::types::Empty>, tonic::Status> {
            if !self.service_config.dump_stop {
                return Err(tonic::Status::unimplemented("`DumpStop` is not enabled."));
            }
            Err(tonic::Status::failed_precondition("Feature \"network_dump\" is not active."))
        }

        async fn get_peers_info(
            &self,
            _request: tonic::Request<crate::grpc2::types::Empty>,
        ) -> Result<tonic::Response<crate::grpc2::types::PeersInfo>, tonic::Status> {
            if !self.service_config.get_peers_info {
                return Err(tonic::Status::unimplemented("`GetPeersInfo` is not enabled."));
            }
            // we do a clone so we can release the lock quickly.
            let peer_statuses = crate::read_or_die!(self.node.peers).peer_states.clone();
            let peers = self
                .node
                .get_peer_stats(None)
                .into_iter()
                .map(|peer_stats| {
                    // Collect the network statistics
                    let network_stats = Some(crate::grpc2::types::peers_info::peer::NetworkStats {
                        packets_sent:     peer_stats.msgs_sent,
                        packets_received: peer_stats.msgs_received,
                        latency:          peer_stats.latency,
                    });
                    // Get the type of the peer.
                    let consensus_info = match peer_stats.peer_type {
                        // Regular nodes do have a catchup status.
                        crate::common::PeerType::Node => {
                            let catchup_status = match peer_statuses.get(&peer_stats.local_id) {
                                Some(crate::consensus_ffi::catch_up::PeerStatus::CatchingUp) => {
                                    crate::grpc2::types::peers_info::peer::CatchupStatus::Catchingup
                                }
                                Some(crate::consensus_ffi::catch_up::PeerStatus::UpToDate) => {
                                    crate::grpc2::types::peers_info::peer::CatchupStatus::Uptodate
                                }
                                _ => crate::grpc2::types::peers_info::peer::CatchupStatus::Pending,
                            };
                            crate::grpc2::types::peers_info::peer::ConsensusInfo::NodeCatchupStatus(
                                catchup_status.into(),
                            )
                        }
                        // Bootstrappers do not have a catchup status as they are not participating
                        // in the consensus protocol.
                        crate::common::PeerType::Bootstrapper => {
                            crate::grpc2::types::peers_info::peer::ConsensusInfo::Bootstrapper(
                                crate::grpc2::types::Empty::default(),
                            )
                        }
                    };
                    // Get the catchup status of the peer.
                    let socket_address = crate::grpc2::types::IpSocketAddress {
                        ip:   Some(crate::grpc2::types::IpAddress {
                            value: peer_stats.external_address().ip().to_string(),
                        }),
                        port: Some(crate::grpc2::types::Port {
                            value: peer_stats.external_port as u32,
                        }),
                    };
                    // Wrap the peer id.
                    let peer_id = crate::grpc2::types::PeerId {
                        value: format!("{}", peer_stats.self_id),
                    };
                    crate::grpc2::types::peers_info::Peer {
                        peer_id: Some(peer_id),
                        socket_address: Some(socket_address),
                        consensus_info: Some(consensus_info),
                        network_stats,
                    }
                })
                .collect();
            Ok(tonic::Response::new(crate::grpc2::types::PeersInfo {
                peers,
            }))
        }

        async fn get_node_info(
            &self,
            _request: tonic::Request<crate::grpc2::types::Empty>,
        ) -> Result<tonic::Response<types::NodeInfo>, tonic::Status> {
            if !self.service_config.get_node_info {
                return Err(tonic::Status::unimplemented("`GetNodeInfo` is not enabled."));
            }
            let peer_version = self.node.get_version();
            let local_time = Some(types::Timestamp {
                value: std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .expect("Time went backwards.")
                    .as_millis() as u64,
            });
            let peer_uptime = Some(types::Duration {
                value: self.node.get_uptime() as u64,
            });

            let network_info = {
                let node_id = self.node.id().to_string();
                let peer_total_sent = self
                    .node
                    .connection_handler
                    .total_sent
                    .load(std::sync::atomic::Ordering::Relaxed);
                let peer_total_received = self
                    .node
                    .connection_handler
                    .total_received
                    .load(std::sync::atomic::Ordering::Relaxed);
                let avg_bps_in = self.node.stats.avg_bps_in.get();
                let avg_bps_out = self.node.stats.avg_bps_out.get();

                types::node_info::NetworkInfo {
                    node_id: Some(types::PeerId {
                        value: node_id,
                    }),
                    peer_total_sent,
                    peer_total_received,
                    avg_bps_in,
                    avg_bps_out,
                }
            };

            let details = match self.node.peer_type() {
                // The node is a bootstrapper
                crate::common::PeerType::Bootstrapper => {
                    types::node_info::Details::Bootstrapper(types::Empty {})
                }
                // The node is a regular node
                crate::common::PeerType::Node => {
                    let consensus_status = if !self.consensus.is_consensus_running() {
                        // The consensus is not running for the node.
                        // The node does not support the current
                        // protocol on the chain.
                        types::node_info::node::ConsensusStatus::NotRunning(types::Empty {})
                    } else if matches!(self.consensus.consensus_type, ConsensusType::Active) {
                        let (in_baking_committee, _, bid, _) = self.consensus.in_baking_committee();
                        let baker_id = types::BakerId {
                            value: bid,
                        };

                        let baker_status = match in_baking_committee {
                            ConsensusIsInBakingCommitteeResponse::ActiveInCommittee => {
                                if self.consensus.in_finalization_committee() {
                                    // The node is configured with baker keys and is an active
                                    // member of
                                    // the finalization committee.
                                    types::node_info::baker_consensus_info::Status::ActiveFinalizerCommitteeInfo(
                                            types::node_info::baker_consensus_info::ActiveFinalizerCommitteeInfo{})
                                } else {
                                    // The node is configured with baker keys and is an active
                                    // member of the baking
                                    // committee.
                                    types::node_info::baker_consensus_info::Status::ActiveBakerCommitteeInfo(
                                            types::node_info::baker_consensus_info::ActiveBakerCommitteeInfo{})
                                }
                            }
                            ConsensusIsInBakingCommitteeResponse::NotInCommittee => {
                                // The node is configured with baker keys but is not in the baking
                                // committee.
                                types::node_info::baker_consensus_info::Status::PassiveCommitteeInfo(
                                        types::node_info::baker_consensus_info::PassiveCommitteeInfo::NotInCommittee.into()
                                    )
                            }
                            ConsensusIsInBakingCommitteeResponse::AddedButNotActiveInCommittee => {
                                // The node is configured with baker keys and it is a baker, but not
                                // for the current epoch.
                                types::node_info::baker_consensus_info::Status::PassiveCommitteeInfo(
                                        types::node_info::baker_consensus_info::PassiveCommitteeInfo::AddedButNotActiveInCommittee.into()
                                    )
                            }
                            ConsensusIsInBakingCommitteeResponse::AddedButWrongKeys => {
                                // The node is configured with baker keys however they do not match
                                // expected baker keys for the associated account.
                                types::node_info::baker_consensus_info::Status::PassiveCommitteeInfo(
                                        types::node_info::baker_consensus_info::PassiveCommitteeInfo::AddedButWrongKeys.into()
                                    )
                            }
                        };
                        // construct the baker status
                        types::node_info::node::ConsensusStatus::Active(
                            types::node_info::BakerConsensusInfo {
                                baker_id: Some(baker_id),
                                status:   Some(baker_status),
                            },
                        )
                    } else {
                        // The node is not configured with baker keys and is participating in
                        // the consensus passively.
                        types::node_info::node::ConsensusStatus::Passive(types::Empty {})
                    };
                    // Construct the details of the node.
                    types::node_info::Details::Node(types::node_info::Node {
                        consensus_status: Some(consensus_status),
                    })
                }
            };

            Ok(tonic::Response::new(types::NodeInfo {
                peer_version,
                local_time,
                peer_uptime,
                network_info: Some(network_info),
                details: Some(details),
            }))
        }

        async fn send_block_item(
            &self,
            request: tonic::Request<crate::grpc2::types::SendBlockItemRequest>,
        ) -> Result<tonic::Response<crate::grpc2::types::TransactionHash>, tonic::Status> {
            use ConsensusFfiResponse::*;
            if !self.service_config.send_block_item {
                return Err(tonic::Status::unimplemented("`SendBlockItem` is not enabled."));
            }

            if self.node.is_network_stopped() {
                return Err(tonic::Status::failed_precondition(
                    "The network is stopped due to unrecognized protocol update.",
                ));
            }

            let ((transaction_hash, consensus_result), transaction_bytes) = self
                .run_blocking(move |consensus| {
                    let transaction_bytes = request.into_inner().get_v0_format()?;
                    if transaction_bytes.len() > crate::configuration::PROTOCOL_MAX_TRANSACTION_SIZE
                    {
                        warn!("Received a transaction that exceeds maximum transaction size.");
                        return Err(tonic::Status::invalid_argument(
                            "Transaction size exceeds maximum allowed size.",
                        ));
                    }
                    Ok((consensus.send_transaction(&transaction_bytes), transaction_bytes))
                })
                .await?;

            let result = if consensus_result == Success {
                let mut payload = Vec::with_capacity(1 + transaction_bytes.len());
                payload.write_u8(PacketType::Transaction as u8)?;
                payload.write_all(&transaction_bytes)?;

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
                    let transaction_hash = match transaction_hash {
                        Some(h) => h,
                        None => {
                            error!("Block item hash not present, but transaction is accepted.");
                            return Err(tonic::Status::internal(
                                "Block item hash not present, but transaction is accepted.",
                            ));
                        }
                    };
                    Ok(tonic::Response::new(crate::grpc2::types::TransactionHash {
                        value: transaction_hash.to_vec(),
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

        async fn get_account_transaction_sign_hash(
            &self,
            request: tonic::Request<crate::grpc2::types::PreAccountTransaction>,
        ) -> Result<tonic::Response<crate::grpc2::types::AccountTransactionSignHash>, tonic::Status>
        {
            if !self.service_config.get_account_transaction_sign_hash {
                return Err(tonic::Status::unimplemented(
                    "`GetAccountTransactionSignHash` is not enabled.",
                ));
            }
            let request = request.into_inner();
            let (header, payload) = request.try_into()?;
            let sign_hash =
                concordium_base::transactions::compute_transaction_sign_hash(&header, &payload);
            Ok(tonic::Response::new(types::AccountTransactionSignHash {
                value: sign_hash.to_vec(),
            }))
        }

        async fn get_block_items(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Self::GetBlockItemsStream>, tonic::Status> {
            if !self.service_config.get_block_items {
                return Err(tonic::Status::unimplemented("`GetBlockItems` is not enabled."));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_block_items_v2(request.get_ref(), sender)
                })
                .await?;
            let mut response = tonic::Response::new(receiver);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_block_certificates(
            &self,
            request: tonic::Request<crate::grpc2::types::BlockHashInput>,
        ) -> Result<tonic::Response<Vec<u8>>, tonic::Status> {
            if !self.service_config.get_block_certificates {
                return Err(tonic::Status::unimplemented("`GetBlockCertificates` is not enabled."));
            }
            let (hash, response) = self
                .run_blocking(move |consensus| {
                    consensus.get_block_certificates_v2(request.get_ref())
                })
                .await?;
            let mut response = tonic::Response::new(response);
            add_hash(&mut response, hash)?;
            Ok(response)
        }

        async fn get_first_block_epoch(
            &self,
            request: tonic::Request<crate::grpc2::types::EpochRequest>,
        ) -> Result<tonic::Response<crate::grpc2::types::BlockHash>, tonic::Status> {
            if !self.service_config.get_first_block_epoch {
                return Err(tonic::Status::unimplemented("`GetFirstBlockEpoch` is not enabled."));
            }
            let hash = self
                .run_blocking(move |consensus| {
                    consensus.get_first_block_epoch_v2(request.get_ref())
                })
                .await?;
            Ok(tonic::Response::new(types::BlockHash {
                value: hash.to_vec(),
            }))
        }

        async fn get_winning_bakers_epoch(
            &self,
            request: tonic::Request<crate::grpc2::types::EpochRequest>,
        ) -> Result<tonic::Response<Self::GetWinningBakersEpochStream>, tonic::Status> {
            if !self.service_config.get_winning_bakers_epoch {
                return Err(tonic::Status::unimplemented(
                    "`GetWinningBakersEpoch` is not enabled.",
                ));
            }
            let (sender, receiver) = futures::channel::mpsc::channel(100);
            self.run_blocking(move |consensus| {
                consensus.get_winning_bakers_epoch_v2(request.get_ref(), sender)
            })
            .await?;
            Ok(tonic::Response::new(receiver))
        }

        async fn dry_run(
            &self,
            request: tonic::Request<tonic::Streaming<crate::grpc2::types::DryRunRequest>>,
        ) -> Result<tonic::Response<Self::DryRunStream>, tonic::Status> {
            if !self.service_config.dry_run {
                return Err(tonic::Status::unimplemented("`DryRun` is not enabled."));
            }
            // If the number of concurrent dry run sessions is limited, we try to get a
            // permit from the semaphore.
            let permit = match self.dry_run_semaphore.as_ref() {
                None => None,
                Some(semaphore) => {
                    let permit = semaphore.clone().try_acquire_owned().map_err(|_| {
                        tonic::Status::resource_exhausted("Too many concurrent `DryRun` requests")
                    })?;
                    Some(permit)
                }
            };

            let energy_quota = self.dry_run_max_energy;
            let dry_run = self.consensus.dry_run(energy_quota);
            let input = request.into_inner();
            let timeout = self.dry_run_timeout;
            let output = DryRunStream::new(dry_run, input, timeout, permit);
            let mut response = tonic::Response::new(Box::pin(output));
            response.metadata_mut().insert("quota", energy_quota.into());
            // u64::MAX milliseconds is already hundreds of millions of years, so even if
            // this is an underestimate of the actual timeout, it doesn't
            // matter.
            response
                .metadata_mut()
                .insert("timeout", u64::try_from(timeout.as_millis()).unwrap_or(u64::MAX).into());
            Ok(response)
        }
    }

    struct DryRunStream {
        dry_run: crate::consensus_ffi::ffi::DryRun,
        input:   tonic::Streaming<types::DryRunRequest>,
        timeout: std::pin::Pin<Box<tokio::time::Sleep>>,
        _permit: Option<OwnedSemaphorePermit>,
        done:    bool,
    }

    impl DryRunStream {
        pub fn new(
            dry_run: crate::consensus_ffi::ffi::DryRun,
            input: tonic::Streaming<types::DryRunRequest>,
            timeout: tokio::time::Duration,
            permit: Option<OwnedSemaphorePermit>,
        ) -> Self {
            DryRunStream {
                dry_run,
                input,
                timeout: Box::pin(tokio::time::sleep(timeout)),
                _permit: permit,
                done: false,
            }
        }
    }

    impl futures::Stream for DryRunStream {
        type Item = tonic::Result<Vec<u8>>;

        fn poll_next(
            mut self: std::pin::Pin<&mut Self>,
            cx: &mut std::task::Context<'_>,
        ) -> std::task::Poll<Option<Self::Item>> {
            use std::task::Poll::Ready;
            if self.done {
                return Ready(None);
            }
            let timeout = std::pin::pin!(&mut self.timeout);
            if timeout.poll(cx).is_ready() {
                self.done = true;
                return Ready(Some(Err(tonic::Status::deadline_exceeded(
                    "dry run deadline elapsed",
                ))));
            }
            let input = std::pin::pin!(&mut self.input);
            let Some(dry_run_request) = futures::ready!(input.poll_next(cx)) else {
                self.done = true;
                return Ready(None);
            };
            let Ok(request) = dry_run_request else {
                self.done = true;
                return Ready(Some(Err(tonic::Status::invalid_argument("invalid dry run request"))));
            };

            use crate::grpc2::types::dry_run_request::Request::*;
            let result = match request.request.require()? {
                LoadBlockState(block_hash_input) => {
                    self.dry_run.load_block_state(&block_hash_input)
                }
                StateQuery(query) => {
                    use crate::grpc2::types::dry_run_state_query::Query::*;
                    match query.query.require()? {
                        GetAccountInfo(account) => self.dry_run.get_account_info(&account),
                        GetInstanceInfo(instance) => self.dry_run.get_instance_info(&instance),
                        InvokeInstance(invoke_instance_input) => {
                            self.dry_run.invoke_instance(&invoke_instance_input)
                        }
                    }
                }
                StateOperation(operation) => {
                    use crate::grpc2::types::dry_run_state_operation::Operation::*;
                    match operation.operation.require()? {
                        SetTimestamp(timestamp) => self.dry_run.set_timestamp(timestamp),
                        MintToAccount(mint) => self.dry_run.mint_to_account(mint),
                        RunTransaction(run_transaction_input) => {
                            self.dry_run.transaction(run_transaction_input)
                        }
                    }
                }
            };
            self.done = result.is_err();
            Ready(Some(result))
        }
    }
}

/// Add a block hash to the metadata of a response. Used for returning the block
/// hash.
fn add_hash<T>(response: &mut tonic::Response<T>, hash: [u8; 32]) -> Result<(), tonic::Status> {
    let value = tonic::metadata::MetadataValue::try_from(hex::encode(hash))
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

/// Tower layer adding middleware updating some of the node statictics related
/// to gRPC API.
#[derive(Clone)]
pub struct StatsLayer {
    stats: Arc<StatsExportService>,
}

impl<S> tower::Layer<S> for StatsLayer {
    type Service = StatsMiddleware<S>;

    fn layer(&self, service: S) -> Self::Service {
        StatsMiddleware {
            stats: self.stats.clone(),
            inner: service,
        }
    }
}

/// Tower middleware for updating some of the node statictics related to the
/// gRPC API.
#[derive(Clone)]
pub struct StatsMiddleware<S> {
    stats: Arc<StatsExportService>,
    inner: S,
}

/// Get the label from a gRPC status code. This is used when updating metrics of
/// the prometheus exporter.
fn get_grpc_code_label(code: tonic::Code) -> &'static str {
    use tonic::Code::*;
    match code {
        Ok => "ok",
        Cancelled => "cancelled",
        Unknown => "unknown",
        InvalidArgument => "invalid argument",
        DeadlineExceeded => "deadline exceeded",
        NotFound => "not found",
        AlreadyExists => "already exists",
        PermissionDenied => "permission denied",
        ResourceExhausted => "resource exhausted",
        FailedPrecondition => "failed precondition",
        Aborted => "aborted",
        OutOfRange => "out of range",
        Unimplemented => "unimplemented",
        Internal => "internal",
        Unavailable => "unavailable",
        DataLoss => "data loss",
        Unauthenticated => "unauthenticated",
    }
}

/// Actual middleware implementation updating the stats.
/// The middleware is called once for each gRPC request, even for the streaming
/// gRPC methods.
impl<S, Body: hyper::body::HttpBody + Unpin + Send + Default>
    tower::Service<hyper::Request<hyper::Body>> for StatsMiddleware<S>
where
    S: tower::Service<
            hyper::Request<hyper::Body>,
            Response = hyper::Response<Body>,
            Error = tower::BoxError,
        > + Clone
        + Send
        + 'static,
    S::Future: Send + 'static,
{
    type Error = tower::BoxError;
    type Future = futures::future::BoxFuture<'static, Result<Self::Response, Self::Error>>;
    type Response = hyper::Response<Body>;

    fn poll_ready(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx)
    }

    fn call(&mut self, req: hyper::Request<hyper::Body>) -> Self::Future {
        // See https://docs.rs/tower/latest/tower/trait.Service.html#be-careful-when-cloning-inner-services
        // for details on why this is necessary
        let clone = self.inner.clone();
        let mut inner = std::mem::replace(&mut self.inner, clone);

        let grpc_request_duration = self.stats.grpc_request_response_time.clone();

        Box::pin(async move {
            let endpoint_name = req.uri().path().to_owned();
            let request_received = tokio::time::Instant::now();

            let (mut response, duration) = {
                // Forward the request.
                let result = inner.call(req).await;
                // Time taken for the inner service to send back a response, meaning for
                // streaming gRPC methods this is the duration for it to first return a stream.
                let duration = request_received.elapsed().as_secs_f64();
                match result {
                    Err(e) => {
                        // If the load shed service terminated the request this will be signalled as
                        // an Err(Overloaded). So record resource exhaustion
                        // in the metrics.
                        let (code, response) = if e.is::<tower::load_shed::error::Overloaded>() {
                            // return a response with empty body of the correct type. `to_http`
                            // constructs a response with a `BoxBody` but
                            // here we need a more general one to make the service generic enough.
                            let new_response =
                                tonic::Status::resource_exhausted("Too many concurrent requests.")
                                    .to_http()
                                    .map(|_| Default::default());
                            (tonic::Code::ResourceExhausted, Ok(new_response))
                        } else {
                            (tonic::Code::Internal, Err(e))
                        };
                        grpc_request_duration
                            .with_label_values(&[endpoint_name.as_str(), get_grpc_code_label(code)])
                            .observe(duration);
                        return response;
                    }
                    Ok(result) => (result, duration),
                }
            };

            // Check if the gRPC status header is part of the HTTP headers, if not check for
            // the status in the HTTP trailers.
            // Every gRPC response are expected to provide the grpc-status in either the
            // header or the trailer.
            let grpc_status_header = response.headers().get("grpc-status");
            if let Some(header_value) = grpc_status_header {
                let status_code_label =
                    get_grpc_code_label(tonic::Code::from_bytes(header_value.as_bytes()));

                grpc_request_duration
                    .with_label_values(&[endpoint_name.as_str(), status_code_label])
                    .observe(duration);
            } else if let Ok(Some(trailers)) =
                hyper::body::HttpBody::trailers(response.body_mut()).await
            {
                if let Some(header_value) = trailers.get("grpc-status") {
                    let status_code_label =
                        get_grpc_code_label(tonic::Code::from_bytes(header_value.as_bytes()));

                    grpc_request_duration
                        .with_label_values(&[endpoint_name.as_str(), status_code_label])
                        .observe(duration);
                }
            } else {
                grpc_request_duration
                    .with_label_values(&[
                        endpoint_name.as_str(),
                        get_grpc_code_label(tonic::Code::Unknown),
                    ])
                    .observe(duration);
            }

            Ok(response)
        })
    }
}

/// Connection stream ([`TcpIncoming`]) with an attached semaphore and gauge
/// that are used to limit and keep track of the number of connected clients.
pub struct ConnStreamWithTicket {
    stream:                 TcpIncoming,
    /// The semaphore is used to enforce a limit on the number of connected
    /// clients. A ticket must be available before we accept each
    /// connection.
    semaphore:              tokio_util::sync::PollSemaphore,
    /// A gauge to record the number of connected client. This is incremented
    /// when we accept a new connection and decremented on the [`drop`] of
    /// the connection.
    grpc_connected_clients: GenericGauge<AtomicU64>,
}

/// A connection with attached permits and counters to enforce limits on the
/// number of open connections maintained by the grpc server.
pub struct AddrStreamWithTicket {
    /// The connection.
    addr:                  AddrStream,
    /// The permit is attached to the connection so that when the connection is
    /// dropped the permit is also dropped, releasing a connection token.
    /// Thus the permit itself is not used directly by the service, it is only
    /// used for its drop behaviour.
    #[allow(dead_code)]
    permit:                OwnedSemaphorePermit,
    /// The gauge that counts the number of open connections. This is attached
    /// to the connection so that we can decrement it when the connection is
    /// dropped.
    num_connected_clients: GenericGauge<AtomicU64>,
}

impl Drop for AddrStreamWithTicket {
    fn drop(&mut self) { self.num_connected_clients.dec() }
}

/// Forward implementation to that of the inner [`AddrStream`].
impl tokio::io::AsyncRead for AddrStreamWithTicket {
    fn poll_read(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        std::pin::Pin::new(&mut self.addr).poll_read(cx, buf)
    }
}

/// Forward implementation to that of the inner [`AddrStream`].
impl tokio::io::AsyncWrite for AddrStreamWithTicket {
    fn poll_write(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<Result<usize, std::io::Error>> {
        std::pin::Pin::new(&mut self.addr).poll_write(cx, buf)
    }

    fn poll_flush(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        std::pin::Pin::new(&mut self.addr).poll_flush(cx)
    }

    fn poll_shutdown(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        std::pin::Pin::new(&mut self.addr).poll_shutdown(cx)
    }
}

/// Forward implementation to that of the inner [`AddrStream`].
impl Connected for AddrStreamWithTicket {
    type ConnectInfo = <AddrStream as Connected>::ConnectInfo;

    fn connect_info(&self) -> Self::ConnectInfo { self.addr.connect_info() }
}

impl futures::Stream for ConnStreamWithTicket {
    type Item = Result<AddrStreamWithTicket, std::io::Error>;

    // This is where we limit the number of connections.
    // We only return a new item if we can combine it with a permit.
    //
    // The choice made here is that we don't even try to accept new connections
    // unless we have a permit. The connections will thus remain in the accept queue
    // maintained by the OS. An alternative choice would have been to accept a
    // connection, then check if we have a permit, and then drop the connection
    // if we don't. This would be better in some circumstances, but it would
    // also mean that if there is a short burst of short-lived connections they
    // would not be handled, whereas with the current approach they do.
    //
    // The best option would be to have a full backlog handling with expiry, and
    // some rate limiting based on IP (i.e., no more than X connections per IP).
    // However that is a more extensive change. The primary purpose of the current
    // solution is to ensure the node always has enough resources. Not load
    // balancing.
    fn poll_next(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        // Wait until a slot is available.
        let Some(permit) = futures::ready!(self.semaphore.poll_acquire(cx)) else {
            // This should never happen. The Semaphore is owned by `ConnStreamWithTicket`
            // which is in turn owned by the tonic server so will not be dropped while this is being polled.
            // We also never call `close` on the Semaphore, which is private to this struct so there is
            // no chance this is done somewhere else.
            //
            // If by some miracle that were to happen, the outcome is that no new connections are accepted.
            log::error!("Semaphore unexpectedly dropped. Stopping receiving new connections.");
            return std::task::Poll::Ready(None);
        };
        // And then accept a new connection.
        std::pin::Pin::new(&mut self.stream).poll_next(cx).map_ok(|addr| {
            log::debug!("Accepting new GRPC connection from {}", addr.remote_addr());
            self.grpc_connected_clients.inc();
            AddrStreamWithTicket {
                addr,
                permit,
                num_connected_clients: self.grpc_connected_clients.clone(), /* we will decrement
                                                                             * this on a drop
                                                                             * of connection */
            }
        })
    }
}
