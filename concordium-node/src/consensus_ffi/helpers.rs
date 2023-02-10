use anyhow::{anyhow, Context};
use std::{convert::TryFrom, fmt};
use thiserror::Error;

/// # Serialization packets
/// Benchmark of each serialization requires to enable it on features
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const APPNAME: &str = env!("CARGO_PKG_NAME");

/// Represents a Message to be sent through a channel or a Stop signal
///
/// This type is intended for using when a loop is consuming the receiver
/// output and we want to gracefully stop such loop.
pub enum QueueMsg<T> {
    Relay(T),
    Stop,
}

/// Represents a `Sender<T>` that is promoted to use `QueueMsg`s
pub type QueueSyncSender<T> = crossbeam_channel::Sender<QueueMsg<T>>;

/// Represents a `Receiver<T>` that is promoted to use `QueueMsg`s
pub type QueueReceiver<T> = crossbeam_channel::Receiver<QueueMsg<T>>;

/// Helper trait to ease readability through the code when dealing with
/// `RelayOrStop` channels
pub trait RelayOrStopSenderHelper<T> {
    /// Sends a `QueueMsg::Stop` message through the channel
    fn send_stop(&self) -> Result<(), crossbeam_channel::SendError<QueueMsg<T>>>;
    /// Sends the provided `msg` wrapped inside a `QueueMsg::Relay`
    fn send_msg(&self, msg: T) -> Result<(), crossbeam_channel::TrySendError<QueueMsg<T>>>;
    /// Sends the provided `msg` wrapped inside a `QueueMsg::Relay` in a
    /// blocking fashion
    fn send_blocking_msg(&self, msg: T) -> Result<(), crossbeam_channel::SendError<QueueMsg<T>>>;
}

impl<T> RelayOrStopSenderHelper<T> for QueueSyncSender<T> {
    #[inline]
    fn send_stop(&self) -> Result<(), crossbeam_channel::SendError<QueueMsg<T>>> {
        self.send(QueueMsg::Stop)
    }

    #[inline]
    fn send_msg(&self, msg: T) -> Result<(), crossbeam_channel::TrySendError<QueueMsg<T>>> {
        self.try_send(QueueMsg::Relay(msg))
    }

    #[inline]
    fn send_blocking_msg(&self, msg: T) -> Result<(), crossbeam_channel::SendError<QueueMsg<T>>> {
        self.send(QueueMsg::Relay(msg))
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum PacketType {
    Block = 0,
    Transaction,
    FinalizationRecord,
    FinalizationMessage,
    CatchUpStatus,
}

static PACKET_TYPE_FROM_INT: &[PacketType] = &[
    PacketType::Block,
    PacketType::Transaction,
    PacketType::FinalizationRecord,
    PacketType::FinalizationMessage,
    PacketType::CatchUpStatus,
];

impl TryFrom<u8> for PacketType {
    type Error = anyhow::Error;

    #[inline]
    fn try_from(value: u8) -> anyhow::Result<PacketType> {
        PACKET_TYPE_FROM_INT
            .get(value as usize)
            .copied()
            .context(format! {"Unsupported packet type ({})", value})
    }
}

impl fmt::Display for PacketType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            PacketType::Block => "block",
            PacketType::Transaction => "transaction",
            PacketType::FinalizationRecord => "finalization record",
            PacketType::FinalizationMessage => "finalization message",
            PacketType::CatchUpStatus => "catch-up status message",
        };

        write!(f, "{}", name)
    }
}

impl PacketType {
    /// Determine whether a packet type can be relayed.
    /// Those that are must be subject to appropriate de-duplication
    /// checks to ensure they are not relayed endlessly.
    pub fn is_rebroadcastable(&self) -> bool {
        match self {
            PacketType::Block => true,
            PacketType::Transaction => true,
            PacketType::FinalizationRecord => true,
            PacketType::FinalizationMessage => true,
            PacketType::CatchUpStatus => false,
        }
    }

    /// Get the label. This is used when updating metrics of the prometheus
    /// exporter.
    pub fn as_label(&self) -> &str {
        match self {
            PacketType::Block => "block",
            PacketType::Transaction => "transaction",
            PacketType::FinalizationRecord => "finalization record",
            PacketType::FinalizationMessage => "finalization message",
            PacketType::CatchUpStatus => "catch-up status message",
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Error)]
#[must_use]
pub enum ConsensusFfiResponse {
    #[error("Baker not found")]
    BakerNotFound = -1,
    #[error("Success")]
    Success,
    #[error("Deserialization error")]
    DeserializationError,
    #[error("Invalid result")]
    InvalidResult,
    #[error("Pending block")]
    PendingBlock,
    #[error("Pending finalization")]
    PendingFinalization,
    #[error("Asynchronous")]
    Asynchronous,
    #[error("Duplicate entry")]
    DuplicateEntry,
    #[error("Stale")]
    Stale,
    #[error("Incorrect finalization session")]
    IncorrectFinalizationSession,
    #[error("Unverifiable")]
    Unverifiable,
    #[error("Continue catchup")]
    ContinueCatchUp,
    #[error("Block too early")]
    BlockTooEarly,
    #[error("Missing import file")]
    MissingImportFile,
    #[error("Consensus shutdown")]
    ConsensusShutDown,
    #[error("Expiry too late")]
    ExpiryTooLate,
    #[error("Verification failed")]
    VerificationFailed,
    #[error("Nonexisting sender account")]
    NonexistingSenderAccount,
    #[error("Duplicate nonce")]
    DuplicateNonce,
    #[error("Nonce too large")]
    NonceTooLarge,
    #[error("Too low energy")]
    TooLowEnergy,
    #[error("Invalid genesis index")]
    InvalidGenesisIndex,
    #[error("An account already exists with the given registration id")]
    DuplicateAccountRegistrationID,
    #[error("The credential deployment contained invalid signatures")]
    CredentialDeploymentInvalidSignatures,
    #[error("The credential deployment contained an invalid identity provider")]
    CredentialDeploymentInvalidIP,
    #[error("The credential deployment contained invalid anonymity revokers")]
    CredentialDeploymentInvalidAR,
    #[error("The credential deployment was expired")]
    CredentialDeploymentExpired,
    #[error("The chain update had an invalid effective time")]
    ChainUpdateInvalidEffectiveTime,
    #[error("The chain update had an old nonce")]
    ChainUpdateSequenceNumberTooOld,
    #[error("The chain update contained invalid signatures")]
    ChainUpdateInvalidSignatures,
    #[error("The stated energy of the transaction exceeds the maximum allowed")]
    MaxBlockEnergyExceeded,
    #[error("The sender did not have enough funds to cover the costs")]
    InsufficientFunds,
}

impl ConsensusFfiResponse {
    pub fn is_successful(self) -> bool {
        use ConsensusFfiResponse::*;

        matches!(self, Success | PendingBlock | PendingFinalization | Asynchronous)
    }

    pub fn is_pending(self) -> bool {
        use ConsensusFfiResponse::*;

        matches!(self, PendingBlock | PendingFinalization | InvalidGenesisIndex)
    }

    pub fn is_acceptable(self) -> bool {
        use ConsensusFfiResponse::*;

        !matches!(
            self,
            BakerNotFound
                | DeserializationError
                | InvalidResult
                | Unverifiable
                | BlockTooEarly
                | ExpiryTooLate
                | VerificationFailed
                | NonexistingSenderAccount
                | DuplicateNonce
                | NonceTooLarge
                | TooLowEnergy
                | ConsensusShutDown
                | InvalidGenesisIndex
                | InsufficientFunds
        )
    }

    pub fn is_rebroadcastable(self, packet_type: PacketType) -> bool {
        use ConsensusFfiResponse::*;

        match self {
            DeserializationError
            | InvalidResult
            | Unverifiable
            | DuplicateEntry
            | Stale
            | IncorrectFinalizationSession
            | BlockTooEarly
            | ExpiryTooLate
            | VerificationFailed
            | NonexistingSenderAccount
            | DuplicateNonce
            | NonceTooLarge
            | TooLowEnergy
            | ConsensusShutDown
            | InvalidGenesisIndex
            | DuplicateAccountRegistrationID
            | CredentialDeploymentInvalidSignatures
            | CredentialDeploymentInvalidIP
            | CredentialDeploymentInvalidAR
            | CredentialDeploymentExpired
            | ChainUpdateInvalidEffectiveTime
            | ChainUpdateSequenceNumberTooOld
            | ChainUpdateInvalidSignatures
            | MaxBlockEnergyExceeded
            | InsufficientFunds
            | BakerNotFound
            | MissingImportFile
            | ContinueCatchUp => false,
            PendingBlock => packet_type != PacketType::Block,
            Success | PendingFinalization | Asynchronous => true,
        }
    }

    /// Get the label. This is used when updating metrics of the prometheus
    /// exporter.
    pub fn as_label(&self) -> &str {
        match self {
            ConsensusFfiResponse::Success => "valid",
            ConsensusFfiResponse::DuplicateEntry => "duplicate",
            _ => "invalid",
        }
    }
}

#[derive(Debug, Error)]
#[error("Unsupported FFI return code ({unknown_code})")]
pub struct ConsensusFfiResponseConversionError {
    unknown_code: i64,
}

impl From<ConsensusFfiResponseConversionError> for tonic::Status {
    fn from(code: ConsensusFfiResponseConversionError) -> Self {
        Self::internal(format!("Unexpected response from FFI call: {}.", code.unknown_code))
    }
}

impl TryFrom<i64> for ConsensusFfiResponse {
    type Error = ConsensusFfiResponseConversionError;

    #[inline]
    fn try_from(value: i64) -> Result<ConsensusFfiResponse, ConsensusFfiResponseConversionError> {
        use ConsensusFfiResponse::*;

        match value {
            -1 => Ok(BakerNotFound),
            0 => Ok(Success),
            1 => Ok(DeserializationError),
            2 => Ok(InvalidResult),
            3 => Ok(PendingBlock),
            4 => Ok(PendingFinalization),
            5 => Ok(Asynchronous),
            6 => Ok(DuplicateEntry),
            7 => Ok(Stale),
            8 => Ok(IncorrectFinalizationSession),
            9 => Ok(Unverifiable),
            10 => Ok(ContinueCatchUp),
            11 => Ok(BlockTooEarly),
            12 => Ok(MissingImportFile),
            13 => Ok(ConsensusShutDown),
            14 => Ok(ExpiryTooLate),
            15 => Ok(VerificationFailed),
            16 => Ok(NonexistingSenderAccount),
            17 => Ok(DuplicateNonce),
            18 => Ok(NonceTooLarge),
            19 => Ok(TooLowEnergy),
            20 => Ok(InvalidGenesisIndex),
            21 => Ok(DuplicateAccountRegistrationID),
            22 => Ok(CredentialDeploymentInvalidSignatures),
            23 => Ok(CredentialDeploymentInvalidIP),
            24 => Ok(CredentialDeploymentInvalidAR),
            25 => Ok(CredentialDeploymentExpired),
            26 => Ok(ChainUpdateInvalidEffectiveTime),
            27 => Ok(ChainUpdateSequenceNumberTooOld),
            28 => Ok(ChainUpdateInvalidSignatures),
            29 => Ok(MaxBlockEnergyExceeded),
            30 => Ok(InsufficientFunds),
            _ => Err(ConsensusFfiResponseConversionError {
                unknown_code: value,
            }),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ConsensusIsInBakingCommitteeResponse {
    ActiveInCommittee,
    NotInCommittee,
    AddedButNotActiveInCommittee,
    AddedButWrongKeys,
}

impl TryFrom<u8> for ConsensusIsInBakingCommitteeResponse {
    type Error = anyhow::Error;

    #[inline]
    fn try_from(value: u8) -> anyhow::Result<ConsensusIsInBakingCommitteeResponse> {
        use ConsensusIsInBakingCommitteeResponse::*;

        match value {
            0 => Ok(ActiveInCommittee),
            1 => Ok(NotInCommittee),
            2 => Ok(AddedButNotActiveInCommittee),
            3 => Ok(AddedButWrongKeys),
            _ => Err(anyhow!("Unsupported FFI return code ({}) for committee status", value)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ConsensusIsInFinalizationCommitteeResponse {
    NotInCommittee = 0,
    AddedButNotActiveInCommittee,
    ActiveInCommittee,
}

impl TryFrom<u8> for ConsensusIsInFinalizationCommitteeResponse {
    type Error = anyhow::Error;

    #[inline]
    fn try_from(value: u8) -> anyhow::Result<ConsensusIsInFinalizationCommitteeResponse> {
        use ConsensusIsInFinalizationCommitteeResponse::*;

        match value {
            0 => Ok(NotInCommittee),
            1 => Ok(AddedButNotActiveInCommittee),
            2 => Ok(ActiveInCommittee),
            _ => Err(anyhow!("Unsupported FFI return code for committee status ({})", value)),
        }
    }
}

/// Response to a consensus GRPC V2 query.
/// This is a response that is already parsed from the error code return through
/// FFI.
pub enum ConsensusQueryResponse {
    InvalidArgument,
    InternalError,
    Ok,
    NotFound,
}

impl ConsensusQueryResponse {
    /// Convert the response to a [Result]. The concrete type makes it
    /// convenient to use in the implementations of the different queries.
    pub fn ensure_ok(self, msg: impl std::fmt::Display) -> Result<(), tonic::Status> {
        match self {
            Self::InvalidArgument => Err(tonic::Status::invalid_argument("Invalid argument.")),
            Self::InternalError => Err(tonic::Status::internal(format!("Internal error: {}. Please report this bug at https://github.com/Concordium/concordium-node/issues.", msg))),
            Self::Ok => Ok(()),
            Self::NotFound => Err(tonic::Status::not_found(format!("{} not found.", msg))),
        }
    }
}

#[derive(Debug, Error)]
#[error("Unsupported query response return code ({unknown_code})")]
pub struct ConsensusQueryUnknownCode {
    unknown_code: i64,
}

impl From<ConsensusQueryUnknownCode> for tonic::Status {
    fn from(code: ConsensusQueryUnknownCode) -> Self {
        Self::internal(format!("Unexpected response from internal query: {}.", code.unknown_code))
    }
}

impl TryFrom<i64> for ConsensusQueryResponse {
    type Error = ConsensusQueryUnknownCode;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        match value {
            -2 => Ok(Self::InvalidArgument),
            -1 => Ok(Self::InternalError),
            0 => Ok(Self::Ok),
            1 => Ok(Self::NotFound),
            unknown_code => Err(ConsensusQueryUnknownCode {
                unknown_code,
            }),
        }
    }
}

pub enum ContractStateResponse {
    V0 {
        state: Vec<u8>,
    },
    V1 {
        state:  concordium_smart_contract_engine::v1::trie::PersistentState,
        loader: concordium_smart_contract_engine::v1::trie::LoadCallback,
    },
}
