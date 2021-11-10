use anyhow::{anyhow, Context};
use byteorder::{NetworkEndian, ReadBytesExt};
use serde::{Deserialize, Serialize};
use std::{
    convert::{TryFrom, TryInto},
    fmt,
    ops::Deref,
    str::FromStr,
};
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

pub const SHA256: u8 = 32;

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct HashBytes([u8; SHA256 as usize]);

impl HashBytes {
    /// Construct HashBytes from a slice.
    /// This will succeed if and only if the length of the slice is 32.
    pub fn new(bytes: &[u8]) -> anyhow::Result<Self> {
        let buf = bytes.try_into()?;
        Ok(HashBytes(buf))
    }
}

impl From<[u8; 32]> for HashBytes {
    fn from(array: [u8; 32]) -> Self { HashBytes(array) }
}

impl Deref for HashBytes {
    type Target = [u8];

    fn deref(&self) -> &Self::Target { &self.0 }
}

impl AsRef<[u8]> for HashBytes {
    fn as_ref(&self) -> &[u8] { &self }
}

impl FromStr for HashBytes {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let hex_decoded = hex::decode(s)?;
        HashBytes::new(&hex_decoded)
    }
}

impl TryFrom<String> for HashBytes {
    type Error = anyhow::Error;

    fn try_from(value: String) -> Result<Self, Self::Error> { Self::from_str(value.as_str()) }
}

impl From<HashBytes> for String {
    fn from(x: HashBytes) -> Self { x.to_string() }
}

// a short, 8-character beginning of the SHA
impl fmt::Debug for HashBytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:08x}", (&self.0[..]).read_u32::<NetworkEndian>().unwrap(),)
    }
}

// the full SHA256 in hex
impl fmt::Display for HashBytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for byte in self.iter() {
            write!(f, "{:02x}", byte)?;
        }
        Ok(())
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
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Error)]
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
        )
    }

    pub fn is_rebroadcastable(self) -> bool {
        use ConsensusFfiResponse::*;

        !matches!(
            self,
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
                | CredentialDeploymentExpired
                | DuplicateAccountRegistrationID
                | CredentialDeploymentInvalidSignatures
                | CredentialDeploymentInvalidIP
                | CredentialDeploymentInvalidAR
        )
    }
}

impl TryFrom<i64> for ConsensusFfiResponse {
    type Error = anyhow::Error;

    #[inline]
    fn try_from(value: i64) -> anyhow::Result<ConsensusFfiResponse> {
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
            _ => Err(anyhow!("Unsupported FFI return code ({})", value)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ConsensusIsInBakingCommitteeResponse {
    NotInCommittee,
    AddedButNotActiveInCommittee,
    AddedButWrongKeys,
    ActiveInCommittee(u64),
}

impl TryFrom<i64> for ConsensusIsInBakingCommitteeResponse {
    type Error = anyhow::Error;

    #[inline]
    fn try_from(value: i64) -> anyhow::Result<ConsensusIsInBakingCommitteeResponse> {
        use ConsensusIsInBakingCommitteeResponse::*;

        match value {
            -1 => Ok(NotInCommittee),
            -2 => Ok(AddedButNotActiveInCommittee),
            -3 => Ok(AddedButWrongKeys),
            baker_id if baker_id >= 0 => Ok(ActiveInCommittee(baker_id as u64)),
            _ => Err(anyhow!("Unsupported FFI return code for committee status ({})", value)),
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
