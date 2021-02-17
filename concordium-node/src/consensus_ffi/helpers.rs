use byteorder::{NetworkEndian, ReadBytesExt};
use failure::{format_err, Fallible};

use std::{convert::TryFrom, fmt, ops::Deref, str::FromStr};

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

#[derive(Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "s11n_serde", derive(Serialize, Deserialize))]
pub struct HashBytes([u8; SHA256 as usize]);

impl HashBytes {
    pub fn new(bytes: &[u8]) -> Self {
        let mut buf = [0u8; SHA256 as usize];
        buf.copy_from_slice(bytes);

        HashBytes(buf)
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
    type Err = failure::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        hex::decode(s)
            .map(|x| HashBytes::new(&x))
            .map_err(|e| failure::Error::from_boxed_compat(Box::new(e)))
    }
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
    type Error = failure::Error;

    #[inline]
    fn try_from(value: u8) -> Fallible<PacketType> {
        PACKET_TYPE_FROM_INT
            .get(value as usize)
            .copied()
            .ok_or_else(|| format_err!("Unsupported packet type ({})", value))
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ConsensusFfiResponse {
    BakerNotFound = -1,
    Success,
    DeserializationError,
    InvalidResult,
    PendingBlock,
    PendingFinalization,
    Asynchronous,
    DuplicateEntry,
    Stale,
    IncorrectFinalizationSession,
    Unverifiable,
    ContinueCatchUp,
    BlockTooEarly,
    MissingImportFile,
}

impl ConsensusFfiResponse {
    pub fn is_successful(self) -> bool {
        use ConsensusFfiResponse::*;

        match self {
            Success | PendingBlock | PendingFinalization | Asynchronous => true,
            _ => false,
        }
    }

    pub fn is_pending(self) -> bool {
        use ConsensusFfiResponse::*;

        match self {
            PendingBlock | PendingFinalization => true,
            _ => false,
        }
    }

    pub fn is_acceptable(self) -> bool {
        use ConsensusFfiResponse::*;

        match self {
            BakerNotFound | DeserializationError | InvalidResult | Unverifiable | BlockTooEarly => {
                false
            }
            _ => true,
        }
    }

    pub fn is_rebroadcastable(self) -> bool {
        use ConsensusFfiResponse::*;

        match self {
            DeserializationError
            | InvalidResult
            | Unverifiable
            | DuplicateEntry
            | Stale
            | IncorrectFinalizationSession
            | BlockTooEarly => false,
            _ => true,
        }
    }
}

impl TryFrom<i64> for ConsensusFfiResponse {
    type Error = failure::Error;

    #[inline]
    fn try_from(value: i64) -> Fallible<ConsensusFfiResponse> {
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
            _ => Err(format_err!("Unsupported FFI return code ({})", value)),
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
    type Error = failure::Error;

    #[inline]
    fn try_from(value: i64) -> Fallible<ConsensusIsInBakingCommitteeResponse> {
        use ConsensusIsInBakingCommitteeResponse::*;

        match value {
            -1 => Ok(NotInCommittee),
            -2 => Ok(AddedButNotActiveInCommittee),
            -3 => Ok(AddedButWrongKeys),
            baker_id if baker_id >= 0 => Ok(ActiveInCommittee(baker_id as u64)),
            _ => Err(format_err!("Unsupported FFI return code for committee status ({})", value)),
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
    type Error = failure::Error;

    #[inline]
    fn try_from(value: u8) -> Fallible<ConsensusIsInFinalizationCommitteeResponse> {
        use ConsensusIsInFinalizationCommitteeResponse::*;

        match value {
            0 => Ok(NotInCommittee),
            1 => Ok(AddedButNotActiveInCommittee),
            2 => Ok(ActiveInCommittee),
            _ => Err(format_err!("Unsupported FFI return code for committee status ({})", value)),
        }
    }
}
