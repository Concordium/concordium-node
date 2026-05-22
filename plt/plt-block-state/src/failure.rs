/// Unrecoverable failure accessing the block state. This is generally an error that
/// should never happen and is unrecoverable.
///
/// If returned when **applying a block item to the block state**,
/// it may leave the block state in an indeterminate state. E.g. can parts of the effects
/// of processing the block item be applied, an others not. Hence, the resulting block
/// state should not be used.
///
/// If returned when **querying the block state**, the query itself fails,
/// but the block state is still in a valid state.
#[derive(Debug, thiserror::Error)]
pub enum BlockStateFailure {
    /// An error happened when decoding a block state value from the blob store.
    #[error("Error decoding state from blob store: {0}")]
    BlobStoreDecode(String),
    /// An invariant that must be true is broken. The invariant can either be in the
    /// stored block state or a runtime logical invariant related to the in-memory block state.
    #[error("State invariant broken: {0}")]
    Invariant(String),
    /// When looking up a value with in an owned
    /// [blob reference](super::block_state::blob_reference::hashed_cacheable_reference::HashedCacheableRef),
    /// a borrowed value was returned. This should generally never happen in they way we maintain
    /// blob references.
    #[error("Borrowed value found inside of owned value: {0}")]
    CowJoin(&'static str),
}

pub type BlockStateResult<T> = Result<T, BlockStateFailure>;