use plt_block_state::entity::block_state::LockNotFoundByIdError;
use plt_block_state::failure::{BlockStateFailure, BlockStateResult};
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;

/// [`BlockStateFailure`] and [`T`] flattened into one error
/// for convenience.
#[derive(Debug, thiserror::Error)]
pub enum WithBlockStateFailure<T> {
    /// Higher protocol level error
    #[error("{0}")]
    Error(T),
    /// An unrecoverable error occurred in block state when executing the transaction.
    #[error("Block state failure: {0}")]
    BlockStateFailure(#[from] BlockStateFailure),
}

/// Marker trait that allows an error to be used in [`WithBlockStateFailure`] (acts as a
/// "negative" bound in the `From<T>` implementation to avoid conflict with `From<BlockStateFailure>`).
pub trait HigherLevelProtocolError {}

impl<T: HigherLevelProtocolError> From<T> for WithBlockStateFailure<T> {
    fn from(error: T) -> Self {
        Self::Error(error)
    }
}

pub type WithBlockStateResult<T, E> = Result<T, WithBlockStateFailure<E>>;

/// Create two nested results, with [`BlockStateFailure`] in the outer, and the higher level protocol
/// error in the inner.
pub fn nest<E, T>(result: WithBlockStateResult<T, E>) -> BlockStateResult<Result<T, E>> {
    match result {
        Ok(val) => Ok(Ok(val)),
        Err(WithBlockStateFailure::Error(err)) => Ok(Err(err)),
        Err(WithBlockStateFailure::BlockStateFailure(err)) => Err(err),
    }
}

impl HigherLevelProtocolError for TransactionRejectReason {}
impl HigherLevelProtocolError for LockNotFoundByIdError {}
