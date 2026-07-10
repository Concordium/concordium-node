use plt_block_state::entity::block_state::LockNotFoundByIdError;
use plt_block_state::external::AccountNotFoundByAddressError;
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

impl<E: HigherLevelProtocolError + Into<F>, F> From<E> for WithBlockStateFailure<F> {
    fn from(error: E) -> Self {
        Self::Error(error.into())
    }
}

pub type ResultWithBlockStateFailure<T, E> = Result<T, WithBlockStateFailure<E>>;

impl HigherLevelProtocolError for TransactionRejectReason {}
impl HigherLevelProtocolError for LockNotFoundByIdError {}
impl HigherLevelProtocolError for AccountNotFoundByAddressError {}

/// Extension trait for [`ResultWithBlockStateFailure`]
pub trait ResultWithBlockStateFailureExt<T, E> {
    /// Map [`ResultWithBlockStateFailure`] to two nested results, with [`BlockStateFailure`]
    /// as the error type in the outer, and the higher level protocol
    /// error as the error type in the inner.
    fn nest(self) -> BlockStateResult<Result<T, E>>;

    /// Map the inner higher level protocol error in [`ResultWithBlockStateFailure`] using `op`.
    fn map_nested_err<F, O>(self, op: O) -> ResultWithBlockStateFailure<T, F>
    where
        O: FnOnce(E) -> F;
}

impl<T, E> ResultWithBlockStateFailureExt<T, E> for ResultWithBlockStateFailure<T, E> {
    fn nest(self) -> BlockStateResult<Result<T, E>> {
        match self {
            Ok(t) => Ok(Ok(t)),
            Err(WithBlockStateFailure::BlockStateFailure(failure)) => Err(failure),
            Err(WithBlockStateFailure::Error(err)) => Ok(Err(err)),
        }
    }

    fn map_nested_err<F, O>(self, op: O) -> ResultWithBlockStateFailure<T, F>
    where
        O: FnOnce(E) -> F,
    {
        match self {
            Ok(t) => Ok(t),
            Err(WithBlockStateFailure::BlockStateFailure(failure)) => {
                Err(WithBlockStateFailure::BlockStateFailure(failure))
            }
            Err(WithBlockStateFailure::Error(err)) => Err(WithBlockStateFailure::Error(op(err))),
        }
    }
}
