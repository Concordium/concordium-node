//! This module contains the [`BlockState`] which provides an implementation of [`BlockStateOperations`].
//!

use crate::block_state::blob_store::BackingStoreLoad;
use crate::block_state_interface::{RawTokenAmountDelta, UnderOrOverflowError};
use concordium_base::base::AccountIndex;

pub mod blob_store;
#[cfg(feature = "ffi")]
pub mod ffi;

/// Index of the protocol-level token in the block state map of tokens.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TokenIndex(u64);

pub enum PltBlockStateHashMarker {}
pub type PltBlockStateHash = concordium_base::hashes::HashBytes<PltBlockStateHashMarker>;

/// Immutable block state save-point.
///
/// This is a safe wrapper around a [`BlockState`] ensuring further mutations can only be done by
/// unwrapping using [`BlockStateSavepoint::new_generation`] which creates a new generation.
#[derive(Debug)]
pub struct BlockStateSavepoint {
    /// The inner block state, which will not be mutated further for this generation.
    block_state: BlockState,
}

impl BlockStateSavepoint {
    /// Initialize a new block state.
    pub fn empty() -> Self {
        Self {
            block_state: BlockState::empty(),
        }
    }

    /// Compute the hash.
    pub fn hash(&self, _loader: impl blob_store::BackingStoreLoad) -> PltBlockStateHash {
        todo!()
    }

    /// Store a PLT block state in a blob store.
    pub fn store_update(
        &mut self,
        _storer: &mut impl blob_store::BackingStoreStore,
    ) -> blob_store::StoreResult<blob_store::Reference> {
        todo!()
    }

    /// Migrate the PLT block state from one blob store to another.
    pub fn migrate(
        &mut self,
        _loader: &impl blob_store::BackingStoreLoad,
        _storer: &mut impl blob_store::BackingStoreStore,
    ) -> blob_store::LoadStoreResult<Self> {
        todo!()
    }

    /// Cache the block state in memory.
    pub fn cache(&mut self, _loader: &impl blob_store::BackingStoreLoad) {
        todo!()
    }

    /// Construct a new generation block state which can be mutated without affecting this
    /// save-point.
    pub fn new_generation(&self) -> BlockState {
        let mut block_state = self.block_state.clone();
        block_state.generation += 1;
        block_state
    }
}

impl blob_store::Loadable for BlockStateSavepoint {
    fn load<S: std::io::Read, F: blob_store::BackingStoreLoad>(
        _loader: &mut F,
        _source: &mut S,
    ) -> blob_store::LoadResult<Self> {
        todo!()
    }
}

/// Block state providing the various block state operations.
#[derive(Debug, Clone)]
pub struct BlockState {
    /// The generation counter for the block state.
    generation: u64,
}

impl BlockState {
    /// Construct an empty block state.
    fn empty() -> Self {
        BlockState { generation: 0 }
    }

    /// Consume the mutable block state and create an immutable save-point.
    pub fn savepoint(self) -> BlockStateSavepoint {
        BlockStateSavepoint { block_state: self }
    }
}

/// Trait allowing updating the account token balance in the block state.
/// The account token balance block state is currently managed in Haskell.
pub trait UpdateTokenAccountBalanceInBlockState {
    /// Change the account.
    fn update_token_account_balance(
        &mut self,
        token: TokenIndex,
        account: AccountIndex,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), UnderOrOverflowError>;
}

/// Runtime/execution state relevant for providing an implementation of
/// [`BlockStateOperations`].
///
/// This is needed since callbacks are only available during the execution time.
#[derive(Debug)]
pub struct ExecutionTimeBlockState<L: BackingStoreLoad, A: UpdateTokenAccountBalanceInBlockState> {
    /// The library block state implementation.
    pub inner_block_state: BlockState,
    // Temporary disable warning until we have the implementation below started.
    #[expect(dead_code)]
    /// External function for reading from the blob store.
    pub load_callback: L,
    /// External function for updating the token balance for an account.
    pub update_token_account_balance_callback: A,
}

// impl BlockStateOperations for ExecutionTimeBlockState {
//     fn get_plt_list(
//         &self,
//     ) -> impl std::iter::Iterator<Item = concordium_base::protocol_level_tokens::TokenId> {
//         // TODO implement this. The implementation below is just to help the type checker infer
//         // enough for this to compile.
//         Vec::new().into_iter()
//     }
//
//     fn get_token_index(
//         &self,
//         _token_id: concordium_base::protocol_level_tokens::TokenId,
//     ) -> Option<crate::TokenIndex> {
//         todo!()
//     }
//
//     fn get_mutable_token_state(&self, _token_index: crate::TokenIndex) -> crate::MutableTokenState {
//         todo!()
//     }
//
//     fn get_token_configuration(&self, _token_index: crate::TokenIndex) -> crate::PLTConfiguration {
//         todo!()
//     }
//
//     fn get_token_circulating_supply(
//         &self,
//         _token_index: crate::TokenIndex,
//     ) -> plt_token_module::host_interface::TokenRawAmount {
//         todo!()
//     }
//
//     fn set_token_circulating_supply(
//         &mut self,
//         _token_index: crate::TokenIndex,
//         _circulating_supply: plt_token_module::host_interface::TokenRawAmount,
//     ) {
//         todo!()
//     }
//
//     fn create_token(&mut self, _configuration: crate::PLTConfiguration) -> crate::TokenIndex {
//         todo!()
//     }
//
//     fn update_token_account_balance(
//         &mut self,
//         token_index: crate::TokenIndex,
//         account_index: concordium_base::base::AccountIndex,
//         amount_delta: crate::TokenAmountDelta,
//     ) -> Result<(), crate::OverflowError> {
//         let (add_amount, remove_amount) = if amount_delta.is_negative() {
//             let remove_amount =
//                 u64::try_from(amount_delta.abs()).map_err(|_| crate::OverflowError)?;
//             (0, remove_amount)
//         } else {
//             let add_amount = u64::try_from(amount_delta).map_err(|_| crate::OverflowError)?;
//             (add_amount, 0)
//         };
//
//         let overflow = (self.update_token_account_balance_callback)(
//             account_index.into(),
//             token_index.into(),
//             add_amount,
//             remove_amount,
//         );
//         if overflow != 0 {
//             Err(crate::OverflowError)
//         } else {
//             Ok(())
//         }
//     }
//
//     fn touch_token_account(
//         &mut self,
//         _token_index: crate::TokenIndex,
//         _account_index: concordium_base::base::AccountIndex,
//     ) -> bool {
//         todo!()
//     }
//
//     fn increment_plt_update_sequence_number(&mut self) {
//         todo!()
//     }
//
//     fn set_token_state(
//         &mut self,
//         _token_index: crate::TokenIndex,
//         _mutable_token_state: crate::MutableTokenState,
//     ) {
//         todo!()
//     }
// }
