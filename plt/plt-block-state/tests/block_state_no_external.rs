// Allow items in this file to be unused. This is needed because it is imported from multiple
// compile targets (each of the integration tests), and some of the targets may not use all
// items in the file.
#![allow(unused)]

use assert_matches::assert_matches;
use concordium_base::base::{AccountIndex, Energy};
use concordium_base::common::cbor;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, MetadataUrl, RawCbor, TokenAmount, TokenId,
    TokenModuleInitializationParameters, TokenModuleState, TokenOperation, TokenOperationsPayload,
    TokenSupplyUpdateDetails, TokenTransfer,
};
use concordium_base::transactions::Payload;
use concordium_base::updates::{CreatePlt, UpdatePayload};
use plt_block_state::block_state::blob_store::{BackingStoreLoad, Reference};
use plt_block_state::block_state::external::{
    ExternalBlockStateOperations, ExternalBlockStateQuery,
};
use plt_block_state::block_state::types::{TokenAccountState, TokenConfiguration, TokenIndex};
use plt_block_state::block_state::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, ExecutionTimePltBlockState,
    PltBlockState, PltBlockStateSavepoint,
};
use plt_block_state::block_state_interface::{
    BlockStateOperations, BlockStateQuery, OverflowError, RawTokenAmountDelta,
    TokenNotFoundByIdError,
};
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use std::collections::BTreeMap;

/// Block store load stub for tests.
#[derive(Debug)]
pub struct BlobStoreLoadStub;

impl BackingStoreLoad for BlobStoreLoadStub {
    fn load_raw(&mut self, location: Reference) -> Vec<u8> {
        unimplemented!("should not be called")
    }
}

type ExecutionTimePltBlockStateWithNoExternalState =
    ExecutionTimePltBlockState<PltBlockState, BlobStoreLoadStub, NoExternalBlockStateStub>;
type Token = <ExecutionTimePltBlockStateWithNoExternalState as BlockStateQuery>::Token;

/// Block state where external interactions with the Haskell maintained block
/// state is not possible.
#[derive(Debug)]
pub struct BlockStateWithNoExternalState {
    block_state: ExecutionTimePltBlockStateWithNoExternalState,
}

/// Non-accessible block state representing the Haskell maintained part of the block state.
#[derive(Debug)]
pub struct NoExternalBlockStateStub;

impl BlockStateWithNoExternalState {
    /// Create block state stub
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let inner_block_state = PltBlockStateSavepoint::empty().mutable_state();

        let block_state = ExecutionTimePltBlockState {
            internal_block_state: inner_block_state,
            backing_store_load: BlobStoreLoadStub,
            external_block_state: NoExternalBlockStateStub,
        };

        Self { block_state }
    }

    /// Access to the underlying block state.
    pub fn state(&self) -> &ExecutionTimePltBlockStateWithNoExternalState {
        &self.block_state
    }

    /// Mutable access to the underlying block state.
    pub fn state_mut(&mut self) -> &mut ExecutionTimePltBlockStateWithNoExternalState {
        &mut self.block_state
    }
}

impl ExternalBlockStateQuery for NoExternalBlockStateStub {
    fn read_token_account_balance(
        &self,
        account: AccountIndex,
        token: TokenIndex,
    ) -> RawTokenAmount {
        unreachable!()
    }

    fn account_canonical_address_by_account_index(
        &self,
        account_index: AccountIndex,
    ) -> Result<AccountAddress, AccountNotFoundByIndexError> {
        unreachable!()
    }

    fn account_index_by_account_address(
        &self,
        account_address: &AccountAddress,
    ) -> Result<AccountIndex, AccountNotFoundByAddressError> {
        unreachable!()
    }

    fn token_account_states(
        &self,
        account_index: AccountIndex,
    ) -> Vec<(TokenIndex, TokenAccountState)> {
        unreachable!()
    }
}

impl ExternalBlockStateOperations for NoExternalBlockStateStub {
    fn update_token_account_balance(
        &mut self,
        account: AccountIndex,
        token: TokenIndex,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError> {
        unreachable!()
    }

    fn increment_plt_update_sequence_number(&mut self) {
        unreachable!()
    }
}
