//! [Execution time block state](ExecutionTimeBlockState) where all interactions with
//! externally (Haskell) managed block stated will result in panicking.

// Allow items in this file to be unused. This is needed because it is imported from multiple
// compile targets (each of the integration tests), and some of the targets may not use all
// items in the file.
#![allow(unused)]

use assert_matches::assert_matches;
use concordium_base::base::{AccountIndex, Energy, ProtocolVersion};
use concordium_base::common::cbor;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, MetadataUrl, RawCbor, TokenAmount, TokenId,
    TokenModuleInitializationParameters, TokenModuleState, TokenOperation, TokenOperationsPayload,
    TokenSupplyUpdateDetails, TokenTransfer,
};
use concordium_base::transactions::Payload;
use concordium_base::updates::{CreatePlt, UpdatePayload};
use plt_block_state::block_state::blob_store::test_stub::BlobStoreStub;
use plt_block_state::block_state::blob_store::{BlobStoreLoad, BlobStoreLocation};
use plt_block_state::block_state::external::{
    ExternalBlockStateOperations, ExternalBlockStateQuery,
};
use plt_block_state::block_state::types::protocol_level_tokens::{TokenAccountState, TokenIndex};
use plt_block_state::block_state::{
    BlockState, BlockStateData, ExecutionTimeBlockState, MutableBlockState,
};
use plt_block_state::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, BlockStateOperations,
    BlockStateQuery, OverflowError, RawTokenAmountDelta, TokenNotFoundByIdError,
};
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use std::collections::BTreeMap;

/// Non-accessible block state representing the Haskell maintained part of the block state.
#[derive(Debug)]
pub struct NoExternalBlockStateStub;

/// Create fresh mutable execution time block state.
pub fn new_mutable_block_state(
    protocol_version: ProtocolVersion,
) -> ExecutionTimeBlockState<MutableBlockState, BlobStoreStub, NoExternalBlockStateStub> {
    let inner_block_state = BlockState::empty(protocol_version).into_mutable();
    let blob_store = BlobStoreStub::default();

    ExecutionTimeBlockState {
        internal_block_state: inner_block_state,
        blob_store_load: blob_store,
        external_block_state: NoExternalBlockStateStub,
    }
}

/// Create execution time block state from immutable block state and blob store.
pub fn with_block_state(
    blob_store: BlobStoreStub,
    block_state: BlockState,
) -> ExecutionTimeBlockState<BlockState, BlobStoreStub, NoExternalBlockStateStub> {
    ExecutionTimeBlockState {
        internal_block_state: block_state,
        blob_store_load: blob_store,
        external_block_state: NoExternalBlockStateStub,
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

    fn touch_token_account(&mut self, account: AccountIndex, token: TokenIndex) {
        unreachable!()
    }

    fn increment_plt_update_sequence_number(&mut self) {
        unreachable!()
    }
}
