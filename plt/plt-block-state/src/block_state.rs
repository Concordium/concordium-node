//! This module contains the [`BlockState`] which provides an implementation of [`BlockStateOperations`].

use crate::block_state::blob_store::{
    BlobStoreLoad, BlobStoreLocation, BlobStoreStore, Loadable, Storable,
};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::external::{ExternalBlockStateOperations, ExternalBlockStateQuery};
use crate::block_state::hash::Hashable;
use crate::block_state::persistent::protocol_level_tokens::PersistentPlTokens;
use crate::block_state::entity::AccountWithCanonicalAddress;
use crate::block_state::entity::protocol_level_tokens::{
    TokenAccountState, TokenConfiguration, TokenIndex, TokenStateKey, TokenStateValue,
};
use crate::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, BlockStateFailure,
    BlockStateOperations, BlockStateQuery, BlockStateResult, OverflowError, RawTokenAmountDelta,
    TokenNotFoundByIdError,
};
use concordium_base::base::{AccountIndex, ProtocolVersion};
use concordium_base::common::Buffer;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::hashes::Hash;
use concordium_base::protocol_level_tokens::TokenId;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use std::io::Read;
use std::{any, mem};
use crate::entity::protocol_level_tokens::PlTokenEntity;

pub mod blob_reference;
pub mod blob_store;
pub mod cacheable;
pub mod external;
pub mod hash;
pub mod lfmb_tree;
pub mod smart_contract_trie;
pub mod utils;



/// Runtime/execution state relevant for providing an implementation of
/// [`BlockStateQuery`] and [`BlockStateOperations`].
///
/// In addition to the PLT block state, this type contains callbacks
/// for the parts of the state that is managed on the Haskell side.
#[derive(Debug)]
pub struct ExecutionTimeBlockState<IntState, Load, ExtState> {
    /// The library block state implementation.
    pub internal_block_state: IntState,
    /// External function for reading from the blob store.
    pub blob_store_load: Load,
    /// Part of block state that is managed externally.
    pub external_block_state: ExtState,
}


impl<IntState: HasBlockState, Load: BlobStoreLoad, ExtState: ExternalBlockStateQuery>
    BlockStateQuery for ExecutionTimeBlockState<IntState, Load, ExtState>
{
    type MutableTokenKeyValueState = smart_contract_trie::MutableState;
    type Account = AccountIndex;
    type Token = PlTokenEntity<>;

    fn plt_list(&self) -> impl ExactSizeIterator<Item = TokenId> {
        todo!()
    }

    fn token_by_id(&self, token_id: &TokenId) -> Result<Self::Token, TokenNotFoundByIdError> {
        todo!()
    }

    fn mutable_token_key_value_state(
        &self,
        token: &Self::Token,
    ) -> Self::MutableTokenKeyValueState {
        // todo propagate block state error as part of https://linear.app/concordium/issue/COR-2346/push-blockstateerror-to-scheduler-code
        self.internal_block_state
            .block_state()
            .tokens
            .mutable_token_key_value_state(&self.blob_store_load, *token)
            .unwrap()
    }

    fn token_configuration(&self, token: &Self::Token) -> TokenConfiguration {
        // todo propagate block state error as part of https://linear.app/concordium/issue/COR-2346/push-blockstateerror-to-scheduler-code
        self.internal_block_state
            .block_state()
            .tokens
            .token_configuration(&self.blob_store_load, *token)
            .unwrap()
    }

    fn token_circulating_supply(&self, token: &Self::Token) -> RawTokenAmount {
        // todo propagate block state error as part of https://linear.app/concordium/issue/COR-2346/push-blockstateerror-to-scheduler-code
        self.internal_block_state
            .block_state()
            .tokens
            .token_circulating_supply(&self.blob_store_load, *token)
            .unwrap()
    }

    fn lookup_token_state_value(
        &self,
        token_key_value: &Self::MutableTokenKeyValueState,
        key: &TokenStateKey,
    ) -> Option<TokenStateValue> {
        token_key_value
            .lookup_value(&self.blob_store_load, &key.0)
            .map(TokenStateValue)
    }

    fn iter_token_state_prefix<'a>(
        &'a self,
        token_key_value: &Self::MutableTokenKeyValueState,
        prefix: &TokenStateKey,
    ) -> impl Iterator<Item = (TokenStateKey, TokenStateValue)> + use<'a, IntState, Load, ExtState>
    {
        // todo propagate block state error as part of https://linear.app/concordium/issue/COR-2346/push-blockstateerror-to-scheduler-code
        token_key_value
            .iter_prefix(&self.blob_store_load, &prefix.0)
            .unwrap()
            .map(|entry| (TokenStateKey(entry.0), TokenStateValue(entry.1)))
    }

    fn update_token_state_value(
        &self,
        token_key_value_state: &mut Self::MutableTokenKeyValueState,
        key: &TokenStateKey,
        value: Option<TokenStateValue>,
    ) {
        // todo propagate block state error as part of https://linear.app/concordium/issue/COR-2346/push-blockstateerror-to-scheduler-code
        if let Some(value) = value {
            token_key_value_state
                .insert_value(&self.blob_store_load, &key.0, value.0)
                .unwrap();
        } else {
            token_key_value_state
                .delete_value(&self.blob_store_load, &key.0)
                .unwrap();
        }
    }

    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Self::Account, AccountNotFoundByAddressError> {
        let index = self
            .external_block_state
            .account_index_by_account_address(address)?;

        Ok(index)
    }

    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress<Self::Account>, AccountNotFoundByIndexError> {
        let canonical_account_address = self
            .external_block_state
            .account_canonical_address_by_account_index(index)?;

        Ok(AccountWithCanonicalAddress {
            account: index,
            canonical_account_address,
        })
    }

    fn account_index(&self, account: &Self::Account) -> AccountIndex {
        *account
    }

    fn account_token_balance(
        &self,
        account: &Self::Account,
        token: &Self::Token,
    ) -> RawTokenAmount {
        self.external_block_state
            .read_token_account_balance(*account, *token)
    }

    fn token_account_states(
        &self,
        account: &Self::Account,
    ) -> impl Iterator<Item = (Self::Token, TokenAccountState)> {
        self.external_block_state
            .token_account_states(*account)
            .into_iter()
    }

    fn protocol_version(&self) -> ProtocolVersion {
        self.internal_block_state.protocol_version()
    }
}

impl<Load: BlobStoreLoad, ExtState: ExternalBlockStateOperations> BlockStateOperations
    for ExecutionTimeBlockState<MutableBlockState, Load, ExtState>
{
    fn set_token_circulating_supply(
        &mut self,
        token: &Self::Token,
        circulating_supply: RawTokenAmount,
    ) {
        // todo propagate block state error as part of https://linear.app/concordium/issue/COR-2346/push-blockstateerror-to-scheduler-code
        self.internal_block_state
            .update_block_state_(|state| {
                Ok(BlockStateData {
                    tokens: state.tokens.set_token_circulating_supply(
                        &self.blob_store_load,
                        *token,
                        circulating_supply,
                    )?,
                })
            })
            .unwrap();
    }

    fn create_token(&mut self, configuration: TokenConfiguration) -> Self::Token {
        // todo propagate block state error as part of https://linear.app/concordium/issue/COR-2346/push-blockstateerror-to-scheduler-code
        self.internal_block_state
            .update_block_state(|state| {
                let (token_index, tokens) = state
                    .tokens
                    .create_token(&self.blob_store_load, configuration)?;
                Ok((token_index, BlockStateData { tokens }))
            })
            .unwrap()
    }

    fn update_token_account_balance(
        &mut self,
        token: &Self::Token,
        account: &Self::Account,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError> {
        self.external_block_state
            .update_token_account_balance(*account, *token, amount_delta)
    }

    fn touch_token_account(&mut self, token: &Self::Token, account: &Self::Account) {
        self.external_block_state
            .touch_token_account(*account, *token);
    }

    fn increment_plt_update_instruction_sequence_number(&mut self) {
        self.external_block_state
            .increment_plt_update_sequence_number();
    }

    fn set_token_key_value_state(
        &mut self,
        token: &Self::Token,
        token_key_value_state: Self::MutableTokenKeyValueState,
    ) {
        // todo propagate block state error as part of https://linear.app/concordium/issue/COR-2346/push-blockstateerror-to-scheduler-code
        self.internal_block_state
            .update_block_state_(|state| {
                Ok(BlockStateData {
                    tokens: state.tokens.set_token_key_value_state(
                        &self.blob_store_load,
                        *token,
                        token_key_value_state,
                    )?,
                })
            })
            .unwrap();
    }
}
