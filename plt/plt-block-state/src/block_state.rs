//! This module contains temporary implementations of [`BlockStateOperations`]. The trait
//! [`BlockStateOperations`] and [`BlockStateQuery`] will be removed.

use crate::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, BlockStateOperations,
    BlockStateQuery, OverflowError, RawTokenAmountDelta, TokenNotFoundByIdError, TokenStateKey,
    TokenStateValue,
};
use crate::entity::accounts::{Account, AccountWithCanonicalAddress};
use crate::entity::block_state::Accounts;
use crate::entity::block_state::p9::BlockStateP9;
use crate::entity::block_state::p11::BlockStateP11;
use crate::entity::{EntityContext, EntityContextTypes};
use crate::external::TokenAccountState;
use crate::persistent::smart_contract_trie;
use concordium_base::base::{AccountIndex, ProtocolVersion};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::TokenId;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use crate::persistent::protocol_level_tokens::p9::{TokenConfiguration, TokenIndex};

/// Runtime/execution state relevant for providing an implementation of
/// [`BlockStateQuery`] and [`BlockStateOperations`].
#[derive(Debug)]
pub struct ExecutionTimeBlockStateP9<C: EntityContextTypes> {
    /// The library block state implementation.
    pub block_state: BlockStateP9,
    /// External function for reading from the blob store.
    pub context: EntityContext<C>,
}

impl<C: EntityContextTypes> BlockStateQuery for ExecutionTimeBlockStateP9<C> {
    type MutableTokenKeyValueState = smart_contract_trie::MutableState;
    type Account = Account;
    type Token = TokenIndex;

    fn plt_list(&self) -> impl ExactSizeIterator<Item = TokenId> {
        self.block_state
            .plt_list(&self.context)
            .map(|token| token.unwrap())
    }

    fn token_by_id(&self, token_id: &TokenId) -> Result<Self::Token, TokenNotFoundByIdError> {
        self.block_state
            .token_by_id(&self.context, token_id)
            .unwrap()
            .map(|token| token.token_index)
    }

    fn mutable_token_key_value_state(
        &self,
        token: &Self::Token,
    ) -> Self::MutableTokenKeyValueState {
        let token = self
            .block_state
            .token_by_index(&self.context, *token)
            .unwrap();
        token.mutable_key_value_state
    }

    fn token_configuration(&self, token: &Self::Token) -> TokenConfiguration {
        let token = self
            .block_state
            .token_by_index(&self.context, *token)
            .unwrap();
        token.token_configuration(&self.context).unwrap()
    }

    fn token_circulating_supply(&self, token: &Self::Token) -> RawTokenAmount {
        let token = self
            .block_state
            .token_by_index(&self.context, *token)
            .unwrap();
        token.token_circulating_supply()
    }

    fn lookup_token_state_value(
        &self,
        token_key_value: &Self::MutableTokenKeyValueState,
        key: &TokenStateKey,
    ) -> Option<TokenStateValue> {
        token_key_value
            .lookup_value(&self.context.loader, &key.0)
            .map(TokenStateValue)
    }

    fn iter_token_state_prefix(
        &self,
        token_key_value: &Self::MutableTokenKeyValueState,
        prefix: &TokenStateKey,
    ) -> impl Iterator<Item = (TokenStateKey, TokenStateValue)> + use<'_, C> {
        token_key_value
            .iter_prefix(&self.context.loader, &prefix.0)
            .unwrap()
            .map(|entry| (TokenStateKey(entry.0), TokenStateValue(entry.1)))
    }

    fn update_token_state_value(
        &self,
        token_key_value_state: &mut Self::MutableTokenKeyValueState,
        key: &TokenStateKey,
        value: Option<TokenStateValue>,
    ) {
        if let Some(value) = value {
            token_key_value_state
                .insert_value(&self.context.loader, &key.0, value.0)
                .unwrap();
        } else {
            token_key_value_state
                .delete_value(&self.context.loader, &key.0)
                .unwrap();
        }
    }

    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Self::Account, AccountNotFoundByAddressError> {
        self.block_state.account_by_address(&self.context, address)
    }

    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress, AccountNotFoundByIndexError> {
        self.block_state.account_by_index(&self.context, index)
    }

    fn account_index(&self, account: &Self::Account) -> AccountIndex {
        account.account_index()
    }

    fn account_token_balance(
        &self,
        account: &Self::Account,
        token: &Self::Token,
    ) -> RawTokenAmount {
        let token = self
            .block_state
            .token_by_index(&self.context, *token)
            .unwrap();
        account.account_token_balance(&self.context, token.token_index)
    }

    fn token_account_states(
        &self,
        account: &Self::Account,
    ) -> impl Iterator<Item = (Self::Token, TokenAccountState)> {
        account.token_account_states(&self.context)
    }

    fn protocol_version(&self) -> ProtocolVersion {
        ProtocolVersion::P9
    }
}

impl<C: EntityContextTypes> BlockStateOperations for ExecutionTimeBlockStateP9<C> {
    fn set_token_circulating_supply(
        &mut self,
        token: &Self::Token,
        circulating_supply: RawTokenAmount,
    ) {
        let mut token = self
            .block_state
            .token_by_index(&self.context, *token)
            .unwrap();
        token.set_token_circulating_supply(circulating_supply);
        self.block_state.update_token(&self.context, token).unwrap();
    }

    fn create_token(&mut self, configuration: TokenConfiguration) -> Self::Token {
        self.block_state
            .create_token(&self.context, configuration)
            .unwrap()
    }

    fn update_token_account_balance(
        &mut self,
        token: &Self::Token,
        account: &Self::Account,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError> {
        let token = self
            .block_state
            .token_by_index(&self.context, *token)
            .unwrap();
        account.update_token_account_balance(&mut self.context, token.token_index, amount_delta)
    }

    fn touch_token_account(&mut self, token: &Self::Token, account: &Self::Account) {
        let token = self
            .block_state
            .token_by_index(&self.context, *token)
            .unwrap();
        account.touch_token_account(&mut self.context, token.token_index)
    }

    fn increment_plt_update_instruction_sequence_number(&mut self) {
        self.block_state
            .increment_plt_update_instruction_sequence_number(&mut self.context);
    }

    fn set_token_key_value_state(
        &mut self,
        token: &Self::Token,
        token_key_value_state: Self::MutableTokenKeyValueState,
    ) {
        let mut token = self
            .block_state
            .token_by_index(&self.context, *token)
            .unwrap();
        token.mutable_key_value_state = token_key_value_state;
        self.block_state.update_token(&self.context, token).unwrap();
    }
}

/// Runtime/execution state relevant for providing an implementation of
/// [`BlockStateQuery`] and [`BlockStateOperations`].
pub type ExecutionTimeBlockStateP10<C> = ExecutionTimeBlockStateP9<C>;

/// Runtime/execution state relevant for providing an implementation of
/// [`BlockStateQuery`] and [`BlockStateOperations`].
#[derive(Debug)]
pub struct ExecutionTimeBlockStateP11<C: EntityContextTypes> {
    /// The library block state implementation.
    pub block_state: BlockStateP11,
    /// External function for reading from the blob store.
    pub context: EntityContext<C>,
}

impl<C: EntityContextTypes> BlockStateQuery for ExecutionTimeBlockStateP11<C> {
    type MutableTokenKeyValueState = smart_contract_trie::MutableState;
    type Account = Account;
    type Token = TokenIndex;

    fn plt_list(&self) -> impl ExactSizeIterator<Item = TokenId> {
        self.block_state
            .plt_list(&self.context)
            .unwrap()
            .into_iter()
    }

    fn token_by_id(&self, token_id: &TokenId) -> Result<Self::Token, TokenNotFoundByIdError> {
        self.block_state
            .token_by_id(&self.context, token_id)
            .unwrap()
            .map(|token| token.token_p9.token_index)
    }

    fn mutable_token_key_value_state(
        &self,
        token: &Self::Token,
    ) -> Self::MutableTokenKeyValueState {
        let token = self
            .block_state
            .token_by_index(&self.context, *token)
            .unwrap();
        token.token_p9.mutable_key_value_state
    }

    fn token_configuration(&self, token: &Self::Token) -> TokenConfiguration {
        let token = self
            .block_state
            .token_by_index(&self.context, *token)
            .unwrap();
        token.token_p9.token_configuration(&self.context).unwrap()
    }

    fn token_circulating_supply(&self, token: &Self::Token) -> RawTokenAmount {
        let token = self
            .block_state
            .token_by_index(&self.context, *token)
            .unwrap();
        token.token_p9.token_circulating_supply()
    }

    fn lookup_token_state_value(
        &self,
        token_key_value: &Self::MutableTokenKeyValueState,
        key: &TokenStateKey,
    ) -> Option<TokenStateValue> {
        token_key_value
            .lookup_value(&self.context.loader, &key.0)
            .map(TokenStateValue)
    }

    fn iter_token_state_prefix(
        &self,
        token_key_value: &Self::MutableTokenKeyValueState,
        prefix: &TokenStateKey,
    ) -> impl Iterator<Item = (TokenStateKey, TokenStateValue)> + use<'_, C> {
        token_key_value
            .iter_prefix(&self.context.loader, &prefix.0)
            .unwrap()
            .map(|entry| (TokenStateKey(entry.0), TokenStateValue(entry.1)))
    }

    fn update_token_state_value(
        &self,
        token_key_value_state: &mut Self::MutableTokenKeyValueState,
        key: &TokenStateKey,
        value: Option<TokenStateValue>,
    ) {
        if let Some(value) = value {
            token_key_value_state
                .insert_value(&self.context.loader, &key.0, value.0)
                .unwrap();
        } else {
            token_key_value_state
                .delete_value(&self.context.loader, &key.0)
                .unwrap();
        }
    }

    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Self::Account, AccountNotFoundByAddressError> {
        self.block_state.account_by_address(&self.context, address)
    }

    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress, AccountNotFoundByIndexError> {
        self.block_state.account_by_index(&self.context, index)
    }

    fn account_index(&self, account: &Self::Account) -> AccountIndex {
        account.account_index()
    }

    fn account_token_balance(
        &self,
        account: &Self::Account,
        token: &Self::Token,
    ) -> RawTokenAmount {
        let token = self
            .block_state
            .token_by_index(&self.context, *token)
            .unwrap();
        account.account_token_balance(&self.context, token.token_p9.token_index)
    }

    fn token_account_states(
        &self,
        account: &Self::Account,
    ) -> impl Iterator<Item = (Self::Token, TokenAccountState)> {
        account.token_account_states(&self.context)
    }

    fn protocol_version(&self) -> ProtocolVersion {
        ProtocolVersion::P11
    }
}

impl<C: EntityContextTypes> BlockStateOperations for ExecutionTimeBlockStateP11<C> {
    fn set_token_circulating_supply(
        &mut self,
        token: &Self::Token,
        circulating_supply: RawTokenAmount,
    ) {
        let mut token = self
            .block_state
            .token_by_index(&self.context, *token)
            .unwrap();
        token
            .token_p9
            .set_token_circulating_supply(circulating_supply);
        self.block_state.update_token(&self.context, token).unwrap();
    }

    fn create_token(&mut self, configuration: TokenConfiguration) -> Self::Token {
        self.block_state
            .create_token(&self.context, configuration)
            .unwrap()
    }

    fn update_token_account_balance(
        &mut self,
        token: &Self::Token,
        account: &Self::Account,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError> {
        let token = self
            .block_state
            .token_by_index(&self.context, *token)
            .unwrap();
        account.update_token_account_balance(
            &mut self.context,
            token.token_p9.token_index,
            amount_delta,
        )
    }

    fn touch_token_account(&mut self, token: &Self::Token, account: &Self::Account) {
        let token = self
            .block_state
            .token_by_index(&self.context, *token)
            .unwrap();
        account.touch_token_account(&mut self.context, token.token_p9.token_index)
    }

    fn increment_plt_update_instruction_sequence_number(&mut self) {
        self.block_state
            .increment_plt_update_instruction_sequence_number(&mut self.context);
    }

    fn set_token_key_value_state(
        &mut self,
        token: &Self::Token,
        token_key_value_state: Self::MutableTokenKeyValueState,
    ) {
        let mut token = self
            .block_state
            .token_by_index(&self.context, *token)
            .unwrap();
        token.token_p9.mutable_key_value_state = token_key_value_state;
        self.block_state.update_token(&self.context, token).unwrap();
    }
}
