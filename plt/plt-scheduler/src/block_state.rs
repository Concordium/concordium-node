use crate::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, BlockStateOperations,
    BlockStateQuery, OverflowError, RawTokenAmountDelta, TokenConfiguration,
    TokenNotFoundByIdError,
};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use plt_token_module::token_kernel_interface::{ModuleStateKey, ModuleStateValue, RawTokenAmount};

pub struct BlockState {}

/// Index of the protocol-level token in the block state map of tokens.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TokenIndex(u64);

impl BlockStateQuery for BlockState {
    type MutableTokenModuleState = ();
    type Account = AccountIndex;
    type Token = TokenIndex;

    fn plt_list(&self) -> impl Iterator<Item = concordium_base::protocol_level_tokens::TokenId> {
        // TODO implement this. The implementation below is just to help the type checker infer
        // enough for this to compile
        Vec::new().into_iter()
    }

    fn token_by_id(
        &self,
        _token_id: &concordium_base::protocol_level_tokens::TokenId,
    ) -> Result<TokenIndex, TokenNotFoundByIdError> {
        todo!()
    }

    fn mutable_token_module_state(
        &self,
        _token_index: &TokenIndex,
    ) -> Self::MutableTokenModuleState {
        todo!()
    }

    fn token_configuration(&self, _token_index: &TokenIndex) -> TokenConfiguration {
        todo!()
    }

    fn token_circulating_supply(&self, _token_index: &TokenIndex) -> RawTokenAmount {
        todo!()
    }

    fn lookup_token_module_state_value(
        &self,
        _token_state: &Self::MutableTokenModuleState,
        _key: &ModuleStateKey,
    ) -> Option<ModuleStateValue> {
        todo!()
    }

    fn update_token_module_state_value(
        &self,
        _token_state: &mut Self::MutableTokenModuleState,
        _key: &ModuleStateKey,
        _value: Option<ModuleStateValue>,
    ) {
        todo!()
    }

    fn account_by_address(
        &self,
        _address: &AccountAddress,
    ) -> Result<Self::Account, AccountNotFoundByAddressError> {
        todo!()
    }

    fn account_by_index(
        &self,
        _index: AccountIndex,
    ) -> Result<Self::Account, AccountNotFoundByIndexError> {
        todo!()
    }

    fn account_index(&self, _account: &Self::Account) -> AccountIndex {
        todo!()
    }

    fn account_canonical_address(&self, _account: &Self::Account) -> AccountAddress {
        todo!()
    }

    fn account_token_balance(
        &self,
        _account: &Self::Account,
        _token: &Self::Token,
    ) -> RawTokenAmount {
        todo!()
    }
}

impl BlockStateOperations for BlockState {
    fn set_token_circulating_supply(
        &mut self,
        _token_index: &TokenIndex,
        _circulating_supply: RawTokenAmount,
    ) {
        todo!()
    }

    fn create_token(&mut self, _configuration: TokenConfiguration) -> TokenIndex {
        todo!()
    }

    fn update_token_account_balance(
        &mut self,
        _token_index: &TokenIndex,
        _account_index: &AccountIndex,
        _amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError> {
        todo!()
    }

    fn touch_token_account(&mut self, _token_index: &TokenIndex, _account_index: &AccountIndex) {
        todo!()
    }

    fn increment_plt_update_sequence_number(&mut self) {
        todo!()
    }

    fn set_token_module_state(
        &mut self,
        _token_index: &TokenIndex,
        _mutable_token_state: Self::MutableTokenModuleState,
    ) {
        todo!()
    }
}
