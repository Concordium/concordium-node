use crate::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, BlockStateOperations,
    BlockStateOperationsP11, BlockStateQuery, BlockStateQueryP11, RawTokenAmountDelta,
    TokenConfiguration, TokenNotFoundByIdError, UnderOrOverflowError,
};
use concordium_base::base::{AccountIndex, ProtocolVersion};
use concordium_base::contracts_common::AccountAddress;
use plt_token_module::token_kernel_interface::{ModuleStateKey, ModuleStateValue, RawTokenAmount};

pub struct BlockState {
    protocol_version: ProtocolVersion,
}

impl BlockState {
    pub fn new(protocol_version: ProtocolVersion) -> Self {
        Self { protocol_version }
    }
}

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

    fn queries_p11(&self) -> Option<&impl BlockStateQueryP11> {
        (self.protocol_version >= ProtocolVersion::P11).then_some(self)
    }
}

impl BlockStateQueryP11 for BlockState {
    fn query_p11(&self) {
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
    ) -> Result<(), UnderOrOverflowError> {
        todo!()
    }

    fn touch_token_account(&mut self, _token_index: &TokenIndex, _account_index: &AccountIndex) {
        todo!()
    }

    fn increment_plt_update_instruction_sequence_number(&mut self) {
        todo!()
    }

    fn set_token_module_state(
        &mut self,
        _token_index: &TokenIndex,
        _mutable_token_state: Self::MutableTokenModuleState,
    ) {
        todo!()
    }

    fn operations_p11(&mut self) -> Option<&mut impl BlockStateOperationsP11> {
        (self.protocol_version >= ProtocolVersion::P11).then_some(self)
    }
}

impl BlockStateOperationsP11 for BlockState {
    fn operation_p11(&mut self) {
        todo!()
    }
}
