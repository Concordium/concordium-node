use concordium_base::base::AccountIndex;
use crate::block_state_interface::{
    BlockStateOperations, BlockStateQuery, MutableTokenModuleState, OverflowError,
    PLTConfiguration, TokenAmountDelta,
};
use plt_token_module::token_kernel_interface::{ModuleStateKey, ModuleStateValue};

pub struct BlockState {}

/// Index of the protocol-level token in the block state map of tokens.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TokenIndex(pub u64);

impl BlockStateQuery for BlockState {
    type Account = AccountIndex;
    type Token = TokenIndex;

    fn plt_list(
        &self,
    ) -> impl Iterator<Item = concordium_base::protocol_level_tokens::TokenId> {
        // TODO implement this. The implementation below is just to help the type checker infer
        // enough for this to compile
        Vec::new().into_iter()
    }

    fn token_by_id(
        &self,
        _token_id: &concordium_base::protocol_level_tokens::TokenId,
    ) -> Option<TokenIndex> {
        todo!()
    }

    fn mutable_token_module_state(&self, _token_index: &TokenIndex) -> MutableTokenModuleState {
        todo!()
    }

    fn token_configuration(&self, _token_index: &TokenIndex) -> PLTConfiguration {
        todo!()
    }

    fn token_circulating_supply(
        &self,
        _token_index: &TokenIndex,
    ) -> plt_token_module::token_kernel_interface::RawTokenAmount {
        todo!()
    }

    fn lookup_token_module_state_value(
        &self,
        _token_state: &MutableTokenModuleState,
        _key: &ModuleStateKey,
    ) -> Option<ModuleStateValue> {
        todo!()
    }

    fn update_token_module_state_value(
        &self,
        _token_state: &MutableTokenModuleState,
        _key: &ModuleStateKey,
        _value: Option<ModuleStateValue>,
    ) {
        todo!()
    }
}

impl BlockStateOperations for BlockState {
    fn set_token_circulating_supply(
        &mut self,
        _token_index: &TokenIndex,
        _circulating_supply: plt_token_module::token_kernel_interface::RawTokenAmount,
    ) {
        todo!()
    }

    fn create_token(&mut self, _configuration: PLTConfiguration) -> TokenIndex {
        todo!()
    }

    fn update_token_account_balance(
        &mut self,
        _token_index: &TokenIndex,
        _account_index: &AccountIndex,
        _amount_delta: TokenAmountDelta,
    ) -> Result<(), OverflowError> {
        todo!()
    }

    fn touch_token_account(
        &mut self,
        _token_index: &TokenIndex,
        _account_index: &AccountIndex,
    ) -> bool {
        todo!()
    }

    fn increment_plt_update_sequence_number(&mut self) {
        todo!()
    }

    fn set_token_module_state(
        &mut self,
        _token_index: &TokenIndex,
        _mutable_token_state: MutableTokenModuleState,
    ) {
        todo!()
    }
}
