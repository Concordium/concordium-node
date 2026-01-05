use crate::block_state_interface::{
    BlockStateOperations, BlockStateQuery, MutableTokenState, OverflowError, PLTConfiguration,
    TokenAmountDelta, TokenIndex,
};

pub struct BlockState {}

impl BlockStateQuery for BlockState {
    fn get_plt_list(
        &self,
    ) -> impl std::iter::Iterator<Item = concordium_base::protocol_level_tokens::TokenId> {
        // TODO implement this. The implementation below is just to help the type checker infer
        // enough for this to compile
        Vec::new().into_iter()
    }

    fn get_token_index(
        &self,
        _token_id: &concordium_base::protocol_level_tokens::TokenId,
    ) -> Option<TokenIndex> {
        todo!()
    }

    fn get_mutable_token_state(&self, _token_index: TokenIndex) -> MutableTokenState {
        todo!()
    }

    fn get_token_configuration(&self, _token_index: TokenIndex) -> PLTConfiguration {
        todo!()
    }

    fn get_token_circulating_supply(
        &self,
        _token_index: TokenIndex,
    ) -> plt_token_module::token_kernel_interface::RawTokenAmount {
        todo!()
    }
}

impl BlockStateOperations for BlockState {
    fn set_token_circulating_supply(
        &mut self,
        _token_index: TokenIndex,
        _circulating_supply: plt_token_module::token_kernel_interface::RawTokenAmount,
    ) {
        todo!()
    }

    fn create_token(&mut self, _configuration: PLTConfiguration) -> TokenIndex {
        todo!()
    }

    fn update_token_account_balance(
        &mut self,
        _token_index: TokenIndex,
        _account_index: concordium_base::base::AccountIndex,
        _amount_delta: TokenAmountDelta,
    ) -> Result<(), OverflowError> {
        todo!()
    }

    fn touch_token_account(
        &mut self,
        _token_index: TokenIndex,
        _account_index: concordium_base::base::AccountIndex,
    ) -> bool {
        todo!()
    }

    fn increment_plt_update_sequence_number(&mut self) {
        todo!()
    }

    fn set_token_state(
        &mut self,
        _token_index: TokenIndex,
        _mutable_token_state: MutableTokenState,
    ) {
        todo!()
    }
}
