use crate::BlockStateOperations;

pub struct BlockState {}

impl BlockStateOperations for BlockState {
    fn get_plt_list(
        &self,
    ) -> impl std::iter::Iterator<Item = concordium_base::protocol_level_tokens::TokenId> {
        // TODO implement this. The implementation below is just to help the type checker infer
        // enough for this to compile
        Vec::new().into_iter()
    }

    fn get_token_index(
        &self,
        _token_id: concordium_base::protocol_level_tokens::TokenId,
    ) -> Option<crate::TokenIndex> {
        todo!()
    }

    fn get_mutable_token_state(&self, _token_index: crate::TokenIndex) -> crate::MutableTokenState {
        todo!()
    }

    fn get_token_configuration(&self, _token_index: crate::TokenIndex) -> crate::PLTConfiguration {
        todo!()
    }

    fn get_token_circulating_supply(
        &self,
        _token_index: crate::TokenIndex,
    ) -> plt_token_module::host_interface::TokenRawAmount {
        todo!()
    }

    fn set_token_circulating_supply(
        &mut self,
        _token_index: crate::TokenIndex,
        _circulating_supply: plt_token_module::host_interface::TokenRawAmount,
    ) {
        todo!()
    }

    fn create_token(&mut self, _configuration: crate::PLTConfiguration) -> crate::TokenIndex {
        todo!()
    }

    fn update_token_account_balance(
        &mut self,
        _token_index: crate::TokenIndex,
        _account_index: concordium_base::base::AccountIndex,
        _amount_delta: crate::TokenAmountDelta,
    ) -> Result<(), crate::OverflowError> {
        todo!()
    }

    fn touch_token_account(
        &mut self,
        _token_index: crate::TokenIndex,
        _account_index: concordium_base::base::AccountIndex,
    ) -> bool {
        todo!()
    }

    fn increment_plt_update_sequence_number(&mut self) {
        todo!()
    }

    fn set_token_state(
        &mut self,
        _token_index: crate::TokenIndex,
        _mutable_token_state: crate::MutableTokenState,
    ) {
        todo!()
    }
}
