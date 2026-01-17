//! Implementation of the protocol-level token kernel.

use crate::block_state_interface;
use crate::block_state_interface::{
    BlockStateOperations, BlockStateQuery, OverflowError, RawTokenAmountDelta, TokenConfiguration,
};
use crate::types::events::{BlockItemEvent, TokenTransferEvent};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::TokenAmount;
use concordium_base::transactions::Memo;
use plt_token_module::token_kernel_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, AmountNotRepresentableError,
    InsufficientBalanceError, ModuleStateKey, ModuleStateValue, RawTokenAmount, TokenBurnError,
    TokenKernelOperations, TokenKernelQueries, TokenModuleEvent, TokenStateInvariantError,
    TokenTransferError,
};

/// Implementation of token kernel queries with a specific token in context.
pub struct TokenKernelQueriesImpl<'a, BSQ: BlockStateQuery> {
    /// The block state
    pub block_state: &'a BSQ,
    /// Token in context
    pub token: &'a BSQ::Token,
    /// Token module state for the token in context
    pub token_module_state: &'a BSQ::MutableTokenModuleState,
}

impl<BSQ: BlockStateQuery> TokenKernelQueries for TokenKernelQueriesImpl<'_, BSQ> {
    type Account = BSQ::Account;

    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Self::Account, AccountNotFoundByAddressError> {
        self.block_state.account_by_address(address).map_err(
            |block_state_interface::AccountNotFoundByAddressError(account_address)| {
                AccountNotFoundByAddressError(account_address)
            },
        )
    }

    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<Self::Account, AccountNotFoundByIndexError> {
        self.block_state.account_by_index(index).map_err(
            |block_state_interface::AccountNotFoundByIndexError(index)| {
                AccountNotFoundByIndexError(index)
            },
        )
    }

    fn account_index(&self, account: &Self::Account) -> AccountIndex {
        self.block_state.account_index(account)
    }

    fn account_canonical_address(&self, account: &Self::Account) -> AccountAddress {
        self.block_state.account_canonical_address(account)
    }

    fn account_token_balance(&self, account: &Self::Account) -> RawTokenAmount {
        self.block_state.account_token_balance(account, self.token)
    }

    fn decimals(&self) -> u8 {
        self.block_state.token_configuration(self.token).decimals
    }

    fn lookup_token_module_state_value(&self, key: ModuleStateKey) -> Option<ModuleStateValue> {
        self.block_state
            .lookup_token_module_state_value(self.token_module_state, &key)
    }
}

/// Implementation of token kernel operations with a specific token in context.
pub struct TokenKernelOperationsImpl<'a, BSQ: BlockStateQuery> {
    /// The block state
    pub block_state: &'a mut BSQ,
    /// Token in context
    pub token: &'a BSQ::Token,
    /// Configuration for the token in context
    pub token_configuration: &'a TokenConfiguration,
    /// Token module state for the token in context
    pub token_module_state: &'a mut BSQ::MutableTokenModuleState,
    /// Whether token module state has been changed so far
    pub token_module_state_dirty: bool,
    /// Events produced so far
    pub events: Vec<BlockItemEvent>,
}

impl<BSO: BlockStateOperations> TokenKernelOperations for TokenKernelOperationsImpl<'_, BSO> {
    fn touch_account(&mut self, account: &Self::Account) {
        self.block_state.touch_token_account(self.token, account);
    }

    fn mint(
        &mut self,
        _account: &Self::Account,
        _amount: RawTokenAmount,
    ) -> Result<(), AmountNotRepresentableError> {
        todo!()
    }

    fn burn(
        &mut self,
        _account: &Self::Account,
        _amount: RawTokenAmount,
    ) -> Result<(), TokenBurnError> {
        todo!()
    }

    fn transfer(
        &mut self,
        from: &Self::Account,
        to: &Self::Account,
        amount: RawTokenAmount,
        memo: Option<Memo>,
    ) -> Result<(), TokenTransferError> {
        // Update sender balance
        self.block_state
            .update_token_account_balance(self.token, from, RawTokenAmountDelta::Subtract(amount))
            .map_err(|_err: OverflowError| InsufficientBalanceError {
                available: self.account_token_balance(from),
                required: amount,
            })?;

        // Update receiver balance
        self.block_state
            .update_token_account_balance(self.token, to, RawTokenAmountDelta::Add(amount))
            .map_err(|_err: OverflowError| {
                // We should never overflow at transfer, since the total circulating supply of the token
                // is always less that what is representable as a token amount.
                TokenStateInvariantError("Transfer destination token amount overflow".to_string())
            })?;

        // Issue event
        let event = BlockItemEvent::TokenTransfer(TokenTransferEvent {
            token_id: self.token_configuration.token_id.clone(),
            from: self.account_canonical_address(from),
            to: self.account_canonical_address(to),
            amount: TokenAmount::from_raw(
                amount.0,
                self.block_state.token_configuration(self.token).decimals,
            ),
            memo,
        });

        self.events.push(event);

        Ok(())
    }

    fn set_token_module_state_value(
        &mut self,
        key: ModuleStateKey,
        value: Option<ModuleStateValue>,
    ) {
        self.token_module_state_dirty = true;
        self.block_state
            .update_token_module_state_value(self.token_module_state, &key, value);
    }

    fn log_token_event(&mut self, module_event: TokenModuleEvent) {
        self.events.push(BlockItemEvent::TokenModule(module_event))
    }
}

impl<BSO: BlockStateOperations> TokenKernelQueries for TokenKernelOperationsImpl<'_, BSO> {
    type Account = BSO::Account;

    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Self::Account, AccountNotFoundByAddressError> {
        self.block_state.account_by_address(address).map_err(
            |block_state_interface::AccountNotFoundByAddressError(account_address)| {
                AccountNotFoundByAddressError(account_address)
            },
        )
    }

    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<Self::Account, AccountNotFoundByIndexError> {
        self.block_state.account_by_index(index).map_err(
            |block_state_interface::AccountNotFoundByIndexError(index)| {
                AccountNotFoundByIndexError(index)
            },
        )
    }

    fn account_index(&self, account: &Self::Account) -> AccountIndex {
        self.block_state.account_index(account)
    }

    fn account_canonical_address(&self, account: &Self::Account) -> AccountAddress {
        self.block_state.account_canonical_address(account)
    }

    fn account_token_balance(&self, account: &Self::Account) -> RawTokenAmount {
        self.block_state.account_token_balance(account, self.token)
    }

    fn decimals(&self) -> u8 {
        self.block_state.token_configuration(self.token).decimals
    }

    fn lookup_token_module_state_value(&self, key: ModuleStateKey) -> Option<ModuleStateValue> {
        self.block_state
            .lookup_token_module_state_value(self.token_module_state, &key)
    }
}
