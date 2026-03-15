//! Implementation of the protocol-level token kernel.

use concordium_base::base::{AccountIndex, ProtocolVersion};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{RawCbor, TokenModuleCborTypeDiscriminator};
use concordium_base::transactions::Memo;
use plt_block_state::block_state::types::AccountWithCanonicalAddress;
use plt_block_state::block_state::types::protocol_level_tokens::{
    TokenConfiguration, TokenStateKey, TokenStateValue,
};
use plt_block_state::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, BlockStateOperations,
    BlockStateQuery, OverflowError, RawTokenAmountDelta,
};
use plt_scheduler_interface::token_kernel_interface::{
    InsufficientBalanceError, MintWouldOverflowError, TokenBurnError, TokenKernelOperations,
    TokenKernelQueries, TokenMintError, TokenStateInvariantError, TokenTransferError,
};
use plt_scheduler_types::types::events::{
    BlockItemEvent, EncodedTokenModuleEvent, TokenBurnEvent, TokenMintEvent, TokenTransferEvent,
};
use plt_scheduler_types::types::tokens::{RawTokenAmount, TokenAmount, TokenHolder};

/// Implementation of token kernel queries with a specific token in context.
pub struct TokenKernelQueriesImpl<'a, BSQ: BlockStateQuery> {
    /// The block state
    pub block_state: &'a BSQ,
    /// Token in context
    pub token: &'a BSQ::Token,
    /// Token module state for the token in context
    pub token_module_state: &'a BSQ::TokenKeyValueState,
}

impl<BSQ: BlockStateQuery> TokenKernelQueries for TokenKernelQueriesImpl<'_, BSQ> {
    type Account = BSQ::Account;

    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Self::Account, AccountNotFoundByAddressError> {
        self.block_state.account_by_address(address)
    }

    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress<Self::Account>, AccountNotFoundByIndexError> {
        self.block_state.account_by_index(index)
    }

    fn account_index(&self, account: &Self::Account) -> AccountIndex {
        self.block_state.account_index(account)
    }

    fn account_token_balance(&self, account: &Self::Account) -> RawTokenAmount {
        self.block_state.account_token_balance(account, self.token)
    }

    fn decimals(&self) -> u8 {
        self.block_state.token_configuration(self.token).decimals
    }

    fn lookup_token_state_value(&self, key: TokenStateKey) -> Option<TokenStateValue> {
        self.block_state
            .lookup_token_state_value(self.token_module_state, &key)
    }

    fn protocol_version(&self) -> ProtocolVersion {
        self.block_state.protocol_version()
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
    pub token_module_state: &'a mut BSQ::TokenKeyValueState,
    /// Whether token module state has been changed so far
    pub token_module_state_dirty: &'a mut bool,
    /// Events produced so far
    pub events: &'a mut Vec<BlockItemEvent>,
}

impl<BSO: BlockStateOperations> TokenKernelOperations for TokenKernelOperationsImpl<'_, BSO> {
    fn touch_account(&mut self, account: &Self::Account) {
        self.block_state.touch_token_account(self.token, account);
    }

    fn mint(
        &mut self,
        account: &Self::Account,
        account_address: AccountAddress,
        amount: RawTokenAmount,
    ) -> Result<(), TokenMintError> {
        // Update total supply
        let mut circulating_supply = self.block_state.token_circulating_supply(self.token);
        circulating_supply.0 =
            circulating_supply
                .0
                .checked_add(amount.0)
                .ok_or(MintWouldOverflowError {
                    requested_amount: amount,
                    current_supply: self.block_state.token_circulating_supply(self.token),
                    max_representable_amount: RawTokenAmount::MAX,
                })?;
        self.block_state
            .set_token_circulating_supply(self.token, circulating_supply);

        // Update balance of the account
        self.block_state
            .update_token_account_balance(self.token, account, RawTokenAmountDelta::Add(amount))
            .map_err(|_err: OverflowError| {
                // We should never overflow account balance at mint, since the total circulating supply of the token
                // is always less that what is representable as a token amount.
                TokenStateInvariantError("Mint destination account amount overflow".to_string())
            })?;

        // Issue event
        let event = BlockItemEvent::TokenMint(TokenMintEvent {
            token_id: self.token_configuration.token_id.clone(),
            target: TokenHolder::Account(account_address),
            amount: TokenAmount {
                amount,
                decimals: self.block_state.token_configuration(self.token).decimals,
            },
        });

        self.events.push(event);

        Ok(())
    }

    fn burn(
        &mut self,
        account: &Self::Account,
        account_address: AccountAddress,
        amount: RawTokenAmount,
    ) -> Result<(), TokenBurnError> {
        // Update balance of the account
        self.block_state
            .update_token_account_balance(
                self.token,
                account,
                RawTokenAmountDelta::Subtract(amount),
            )
            .map_err(|_err: OverflowError| InsufficientBalanceError {
                available: self.account_token_balance(account),
                required: amount,
            })?;

        // Update total supply
        let mut circulating_supply = self.block_state.token_circulating_supply(self.token);
        circulating_supply.0 = circulating_supply.0.checked_sub(amount.0).ok_or_else(||
            // We should never overflow total supply at burn, since the total circulating supply of the token
            // is always more than any account balance.
            TokenStateInvariantError(
                "Circulating supply amount overflow at burn".to_string(),
            ))?;
        self.block_state
            .set_token_circulating_supply(self.token, circulating_supply);

        // Issue event
        let event = BlockItemEvent::TokenBurn(TokenBurnEvent {
            token_id: self.token_configuration.token_id.clone(),
            target: TokenHolder::Account(account_address),
            amount: TokenAmount {
                amount,
                decimals: self.block_state.token_configuration(self.token).decimals,
            },
        });

        self.events.push(event);

        Ok(())
    }

    fn transfer(
        &mut self,
        from: &Self::Account,
        from_address: AccountAddress,
        to: &Self::Account,
        to_address: AccountAddress,
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
            from: TokenHolder::Account(from_address),
            to: TokenHolder::Account(to_address),
            amount: TokenAmount {
                amount,
                decimals: self.block_state.token_configuration(self.token).decimals,
            },
            memo,
        });

        self.events.push(event);

        Ok(())
    }

    fn set_token_state_value(&mut self, key: TokenStateKey, value: Option<TokenStateValue>) {
        *self.token_module_state_dirty = true;
        self.block_state
            .update_token_state_value(self.token_module_state, &key, value);
    }

    fn log_token_event(&mut self, event_type: TokenModuleCborTypeDiscriminator, details: RawCbor) {
        self.events
            .push(BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
                token_id: self.token_configuration.token_id.clone(),
                event_type,
                details,
            }))
    }
}

impl<BSO: BlockStateOperations> TokenKernelQueries for TokenKernelOperationsImpl<'_, BSO> {
    type Account = BSO::Account;

    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Self::Account, AccountNotFoundByAddressError> {
        self.block_state.account_by_address(address)
    }

    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress<Self::Account>, AccountNotFoundByIndexError> {
        self.block_state.account_by_index(index)
    }

    fn account_index(&self, account: &Self::Account) -> AccountIndex {
        self.block_state.account_index(account)
    }

    fn account_token_balance(&self, account: &Self::Account) -> RawTokenAmount {
        self.block_state.account_token_balance(account, self.token)
    }

    fn decimals(&self) -> u8 {
        self.block_state.token_configuration(self.token).decimals
    }

    fn lookup_token_state_value(&self, key: TokenStateKey) -> Option<TokenStateValue> {
        self.block_state
            .lookup_token_state_value(self.token_module_state, &key)
    }

    fn protocol_version(&self) -> ProtocolVersion {
        self.block_state.protocol_version()
    }
}
