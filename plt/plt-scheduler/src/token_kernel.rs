//! Implementation of the protocol-level token kernel.

use crate::block_state_interface::{
    BlockStateOperations, BlockStateQuery, OverflowError, RawTokenAmountDelta, TokenConfiguration,
};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    RawCbor, TokenAmount, TokenModuleCborTypeDiscriminator,
};
use concordium_base::transactions::Memo;
use plt_scheduler_interface::error::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
use plt_scheduler_interface::token_kernel_interface::{
    AccountWithCanonicalAddress, InsufficientBalanceError, MintWouldOverflowError, TokenBurnError,
    TokenKernelOperations, TokenKernelQueries, TokenMintError, TokenStateInvariantError,
    TokenStateKey, TokenStateValue, TokenTransferError,
};
use plt_types::types::events::{
    BlockItemEvent, EncodedTokenModuleEvent, TokenBurnEvent, TokenMintEvent, TokenTransferEvent,
};
use plt_types::types::primitives::RawTokenAmount;

/// Implementation of token kernel queries with a specific token in context.
pub struct TokenKernelQueriesImpl<'a, BSQ: BlockStateQuery> {
    /// The block state
    pub block_state: &'a BSQ,
    /// Token in context
    pub token: &'a BSQ::Token,
    /// Token module state for the token in context
    pub token_module_state: &'a BSQ::TokenKeyValueState,
}

impl<'a, BSQ: BlockStateQuery> TokenKernelQueriesImpl<'a, BSQ> {
    pub fn account_from_block_state_account(
        &self,
        account: &BSQ::Account,
    ) -> AccountWithCanonicalAddress<<Self as TokenKernelQueries>::AccountWithAddress> {
        let account_index = self.block_state.account_index(account);
        // the account exists in BSQ, and thus must also exist in the token kernel.
        self.account_by_index(account_index)
            .expect("The account exists")
    }
}

impl<BSQ: BlockStateQuery> TokenKernelQueries for TokenKernelQueriesImpl<'_, BSQ> {
    type AccountWithAddress = (BSQ::Account, AccountAddress);

    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Self::AccountWithAddress, AccountNotFoundByAddressError> {
        let account = self.block_state.account_by_address(address)?;

        Ok((account, *address))
    }

    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress<Self::AccountWithAddress>, AccountNotFoundByIndexError>
    {
        let account_with_canonical_address = self.block_state.account_by_index(index)?;

        Ok(AccountWithCanonicalAddress {
            account: (
                account_with_canonical_address.account,
                account_with_canonical_address.canonical_account_address,
            ),
            canonical_account_address: account_with_canonical_address.canonical_account_address,
        })
    }

    fn account_index(&self, account: &Self::AccountWithAddress) -> AccountIndex {
        self.block_state.account_index(&account.0)
    }

    fn account_token_balance(&self, account: &Self::AccountWithAddress) -> RawTokenAmount {
        self.block_state
            .account_token_balance(&account.0, self.token)
    }

    fn decimals(&self) -> u8 {
        self.block_state.token_configuration(self.token).decimals
    }

    fn lookup_token_state_value(&self, key: TokenStateKey) -> Option<TokenStateValue> {
        self.block_state
            .lookup_token_state_value(self.token_module_state, &key)
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
    fn touch_account(&mut self, account: &Self::AccountWithAddress) {
        self.block_state
            .update_token_account_balance(
                self.token,
                &account.0,
                RawTokenAmountDelta::Add(RawTokenAmount(0)),
            )
            .ok();
    }

    fn mint(
        &mut self,
        account: &Self::AccountWithAddress,
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
            .update_token_account_balance(self.token, &account.0, RawTokenAmountDelta::Add(amount))
            .map_err(|_err: OverflowError| {
                // We should never overflow account balance at mint, since the total circulating supply of the token
                // is always less that what is representable as a token amount.
                TokenStateInvariantError("Mint destination account amount overflow".to_string())
            })?;

        // Issue event
        let event = BlockItemEvent::TokenMint(TokenMintEvent {
            token_id: self.token_configuration.token_id.clone(),
            target: account.1,
            amount: TokenAmount::from_raw(
                amount.0,
                self.block_state.token_configuration(self.token).decimals,
            ),
        });

        self.events.push(event);

        Ok(())
    }

    fn burn(
        &mut self,
        account: &Self::AccountWithAddress,
        amount: RawTokenAmount,
    ) -> Result<(), TokenBurnError> {
        // Update balance of the account
        self.block_state
            .update_token_account_balance(
                self.token,
                &account.0,
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
            target: account.1,
            amount: TokenAmount::from_raw(
                amount.0,
                self.block_state.token_configuration(self.token).decimals,
            ),
        });

        self.events.push(event);

        Ok(())
    }

    fn transfer(
        &mut self,
        from: &Self::AccountWithAddress,
        to: &Self::AccountWithAddress,
        amount: RawTokenAmount,
        memo: Option<Memo>,
    ) -> Result<(), TokenTransferError> {
        // Update sender balance
        self.block_state
            .update_token_account_balance(
                self.token,
                &from.0,
                RawTokenAmountDelta::Subtract(amount),
            )
            .map_err(|_err: OverflowError| InsufficientBalanceError {
                available: self.account_token_balance(from),
                required: amount,
            })?;

        // Update receiver balance
        self.block_state
            .update_token_account_balance(self.token, &to.0, RawTokenAmountDelta::Add(amount))
            .map_err(|_err: OverflowError| {
                // We should never overflow at transfer, since the total circulating supply of the token
                // is always less that what is representable as a token amount.
                TokenStateInvariantError("Transfer destination token amount overflow".to_string())
            })?;

        // Issue event
        let event = BlockItemEvent::TokenTransfer(TokenTransferEvent {
            token_id: self.token_configuration.token_id.clone(),
            from: from.1,
            to: to.1,
            amount: TokenAmount::from_raw(
                amount.0,
                self.block_state.token_configuration(self.token).decimals,
            ),
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
    type AccountWithAddress = (BSO::Account, AccountAddress);

    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Self::AccountWithAddress, AccountNotFoundByAddressError> {
        let account = self.block_state.account_by_address(address)?;

        Ok((account, *address))
    }

    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress<Self::AccountWithAddress>, AccountNotFoundByIndexError>
    {
        let account_with_canonical_address = self.block_state.account_by_index(index)?;

        Ok(AccountWithCanonicalAddress {
            account: (
                account_with_canonical_address.account,
                account_with_canonical_address.canonical_account_address,
            ),
            canonical_account_address: account_with_canonical_address.canonical_account_address,
        })
    }

    fn account_index(&self, account: &Self::AccountWithAddress) -> AccountIndex {
        self.block_state.account_index(&account.0)
    }

    fn account_token_balance(&self, account: &Self::AccountWithAddress) -> RawTokenAmount {
        self.block_state
            .account_token_balance(&account.0, self.token)
    }

    fn decimals(&self) -> u8 {
        self.block_state.token_configuration(self.token).decimals
    }

    fn lookup_token_state_value(&self, key: TokenStateKey) -> Option<TokenStateValue> {
        self.block_state
            .lookup_token_state_value(self.token_module_state, &key)
    }
}
