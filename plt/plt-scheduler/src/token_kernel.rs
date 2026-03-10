//! Implementation of the protocol-level token kernel.

use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{RawCbor, TokenModuleCborTypeDiscriminator};
use concordium_base::transactions::Memo;
use plt_block_state::block_state::external::{
    ExternalBlockStateOperations, ExternalBlockStateQuery,
};
use plt_block_state::block_state::p10::{self, SimplisticTokenKeyValueState};
use plt_block_state::block_state::types::{
    AccountWithAddress, AccountWithCanonicalAddress, TokenConfiguration, TokenIndex, TokenStateKey,
    TokenStateValue,
};
use plt_block_state::block_state::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
use plt_block_state::block_state_interface::{OverflowError, RawTokenAmountDelta};
use plt_scheduler_interface::token_kernel_interface::{
    InsufficientBalanceError, MintWouldOverflowError, TokenBurnError, TokenKernelOperations,
    TokenKernelQueries, TokenMintError, TokenStateInvariantError, TokenTransferError,
};
use plt_scheduler_types::types::events::{
    BlockItemEvent, EncodedTokenModuleEvent, TokenBurnEvent, TokenMintEvent, TokenTransferEvent,
};
use plt_scheduler_types::types::tokens::{RawTokenAmount, TokenAmount, TokenHolder};

/// Implementation of token kernel queries with a specific token in context.
pub struct TokenKernelQueriesImpl<'a, External> {
    pub tokens: &'a p10::Tokens,
    pub external: &'a External,
    /// Token in context
    pub token: TokenIndex,
    /// Token module state for the token in context
    pub token_module_state: &'a SimplisticTokenKeyValueState,
}

impl<Ext: ExternalBlockStateQuery> TokenKernelQueries for TokenKernelQueriesImpl<'_, Ext> {
    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<AccountWithAddress, AccountNotFoundByAddressError> {
        let index = self.external.account_index_by_account_address(address)?;
        Ok(AccountWithAddress {
            index,
            address: *address,
        })
    }

    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress, AccountNotFoundByIndexError> {
        let canonical_account_address = self
            .external
            .account_canonical_address_by_account_index(index)?;
        Ok(AccountWithCanonicalAddress(AccountWithAddress {
            index,
            address: canonical_account_address,
        }))
    }

    fn account_index(&self, account: &AccountWithAddress) -> AccountIndex {
        account.index
    }

    fn account_token_balance(&self, account: &AccountWithAddress) -> RawTokenAmount {
        self.external
            .read_token_account_balance(account.index, self.token)
    }

    fn decimals(&self) -> u8 {
        self.tokens.token_configuration(self.token).decimals
    }

    fn lookup_token_state_value(&self, key: TokenStateKey) -> Option<TokenStateValue> {
        self.tokens
            .lookup_token_state_value(self.token_module_state, &key)
    }
}

/// Implementation of token kernel operations with a specific token in context.
pub struct TokenKernelOperationsImpl<'a, External> {
    pub tokens: &'a mut p10::Tokens,
    /// Token in context
    pub token: TokenIndex,

    pub external: &'a mut External,
    /// Configuration for the token in context
    pub token_configuration: &'a TokenConfiguration,
    /// Token module state for the token in context
    pub token_module_state: &'a mut SimplisticTokenKeyValueState,
    /// Whether token module state has been changed so far
    pub token_module_state_dirty: &'a mut bool,
    /// Events produced so far
    pub events: &'a mut Vec<BlockItemEvent>,
}

impl<External: ExternalBlockStateOperations> TokenKernelOperations
    for TokenKernelOperationsImpl<'_, External>
{
    fn touch_account(&mut self, account: &AccountWithAddress) {
        self.external.touch_token_account(account.index, self.token);
    }

    fn mint(
        &mut self,
        account: &AccountWithAddress,
        amount: RawTokenAmount,
    ) -> Result<(), TokenMintError> {
        // Update total supply
        let mut circulating_supply = self.tokens.token_circulating_supply(self.token);
        circulating_supply.0 =
            circulating_supply
                .0
                .checked_add(amount.0)
                .ok_or(MintWouldOverflowError {
                    requested_amount: amount,
                    current_supply: self.tokens.token_circulating_supply(self.token),
                    max_representable_amount: RawTokenAmount::MAX,
                })?;
        self.tokens
            .set_token_circulating_supply(self.token, circulating_supply);

        // Update balance of the account
        self.external
            .update_token_account_balance(
                account.index,
                self.token,
                RawTokenAmountDelta::Add(amount),
            )
            .map_err(|_err: OverflowError| {
                // We should never overflow account balance at mint, since the total circulating supply of the token
                // is always less that what is representable as a token amount.
                TokenStateInvariantError("Mint destination account amount overflow".to_string())
            })?;

        // Issue event
        let event = BlockItemEvent::TokenMint(TokenMintEvent {
            token_id: self.token_configuration.token_id.clone(),
            target: TokenHolder::Account(account.address),
            amount: TokenAmount {
                amount,
                decimals: self.tokens.token_configuration(self.token).decimals,
            },
        });

        self.events.push(event);

        Ok(())
    }

    fn burn(
        &mut self,
        account: &AccountWithAddress,
        amount: RawTokenAmount,
    ) -> Result<(), TokenBurnError> {
        // Update balance of the account
        self.external
            .update_token_account_balance(
                account.index,
                self.token,
                RawTokenAmountDelta::Subtract(amount),
            )
            .map_err(|_err: OverflowError| InsufficientBalanceError {
                available: self.account_token_balance(account),
                required: amount,
            })?;

        // Update total supply
        let mut circulating_supply = self.tokens.token_circulating_supply(self.token);
        circulating_supply.0 = circulating_supply.0.checked_sub(amount.0).ok_or_else(||
            // We should never overflow total supply at burn, since the total circulating supply of the token
            // is always more than any account balance.
            TokenStateInvariantError(
                "Circulating supply amount overflow at burn".to_string(),
            ))?;
        self.tokens
            .set_token_circulating_supply(self.token, circulating_supply);

        // Issue event
        let event = BlockItemEvent::TokenBurn(TokenBurnEvent {
            token_id: self.token_configuration.token_id.clone(),
            target: TokenHolder::Account(account.address),
            amount: TokenAmount {
                amount,
                decimals: self.token_configuration.decimals,
            },
        });

        self.events.push(event);

        Ok(())
    }

    fn transfer(
        &mut self,
        from: &AccountWithAddress,
        to: &AccountWithAddress,
        amount: RawTokenAmount,
        memo: Option<Memo>,
    ) -> Result<(), TokenTransferError> {
        // Update sender balance
        self.external
            .update_token_account_balance(
                from.index,
                self.token,
                RawTokenAmountDelta::Subtract(amount),
            )
            .map_err(|_err: OverflowError| InsufficientBalanceError {
                available: self.account_token_balance(from),
                required: amount,
            })?;

        // Update receiver balance
        self.external
            .update_token_account_balance(to.index, self.token, RawTokenAmountDelta::Add(amount))
            .map_err(|_err: OverflowError| {
                // We should never overflow at transfer, since the total circulating supply of the token
                // is always less that what is representable as a token amount.
                TokenStateInvariantError("Transfer destination token amount overflow".to_string())
            })?;

        // Issue event
        let event = BlockItemEvent::TokenTransfer(TokenTransferEvent {
            token_id: self.token_configuration.token_id.clone(),
            from: TokenHolder::Account(from.address),
            to: TokenHolder::Account(to.address),
            amount: TokenAmount {
                amount,
                decimals: self.token_configuration.decimals,
            },
            memo,
        });

        self.events.push(event);

        Ok(())
    }

    fn set_token_state_value(&mut self, key: TokenStateKey, value: Option<TokenStateValue>) {
        *self.token_module_state_dirty = true;
        self.tokens
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

impl<Ext: ExternalBlockStateOperations> TokenKernelQueries for TokenKernelOperationsImpl<'_, Ext> {
    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<AccountWithAddress, AccountNotFoundByAddressError> {
        let index = self.external.account_index_by_account_address(address)?;
        Ok(AccountWithAddress {
            index,
            address: *address,
        })
    }

    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<AccountWithCanonicalAddress, AccountNotFoundByIndexError> {
        let canonical_account_address = self
            .external
            .account_canonical_address_by_account_index(index)?;
        Ok(AccountWithCanonicalAddress(AccountWithAddress {
            index,
            address: canonical_account_address,
        }))
    }

    fn account_index(&self, account: &AccountWithAddress) -> AccountIndex {
        account.index
    }

    fn account_token_balance(&self, account: &AccountWithAddress) -> RawTokenAmount {
        self.external
            .read_token_account_balance(account.index, self.token)
    }

    fn decimals(&self) -> u8 {
        self.tokens.token_configuration(self.token).decimals
    }

    fn lookup_token_state_value(&self, key: TokenStateKey) -> Option<TokenStateValue> {
        self.tokens
            .lookup_token_state_value(self.token_module_state, &key)
    }
}
