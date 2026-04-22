//! Context for running queries and operations on protocol-level tokens.

use crate::token_module::errors::{
    InsufficientBalanceError, MintWouldOverflowError, TokenBurnError, TokenMintError,
    TokenStateInvariantError, TokenTransferError,
};
use concordium_base::base::ProtocolVersion;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{RawCbor, TokenModuleCborTypeDiscriminator};
use concordium_base::transactions::Memo;
use plt_block_state::block_state::types::protocol_level_tokens::{
    TokenConfiguration, TokenStateKey, TokenStateValue,
};
use plt_block_state::block_state_interface::{
    BlockStateOperations, BlockStateQuery, OverflowError, RawTokenAmountDelta,
};
use plt_scheduler_types::types::events::{
    BlockItemEvent, EncodedTokenModuleEvent, TokenBurnEvent, TokenMintEvent, TokenTransferEvent,
};
use plt_scheduler_types::types::tokens::{RawTokenAmount, TokenAmount, TokenHolder};

/// Context for running token queries with a specific token in context.
pub struct TokenQueryContext<'a, BSQ: BlockStateQuery> {
    /// The block state
    pub block_state: &'a BSQ,
    /// Token module state for the token in context
    pub token_module_state: &'a BSQ::MutableTokenKeyValueState,
}

/// Context for running token operations with a specific token in context.
pub struct TokenOperationContext<'a, BSQ: BlockStateQuery> {
    /// The block state
    pub block_state: &'a mut BSQ,
    /// Token in context
    pub token: &'a BSQ::Token,
    /// Configuration for the token in context
    pub token_configuration: &'a TokenConfiguration,
    /// Token module state for the token in context
    pub token_module_state: &'a mut BSQ::MutableTokenKeyValueState,
    /// Whether token module state has been changed so far
    pub token_module_state_dirty: &'a mut bool,
    /// Events produced so far
    pub events: &'a mut Vec<BlockItemEvent>,
}

impl<BSO: BlockStateOperations> TokenOperationContext<'_, BSO> {
    /// Mint a specified amount and deposit it in the account.
    ///
    /// # Events
    ///
    /// This will produce a `TokenMintEvent` in the logs.
    ///
    /// # Errors
    ///
    /// - [`TokenMintError::MintWouldOverflow`] The total supply would exceed the representable amount.
    /// - [`TokenMintError::StateInvariantViolation`] If an internal token state invariant is broken.
    pub fn mint(
        &mut self,
        account: &BSO::Account,
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

    /// Burn a specified amount from the account.
    ///
    /// # Events
    ///
    /// This will produce a `TokenBurnEvent` in the logs.
    ///
    /// # Errors
    ///
    /// - [`TokenBurnError::InsufficientBalance`] The sender has insufficient balance.
    /// - [`TokenBurnError::StateInvariantViolation`] If an internal token state invariant is broken.
    pub fn burn(
        &mut self,
        account: &BSO::Account,
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
                available: self.block_state.account_token_balance(account, self.token),
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

    /// Transfer a token amount from one account to another, with an optional memo.
    ///
    /// # Events
    ///
    /// This will produce a `TokenTransferEvent` in the logs.
    ///
    /// # Errors
    ///
    /// - [`TokenTransferError::InsufficientBalance`] The sender has insufficient balance.
    /// - [`TokenTransferError::StateInvariantViolation`] If an internal token state invariant is broken.
    pub fn transfer(
        &mut self,
        from: &BSO::Account,
        from_address: AccountAddress,
        to: &BSO::Account,
        to_address: AccountAddress,
        amount: RawTokenAmount,
        memo: Option<Memo>,
    ) -> Result<(), TokenTransferError> {
        // Update sender balance
        self.block_state
            .update_token_account_balance(self.token, from, RawTokenAmountDelta::Subtract(amount))
            .map_err(|_err: OverflowError| InsufficientBalanceError {
                available: self.block_state.account_token_balance(from, self.token),
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

    /// Set or clear a value in the token key-value state at the corresponding key.
    pub fn update_token_state_value(&mut self, key: TokenStateKey, value: Option<TokenStateValue>) {
        *self.token_module_state_dirty = true;
        self.block_state
            .update_token_state_value(self.token_module_state, &key, value);
    }

    /// Log a token module event with the specified type and details.
    ///
    /// # Events
    ///
    /// This will produce a `TokenModuleEvent` in the logs.
    pub fn log_token_event(
        &mut self,
        event_type: TokenModuleCborTypeDiscriminator,
        details: RawCbor,
    ) {
        self.events
            .push(BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
                token_id: self.token_configuration.token_id.clone(),
                event_type,
                details,
            }))
    }

    /// Whether the protocol version of the block supports RBAC token feature.
    pub fn support_rbac(&self) -> bool {
        self.block_state.protocol_version() >= ProtocolVersion::P11
    }

    /// Whether the protocol version of the block supports updating the token metadata.
    pub fn support_updating_metadata(&self) -> bool {
        self.block_state.protocol_version() >= ProtocolVersion::P11
    }
}
