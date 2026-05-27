//! Context for running queries and operations on protocol-level tokens.

use crate::token_module::errors::{
    InsufficientBalanceError, MintWouldOverflowError, TokenBurnError, TokenMintError,
    TokenStateInvariantError, TokenTransferError,
};
use crate::token_module::key_value_state;
use concordium_base::base::{AccountIndex, ProtocolVersion};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::{RawCbor, TokenModuleCborTypeDiscriminator};
use concordium_base::transactions::Memo;
use plt_block_state::block_state_interface::{
    BlockStateOperations, BlockStateQuery, OverflowError, RawTokenAmountDelta, TokenStateKey,
    TokenStateValue,
};
use plt_block_state::persistent::protocol_level_tokens::p9::TokenConfiguration;
use plt_scheduler_types::types::events::{
    BlockItemEvent, EncodedTokenModuleEvent, TokenBurnEvent, TokenMintEvent, TokenTransferEvent,
};
use plt_scheduler_types::types::tokens::{RawTokenAmount, TokenAmount, TokenHolder};

/// Get the available balance for an account.
/// This can throw a `TokenStateInvariantError` if the computed locked balance
/// exceeds the total balance.
fn get_available_balance<BSQ: BlockStateQuery>(
    block_state: &BSQ,
    token: &BSQ::Token,
    token_module_state: &BSQ::MutableTokenKeyValueState,
    account: &BSQ::Account,
) -> Result<RawTokenAmount, TokenStateInvariantError> {
    let total = block_state.account_token_balance(account, token);
    let context = TokenQueryContext {
        block_state,
        token_module_state,
    };
    let account_index = block_state.account_index(account);
    let locked_balances =
        key_value_state::get_locked_balances_for_account(&context, account_index)?;
    let mut available = total;
    let on_overflow = || {
        let token_name = block_state.token_configuration(token).token_id;
        TokenStateInvariantError(format!(
            "locked balance exceeds total balance for token {token_name} on account index {account_index}"
        ))
    };
    for (_, amount) in locked_balances {
        available = available.checked_sub(amount).ok_or_else(on_overflow)?;
    }
    Ok(available)
}

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
    /// - [`TokenTransferError::InsufficientBalance`] The sender has insufficient available balance.
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
        // Check that the available balance is sufficient.
        let available =
            get_available_balance(self.block_state, self.token, self.token_module_state, from)?;
        if amount > available {
            return Err(InsufficientBalanceError {
                available,
                required: amount,
            }
            .into());
        }
        // Update sender balance
        self.block_state
            .update_token_account_balance(self.token, from, RawTokenAmountDelta::Subtract(amount))
            .map_err(|_err: OverflowError| InsufficientBalanceError {
                available,
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
            from_lock: None,
            to_lock: None,
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

    /// Fund a lock with a specified amount of the token. This generates a
    /// `TokenTransferEvent` to reflect the change in the locked balance.
    ///
    /// Note: this does not update the lock itself, which should also record
    /// that this account has a balance associated with the lock.
    ///
    /// Returns `true` if the account previously had no balance controlled by
    /// the lock but now has a (positive) balance controlled by it.
    ///
    /// # Preconditions
    ///
    /// - The protocol version MUST support protocol-level locks.
    /// - The lock MUST exist in the block state.
    ///
    /// # Errors
    ///
    /// - [`TokenTransferError::InsufficientBalance`] The sender has insufficient balance.
    /// - [`TokenStateInvariantError`] If an internal token state invariant is broken, e.g. locked balance overflow.
    pub fn transfer_into_lock(
        &mut self,
        from: &BSO::Account,
        from_address: AccountAddress,
        lock_id: &LockId,
        amount: RawTokenAmount,
        memo: Option<Memo>,
    ) -> Result<bool, TokenTransferError> {
        let available =
            get_available_balance(self.block_state, self.token, self.token_module_state, from)?;
        if amount > available {
            return Err(InsufficientBalanceError {
                available,
                required: amount,
            }
            .into());
        }

        // Update locked balance of the lock
        let from_index = self.block_state.account_index(from);
        let locked_balance = key_value_state::get_locked_balance_for(self, from_index, lock_id)?;
        let new_locked_balance = locked_balance.checked_add(amount).ok_or_else(|| {
            // We should never overflow locked balance at fund, since the total circulating supply of the token
            // is always less that what is representable as a token amount.
            TokenStateInvariantError("Locked balance overflow at fund".to_string())
        })?;
        key_value_state::set_locked_balance_for(self, from_index, lock_id, new_locked_balance);

        // Issue event
        let event = BlockItemEvent::TokenTransfer(TokenTransferEvent {
            token_id: self.token_configuration.token_id.clone(),
            from: TokenHolder::Account(from_address),
            to: TokenHolder::Account(from_address), // Locked balance is still associated with the same account
            amount: TokenAmount {
                amount,
                decimals: self.block_state.token_configuration(self.token).decimals,
            },
            memo,
            from_lock: None,
            to_lock: Some(lock_id.clone()),
        });

        self.events.push(event);

        Ok(locked_balance.0 == 0 && new_locked_balance.0 > 0)
    }

    /// Send locked funds from one account to another. The funds arrive on the
    /// available balance of the receiving account. If the destination is
    /// `None`, the funds are returned to the sender's available balance. This
    /// generates a `TokenTransferEvent` to reflect the transfer. The event is
    /// generated even if the transfer amount is 0.
    ///
    /// Note: this does not update the locks themselves, which should also be
    /// updated if the sending account's locked funds are reduced to 0.
    ///
    /// Returns `true` if the sending account had a positive balance controlled by
    /// the lock before the transfer but has no balance controlled by it after the transfer.
    ///
    /// # Preconditions
    ///
    /// - The protocol version MUST support protocol-level locks.
    /// - The lock MUST exist in the block state.
    ///
    /// # Errors
    ///
    /// - [`TokenTransferError::InsufficientBalance`] The sender has insufficient balance.
    /// - [`TokenStateInvariantError`] If an internal token state invariant is broken.
    pub fn transfer_from_lock(
        &mut self,
        from: &BSO::Account,
        from_address: AccountAddress,
        destination: Option<(&BSO::Account, AccountAddress)>,
        lock_id: &LockId,
        amount: RawTokenAmount,
        memo: Option<Memo>,
    ) -> Result<bool, TokenTransferError> {
        let old_balance = key_value_state::get_locked_balance_for(
            self,
            self.block_state.account_index(from),
            lock_id,
        )?;
        let new_balance = old_balance
            .checked_sub(amount)
            .ok_or(InsufficientBalanceError {
                available: old_balance,
                required: amount,
            })?;
        key_value_state::set_locked_balance_for(
            self,
            self.block_state.account_index(from),
            lock_id,
            new_balance,
        );

        let to_address = match destination {
            None => {
                // Returning to sender's available balance: no change in
                // the account balance is required.
                from_address
            }
            Some((to, to_addr)) => {
                // Update sender balance
                self.block_state
                    .update_token_account_balance(
                        self.token,
                        from,
                        RawTokenAmountDelta::Subtract(amount),
                    )
                    .map_err(|_err: OverflowError| {
                        // An overflow can only occur here if the locked balance exceed the
                        // account balance, which is an invariant violation.
                        TokenStateInvariantError(
                            "Transfer source token amount overflow".to_string(),
                        )
                    })?;

                // Update receiver balance
                self.block_state
                    .update_token_account_balance(self.token, to, RawTokenAmountDelta::Add(amount))
                    .map_err(|_err: OverflowError| {
                        // We should never overflow at transfer, since the total circulating supply
                        // of the token is always less that what is representable as a token amount.
                        TokenStateInvariantError(
                            "Transfer destination token amount overflow".to_string(),
                        )
                    })?;
                to_addr
            }
        };

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
            from_lock: Some(lock_id.clone()),
            to_lock: None,
        });
        self.events.push(event);

        Ok(old_balance == amount && old_balance.0 > 0)
    }

    /// Unlock the balance of an account associated with a particular lock for
    /// this particular token. This generates a `TokenTransferEvent` to reflect
    /// the change in the locked balance. No transfer occurs (and no event is
    /// generated) if there is no locked balance to unlock.
    pub fn unlock_balance(
        &mut self,
        account_index: AccountIndex,
        lock_id: &LockId,
        memo: &Option<Memo>,
    ) -> Result<(), TokenStateInvariantError> {
        let old_balance = key_value_state::get_locked_balance_for(self, account_index, lock_id)?;
        if old_balance == RawTokenAmount(0) {
            // No locked balance, nothing to do.
            return Ok(());
        }
        key_value_state::set_locked_balance_for(self, account_index, lock_id, RawTokenAmount(0));
        let account = self
            .block_state
            .account_by_index(account_index)
            .map_err(|e| TokenStateInvariantError(e.to_string()))?
            .canonical_account_address;
        self.events
            .push(BlockItemEvent::TokenTransfer(TokenTransferEvent {
                token_id: self.token_configuration.token_id.clone(),
                from: TokenHolder::Account(account),
                to: TokenHolder::Account(account),
                amount: TokenAmount {
                    amount: old_balance,
                    decimals: self.block_state.token_configuration(self.token).decimals,
                },
                memo: memo.clone(),
                from_lock: Some(lock_id.clone()),
                to_lock: None,
            }));
        Ok(())
    }
}
