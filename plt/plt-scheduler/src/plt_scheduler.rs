//! Scheduler implementation for protocol-level token updates. This module implements execution
//! of transactions related to protocol-level tokens.

use crate::block_state_interface::{
    BlockStateOperations, BlockStateQuery, RawTokenAmountDelta, TokenNotFoundByIdError,
    UnderOrOverflowError,
};
use crate::scheduler::{TransactionEvent, TransactionRejectReason};
use crate::scheduler_interface::TransactionExecution;
use crate::{block_state_interface, scheduler_interface};
use concordium_base::base::{AccountIndex, Energy};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{TokenAmount, TokenOperationsPayload};
use concordium_base::transactions::Memo;
use plt_token_module::token_kernel_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, AmountNotRepresentableError,
    InsufficientBalanceError, ModuleStateKey, ModuleStateValue, OutOfEnergyError, RawTokenAmount,
    TokenKernelOperations, TokenKernelQueries, TokenModuleEvent, TokenStateInvariantError,
    TransferError,
};
use plt_token_module::token_module;
use plt_token_module::token_module::{TokenUpdateError, TransactionContext};
use std::mem;

/// An event emitted when a transfer of tokens from `from` to `to` is performed.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TokenTransferEvent {
    /// The token holder from which the tokens are transferred.
    pub from: AccountAddress,
    /// The token holder to which the tokens are transferred.
    pub to: AccountAddress,
    /// The amount of tokens transferred.
    pub amount: TokenAmount,
    /// An optional memo field that can be used to attach a message to the token
    /// transfer.
    pub memo: Option<Memo>,
}

/// An event emitted when the token supply is updated, i.e. by minting/burning
/// tokens to/from the balance of the `target`.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TokenSupplyUpdateEvent {
    /// The token holder the balance update is performed on.
    pub target: AccountAddress,
    /// The balance difference to be applied to the target.
    pub amount: TokenAmount,
}

/// Execute a transaction payload modifying `transaction_execution` and `block_state` accordingly.
/// Returns the events produced if successful otherwise a reject reason.
///
/// The caller must ensure to rollback state changes in case of the transaction being rejected.
pub fn execute_plt_transaction<
    BSO: BlockStateOperations,
    TE: TransactionExecution<Account = BSO::Account>,
>(
    transaction_execution: &mut TE,
    block_state: &mut BSO,
    payload: TokenOperationsPayload,
) -> Result<Vec<TransactionEvent>, TransactionRejectReason> {
    let token =
        block_state
            .token_by_id(&payload.token_id)
            .map_err(|_err: TokenNotFoundByIdError| {
                TransactionRejectReason::NonExistentTokenId(payload.token_id)
            })?;
    let mut token_module_state = block_state.mutable_token_module_state(&token);

    let transaction_context = TransactionContext {
        sender: transaction_execution.sender_account(),
    };

    let mut kernel = TokenKernelExecutionImpl {
        block_state,
        transaction_execution,
        token: &token,
        token_module_state: &mut token_module_state,
        token_module_state_dirty: false,
        events: Default::default(),
    };

    match token_module::execute_token_update_transaction(
        &mut kernel,
        transaction_context,
        payload.operations,
    ) {
        Ok(()) => {
            let events = mem::take(&mut kernel.events);
            let token_module_state_dirty = kernel.token_module_state_dirty;
            drop(kernel);
            if token_module_state_dirty {
                block_state.set_token_module_state(&token, token_module_state);
            }
            Ok(events)
        }
        Err(TokenUpdateError::TokenModuleReject(reject_reason)) => {
            Err(TransactionRejectReason::TokenModule(reject_reason))
        }
        Err(TokenUpdateError::OutOfEnergy(_)) => Err(TransactionRejectReason::OutOfEnergy),
        Err(TokenUpdateError::StateInvariantViolation(err)) => {
            // todo handle as part of https://linear.app/concordium/issue/PSR-38/handle-broken-state-invariant
            todo!("Handle state invariant error: {}", err)
        }
    }
}

// todo remove pub as part of https://linear.app/concordium/issue/PSR-34/token-initialization when this type is no longer needed as part of tests
pub struct TokenKernelExecutionImpl<'a, BSQ: BlockStateQuery, TE: TransactionExecution> {
    pub block_state: &'a mut BSQ,
    pub transaction_execution: &'a mut TE,
    pub token: &'a BSQ::Token,
    pub token_module_state: &'a mut BSQ::MutableTokenModuleState,
    pub events: Vec<TransactionEvent>,
    pub token_module_state_dirty: bool,
}

impl<BSQ: BlockStateQuery, TE: TransactionExecution> TokenKernelQueries
    for TokenKernelExecutionImpl<'_, BSQ, TE>
{
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

    fn circulating_supply(&self) -> RawTokenAmount {
        self.block_state.token_circulating_supply(self.token)
    }

    fn decimals(&self) -> u8 {
        self.block_state.token_configuration(self.token).decimals
    }

    fn lookup_token_module_state_value(&self, key: ModuleStateKey) -> Option<ModuleStateValue> {
        self.block_state
            .lookup_token_module_state_value(self.token_module_state, &key)
    }
}

impl<BSO: BlockStateOperations, TE: TransactionExecution> TokenKernelOperations
    for TokenKernelExecutionImpl<'_, BSO, TE>
{
    fn touch(&mut self, account: &Self::Account) {
        self.block_state.touch_token_account(self.token, account);
    }

    fn mint(
        &mut self,
        account: &Self::Account,
        amount: RawTokenAmount,
    ) -> Result<(), AmountNotRepresentableError> {
        self.block_state
            .update_token_account_balance(self.token, account, RawTokenAmountDelta::Add(amount))
            .map_err(|_err: UnderOrOverflowError| AmountNotRepresentableError)?;

        self.events
            .push(TransactionEvent::TokenMint(TokenSupplyUpdateEvent {
                target: self.account_canonical_address(account),
                amount: TokenAmount::from_raw(
                    amount.0,
                    self.block_state.token_configuration(self.token).decimals,
                ),
            }));

        Ok(())
    }

    fn burn(
        &mut self,
        account: &Self::Account,
        amount: RawTokenAmount,
    ) -> Result<(), InsufficientBalanceError> {
        self.block_state
            .update_token_account_balance(
                self.token,
                account,
                RawTokenAmountDelta::Subtract(amount),
            )
            .map_err(|_err: UnderOrOverflowError| InsufficientBalanceError {
                available: self.account_token_balance(account),
                required: amount,
            })?;

        self.events
            .push(TransactionEvent::TokenBurn(TokenSupplyUpdateEvent {
                target: self.account_canonical_address(account),
                amount: TokenAmount::from_raw(
                    amount.0,
                    self.block_state.token_configuration(self.token).decimals,
                ),
            }));

        Ok(())
    }

    fn transfer(
        &mut self,
        from: &Self::Account,
        to: &Self::Account,
        amount: RawTokenAmount,
        memo: Option<Memo>,
    ) -> Result<(), TransferError> {
        self.block_state
            .update_token_account_balance(self.token, from, RawTokenAmountDelta::Subtract(amount))
            .map_err(|_err: UnderOrOverflowError| InsufficientBalanceError {
                available: self.account_token_balance(from),
                required: amount,
            })?;
        self.block_state
            .update_token_account_balance(self.token, to, RawTokenAmountDelta::Add(amount))
            .map_err(|_err: UnderOrOverflowError| {
                // We should never overflow at transfer, since the total circulating supply of the token
                // is always less that what is representable as a token amount.
                TokenStateInvariantError(
                    "Token amount overflow in transfer destination account".to_string(),
                )
            })?;

        self.events
            .push(TransactionEvent::TokenTransfer(TokenTransferEvent {
                from: self.account_canonical_address(from),
                to: self.account_canonical_address(to),
                amount: TokenAmount::from_raw(
                    amount.0,
                    self.block_state.token_configuration(self.token).decimals,
                ),
                memo,
            }));

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

    fn tick_energy(&mut self, energy: Energy) -> Result<(), OutOfEnergyError> {
        self.transaction_execution
            .tick_energy(energy)
            .map_err(|_err: scheduler_interface::OutOfEnergyError| OutOfEnergyError)
    }

    fn log_token_event(&mut self, module_event: TokenModuleEvent) {
        self.events
            .push(TransactionEvent::TokenModule(module_event))
    }
}
