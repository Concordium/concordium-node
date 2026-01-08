//! Scheduler implementation for protocol-level token updates. This module implements execution
//! of transactions related to protocol-level tokens.

use crate::block_state_interface::{
    BlockStateOperations, BlockStateQuery, RawTokenAmountDelta, TokenConfiguration,
    TokenNotFoundByIdError, UnderOrOverflowError,
};
use crate::scheduler::{
    TransactionExecutionError, TransactionRejectReason, UpdateInstructionExecutionError,
    scheduler_interface,
};

use crate::block_state_interface;
use crate::events::{TokenSupplyUpdateEvent, TokenTransferEvent, TransactionEvent};
use crate::scheduler::scheduler_interface::TransactionExecution;
use concordium_base::base::{AccountIndex, Energy};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{TokenAmount, TokenOperationsPayload};
use concordium_base::transactions::Memo;
use concordium_base::updates::CreatePlt;
use plt_token_module::token_kernel_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, AmountNotRepresentableError,
    InsufficientBalanceError, ModuleStateKey, ModuleStateValue, OutOfEnergyError, RawTokenAmount,
    TokenKernelOperations, TokenKernelQueries, TokenKernelTransactionExecution, TokenModuleEvent,
    TokenStateInvariantError, TransferError,
};
use plt_token_module::token_module;
use plt_token_module::token_module::TokenUpdateError;
use std::mem;

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
) -> Result<Result<Vec<TransactionEvent>, TransactionRejectReason>, TransactionExecutionError> {
    let token = match block_state.token_by_id(&payload.token_id) {
        Ok(token) => token,
        Err(err) => {
            let _err: TokenNotFoundByIdError = err; // assert type of error
            return Ok(Err(TransactionRejectReason::NonExistentTokenId(
                payload.token_id,
            )));
        }
    };

    let mut token_module_state = block_state.mutable_token_module_state(&token);

    let mut kernel_transaction_execution = TokenKernelTransactionExecutionImpl {
        transaction_execution,
    };

    let mut kernel = TokenKernelOperationsImpl {
        block_state,
        token: &token,
        token_module_state: &mut token_module_state,
        token_module_state_dirty: false,
        events: Default::default(),
    };

    match token_module::execute_token_update_transaction(
        &mut kernel_transaction_execution,
        &mut kernel,
        payload.operations,
    ) {
        Ok(()) => {
            let events = mem::take(&mut kernel.events);
            let token_module_state_dirty = kernel.token_module_state_dirty;
            drop(kernel);
            if token_module_state_dirty {
                block_state.set_token_module_state(&token, token_module_state);
            }
            Ok(Ok(events))
        }
        Err(TokenUpdateError::TokenModuleReject(reject_reason)) => {
            Ok(Err(TransactionRejectReason::TokenModule(reject_reason)))
        }
        Err(TokenUpdateError::OutOfEnergy(_)) => Ok(Err(TransactionRejectReason::OutOfEnergy)),
        Err(TokenUpdateError::StateInvariantViolation(err)) => Err(
            TransactionExecutionError::TokenStateInvariantBroken(err.to_string()),
        ),
    }
}

/// Execute an update instruction payload modifying `block_state` accordingly.
/// Returns the events produced if successful.
pub fn execute_plt_update_instruction<BSO: BlockStateOperations>(
    block_state: &mut BSO,
    payload: CreatePlt,
) -> Result<Vec<TransactionEvent>, UpdateInstructionExecutionError> {
    let token_configuration = TokenConfiguration {
        token_id: payload.token_id,
        module_ref: payload.token_module,
        decimals: payload.decimals,
    };

    // todo ar token id, sequence and uniqueness

    let token = block_state.create_token(token_configuration);
    let mut token_module_state = block_state.mutable_token_module_state(&token);

    let mut kernel = TokenKernelOperationsImpl {
        block_state,
        token: &token,
        token_module_state: &mut token_module_state,
        token_module_state_dirty: false,
        events: Default::default(),
    };

    match token_module::initialize_token(&mut kernel, payload.initialization_parameters) {
        Ok(()) => {
            let events = mem::take(&mut kernel.events);
            let token_module_state_dirty = kernel.token_module_state_dirty;
            drop(kernel);
            block_state.increment_plt_update_sequence_number();
            if token_module_state_dirty {
                block_state.set_token_module_state(&token, token_module_state);
            }
            Ok(events)
        }
        Err(err) => Err(UpdateInstructionExecutionError::UpdateInstructionFailed(
            err.to_string(),
        )),
    }
}

struct TokenKernelOperationsImpl<'a, BSQ: BlockStateQuery> {
    block_state: &'a mut BSQ,
    token: &'a BSQ::Token,
    token_module_state: &'a mut BSQ::MutableTokenModuleState,
    events: Vec<TransactionEvent>,
    token_module_state_dirty: bool,
}

impl<BSQ: BlockStateQuery> TokenKernelQueries for TokenKernelOperationsImpl<'_, BSQ> {
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

impl<BSO: BlockStateOperations> TokenKernelOperations for TokenKernelOperationsImpl<'_, BSO> {
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

    fn log_token_event(&mut self, module_event: TokenModuleEvent) {
        self.events
            .push(TransactionEvent::TokenModule(module_event))
    }
}

struct TokenKernelTransactionExecutionImpl<'a, TE: TransactionExecution> {
    transaction_execution: &'a mut TE,
}

impl<TE: TransactionExecution> TokenKernelTransactionExecution
    for TokenKernelTransactionExecutionImpl<'_, TE>
{
    type Account = TE::Account;

    fn sender_account(&self) -> Self::Account {
        self.transaction_execution.sender_account()
    }

    fn tick_energy(&mut self, energy: Energy) -> Result<(), OutOfEnergyError> {
        self.transaction_execution
            .tick_energy(energy)
            .map_err(|_err: scheduler_interface::OutOfEnergyError| OutOfEnergyError)
    }
}
