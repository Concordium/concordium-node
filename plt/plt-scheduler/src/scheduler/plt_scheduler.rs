//! Scheduler implementation for protocol-level token updates. This module implements execution
//! of transactions related to protocol-level tokens.

use crate::block_state_interface;
use crate::block_state_interface::{
    BlockStateOperations, BlockStateQuery, RawTokenAmountDelta, TokenConfiguration,
    TokenNotFoundByIdError, UnderOrOverflowError,
};
use crate::scheduler::{
    TransactionExecutionError, TransactionRejectReason, UpdateInstructionExecutionError,
};
use crate::types::events::{TokenBurnEvent, TokenMintEvent, TokenTransferEvent, TransactionEvent};
use crate::types::reject_reasons::TokenModuleRejectReason;
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{TokenAmount, TokenOperationsPayload};
use concordium_base::transactions::Memo;
use concordium_base::updates::CreatePlt;
use plt_scheduler_interface::TransactionExecution;
use plt_token_module::token_kernel_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, AmountNotRepresentableError,
    InsufficientBalanceError, ModuleStateKey, ModuleStateValue, RawTokenAmount, TokenBurnError,
    TokenKernelOperations, TokenKernelQueries, TokenModuleEvent, TokenStateInvariantError,
    TokenTransferError,
};
use plt_token_module::token_module::TokenUpdateError;
use plt_token_module::{TOKEN_MODULE_REF, token_module};
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
    // Lookup token
    let token = match block_state.token_by_id(&payload.token_id) {
        Ok(token) => token,
        Err(TokenNotFoundByIdError(_)) => {
            return Ok(Err(TransactionRejectReason::NonExistentTokenId(
                payload.token_id,
            )));
        }
    };

    let token_configuration = block_state.token_configuration(&token);

    let mut token_module_state = block_state.mutable_token_module_state(&token);

    let mut kernel = TokenKernelOperationsImpl {
        block_state,
        token: &token,
        token_configuration: &token_configuration,
        token_module_state: &mut token_module_state,
        token_module_state_dirty: false,
        events: Default::default(),
    };

    // Call token module to execute operations
    let token_update_result = token_module::execute_token_update_transaction(
        transaction_execution,
        &mut kernel,
        payload.operations,
    );

    match token_update_result {
        Ok(()) => {
            let events = mem::take(&mut kernel.events);
            let token_module_state_dirty = kernel.token_module_state_dirty;
            drop(kernel);

            // Update token module state if dirty
            if token_module_state_dirty {
                block_state.set_token_module_state(&token, token_module_state);
            }

            // Return events
            Ok(Ok(events))
        }
        Err(TokenUpdateError::TokenModuleReject(reject_reason)) => Ok(Err(
            TransactionRejectReason::TokenModule(TokenModuleRejectReason {
                // Use the canonical token id from the token configuration
                token_id: token_configuration.token_id.clone(),
                reason_type: reject_reason.reason_type,
                details: reject_reason.details,
            }),
        )),
        Err(TokenUpdateError::OutOfEnergy(_)) => Ok(Err(TransactionRejectReason::OutOfEnergy)),
        Err(TokenUpdateError::StateInvariantViolation(err)) => Err(
            TransactionExecutionError::TokenStateInvariantBroken(err.to_string()),
        ),
    }
}

/// Execute an update instruction payload modifying `block_state` accordingly.
/// Returns the events produced if successful.
pub fn execute_plt_create_instruction<BSO: BlockStateOperations>(
    block_state: &mut BSO,
    payload: CreatePlt,
) -> Result<Vec<TransactionEvent>, UpdateInstructionExecutionError> {
    // Check that token id is not already used (notice that token_by_id lookup is case-insensitive
    // as the check should be)
    if let Ok(existing_token) = block_state.token_by_id(&payload.token_id) {
        return Err(UpdateInstructionExecutionError::TokenIdAlreadyUsed(
            block_state.token_configuration(&existing_token).token_id,
        ));
    }

    // Check token module ref matches the implemented token module
    if payload.token_module != TOKEN_MODULE_REF {
        return Err(UpdateInstructionExecutionError::UnknownTokenModuleRef(
            payload.token_module,
        ));
    }

    let token_configuration = TokenConfiguration {
        token_id: payload.token_id,
        module_ref: payload.token_module,
        decimals: payload.decimals,
    };

    // Create token in block state
    let token = block_state.create_token(token_configuration.clone());

    let mut token_module_state = block_state.mutable_token_module_state(&token);

    let mut kernel = TokenKernelOperationsImpl {
        block_state,
        token: &token,
        token_configuration: &token_configuration,
        token_module_state: &mut token_module_state,
        token_module_state_dirty: false,
        events: Default::default(),
    };

    // Initialize token in token module
    let token_initialize_result =
        token_module::initialize_token(&mut kernel, payload.initialization_parameters);

    match token_initialize_result {
        Ok(()) => {
            let events = mem::take(&mut kernel.events);
            let token_module_state_dirty = kernel.token_module_state_dirty;
            drop(kernel);

            // Increment protocol-level token update sequence number
            block_state.increment_plt_update_instruction_sequence_number();

            // Update token module state if dirty
            if token_module_state_dirty {
                block_state.set_token_module_state(&token, token_module_state);
            }

            // Return events
            Ok(events)
        }
        Err(err) => {
            Err(UpdateInstructionExecutionError::ModuleTokenInitializationFailed(err.to_string()))
        }
    }
}

struct TokenKernelOperationsImpl<'a, BSQ: BlockStateQuery> {
    block_state: &'a mut BSQ,
    token: &'a BSQ::Token,
    token_configuration: &'a TokenConfiguration,
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

    fn decimals(&self) -> u8 {
        self.block_state.token_configuration(self.token).decimals
    }

    fn lookup_token_module_state_value(&self, key: ModuleStateKey) -> Option<ModuleStateValue> {
        self.block_state
            .lookup_token_module_state_value(self.token_module_state, &key)
    }
}

impl<BSO: BlockStateOperations> TokenKernelOperations for TokenKernelOperationsImpl<'_, BSO> {
    fn touch_account(&mut self, account: &Self::Account) {
        self.block_state.touch_token_account(self.token, account);
    }

    fn mint(
        &mut self,
        account: &Self::Account,
        amount: RawTokenAmount,
    ) -> Result<(), AmountNotRepresentableError> {
        // Update balance of the account
        self.block_state
            .update_token_account_balance(self.token, account, RawTokenAmountDelta::Add(amount))
            .map_err(|_err: UnderOrOverflowError| AmountNotRepresentableError)?;

        // Update total supply
        let mut circulating_supply = self.block_state.token_circulating_supply(self.token);
        circulating_supply.0 = circulating_supply
            .0
            .checked_add(amount.0)
            .ok_or(AmountNotRepresentableError)?;
        self.block_state
            .set_token_circulating_supply(self.token, circulating_supply);

        // Issue event
        let event = TransactionEvent::TokenMint(TokenMintEvent {
            token_id: self.token_configuration.token_id.clone(),
            target: self.account_canonical_address(account),
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
        account: &Self::Account,
        amount: RawTokenAmount,
    ) -> Result<(), TokenBurnError> {
        // Update balance of the account
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

        // Update total supply
        let mut circulating_supply = self.block_state.token_circulating_supply(self.token);
        circulating_supply.0 = circulating_supply.0.checked_sub(amount.0).ok_or_else(||
            // We should never underflow total supply at transfer, since the total circulating supply of the token
            // is always more than any account balance.
            TokenStateInvariantError(
                "Circulating supply token amount underflow at burn".to_string(),
            ))?;
        self.block_state
            .set_token_circulating_supply(self.token, circulating_supply);

        // Issue event
        let event = TransactionEvent::TokenBurn(TokenBurnEvent {
            token_id: self.token_configuration.token_id.clone(),
            target: self.account_canonical_address(account),
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
        from: &Self::Account,
        to: &Self::Account,
        amount: RawTokenAmount,
        memo: Option<Memo>,
    ) -> Result<(), TokenTransferError> {
        // Update sender balance
        self.block_state
            .update_token_account_balance(self.token, from, RawTokenAmountDelta::Subtract(amount))
            .map_err(|_err: UnderOrOverflowError| InsufficientBalanceError {
                available: self.account_token_balance(from),
                required: amount,
            })?;

        // Update receiver balance
        self.block_state
            .update_token_account_balance(self.token, to, RawTokenAmountDelta::Add(amount))
            .map_err(|_err: UnderOrOverflowError| {
                // We should never overflow at transfer, since the total circulating supply of the token
                // is always less that what is representable as a token amount.
                TokenStateInvariantError("Transfer destination token amount overflow".to_string())
            })?;

        // Issue event
        let event = TransactionEvent::TokenTransfer(TokenTransferEvent {
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
        self.events
            .push(TransactionEvent::TokenModule(module_event))
    }
}
