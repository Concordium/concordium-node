//! Scheduler implementation for protocol-level token updates. This module implements execution
//! of transactions related to protocol-level tokens.

use crate::scheduler::{ChainUpdateExecutionError, TransactionExecutionError};
use crate::token_context::TokenOperationContext;
use crate::token_module::{self, TOKEN_MODULE_REF, TokenInitializationError, TokenUpdateError};
use crate::transaction_execution::{OutOfEnergyError, TransactionExecution};
use concordium_base::base::AccountIndex;
use concordium_base::common::cbor::{self};
use concordium_base::protocol_level_locks::{LockController, LockId};
use concordium_base::protocol_level_tokens::{CborHolderAccount, RawCbor, TokenId};
use concordium_base::protocol_level_tokens::{
    TokenOperationsPayload,
    meta_operations::{
        LockOperation, MetaUpdateOperation, MetaUpdateOperations, MetaUpdatePayload,
    },
};
use concordium_base::transactions;
use concordium_base::updates::CreatePlt;
use plt_block_state::block_state::types::protocol_level_locks::LockConfiguration;
use plt_block_state::block_state::types::protocol_level_tokens::TokenConfiguration;
use plt_block_state::block_state_interface::{
    BlockStateOperations, BlockStateQuery, TokenNotFoundByIdError,
};
use plt_scheduler_types::types::events::{self, BlockItemEvent, TokenCreateEvent};
use plt_scheduler_types::types::execution::{ChainUpdateOutcome, FailureKind, TransactionOutcome};
use plt_scheduler_types::types::locks::{self, LockControllerConfig, MetaUpdateOperationKind};
use plt_scheduler_types::types::reject_reasons::{
    EncodedTokenModuleRejectReason, TransactionRejectReason,
};

/// Execute a token update transaction payload modifying `block_state` accordingly.
/// Returns the events produced if successful, otherwise a reject reason.
/// Energy must be charged during execution by calling [`TransactionExecution::tick_energy`]. If
/// execution is out of energy, the function `tick_energy` returns an error which means execution must be stopped,
/// and the [`OutOfEnergy`](TransactionRejectReason::OutOfEnergy) reject reason must be returned.
///
/// NOTICE: The caller must ensure to rollback state changes in case of the transaction being rejected.
///
/// # Arguments
///
/// - `transaction_execution` Context of transaction execution that allows accessing sending account
///   and charging energy.
/// - `block_state` Block state that can be queried and updated during execution.
/// - `payload` The token update transaction payload to execute.
///
/// # Errors
///
/// - [`TransactionExecutionError`] If executing the transaction fails with an unrecoverable error.
///   Returning this error will terminate the scheduler.
pub fn execute_token_update_transaction<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    block_state: &mut BSO,
    payload: TokenOperationsPayload,
) -> Result<TransactionOutcome, TransactionExecutionError> {
    // Charge energy
    if let Err(err) =
        transaction_execution.tick_energy(transactions::cost::PLT_OPERATIONS_TRANSACTIONS)
    {
        let _: OutOfEnergyError = err; // assert type of error
        return Ok(TransactionOutcome::Rejected(
            TransactionRejectReason::OutOfEnergy,
        ));
    }

    // Lookup token
    let token = match block_state.token_by_id(&payload.token_id) {
        Ok(token) => token,
        Err(TokenNotFoundByIdError(_)) => {
            return Ok(TransactionOutcome::Rejected(
                TransactionRejectReason::NonExistentTokenId(payload.token_id),
            ));
        }
    };

    let token_configuration = block_state.token_configuration(&token);

    let mut events = Vec::new();
    let mut token_module_state = block_state.mutable_token_key_value_state(&token);
    let mut token_module_state_dirty = false;
    let mut kernel = TokenOperationContext {
        block_state,
        token: &token,
        token_configuration: &token_configuration,
        token_module_state: &mut token_module_state,
        token_module_state_dirty: &mut token_module_state_dirty,
        events: &mut events,
    };

    // Call token module to execute operations
    let token_update_result = token_module::execute_token_update_transaction(
        transaction_execution,
        &mut kernel,
        payload.operations,
    );

    match token_update_result {
        Ok(()) => {
            // Update token module state if dirty
            if token_module_state_dirty {
                block_state.set_token_key_value_state(&token, token_module_state);
            }

            // Return events
            Ok(TransactionOutcome::Success(events))
        }
        Err(TokenUpdateError::TokenModuleReject(reject_reason)) => {
            Ok(TransactionOutcome::Rejected(
                TransactionRejectReason::TokenUpdateTransactionFailed(
                    EncodedTokenModuleRejectReason {
                        // Use the canonical token id from the token configuration
                        token_id: token_configuration.token_id.clone(),
                        reason_type: reject_reason.reason_type,
                        details: reject_reason.details,
                    },
                ),
            ))
        }
        Err(TokenUpdateError::OutOfEnergy(_)) => Ok(TransactionOutcome::Rejected(
            TransactionRejectReason::OutOfEnergy,
        )),
        Err(TokenUpdateError::StateInvariantViolation(err)) => Err(
            TransactionExecutionError::StateInvariantBroken(err.to_string()),
        ),
    }
}

/// Execute a meta-update transaction payload modifying `block_state` accordingly.
/// Returns the events produced if successful, otherwise a reject reason.
/// Energy must be charged during execution by calling [`TransactionExecution::tick_energy`]. If
/// execution is out of energy, the function `tick_energy` returns an error which means execution must be stopped,
/// and the [`OutOfEnergy`](TransactionRejectReason::OutOfEnergy) reject reason must be returned.
///
/// NOTICE: The caller must ensure to rollback state changes in case of the transaction being rejected.
///
/// # Arguments
///
/// - `transaction_execution` Context of transaction execution that allows accessing sending account
///   and charging energy.
/// - `block_state` Block state that can be queried and updated during execution.
/// - `payload` The meta-update transaction payload to execute.
///
/// # Errors
///
/// - [`TransactionExecutionError`] If executing the transaction fails with an unrecoverable error.
///   Returning this error will terminate the scheduler.
pub fn execute_meta_update_transaction<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    block_state: &mut BSO,
    payload: MetaUpdatePayload,
) -> Result<TransactionOutcome, TransactionExecutionError> {
    // Charge energy
    if let Err(err) =
        transaction_execution.tick_energy(transactions::cost::META_UPDATE_TRANSACTIONS)
    {
        let _: OutOfEnergyError = err; // assert type of error
        return Ok(TransactionOutcome::Rejected(
            TransactionRejectReason::OutOfEnergy,
        ));
    }

    let mut events = Vec::new();

    let operations: Vec<MetaUpdateOperation> =
        match token_module::util::cbor_decode::<MetaUpdateOperations>(payload.operations) {
            Ok(payload) => payload.operations,
            Err(_) => {
                return Ok(TransactionOutcome::Rejected(
                    TransactionRejectReason::SerializationFailure,
                ));
            }
        };

    for (index, operation) in operations.into_iter().enumerate() {
        match operation.into() {
            MetaUpdateOperationKind::Token((token_id, operation)) => {
                // Lookup token
                let token = match block_state.token_by_id(&token_id) {
                    Ok(token) => token,
                    Err(TokenNotFoundByIdError(_)) => {
                        return Ok(TransactionOutcome::Rejected(
                            TransactionRejectReason::NonExistentTokenId(token_id),
                        ));
                    }
                };

                let token_configuration = block_state.token_configuration(&token);
                let mut token_module_state = block_state.mutable_token_key_value_state(&token);
                let mut token_module_state_dirty = false;
                let mut kernel = TokenOperationContext {
                    block_state,
                    token: &token,
                    token_configuration: &token_configuration,
                    token_module_state: &mut token_module_state,
                    token_module_state_dirty: &mut token_module_state_dirty,
                    events: &mut events,
                };
                let token_update_result = token_module::execute_token_update_operation_at_index(
                    transaction_execution,
                    &mut kernel,
                    index,
                    &operation,
                );
                match token_update_result {
                    Ok(()) => {
                        if token_module_state_dirty {
                            block_state.set_token_key_value_state(&token, token_module_state);
                        }
                    }
                    Err(TokenUpdateError::TokenModuleReject(reject_reason)) => {
                        return Ok(TransactionOutcome::Rejected(
                            TransactionRejectReason::TokenUpdateTransactionFailed(
                                EncodedTokenModuleRejectReason {
                                    token_id,
                                    reason_type: reject_reason.reason_type,
                                    details: reject_reason.details,
                                },
                            ),
                        ));
                    }
                    Err(TokenUpdateError::OutOfEnergy(_)) => {
                        return Ok(TransactionOutcome::Rejected(
                            TransactionRejectReason::OutOfEnergy,
                        ));
                    }
                    Err(TokenUpdateError::StateInvariantViolation(err)) => {
                        return Err(TransactionExecutionError::StateInvariantBroken(
                            err.to_string(),
                        ));
                    }
                }
            }
            MetaUpdateOperationKind::Lock(lock_operation) => {
                let result = execute_lock_operation(
                    transaction_execution,
                    block_state,
                    index,
                    lock_operation,
                    &mut events,
                )?;
                if let Some(reject_reason) = result {
                    return Ok(TransactionOutcome::Rejected(reject_reason));
                }
            }
        }
    }
    Ok(TransactionOutcome::Success(events))
}

/// Look up the account index for a [`CborHolderAccount`]. If the account does
/// not exist, a [`TransactionRejectReason::InvalidAccountReference`] is
/// returned.
fn lookup_account_index<BSQ: BlockStateQuery>(
    block_state: &BSQ,
    holder: CborHolderAccount,
) -> Result<AccountIndex, TransactionRejectReason> {
    match block_state.account_by_address(&holder.address) {
        Ok(account) => Ok(block_state.account_index(&account)),
        Err(_) => Err(TransactionRejectReason::InvalidAccountReference(
            holder.address,
        )),
    }
}

/// Look up the token ID in the block state. If the token ID does not exist, a
/// [`TransactionRejectReason::NonExistentTokenId`] is returned.
/// Otherwise, this returns the canonical representation of the token ID.
///
/// TODO: Consider returning token index instead.
fn lookup_token_id<BSQ: BlockStateQuery>(
    block_state: &BSQ,
    token_id: TokenId,
) -> Result<TokenId, TransactionRejectReason> {
    match block_state.token_by_id(&token_id) {
        Ok(token) => Ok(block_state.token_configuration(&token).token_id),
        Err(TokenNotFoundByIdError(_)) => {
            Err(TransactionRejectReason::NonExistentTokenId(token_id))
        }
    }
}

fn execute_lock_operation<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    block_state: &mut BSO,
    _index: usize,
    lock_operation: LockOperation,
    events: &mut Vec<BlockItemEvent>,
) -> Result<Option<TransactionRejectReason>, TransactionExecutionError> {
    match lock_operation {
        LockOperation::Fund(_meta_lock_fund_details) => todo!(),
        LockOperation::Send(_meta_lock_send_details) => todo!(),
        LockOperation::Return(_meta_lock_return_details) => todo!(),
        LockOperation::Create(meta_lock_create_details) => {
            let config = meta_lock_create_details.config;
            let account_index = block_state.account_index(transaction_execution.sender_account());
            let sequence_number = transaction_execution.transaction_sequence_number();
            let creation_order = transaction_execution.next_lock_creation_order();
            let lock_id = LockId::new(account_index, sequence_number, creation_order);

            let controller = match config.controller {
                LockController::SimpleV0(controller) => {
                    let grants = controller.grants.into_iter().map(|grant| {
                        let account_index = lookup_account_index(block_state, grant.account)?;
                        Ok(locks::LockControllerSimpleV0Grant {
                            account: account_index,
                            roles: grant.roles,
                        })
                    });
                    let tokens = controller
                        .tokens
                        .into_iter()
                        .map(|token_id| lookup_token_id(block_state, token_id));

                    match LockControllerConfig::new_simple_v0(
                        grants,
                        tokens,
                        controller.keep_alive,
                        controller.memo,
                    ) {
                        Ok(controller) => controller,
                        Err(err) => {
                            return Ok(Some(err));
                        }
                    }
                }
            };

            let configuration = match LockConfiguration::new(
                config.recipients.iter().map(|recipient| {
                    match block_state.account_by_address(&recipient.address) {
                        Ok(account) => Ok(block_state.account_index(&account)),
                        Err(_) => Err(TransactionRejectReason::InvalidAccountReference(
                            recipient.address,
                        )),
                    }
                }),
                config.expiry,
                controller,
            ) {
                Ok(configuration) => configuration,
                Err(reject_reason) => return Ok(Some(reject_reason)),
            };

            // We reconstruct the lock config for the event, rather than using
            // the original one from the transaction. This results in a config
            // that is in a canonical form.
            let config = configuration.lock_config(block_state).map_err(|err| {
                TransactionExecutionError::StateInvariantBroken(format!(
                    "Failed to get lock config for created lock: {err}"
                ))
            })?;
            let event = events::LockCreateEvent {
                lock_id: lock_id.clone(),
                lock_config: RawCbor::from(cbor::cbor_encode(&config)),
            };
            events.push(BlockItemEvent::LockCreated(event));

            block_state.create_lock(lock_id.clone(), configuration);

            Ok(None)
        }
        LockOperation::Cancel(_meta_lock_cancel_details) => todo!(),
    }
}

/// Execute a create protocol-level token chain update modifying `block_state` accordingly.
/// Returns the events produced if successful, otherwise a failure kind is returned.
///
/// NOTICE: The caller must ensure to rollback state changes in case a failure kind is returned.
///
/// # Arguments
///
/// - `block_state` Block state that can be queried and updated during execution.
/// - `payload` The create PLT chain update.
///
/// # Errors
///
/// - [`ChainUpdateExecutionError`] If executing the update instruction failed.
///   Returning this error will terminate the scheduler.
pub fn execute_create_plt_chain_update<BSO: BlockStateOperations>(
    block_state: &mut BSO,
    payload: CreatePlt,
) -> Result<ChainUpdateOutcome, ChainUpdateExecutionError> {
    // Check that token id is not already used (notice that token_by_id lookup is case-insensitive
    // as the check should be)
    if let Ok(existing_token) = block_state.token_by_id(&payload.token_id) {
        return Ok(ChainUpdateOutcome::Failed(FailureKind::DuplicateTokenId(
            block_state.token_configuration(&existing_token).token_id,
        )));
    }

    // Check token module ref matches the implemented token module
    if payload.token_module != TOKEN_MODULE_REF {
        return Ok(ChainUpdateOutcome::Failed(
            FailureKind::InvalidTokenModuleRef(payload.token_module),
        ));
    }

    let token_configuration = TokenConfiguration {
        token_id: payload.token_id.clone(),
        module_ref: payload.token_module,
        decimals: payload.decimals,
    };

    // Create token in block state
    let token = block_state.create_token(token_configuration.clone());

    let mut events = Vec::new();
    events.push(BlockItemEvent::TokenCreated(TokenCreateEvent {
        payload: payload.clone(),
    }));

    let mut token_module_state = block_state.mutable_token_key_value_state(&token);
    let mut token_module_state_dirty = false;
    let mut context = TokenOperationContext {
        block_state,
        token: &token,
        token_configuration: &token_configuration,
        token_module_state: &mut token_module_state,
        token_module_state_dirty: &mut token_module_state_dirty,
        events: &mut events,
    };

    // Initialize token in token module
    let token_initialize_result =
        token_module::initialize_token(&mut context, payload.initialization_parameters);

    match token_initialize_result {
        Ok(()) => {
            // Increment protocol-level token update sequence number
            block_state.increment_plt_update_instruction_sequence_number();

            // Update token module state if dirty
            if token_module_state_dirty {
                block_state.set_token_key_value_state(&token, token_module_state);
            }

            // Return events
            Ok(ChainUpdateOutcome::Success(events))
        }
        Err(TokenInitializationError::StateInvariantViolation(err)) => Err(
            ChainUpdateExecutionError::StateInvariantBroken(err.to_string()),
        ),
        Err(err) => Ok(ChainUpdateOutcome::Failed(
            FailureKind::TokenInitializeFailure(err.to_string()),
        )),
    }
}
