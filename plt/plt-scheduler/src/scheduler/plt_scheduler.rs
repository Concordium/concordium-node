//! Scheduler implementation for protocol-level token updates. This module implements execution
//! of transactions related to protocol-level tokens.

use crate::block_state_interface::{
    BlockStateOperations, TokenConfiguration, TokenNotFoundByIdError,
};
use crate::scheduler::{
    TransactionExecutionError, TransactionOutcome, TransactionRejectReason,
    UpdateInstructionExecutionError,
};
use crate::token_kernel::TokenKernelOperationsImpl;
use crate::types::events::{BlockItemEvent, TokenCreateEvent};
use crate::types::reject_reasons::TokenModuleRejectReason;
use concordium_base::protocol_level_tokens::TokenOperationsPayload;
use concordium_base::transactions;
use concordium_base::updates::CreatePlt;
use plt_scheduler_interface::TransactionExecution;
use plt_token_module::token_module::TokenUpdateError;
use plt_token_module::{TOKEN_MODULE_REF, token_module};

/// Execute a token update transaction payload modifying `block_state` accordingly.
/// Returns the events produced if successful, otherwise a reject reason.
/// Energy must be charged during execution by calling [`TransactionExecution::tick_energy`]. If
/// execution is out of energy, the function `tick_energy` returns an error which means execution must be stopped,
/// and the [`OutOfEnergyError`](plt_scheduler_interface::OutOfEnergyError) error must be returned by `execute_plt_transaction`.
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
pub fn execute_token_update_transaction<
    BSO: BlockStateOperations,
    TE: TransactionExecution<Account = BSO::Account>,
>(
    transaction_execution: &mut TE,
    block_state: &mut BSO,
    payload: TokenOperationsPayload,
) -> Result<TransactionOutcome, TransactionExecutionError> {
    // Charge energy
    if let Err(err) =
        transaction_execution.tick_energy(transactions::cost::PLT_OPERATIONS_TRANSACTIONS)
    {
        return Ok(TransactionOutcome::Rejected(TransactionRejectReason::from(
            err,
        )));
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
    let mut token_module_state = block_state.mutable_token_state_map(&token);
    let mut token_module_state_dirty = false;
    let mut kernel = TokenKernelOperationsImpl {
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
                block_state.set_token_state_map(&token, token_module_state);
            }

            // Return events
            Ok(TransactionOutcome::Success(events))
        }
        Err(TokenUpdateError::TokenModuleReject(reject_reason)) => {
            Ok(TransactionOutcome::Rejected(
                TransactionRejectReason::TokenModule(TokenModuleRejectReason {
                    // Use the canonical token id from the token configuration
                    token_id: token_configuration.token_id.clone(),
                    reason_type: reject_reason.reason_type,
                    details: reject_reason.details,
                }),
            ))
        }
        Err(TokenUpdateError::OutOfEnergy(_)) => Ok(TransactionOutcome::Rejected(
            TransactionRejectReason::OutOfEnergy,
        )),
        Err(TokenUpdateError::StateInvariantViolation(err)) => Err(
            TransactionExecutionError::TokenStateInvariantBroken(err.to_string()),
        ),
    }
}

/// Execute a protocol-level token create instruction modifying `block_state` accordingly.
/// Returns the events produced if successful.
///
/// NOTICE: The caller must ensure to rollback state changes in case an error is returned.
///
/// # Arguments
///
/// - `block_state` Block state that can be queried and updated during execution.
/// - `payload` The PLT create update instruction
///
/// # Errors
///
/// - [`UpdateInstructionExecutionError`] If executing the update instruction failed.
///   Returning this error will terminate the scheduler.
pub fn execute_create_plt_instruction<BSO: BlockStateOperations>(
    block_state: &mut BSO,
    payload: CreatePlt,
) -> Result<Vec<BlockItemEvent>, UpdateInstructionExecutionError> {
    // Check that token id is not already used (notice that token_by_id lookup is case-insensitive
    // as the check should be)
    if let Ok(existing_token) = block_state.token_by_id(&payload.token_id) {
        return Err(UpdateInstructionExecutionError::DuplicateTokenId(
            block_state.token_configuration(&existing_token).token_id,
        ));
    }

    // Check token module ref matches the implemented token module
    if payload.token_module != TOKEN_MODULE_REF {
        return Err(UpdateInstructionExecutionError::InvalidTokenModuleRef(
            payload.token_module,
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

    let mut token_module_state = block_state.mutable_token_state_map(&token);
    let mut token_module_state_dirty = false;
    let mut kernel = TokenKernelOperationsImpl {
        block_state,
        token: &token,
        token_configuration: &token_configuration,
        token_module_state: &mut token_module_state,
        token_module_state_dirty: &mut token_module_state_dirty,
        events: &mut events,
    };

    // Initialize token in token module
    let token_initialize_result =
        token_module::initialize_token(&mut kernel, payload.initialization_parameters);

    match token_initialize_result {
        Ok(()) => {
            // Increment protocol-level token update sequence number
            block_state.increment_plt_update_instruction_sequence_number();

            // Update token module state if dirty
            if token_module_state_dirty {
                block_state.set_token_state_map(&token, token_module_state);
            }

            // Return events
            Ok(events)
        }
        Err(err) => {
            Err(UpdateInstructionExecutionError::ModuleTokenInitializationFailed(err.to_string()))
        }
    }
}
