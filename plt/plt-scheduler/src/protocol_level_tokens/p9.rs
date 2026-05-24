use crate::block_state_polymorph::token::TokenPXRefMut;
use crate::scheduler::{ChainUpdateExecutionError, TransactionExecutionError};
use crate::token_module::TokenUpdateError;
use crate::transaction_execution::{OutOfEnergyError, TransactionExecution};
use crate::{TOKEN_MODULE_REF, token_module};
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    DeserializationFailureRejectReason, RawCbor, TokenId, TokenModuleInitializationParameters,
    TokenModuleRejectReason, TokenOperations, TokenOperationsPayload,
};
use concordium_base::transactions;
use concordium_base::updates::CreatePlt;
use plt_block_state::entity::accounts::Account;
use plt_block_state::entity::block_state::TokenNotFoundByIdError;
use plt_block_state::entity::block_state::p9::BlockStateP9;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::failure::BlockStateResult;
use plt_block_state::persistent::protocol_level_tokens::p9::TokenConfiguration;
use plt_block_state::utils;
use plt_scheduler_types::types::events::{BlockItemEvent, TokenCreateEvent};
use plt_scheduler_types::types::execution::{ChainUpdateOutcome, FailureKind, TransactionOutcome};
use plt_scheduler_types::types::queries::{
    TokenAccountInfo, TokenAccountState, TokenAuthorizations, TokenInfo, TokenState,
};
use plt_scheduler_types::types::reject_reasons::{
    EncodedTokenModuleRejectReason, TransactionRejectReason,
};
use plt_scheduler_types::types::tokens::TokenAmount;

/// Get the [`TokenId`]s of all protocol-level tokens registered on the chain.
pub fn query_plt_list<C: EntityContextTypes>(
    context: &EntityContext<C>,
    block_state: &BlockStateP9,
) -> BlockStateResult<Vec<TokenId>> {
    block_state.plt_list(context).collect()
}

/// Get the token state associated with the given token id.
pub fn query_token_info<C: EntityContextTypes>(
    context: &EntityContext<C>,
    block_state: &BlockStateP9,
    token_id: &TokenId,
) -> BlockStateResult<Result<TokenInfo, TokenNotFoundByIdError>> {
    let token = match block_state.token_by_id(context, token_id)? {
        Ok(token) => token,
        Err(err) => return Ok(Err(err)),
    };

    let token_configuration = token.token_base.token_configuration(context)?;
    let circulating_supply = token.token_base.token_circulating_supply();

    let total_supply = TokenAmount {
        amount: circulating_supply,
        decimals: token_configuration.decimals,
    };

    let module_state = token_module::query_token_module_state(context, &token.token_base)?;

    let token_state = TokenState {
        token_module_ref: token_configuration.module_ref,
        decimals: token_configuration.decimals,
        total_supply,
        module_state: RawCbor::from(cbor::cbor_encode(&module_state)),
    };

    let token_info = TokenInfo {
        // The token configuration contains the canonical token id specified in the original casing
        token_id: token_configuration.token_id,
        state: token_state,
    };

    Ok(Ok(token_info))
}

/// Get the list of tokens on an account
pub fn query_token_account_infos<C: EntityContextTypes>(
    context: &EntityContext<C>,
    block_state: &BlockStateP9,
    account: Account,
) -> BlockStateResult<Vec<TokenAccountInfo>> {
    account
        .token_account_states(context)
        .map(|(token_index, state)| {
            let token = block_state.token_by_index(context, token_index)?;
            let token_configuration = token.token_base.token_configuration(context)?;

            let module_state = token_module::query_token_module_account_state(
                context,
                &token.token_base,
                account.account_index(),
            )?;

            let balance = TokenAmount {
                amount: state.balance,
                decimals: token_configuration.decimals,
            };

            let account_state = TokenAccountState {
                balance,
                module_state: Some(RawCbor::from(cbor::cbor_encode(&module_state))),
            };

            Ok(TokenAccountInfo {
                token_id: token_configuration.token_id,
                account_state,
            })
        })
        .collect()
}

/// Get the authorizations of a token.
pub fn query_token_authorizations<C: EntityContextTypes>(
    context: &EntityContext<C>,
    block_state: &BlockStateP9,
    token_id: &TokenId,
) -> BlockStateResult<Result<TokenAuthorizations, TokenNotFoundByIdError>> {
    let token = match block_state.token_by_id(context, token_id)? {
        Ok(token) => token,
        Err(err) => return Ok(Err(err)),
    };

    let token_configuration = token.token_base.token_configuration(context)?;

    let details = concordium_base::protocol_level_tokens::TokenAuthorizations::default();

    Ok(Ok(TokenAuthorizations {
        token_id: token_configuration.token_id,
        details: RawCbor::from(cbor::cbor_encode(&details)),
    }))
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
pub fn execute_create_plt_chain_update<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    block_state: &mut BlockStateP9,
    payload: CreatePlt,
) -> Result<ChainUpdateOutcome, ChainUpdateExecutionError> {
    // Check that token id is not already used (notice that token_by_id lookup is case-insensitive
    // as the check should be)
    if let Ok(existing_token) = block_state.token_by_id(context, &payload.token_id)? {
        return Ok(ChainUpdateOutcome::Failed(FailureKind::DuplicateTokenId(
            existing_token
                .token_base
                .token_configuration(context)?
                .token_id,
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
    let token_index = block_state.create_token(context, token_configuration.clone())?;
    let mut token = block_state.token_by_index(context, token_index)?;

    let mut events = Vec::new();
    events.push(BlockItemEvent::TokenCreated(TokenCreateEvent {
        payload: payload.clone(),
    }));

    let initialization_parameters: TokenModuleInitializationParameters =
        match utils::cbor_decode(payload.initialization_parameters) {
            Ok(parameters) => parameters,
            Err(err) => {
                return Ok(ChainUpdateOutcome::Failed(
                    FailureKind::TokenInitializeFailure(format!(
                        "Could not decode token initialization parameters: {}",
                        err
                    )),
                ));
            }
        };

    // Initialize token in token module
    let token_initialize_result = token_module::initialize_token(
        context,
        &mut events,
        TokenPXRefMut::TokenP9(&mut token),
        &initialization_parameters,
    )?;

    match token_initialize_result {
        Ok(()) => {
            // Increment protocol-level token update sequence number
            block_state.increment_plt_update_instruction_sequence_number(context);

            // Write back the token
            block_state.update_token(context, token)?;

            // Return events
            Ok(ChainUpdateOutcome::Success(events))
        }
        Err(err) => Ok(ChainUpdateOutcome::Failed(
            FailureKind::TokenInitializeFailure(err.to_string()),
        )),
    }
}

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
pub fn execute_token_update_transaction<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    transaction_execution: &mut TransactionExecution,
    block_state: &mut BlockStateP9,
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
    let mut token = match block_state.token_by_id(context, &payload.token_id)? {
        Ok(token) => token,
        Err(TokenNotFoundByIdError(_)) => {
            return Ok(TransactionOutcome::Rejected(
                TransactionRejectReason::NonExistentTokenId(payload.token_id),
            ));
        }
    };
    let token_configuration = token.token_base.token_configuration(context)?;

    let mut events = Vec::new();

    // Decode operations
    let operations: TokenOperations = match utils::cbor_decode(payload.operations) {
        Ok(operations) => operations,
        Err(err) => {
            let reject_reason = TokenModuleRejectReason::DeserializationFailure(
                DeserializationFailureRejectReason {
                    cause: Some(err.to_string()),
                },
            );
            let (reason_type, cbor) = reject_reason.encode_reject_reason();
            return Ok(TransactionOutcome::Rejected(
                TransactionRejectReason::TokenUpdateTransactionFailed(
                    EncodedTokenModuleRejectReason {
                        // Use the canonical token id from the token configuration
                        token_id: token_configuration.token_id.clone(),
                        reason_type: reason_type.to_type_discriminator(),
                        details: Some(cbor),
                    },
                ),
            ));
        }
    };

    // Execute operations
    for (index, operation) in operations.operations.into_iter().enumerate() {
        match token_module::execute_token_update_operation_at_index(
            transaction_execution,
            context,
            &mut events,
            &mut TokenPXRefMut::TokenP9(&mut token),
            index,
            &operation,
        )? {
            Ok(()) => (),
            Err(TokenUpdateError::OutOfEnergy(_)) => {
                return Ok(TransactionOutcome::Rejected(
                    TransactionRejectReason::OutOfEnergy,
                ));
            }
            Err(TokenUpdateError::TokenModuleReject(reject_reason)) => {
                let (reason_type, cbor) = reject_reason.encode_reject_reason();
                return Ok(TransactionOutcome::Rejected(
                    TransactionRejectReason::TokenUpdateTransactionFailed(
                        EncodedTokenModuleRejectReason {
                            // Use the canonical token id from the token configuration
                            token_id: token_configuration.token_id.clone(),
                            reason_type: reason_type.to_type_discriminator(),
                            details: Some(cbor),
                        },
                    ),
                ));
            }
        };
    }

    // Write back the token
    block_state.update_token(context, token)?;

    // Return events
    Ok(TransactionOutcome::Success(events))
}
