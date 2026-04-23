use crate::token_context::TokenOperationContext;
use crate::token_module::errors::{
    InsufficientBalanceError, MintWouldOverflowError, TokenAmountDecimalsMismatchError,
    TokenBurnError, TokenMintError, TokenStateInvariantError, TokenTransferError,
};
use crate::token_module::{key_value_state, util};
use crate::transaction_execution::{OutOfEnergyError, TransactionExecution};
use concordium_base::base::Energy;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    AddressNotFoundRejectReason, CborHolderAccount, DeserializationFailureRejectReason,
    MetadataUrl, MintWouldOverflowRejectReason, OperationNotPermittedRejectReason, RawCbor,
    TokenAdminRole, TokenBalanceInsufficientRejectReason, TokenListUpdateDetails,
    TokenListUpdateEventDetails, TokenModuleCborTypeDiscriminator, TokenModuleEvent,
    TokenModuleRejectReason, TokenOperation, TokenPauseEventDetails, TokenSupplyUpdateDetails,
    TokenTransfer, TokenUpdateAdminRolesDetails, TokenUpdateAdminRolesEventDetails,
    TokenUpdateMetadataEventDetails, UnsupportedOperationRejectReason,
};
use concordium_base::transactions::Memo;
use plt_block_state::block_state_interface::{AccountNotFoundByAddressError, BlockStateOperations};

/// Details provided by the token module in the event of rejecting a
/// transaction.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct RejectReason {
    /// The type of the reject reason.
    pub reason_type: TokenModuleCborTypeDiscriminator,
    /// (Optional) CBOR-encoded details.
    pub details: Option<RawCbor>,
}

/// Represents the reasons why [`execute_token_update_transaction`] can fail.
#[derive(Debug, thiserror::Error)]
pub enum TokenUpdateError {
    #[error("Token module rejection")]
    TokenModuleReject(RejectReason),
    #[error("{0}")]
    OutOfEnergy(#[from] OutOfEnergyError),
    #[error("State invariant violation at token update: {0}")]
    StateInvariantViolation(#[from] TokenStateInvariantError),
}

/// Execute a token update transaction using the token context to
/// update state and produce events.
///
/// The caller must ensure to rollback state changes in case of
/// an error is returned.
///
/// The following checks and operations are performed:
///
/// - For each transfer operation:
///
///    - Check that the transfer amount is specified with the correct number of decimals.
///    - Tick energy required for the operation.
///    - Check that the module is not paused.
///    - Check that the recipient is valid.
///    - Check allow and deny list restrictions.
///    - Transfer the amount from the sender to the recipient, if the sender's balance is
///      sufficient (checked by the context).
///
/// - For each list update operation:
///
///    - Tick energy required for the operation.
///    - Check that the governance account is the sender.
///    - Check that the module configuration allows the list operation.
///    - Check that the account to add/remove exists on-chain.
///    - Add or remove the account to/from the list.
///
/// - For each mint operation:
///
///    - Check that the mint amount is specified with the correct number of decimals.
///    - Tick energy required for the operation.
///    - Check that the governance account is the sender.
///    - Check that the module is not paused.
///    - Check that the module configuration allows minting.
///    - Mint the amount to the sender, if the resulting circulating supply is
///      within representable range (checked by the context).
///
/// - For each burn operation:
///
///    - Check that the burn amount is specified with the correct number of decimals.
///    - Tick energy required for the operation.
///    - Check that the governance account is the sender.
///    - Check that the module is not paused.
///    - Check that the module configuration allows burning.
///    - Burn the amount from the sender, if the sender's balance is
///      sufficient (checked by the context).
///
/// - For each pause/unpause operation:
///
///    - Tick energy required for the operation
///    - Check that the governance account is the sender.
///    - Pause/unpause the token.
///
/// If the state stored in the token module contains data that breaks the invariants
/// maintained by the token module, the special error [`TokenUpdateError::StateInvariantViolation`]
/// is returned. This is an unrecoverable error and should never happen.
pub fn execute_token_update_transaction<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    context: &mut TokenOperationContext<'_, BSO>,
    token_operations: RawCbor,
) -> Result<(), TokenUpdateError> {
    let operations: Vec<TokenOperation> = util::cbor_decode(&token_operations).map_err(|err| {
        TokenUpdateError::TokenModuleReject(make_reject_reason(
            TokenModuleRejectReason::DeserializationFailure(DeserializationFailureRejectReason {
                cause: Some(err.to_string()),
            }),
        ))
    })?;

    for (index, operation) in operations.into_iter().enumerate() {
        execute_token_update_operation_at_index(transaction_execution, context, index, &operation)?;
    }
    Ok(())
}

/// Execute a token update operation using the token context to
/// update state and produce events.
///
/// The caller must ensure to rollback state changes in case of
/// an error is returned.
///
/// The following checks and operations are performed:
///
/// - For a transfer operation:
///
///    - Check that the transfer amount is specified with the correct number of decimals.
///    - Tick energy required for the operation.
///    - Check that the module is not paused.
///    - Check that the recipient is valid.
///    - Check allow and deny list restrictions.
///    - Transfer the amount from the sender to the recipient, if the sender's balance is
///      sufficient (checked by the context).
///
/// - For a list update operation:
///
///    - Tick energy required for the operation.
///    - Check that the governance account is the sender.
///    - Check that the module configuration allows the list operation.
///    - Check that the account to add/remove exists on-chain.
///    - Add or remove the account to/from the list.
///
/// - For a mint operation:
///
///    - Check that the mint amount is specified with the correct number of decimals.
///    - Tick energy required for the operation.
///    - Check that the governance account is the sender.
///    - Check that the module is not paused.
///    - Check that the module configuration allows minting.
///    - Mint the amount to the sender, if the resulting circulating supply is
///      within representable range (checked by the context).
///
/// - For a burn operation:
///
///    - Check that the burn amount is specified with the correct number of decimals.
///    - Tick energy required for the operation.
///    - Check that the governance account is the sender.
///    - Check that the module is not paused.
///    - Check that the module configuration allows burning.
///    - Burn the amount from the sender, if the sender's balance is
///      sufficient (checked by the context).
///
/// - For a pause/unpause operation:
///
///    - Tick energy required for the operation
///    - Check that the governance account is the sender.
///    - Pause/unpause the token.
///
/// If the state stored in the token module contains data that breaks the invariants
/// maintained by the token module, the special error [`TokenUpdateError::StateInvariantViolation`]
/// is returned. This is an unrecoverable error and should never happen.
///
///
/// # Arguments
///
/// - `transaction_execution`: the transaction execution context
/// - `context`: the token context operations interface
/// - `index`: the index of the operation in the transaction, used for error reporting
/// - `operation`: the token operation to execute
pub fn execute_token_update_operation_at_index<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    context: &mut TokenOperationContext<'_, BSO>,
    index: usize,
    operation: &TokenOperation,
) -> Result<(), TokenUpdateError> {
    execute_token_update_operation(transaction_execution, context, operation).map_err(|err| {
        match err {
            TokenUpdateErrorInternal::AccountDoesNotExist(err) => {
                TokenUpdateError::TokenModuleReject(make_reject_reason(
                    TokenModuleRejectReason::AddressNotFound(AddressNotFoundRejectReason {
                        index: index as u64,
                        address: CborHolderAccount::from(err.0),
                    }),
                ))
            }
            TokenUpdateErrorInternal::AmountDecimalsMismatch(err) => {
                TokenUpdateError::TokenModuleReject(make_reject_reason(
                    TokenModuleRejectReason::DeserializationFailure(
                        DeserializationFailureRejectReason {
                            cause: Some(err.to_string()),
                        },
                    ),
                ))
            }
            TokenUpdateErrorInternal::InsufficientBalance(err) => {
                TokenUpdateError::TokenModuleReject(make_reject_reason(
                    TokenModuleRejectReason::TokenBalanceInsufficient(
                        TokenBalanceInsufficientRejectReason {
                            index: index as u64,
                            available_balance: util::to_token_amount(
                                context.token_configuration,
                                err.available,
                            ),
                            required_balance: util::to_token_amount(
                                context.token_configuration,
                                err.required,
                            ),
                        },
                    ),
                ))
            }
            TokenUpdateErrorInternal::MintWouldOverflow(err) => {
                TokenUpdateError::TokenModuleReject(make_reject_reason(
                    TokenModuleRejectReason::MintWouldOverflow(MintWouldOverflowRejectReason {
                        index: index as u64,
                        requested_amount: util::to_token_amount(
                            context.token_configuration,
                            err.requested_amount,
                        ),
                        current_supply: util::to_token_amount(
                            context.token_configuration,
                            err.current_supply,
                        ),
                        max_representable_amount: util::to_token_amount(
                            context.token_configuration,
                            err.max_representable_amount,
                        ),
                    }),
                ))
            }
            TokenUpdateErrorInternal::OutOfEnergy(err) => TokenUpdateError::OutOfEnergy(err),
            TokenUpdateErrorInternal::StateInvariantViolation(err) => {
                TokenUpdateError::StateInvariantViolation(err)
            }
            TokenUpdateErrorInternal::Paused => TokenUpdateError::TokenModuleReject(
                make_reject_reason(TokenModuleRejectReason::OperationNotPermitted(
                    OperationNotPermittedRejectReason {
                        index: index as u64,
                        address: None,
                        reason: format!("token operation {} is paused", operation_name(operation))
                            .to_string()
                            .into(),
                    },
                )),
            ),
            TokenUpdateErrorInternal::OperationNotPermitted {
                account_address,
                reason,
            } => TokenUpdateError::TokenModuleReject(make_reject_reason(
                TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
                    index: index as u64,
                    address: account_address.map(Into::into),
                    reason: reason.to_string().into(),
                }),
            )),
            TokenUpdateErrorInternal::UnsupportedOperation { reason } => {
                TokenUpdateError::TokenModuleReject(make_reject_reason(
                    TokenModuleRejectReason::UnsupportedOperation(
                        UnsupportedOperationRejectReason {
                            index: index as u64,
                            operation_type: operation_name(operation).to_string(),
                            reason: reason.to_string().into(),
                        },
                    ),
                ))
            }
        }
    })
}

fn operation_name(operation: &TokenOperation) -> &'static str {
    match operation {
        TokenOperation::Transfer(_) => "transfer",
        TokenOperation::Mint(_) => "mint",
        TokenOperation::Burn(_) => "burn",
        TokenOperation::AddAllowList(_) => "addAllowList",
        TokenOperation::RemoveAllowList(_) => "removeAllowList",
        TokenOperation::AddDenyList(_) => "addDenyList",
        TokenOperation::RemoveDenyList(_) => "removeDenyList",
        TokenOperation::Pause(_) => "pause",
        TokenOperation::Unpause(_) => "unpause",
        TokenOperation::AssignAdminRoles(_) => "assignAdminRoles",
        TokenOperation::RevokeAdminRoles(_) => "revokeAdminRoles",
        TokenOperation::UpdateMetadata(_) => "updateMetadata",
    }
}

fn make_reject_reason(reject_reason: TokenModuleRejectReason) -> RejectReason {
    let (reason_type, cbor) = reject_reason.encode_reject_reason();
    RejectReason {
        reason_type: reason_type.to_type_discriminator(),
        details: Some(cbor),
    }
}

/// Internal variant of `TokenUpdateError` where the reject reason is
/// not encoded as CBOR
#[derive(Debug, thiserror::Error)]
enum TokenUpdateErrorInternal {
    #[error("The given account does not exist: {0}")]
    AccountDoesNotExist(#[from] AccountNotFoundByAddressError),
    #[error("The token amount has wrong number of decimals: {0}")]
    AmountDecimalsMismatch(#[from] TokenAmountDecimalsMismatchError),
    #[error("Insufficient balance on account: {0}")]
    InsufficientBalance(#[from] InsufficientBalanceError),
    #[error("Execution out of energy")]
    OutOfEnergy(#[from] OutOfEnergyError),
    #[error("{0}")]
    StateInvariantViolation(#[from] TokenStateInvariantError),
    #[error("{0}")]
    MintWouldOverflow(#[from] MintWouldOverflowError),
    #[error("The token is paused")]
    Paused,
    #[error("Operation not permitted: {reason}")]
    OperationNotPermitted {
        account_address: Option<AccountAddress>,
        reason: &'static str,
    },
    #[error("Operation not supported: {reason}")]
    UnsupportedOperation { reason: &'static str },
}

impl From<TokenTransferError> for TokenUpdateErrorInternal {
    fn from(err: TokenTransferError) -> Self {
        match err {
            TokenTransferError::StateInvariantViolation(err) => Self::StateInvariantViolation(err),
            TokenTransferError::InsufficientBalance(err) => Self::InsufficientBalance(err),
        }
    }
}

impl From<TokenMintError> for TokenUpdateErrorInternal {
    fn from(err: TokenMintError) -> Self {
        match err {
            TokenMintError::StateInvariantViolation(err) => Self::StateInvariantViolation(err),
            TokenMintError::MintWouldOverflow(err) => Self::MintWouldOverflow(err),
        }
    }
}

impl From<TokenBurnError> for TokenUpdateErrorInternal {
    fn from(err: TokenBurnError) -> Self {
        match err {
            TokenBurnError::StateInvariantViolation(err) => Self::StateInvariantViolation(err),
            TokenBurnError::InsufficientBalance(err) => Self::InsufficientBalance(err),
        }
    }
}

fn execute_token_update_operation<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    context: &mut TokenOperationContext<'_, BSO>,
    token_operation: &TokenOperation,
) -> Result<(), TokenUpdateErrorInternal> {
    // Charge energy
    let energy_cost = energy_cost(token_operation);
    transaction_execution.tick_energy(energy_cost)?;

    const WRONG_PROTOCOL_ERROR: TokenUpdateErrorInternal =
        TokenUpdateErrorInternal::UnsupportedOperation {
            reason: "Operation not supported at the current protocol version",
        };

    // Execute operation
    match token_operation {
        TokenOperation::Transfer(transfer) => {
            execute_token_transfer(transaction_execution, context, transfer)
        }
        TokenOperation::Mint(mint) => execute_token_mint(transaction_execution, context, mint),
        TokenOperation::Burn(burn) => execute_token_burn(transaction_execution, context, burn),
        TokenOperation::Pause(_) => execute_token_pause(transaction_execution, context),
        TokenOperation::Unpause(_) => execute_token_unpause(transaction_execution, context),
        TokenOperation::AddAllowList(list_operation) => {
            execute_add_allow_list(transaction_execution, context, list_operation)
        }
        TokenOperation::RemoveAllowList(list_operation) => {
            execute_remove_allow_list(transaction_execution, context, list_operation)
        }
        TokenOperation::AddDenyList(list_operation) => {
            execute_add_deny_list(transaction_execution, context, list_operation)
        }
        TokenOperation::RemoveDenyList(list_operation) => {
            execute_remove_deny_list(transaction_execution, context, list_operation)
        }
        TokenOperation::AssignAdminRoles(operation) => {
            if context.support_rbac() {
                execute_assign_admin_roles(transaction_execution, context, operation)
            } else {
                Err(WRONG_PROTOCOL_ERROR)
            }
        }
        TokenOperation::RevokeAdminRoles(operation) => {
            if context.support_rbac() {
                execute_revoke_admin_roles(transaction_execution, context, operation)
            } else {
                Err(WRONG_PROTOCOL_ERROR)
            }
        }
        TokenOperation::UpdateMetadata(operation) => {
            if context.support_updating_metadata() {
                execute_update_metadata(transaction_execution, context, operation)
            } else {
                Err(WRONG_PROTOCOL_ERROR)
            }
        }
    }
}

fn energy_cost(operation: &TokenOperation) -> Energy {
    use concordium_base::transactions::cost::*;

    match operation {
        TokenOperation::Transfer(_) => PLT_TRANSFER,
        TokenOperation::Mint(_) => PLT_MINT,
        TokenOperation::Burn(_) => PLT_BURN,
        TokenOperation::AddAllowList(_)
        | TokenOperation::RemoveAllowList(_)
        | TokenOperation::AddDenyList(_)
        | TokenOperation::RemoveDenyList(_) => PLT_LIST_UPDATE,
        TokenOperation::Pause(_) | TokenOperation::Unpause(_) => PLT_PAUSE,
        TokenOperation::AssignAdminRoles(_) | TokenOperation::RevokeAdminRoles(_) => {
            PLT_ASSIGN_REVOKE_ROLES
        }
        TokenOperation::UpdateMetadata(_) => PLT_UPDATE_TOKEN_METADATA,
    }
}

fn check_not_paused<BSO: BlockStateOperations>(
    context: &TokenOperationContext<'_, BSO>,
) -> Result<(), TokenUpdateErrorInternal> {
    if key_value_state::is_paused(context) {
        return Err(TokenUpdateErrorInternal::Paused);
    }
    Ok(())
}

/// Ensure the sender account from the transaction context is authorized to perform the operation.
fn check_authorized<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    context: &TokenOperationContext<'_, BSO>,
    required_role: TokenAdminRole,
) -> Result<(), TokenUpdateErrorInternal> {
    if context.support_rbac() {
        // Ensure the sender holds the specified role.
        let sender_index = context
            .block_state
            .account_index(transaction_execution.sender_account());
        let account_roles = key_value_state::get_account_roles(context, sender_index)?;
        if !account_roles.has(required_role) {
            return Err(TokenUpdateErrorInternal::OperationNotPermitted {
                account_address: Some(transaction_execution.sender_account_address()),
                reason: "sender is not authorized to perform the operation for this token",
            });
        }
    } else {
        // Ensure the sender is the governance account.
        let sender_index = context
            .block_state
            .account_index(transaction_execution.sender_account());
        let gov_index = key_value_state::get_governance_account_index(context)?;
        if gov_index != sender_index {
            return Err(TokenUpdateErrorInternal::OperationNotPermitted {
                account_address: Some(transaction_execution.sender_account_address()),
                reason: "sender is not the token governance account",
            });
        }
    }
    Ok(())
}

fn execute_token_transfer<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    context: &mut TokenOperationContext<'_, BSO>,
    transfer_operation: &TokenTransfer,
) -> Result<(), TokenUpdateErrorInternal> {
    // preprocessing
    let raw_amount =
        util::to_raw_token_amount(context.token_configuration, transfer_operation.amount)?;

    // operation execution
    check_not_paused(context)?;
    let sender = transaction_execution.sender_account();
    let sender_address = transaction_execution.sender_account_address();
    let receiver = context
        .block_state
        .account_by_address(&transfer_operation.recipient.address)?;

    if key_value_state::has_allow_list(context) {
        if !key_value_state::get_allow_list_for(context, context.block_state.account_index(sender))
        {
            return Err(TokenUpdateErrorInternal::OperationNotPermitted {
                account_address: Some(sender_address),
                reason: "sender not in allow list",
            });
        }
        if !key_value_state::get_allow_list_for(
            context,
            context.block_state.account_index(&receiver),
        ) {
            return Err(TokenUpdateErrorInternal::OperationNotPermitted {
                account_address: Some(transfer_operation.recipient.address),
                reason: "recipient not in allow list",
            });
        }
    }

    if key_value_state::has_deny_list(context) {
        if key_value_state::get_deny_list_for(context, context.block_state.account_index(sender)) {
            return Err(TokenUpdateErrorInternal::OperationNotPermitted {
                account_address: Some(sender_address),
                reason: "sender in deny list",
            });
        }
        if key_value_state::get_deny_list_for(context, context.block_state.account_index(&receiver))
        {
            return Err(TokenUpdateErrorInternal::OperationNotPermitted {
                account_address: Some(transfer_operation.recipient.address),
                reason: "recipient in deny list",
            });
        }
    }

    context.transfer(
        sender,
        sender_address,
        &receiver,
        transfer_operation.recipient.address,
        raw_amount,
        transfer_operation.memo.clone().map(Memo::from),
    )?;
    Ok(())
}

fn execute_token_mint<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    context: &mut TokenOperationContext<'_, BSO>,
    mint_operation: &TokenSupplyUpdateDetails,
) -> Result<(), TokenUpdateErrorInternal> {
    // preprocessing
    let raw_amount = util::to_raw_token_amount(context.token_configuration, mint_operation.amount)?;

    // operation execution
    if !key_value_state::is_mintable(context) {
        return Err(TokenUpdateErrorInternal::UnsupportedOperation {
            reason: "feature not enabled",
        });
    };
    check_authorized(transaction_execution, context, TokenAdminRole::Mint)?;
    check_not_paused(context)?;

    context.mint(
        transaction_execution.sender_account(),
        transaction_execution.sender_account_address(),
        raw_amount,
    )?;
    Ok(())
}

fn execute_token_burn<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    context: &mut TokenOperationContext<'_, BSO>,
    burn_operation: &TokenSupplyUpdateDetails,
) -> Result<(), TokenUpdateErrorInternal> {
    // preprocessing
    let raw_amount = util::to_raw_token_amount(context.token_configuration, burn_operation.amount)?;

    // operation execution
    if !key_value_state::is_burnable(context) {
        return Err(TokenUpdateErrorInternal::UnsupportedOperation {
            reason: "feature not enabled",
        });
    }
    check_authorized(transaction_execution, context, TokenAdminRole::Burn)?;
    check_not_paused(context)?;

    context.burn(
        transaction_execution.sender_account(),
        transaction_execution.sender_account_address(),
        raw_amount,
    )?;
    Ok(())
}

fn execute_token_pause<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    context: &mut TokenOperationContext<'_, BSO>,
) -> Result<(), TokenUpdateErrorInternal> {
    check_authorized(transaction_execution, context, TokenAdminRole::Pause)?;

    key_value_state::set_paused(context, true);

    let (event_type, details) = TokenModuleEvent::Pause(TokenPauseEventDetails {}).encode_event();
    context.log_token_event(event_type.to_type_discriminator(), details);
    Ok(())
}

fn execute_token_unpause<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    context: &mut TokenOperationContext<'_, BSO>,
) -> Result<(), TokenUpdateErrorInternal> {
    check_authorized(transaction_execution, context, TokenAdminRole::Pause)?;

    key_value_state::set_paused(context, false);

    let (event_type, details) = TokenModuleEvent::Unpause(TokenPauseEventDetails {}).encode_event();
    context.log_token_event(event_type.to_type_discriminator(), details);
    Ok(())
}

fn execute_add_allow_list<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    context: &mut TokenOperationContext<'_, BSO>,
    list_operation: &TokenListUpdateDetails,
) -> Result<(), TokenUpdateErrorInternal> {
    if !key_value_state::has_allow_list(context) {
        return Err(TokenUpdateErrorInternal::UnsupportedOperation {
            reason: "feature not enabled",
        });
    }
    check_authorized(
        transaction_execution,
        context,
        TokenAdminRole::UpdateAllowList,
    )?;
    let account = context
        .block_state
        .account_by_address(&list_operation.target.address)?;

    context
        .block_state
        .touch_token_account(context.token, &account);
    key_value_state::set_allow_list_for(context, context.block_state.account_index(&account), true);

    let event_details = TokenListUpdateEventDetails {
        target: list_operation.target.clone(),
    };
    let (event_type, details) = TokenModuleEvent::AddAllowList(event_details).encode_event();
    context.log_token_event(event_type.to_type_discriminator(), details);

    Ok(())
}

fn execute_add_deny_list<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    context: &mut TokenOperationContext<'_, BSO>,
    list_operation: &TokenListUpdateDetails,
) -> Result<(), TokenUpdateErrorInternal> {
    if !key_value_state::has_deny_list(context) {
        return Err(TokenUpdateErrorInternal::UnsupportedOperation {
            reason: "feature not enabled",
        });
    }
    check_authorized(
        transaction_execution,
        context,
        TokenAdminRole::UpdateDenyList,
    )?;

    let account = context
        .block_state
        .account_by_address(&list_operation.target.address)?;

    context
        .block_state
        .touch_token_account(context.token, &account);
    key_value_state::set_deny_list_for(context, context.block_state.account_index(&account), true);

    let event_details = TokenListUpdateEventDetails {
        target: list_operation.target.clone(),
    };
    let (event_type, details) = TokenModuleEvent::AddDenyList(event_details).encode_event();
    context.log_token_event(event_type.to_type_discriminator(), details);

    Ok(())
}

fn execute_remove_allow_list<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    context: &mut TokenOperationContext<'_, BSO>,
    list_operation: &TokenListUpdateDetails,
) -> Result<(), TokenUpdateErrorInternal> {
    if !key_value_state::has_allow_list(context) {
        return Err(TokenUpdateErrorInternal::UnsupportedOperation {
            reason: "feature not enabled",
        });
    }
    check_authorized(
        transaction_execution,
        context,
        TokenAdminRole::UpdateAllowList,
    )?;

    let account = context
        .block_state
        .account_by_address(&list_operation.target.address)?;

    context
        .block_state
        .touch_token_account(context.token, &account);
    key_value_state::set_allow_list_for(
        context,
        context.block_state.account_index(&account),
        false,
    );

    let event_details = TokenListUpdateEventDetails {
        target: list_operation.target.clone(),
    };
    let (event_type, details) = TokenModuleEvent::RemoveAllowList(event_details).encode_event();
    context.log_token_event(event_type.to_type_discriminator(), details);

    Ok(())
}

fn execute_remove_deny_list<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    context: &mut TokenOperationContext<'_, BSO>,
    list_operation: &TokenListUpdateDetails,
) -> Result<(), TokenUpdateErrorInternal> {
    if !key_value_state::has_deny_list(context) {
        return Err(TokenUpdateErrorInternal::UnsupportedOperation {
            reason: "feature not enabled",
        });
    }
    check_authorized(
        transaction_execution,
        context,
        TokenAdminRole::UpdateDenyList,
    )?;

    let account = context
        .block_state
        .account_by_address(&list_operation.target.address)?;

    context
        .block_state
        .touch_token_account(context.token, &account);
    key_value_state::set_deny_list_for(context, context.block_state.account_index(&account), false);

    let event_details = TokenListUpdateEventDetails {
        target: list_operation.target.clone(),
    };
    let (event_type, details) = TokenModuleEvent::RemoveDenyList(event_details).encode_event();
    context.log_token_event(event_type.to_type_discriminator(), details);

    Ok(())
}

fn check_roles_supported<BSO: BlockStateOperations>(
    context: &TokenOperationContext<'_, BSO>,
    roles: &[TokenAdminRole],
) -> Result<(), TokenUpdateErrorInternal> {
    for role in roles {
        let supported = match role {
            TokenAdminRole::UpdateAdminRoles => true,
            TokenAdminRole::Mint => key_value_state::is_mintable(context),
            TokenAdminRole::Burn => key_value_state::is_burnable(context),
            TokenAdminRole::UpdateAllowList => key_value_state::has_allow_list(context),
            TokenAdminRole::UpdateDenyList => key_value_state::has_deny_list(context),
            TokenAdminRole::Pause => true,
            TokenAdminRole::UpdateMetadata => true,
        };
        if !supported {
            return Err(TokenUpdateErrorInternal::UnsupportedOperation {
                reason: "feature using role is not enabled",
            });
        }
    }
    Ok(())
}

fn execute_assign_admin_roles<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    context: &mut TokenOperationContext<'_, BSO>,
    operation: &TokenUpdateAdminRolesDetails,
) -> Result<(), TokenUpdateErrorInternal> {
    check_authorized(
        transaction_execution,
        context,
        TokenAdminRole::UpdateAdminRoles,
    )?;
    check_roles_supported(context, &operation.roles)?;
    let account = context
        .block_state
        .account_by_address(&operation.account.address)?;
    key_value_state::assign_account_roles(
        context,
        context.block_state.account_index(&account),
        &operation.roles,
    )?;
    let event = TokenModuleEvent::AssignAdminRoles(TokenUpdateAdminRolesEventDetails {
        roles: operation.roles.clone(),
        account: operation.account.clone(),
    });
    let (event_type, details) = event.encode_event();
    context.log_token_event(event_type.to_type_discriminator(), details);
    Ok(())
}

fn execute_revoke_admin_roles<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    context: &mut TokenOperationContext<'_, BSO>,
    operation: &TokenUpdateAdminRolesDetails,
) -> Result<(), TokenUpdateErrorInternal> {
    check_authorized(
        transaction_execution,
        context,
        TokenAdminRole::UpdateAdminRoles,
    )?;
    check_roles_supported(context, &operation.roles)?;
    let account = context
        .block_state
        .account_by_address(&operation.account.address)?;
    let account_index = context.block_state.account_index(&account);
    if account_index
        == context
            .block_state
            .account_index(transaction_execution.sender_account())
        && operation.roles.contains(&TokenAdminRole::UpdateAdminRoles)
    {
        return Err(TokenUpdateErrorInternal::OperationNotPermitted {
            account_address: Some(operation.account.address),
            reason: "Sender not allowed to remove own update-admin-role role",
        });
    }
    key_value_state::revoke_account_roles(context, account_index, &operation.roles)?;
    let event = TokenModuleEvent::RevokeAdminRoles(TokenUpdateAdminRolesEventDetails {
        roles: operation.roles.clone(),
        account: operation.account.clone(),
    });
    let (event_type, details) = event.encode_event();
    context.log_token_event(event_type.to_type_discriminator(), details);
    Ok(())
}

fn execute_update_metadata<BSO: BlockStateOperations>(
    transaction_execution: &mut TransactionExecution<BSO::Account>,
    context: &mut TokenOperationContext<'_, BSO>,
    metadata_url: &MetadataUrl,
) -> Result<(), TokenUpdateErrorInternal> {
    if !metadata_url.additional.is_empty() {
        return Err(TokenUpdateErrorInternal::UnsupportedOperation {
            reason: "Unknown additional metadata fields",
        });
    }
    check_authorized(
        transaction_execution,
        context,
        TokenAdminRole::UpdateMetadata,
    )?;
    key_value_state::set_metadata_url(context, metadata_url);
    let event = TokenModuleEvent::UpdateMetadata(TokenUpdateMetadataEventDetails {
        metadata_url: metadata_url.clone(),
    });
    let (event_type, details) = event.encode_event();
    context.log_token_event(event_type.to_type_discriminator(), details);
    Ok(())
}
