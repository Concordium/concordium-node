use crate::block_state_traits::accounts::AccountsT;
use crate::block_state_traits::token::TokenT;
use crate::protocol_level_tokens::token_module::errors::{
    InsufficientBalanceError, MintWouldOverflowError, TokenAmountDecimalsMismatchError,
};
use crate::protocol_level_tokens::token_module::{balance_operations, util};
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
use plt_block_state::entity::protocol_level_tokens::p9::TokenP9;
use plt_block_state::entity::protocol_level_tokens::p11::TokenP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::external::AccountNotFoundByAddressError;
use plt_block_state::failure::BlockStateResult;
use plt_scheduler_types::types::events::{BlockItemEvent, EncodedTokenModuleEvent};

/// Represents the reasons why [`execute_token_update_transaction`] can fail.
#[derive(Debug, thiserror::Error)]
pub enum TokenUpdateError {
    #[error("Token module rejection")]
    TokenModuleReject(TokenModuleRejectReason),
    #[error("{0}")]
    OutOfEnergy(#[from] OutOfEnergyError),
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
pub fn execute_token_update_operation_at_index<C: EntityContextTypes>(
    transaction_execution: &mut TransactionExecution,
    context: &mut EntityContext<C>,
    accounts: &impl AccountsT,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut impl TokenT,
    index: usize,
    operation: &TokenOperation,
) -> BlockStateResult<Result<(), TokenUpdateError>> {
    Ok(execute_token_update_operation_internal(
        transaction_execution,
        context,
        accounts,
        events,
        token,
        operation,
    )?
    .map_err(|err| match err {
        TokenUpdateErrorInternal::AccountDoesNotExist(err) => TokenUpdateError::TokenModuleReject(
            TokenModuleRejectReason::AddressNotFound(AddressNotFoundRejectReason {
                index: index as u64,
                address: CborHolderAccount::from(err.0),
            }),
        ),
        TokenUpdateErrorInternal::AmountDecimalsMismatch(err) => {
            TokenUpdateError::TokenModuleReject(TokenModuleRejectReason::DeserializationFailure(
                DeserializationFailureRejectReason {
                    cause: Some(err.to_string()),
                },
            ))
        }
        TokenUpdateErrorInternal::InsufficientBalance(err) => {
            let token_configuration = token.token_p9().token_configuration(context)?;

            TokenUpdateError::TokenModuleReject(TokenModuleRejectReason::TokenBalanceInsufficient(
                TokenBalanceInsufficientRejectReason {
                    index: index as u64,
                    available_balance: util::to_token_amount(&token_configuration, err.available),
                    required_balance: util::to_token_amount(&token_configuration, err.required),
                },
            ))
        }
        TokenUpdateErrorInternal::MintWouldOverflow(err) => {
            let token_configuration = token.token_p9().token_configuration(context)?;

            TokenUpdateError::TokenModuleReject(TokenModuleRejectReason::MintWouldOverflow(
                MintWouldOverflowRejectReason {
                    index: index as u64,
                    requested_amount: util::to_token_amount(
                        &token_configuration,
                        err.requested_amount,
                    ),
                    current_supply: util::to_token_amount(&token_configuration, err.current_supply),
                    max_representable_amount: util::to_token_amount(
                        &token_configuration,
                        err.max_representable_amount,
                    ),
                },
            ))
        }
        TokenUpdateErrorInternal::OutOfEnergy(err) => TokenUpdateError::OutOfEnergy(err),
        TokenUpdateErrorInternal::Paused => TokenUpdateError::TokenModuleReject(
            TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
                index: index as u64,
                address: None,
                reason: format!("token operation {} is paused", operation_name(operation))
                    .to_string()
                    .into(),
            }),
        ),
        TokenUpdateErrorInternal::OperationNotPermitted {
            account_address,
            reason,
        } => TokenUpdateError::TokenModuleReject(TokenModuleRejectReason::OperationNotPermitted(
            OperationNotPermittedRejectReason {
                index: index as u64,
                address: account_address.map(Into::into),
                reason: reason.to_string().into(),
            },
        )),
        TokenUpdateErrorInternal::UnsupportedOperation { reason } => {
            TokenUpdateError::TokenModuleReject(TokenModuleRejectReason::UnsupportedOperation(
                UnsupportedOperationRejectReason {
                    index: index as u64,
                    operation_type: operation_name(operation).to_string(),
                    reason: reason.to_string().into(),
                },
            ))
        }
    }))
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

fn execute_token_update_operation_internal<C: EntityContextTypes>(
    transaction_execution: &mut TransactionExecution,
    context: &mut EntityContext<C>,
    accounts: &impl AccountsT,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut impl TokenT,
    token_operation: &TokenOperation,
) -> BlockStateResult<Result<(), TokenUpdateErrorInternal>> {
    // Charge energy
    let energy_cost = energy_cost(token_operation);
    match transaction_execution.tick_energy(energy_cost) {
        Ok(()) => (),
        Err(err) => return Ok(Err(err.into())),
    };

    const WRONG_PROTOCOL_ERROR: TokenUpdateErrorInternal =
        TokenUpdateErrorInternal::UnsupportedOperation {
            reason: "Operation not supported at the current protocol version",
        };

    // Execute operation
    match token_operation {
        TokenOperation::Transfer(transfer) => execute_token_transfer(
            transaction_execution,
            context,
            accounts,
            events,
            token.token_p9_mut(),
            transfer,
        ),
        TokenOperation::Mint(mint) => {
            execute_token_mint(transaction_execution, context, events, token, mint)
        }
        TokenOperation::Burn(burn) => {
            execute_token_burn(transaction_execution, context, events, token, burn)
        }
        TokenOperation::Pause(_) => {
            execute_token_pause(transaction_execution, context, events, token)
        }
        TokenOperation::Unpause(_) => {
            execute_token_unpause(transaction_execution, context, events, token)
        }
        TokenOperation::AddAllowList(list_operation) => execute_add_allow_list(
            transaction_execution,
            context,
            accounts,
            events,
            token,
            list_operation,
        ),
        TokenOperation::RemoveAllowList(list_operation) => execute_remove_allow_list(
            transaction_execution,
            context,
            accounts,
            events,
            token,
            list_operation,
        ),
        TokenOperation::AddDenyList(list_operation) => execute_add_deny_list(
            transaction_execution,
            context,
            accounts,
            events,
            token,
            list_operation,
        ),
        TokenOperation::RemoveDenyList(list_operation) => execute_remove_deny_list(
            transaction_execution,
            context,
            accounts,
            events,
            token,
            list_operation,
        ),
        TokenOperation::AssignAdminRoles(operation) => {
            if let Some(token) = token.token_p11_mut() {
                execute_assign_admin_roles(
                    transaction_execution,
                    context,
                    accounts,
                    events,
                    token,
                    operation,
                )
            } else {
                Ok(Err(WRONG_PROTOCOL_ERROR))
            }
        }
        TokenOperation::RevokeAdminRoles(operation) => {
            if let Some(token) = token.token_p11_mut() {
                execute_revoke_admin_roles(
                    transaction_execution,
                    context,
                    accounts,
                    events,
                    token,
                    operation,
                )
            } else {
                Ok(Err(WRONG_PROTOCOL_ERROR))
            }
        }
        TokenOperation::UpdateMetadata(operation) => {
            if let Some(token) = token.token_p11_mut() {
                execute_update_metadata(transaction_execution, context, events, token, operation)
            } else {
                Ok(Err(WRONG_PROTOCOL_ERROR))
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

fn check_not_paused<C: EntityContextTypes>(
    context: &EntityContext<C>,
    token: &TokenP9,
) -> Result<(), TokenUpdateErrorInternal> {
    if token.is_paused(context) {
        return Err(TokenUpdateErrorInternal::Paused);
    }
    Ok(())
}

/// Ensure the sender account from the transaction context is authorized to perform the operation.
fn check_authorized<C: EntityContextTypes>(
    transaction_execution: &mut TransactionExecution,
    context: &mut EntityContext<C>,
    token: &impl TokenT,
    required_role: TokenAdminRole,
) -> BlockStateResult<Result<(), TokenUpdateErrorInternal>> {
    if let Some(token) = token.token_p11() {
        // Ensure the sender holds the specified role.
        let account_roles = token.get_account_roles(
            context,
            transaction_execution.sender_account().account_index(),
        )?;
        if !account_roles.has(required_role) {
            return Ok(Err(TokenUpdateErrorInternal::OperationNotPermitted {
                account_address: Some(transaction_execution.sender_account_address()),
                reason: "sender is not authorized to perform the operation for this token",
            }));
        }
    } else {
        let token = token.token_p9();

        // Ensure the sender is the governance account.
        if token.get_governance_account_index(context)?
            != transaction_execution.sender_account().account_index()
        {
            return Ok(Err(TokenUpdateErrorInternal::OperationNotPermitted {
                account_address: Some(transaction_execution.sender_account_address()),
                reason: "sender is not the token governance account",
            }));
        }
    }

    Ok(Ok(()))
}

fn execute_token_transfer<C: EntityContextTypes>(
    transaction_execution: &mut TransactionExecution,
    context: &mut EntityContext<C>,
    accounts: &impl AccountsT,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut TokenP9,
    transfer_operation: &TokenTransfer,
) -> BlockStateResult<Result<(), TokenUpdateErrorInternal>> {
    let token_configuration = token.token_configuration(context)?;

    // preprocessing
    let raw_amount =
        match util::to_raw_token_amount(&token_configuration, transfer_operation.amount) {
            Ok(amount) => amount,
            Err(err) => return Ok(Err(err.into())),
        };

    // operation execution
    match check_not_paused(context, token) {
        Ok(()) => (),
        Err(err) => return Ok(Err(err)),
    };

    let sender = transaction_execution.sender_account();
    let sender_address = transaction_execution.sender_account_address();
    let receiver = match accounts.account_by_address(context, &transfer_operation.recipient.address)
    {
        Ok(account) => account,
        Err(err) => return Ok(Err(err.into())),
    };

    if token.has_allow_list(context) {
        if !token.get_allow_list_for(context, sender.account_index()) {
            return Ok(Err(TokenUpdateErrorInternal::OperationNotPermitted {
                account_address: Some(sender_address),
                reason: "sender not in allow list",
            }));
        }
        if !token.get_allow_list_for(context, receiver.account_index()) {
            return Ok(Err(TokenUpdateErrorInternal::OperationNotPermitted {
                account_address: Some(transfer_operation.recipient.address),
                reason: "recipient not in allow list",
            }));
        }
    }

    if token.has_deny_list(context) {
        if token.get_deny_list_for(context, sender.account_index()) {
            return Ok(Err(TokenUpdateErrorInternal::OperationNotPermitted {
                account_address: Some(sender_address),
                reason: "sender in deny list",
            }));
        }
        if token.get_deny_list_for(context, receiver.account_index()) {
            return Ok(Err(TokenUpdateErrorInternal::OperationNotPermitted {
                account_address: Some(transfer_operation.recipient.address),
                reason: "recipient in deny list",
            }));
        }
    }

    match balance_operations::transfer(
        context,
        events,
        token.token_p9_mut(),
        sender,
        sender_address,
        &receiver,
        transfer_operation.recipient.address,
        raw_amount,
        transfer_operation.memo.clone().map(Memo::from),
    )? {
        Ok(()) => Ok(Ok(())),
        Err(err) => Ok(Err(err.into())),
    }
}

fn execute_token_mint<C: EntityContextTypes>(
    transaction_execution: &mut TransactionExecution,
    context: &mut EntityContext<C>,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut impl TokenT,
    mint_operation: &TokenSupplyUpdateDetails,
) -> BlockStateResult<Result<(), TokenUpdateErrorInternal>> {
    let token_configuration = token.token_p9().token_configuration(context)?;

    // preprocessing
    let raw_amount = match util::to_raw_token_amount(&token_configuration, mint_operation.amount) {
        Ok(amount) => amount,
        Err(err) => return Ok(Err(err.into())),
    };

    // operation execution
    if !token.token_p9().is_mintable(context) {
        return Ok(Err(TokenUpdateErrorInternal::UnsupportedOperation {
            reason: "feature not enabled",
        }));
    };
    match check_authorized(transaction_execution, context, token, TokenAdminRole::Mint)? {
        Ok(()) => (),
        Err(err) => return Ok(Err(err)),
    };
    match check_not_paused(context, token.token_p9()) {
        Ok(()) => (),
        Err(err) => return Ok(Err(err)),
    };

    match balance_operations::mint(
        context,
        events,
        token.token_p9_mut(),
        transaction_execution.sender_account(),
        transaction_execution.sender_account_address(),
        raw_amount,
    )? {
        Ok(()) => Ok(Ok(())),
        Err(err) => Ok(Err(err.into())),
    }
}

fn execute_token_burn<C: EntityContextTypes>(
    transaction_execution: &mut TransactionExecution,
    context: &mut EntityContext<C>,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut impl TokenT,
    burn_operation: &TokenSupplyUpdateDetails,
) -> BlockStateResult<Result<(), TokenUpdateErrorInternal>> {
    let token_configuration = token.token_p9().token_configuration(context)?;

    // preprocessing
    let raw_amount = match util::to_raw_token_amount(&token_configuration, burn_operation.amount) {
        Ok(amount) => amount,
        Err(err) => return Ok(Err(err.into())),
    };

    // operation execution
    if !token.token_p9().is_burnable(context) {
        return Ok(Err(TokenUpdateErrorInternal::UnsupportedOperation {
            reason: "feature not enabled",
        }));
    }
    match check_authorized(transaction_execution, context, token, TokenAdminRole::Burn)? {
        Ok(()) => (),
        Err(err) => return Ok(Err(err)),
    };
    match check_not_paused(context, token.token_p9()) {
        Ok(()) => (),
        Err(err) => return Ok(Err(err)),
    };

    match balance_operations::burn(
        context,
        events,
        token.token_p9_mut(),
        transaction_execution.sender_account(),
        transaction_execution.sender_account_address(),
        raw_amount,
    )? {
        Ok(()) => Ok(Ok(())),
        Err(err) => Ok(Err(err.into())),
    }
}

fn execute_token_pause<C: EntityContextTypes>(
    transaction_execution: &mut TransactionExecution,
    context: &mut EntityContext<C>,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut impl TokenT,
) -> BlockStateResult<Result<(), TokenUpdateErrorInternal>> {
    let token_configuration = token.token_p9().token_configuration(context)?;

    match check_authorized(transaction_execution, context, token, TokenAdminRole::Pause)? {
        Ok(()) => (),
        Err(err) => return Ok(Err(err)),
    };

    token.token_p9_mut().set_paused(context, true)?;

    let (event_type, details) = TokenModuleEvent::Pause(TokenPauseEventDetails {}).encode_event();
    events.extend(Some(BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
        token_id: token_configuration.token_id,
        event_type: event_type.to_type_discriminator(),
        details,
    })));

    Ok(Ok(()))
}

fn execute_token_unpause<C: EntityContextTypes>(
    transaction_execution: &mut TransactionExecution,
    context: &mut EntityContext<C>,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut impl TokenT,
) -> BlockStateResult<Result<(), TokenUpdateErrorInternal>> {
    let token_configuration = token.token_p9().token_configuration(context)?;

    match check_authorized(transaction_execution, context, token, TokenAdminRole::Pause)? {
        Ok(()) => (),
        Err(err) => return Ok(Err(err)),
    };

    token.token_p9_mut().set_paused(context, false)?;

    let (event_type, details) = TokenModuleEvent::Unpause(TokenPauseEventDetails {}).encode_event();
    events.extend(Some(BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
        token_id: token_configuration.token_id,
        event_type: event_type.to_type_discriminator(),
        details,
    })));

    Ok(Ok(()))
}

fn execute_add_allow_list<C: EntityContextTypes>(
    transaction_execution: &mut TransactionExecution,
    context: &mut EntityContext<C>,
    accounts: &impl AccountsT,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut impl TokenT,
    list_operation: &TokenListUpdateDetails,
) -> BlockStateResult<Result<(), TokenUpdateErrorInternal>> {
    let token_configuration = token.token_p9().token_configuration(context)?;

    if !token.token_p9_mut().has_allow_list(context) {
        return Ok(Err(TokenUpdateErrorInternal::UnsupportedOperation {
            reason: "feature not enabled",
        }));
    }
    match check_authorized(
        transaction_execution,
        context,
        token,
        TokenAdminRole::UpdateAllowList,
    )? {
        Ok(()) => (),
        Err(err) => return Ok(Err(err)),
    };

    let account = match accounts.account_by_address(context, &list_operation.target.address) {
        Ok(account) => account,
        Err(err) => return Ok(Err(err.into())),
    };
    account.touch_token_account(context, token.token_p9().token_index());
    token
        .token_p9_mut()
        .set_allow_list_for(context, account.account_index(), true)?;

    let event_details = TokenListUpdateEventDetails {
        target: list_operation.target.clone(),
    };
    let (event_type, details) = TokenModuleEvent::AddAllowList(event_details).encode_event();
    events.extend(Some(BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
        token_id: token_configuration.token_id,
        event_type: event_type.to_type_discriminator(),
        details,
    })));

    Ok(Ok(()))
}

fn execute_add_deny_list<C: EntityContextTypes>(
    transaction_execution: &mut TransactionExecution,
    context: &mut EntityContext<C>,
    accounts: &impl AccountsT,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut impl TokenT,
    list_operation: &TokenListUpdateDetails,
) -> BlockStateResult<Result<(), TokenUpdateErrorInternal>> {
    let token_configuration = token.token_p9().token_configuration(context)?;

    if !token.token_p9().has_deny_list(context) {
        return Ok(Err(TokenUpdateErrorInternal::UnsupportedOperation {
            reason: "feature not enabled",
        }));
    }
    match check_authorized(
        transaction_execution,
        context,
        token,
        TokenAdminRole::UpdateDenyList,
    )? {
        Ok(()) => (),
        Err(err) => return Ok(Err(err)),
    };

    let account = match accounts.account_by_address(context, &list_operation.target.address) {
        Ok(account) => account,
        Err(err) => return Ok(Err(err.into())),
    };
    account.touch_token_account(context, token.token_p9().token_index());
    token
        .token_p9_mut()
        .set_deny_list_for(context, account.account_index(), true)?;

    let event_details = TokenListUpdateEventDetails {
        target: list_operation.target.clone(),
    };
    let (event_type, details) = TokenModuleEvent::AddDenyList(event_details).encode_event();
    events.extend(Some(BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
        token_id: token_configuration.token_id,
        event_type: event_type.to_type_discriminator(),
        details,
    })));

    Ok(Ok(()))
}

fn execute_remove_allow_list<C: EntityContextTypes>(
    transaction_execution: &mut TransactionExecution,
    context: &mut EntityContext<C>,
    accounts: &impl AccountsT,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut impl TokenT,
    list_operation: &TokenListUpdateDetails,
) -> BlockStateResult<Result<(), TokenUpdateErrorInternal>> {
    let token_configuration = token.token_p9().token_configuration(context)?;

    if !token.token_p9().has_allow_list(context) {
        return Ok(Err(TokenUpdateErrorInternal::UnsupportedOperation {
            reason: "feature not enabled",
        }));
    }
    match check_authorized(
        transaction_execution,
        context,
        token,
        TokenAdminRole::UpdateAllowList,
    )? {
        Ok(()) => (),
        Err(err) => return Ok(Err(err)),
    };

    let account = match accounts.account_by_address(context, &list_operation.target.address) {
        Ok(account) => account,
        Err(err) => return Ok(Err(err.into())),
    };
    account.touch_token_account(context, token.token_p9().token_index());
    token
        .token_p9_mut()
        .set_allow_list_for(context, account.account_index(), false)?;

    let event_details = TokenListUpdateEventDetails {
        target: list_operation.target.clone(),
    };
    let (event_type, details) = TokenModuleEvent::RemoveAllowList(event_details).encode_event();
    events.extend(Some(BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
        token_id: token_configuration.token_id,
        event_type: event_type.to_type_discriminator(),
        details,
    })));

    Ok(Ok(()))
}

fn execute_remove_deny_list<C: EntityContextTypes>(
    transaction_execution: &mut TransactionExecution,
    context: &mut EntityContext<C>,
    accounts: &impl AccountsT,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut impl TokenT,
    list_operation: &TokenListUpdateDetails,
) -> BlockStateResult<Result<(), TokenUpdateErrorInternal>> {
    let token_configuration = token.token_p9().token_configuration(context)?;

    if !token.token_p9().has_deny_list(context) {
        return Ok(Err(TokenUpdateErrorInternal::UnsupportedOperation {
            reason: "feature not enabled",
        }));
    }
    match check_authorized(
        transaction_execution,
        context,
        token,
        TokenAdminRole::UpdateDenyList,
    )? {
        Ok(()) => (),
        Err(err) => return Ok(Err(err)),
    };

    let account = match accounts.account_by_address(context, &list_operation.target.address) {
        Ok(account) => account,
        Err(err) => return Ok(Err(err.into())),
    };
    account.touch_token_account(context, token.token_p9().token_index());
    token
        .token_p9_mut()
        .set_deny_list_for(context, account.account_index(), false)?;

    let event_details = TokenListUpdateEventDetails {
        target: list_operation.target.clone(),
    };
    let (event_type, details) = TokenModuleEvent::RemoveDenyList(event_details).encode_event();
    events.extend(Some(BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
        token_id: token_configuration.token_id,
        event_type: event_type.to_type_discriminator(),
        details,
    })));

    Ok(Ok(()))
}

fn check_roles_supported<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    token: &TokenP11,
    roles: &[TokenAdminRole],
) -> Result<(), TokenUpdateErrorInternal> {
    for role in roles {
        let supported = match role {
            TokenAdminRole::UpdateAdminRoles => true,
            TokenAdminRole::Mint => token.token_base.is_mintable(context),
            TokenAdminRole::Burn => token.token_base.is_burnable(context),
            TokenAdminRole::UpdateAllowList => token.token_base.has_allow_list(context),
            TokenAdminRole::UpdateDenyList => token.token_base.has_deny_list(context),
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

fn execute_assign_admin_roles<C: EntityContextTypes>(
    transaction_execution: &mut TransactionExecution,
    context: &mut EntityContext<C>,
    accounts: &impl AccountsT,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut TokenP11,
    operation: &TokenUpdateAdminRolesDetails,
) -> BlockStateResult<Result<(), TokenUpdateErrorInternal>> {
    let token_configuration = token.token_base.token_configuration(context)?;

    match check_authorized(
        transaction_execution,
        context,
        token,
        TokenAdminRole::UpdateAdminRoles,
    )? {
        Ok(()) => (),
        Err(err) => return Ok(Err(err)),
    };
    match check_roles_supported(context, token, &operation.roles) {
        Ok(()) => (),
        Err(err) => return Ok(Err(err)),
    };

    let account = match accounts.account_by_address(context, &operation.account.address) {
        Ok(account) => account,
        Err(err) => return Ok(Err(err.into())),
    };
    token.assign_account_roles(context, account.account_index(), &operation.roles)?;

    let event = TokenModuleEvent::AssignAdminRoles(TokenUpdateAdminRolesEventDetails {
        roles: operation.roles.clone(),
        account: operation.account.clone(),
    });
    let (event_type, details) = event.encode_event();
    events.extend(Some(BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
        token_id: token_configuration.token_id,
        event_type: event_type.to_type_discriminator(),
        details,
    })));

    Ok(Ok(()))
}

fn execute_revoke_admin_roles<C: EntityContextTypes>(
    transaction_execution: &mut TransactionExecution,
    context: &mut EntityContext<C>,
    accounts: &impl AccountsT,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut TokenP11,
    operation: &TokenUpdateAdminRolesDetails,
) -> BlockStateResult<Result<(), TokenUpdateErrorInternal>> {
    let token_configuration = token.token_base.token_configuration(context)?;

    match check_authorized(
        transaction_execution,
        context,
        token,
        TokenAdminRole::UpdateAdminRoles,
    )? {
        Ok(()) => (),
        Err(err) => return Ok(Err(err)),
    };
    match check_roles_supported(context, token, &operation.roles) {
        Ok(()) => (),
        Err(err) => return Ok(Err(err)),
    };

    let account = match accounts.account_by_address(context, &operation.account.address) {
        Ok(account) => account,
        Err(err) => return Ok(Err(err.into())),
    };
    if account.account_index() == transaction_execution.sender_account().account_index()
        && operation.roles.contains(&TokenAdminRole::UpdateAdminRoles)
    {
        return Ok(Err(TokenUpdateErrorInternal::OperationNotPermitted {
            account_address: Some(operation.account.address),
            reason: "Sender not allowed to remove own update-admin-role role",
        }));
    }
    token.revoke_account_roles(context, account.account_index(), &operation.roles)?;

    let event = TokenModuleEvent::RevokeAdminRoles(TokenUpdateAdminRolesEventDetails {
        roles: operation.roles.clone(),
        account: operation.account.clone(),
    });
    let (event_type, details) = event.encode_event();
    events.extend(Some(BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
        token_id: token_configuration.token_id,
        event_type: event_type.to_type_discriminator(),
        details,
    })));

    Ok(Ok(()))
}

fn execute_update_metadata<C: EntityContextTypes>(
    transaction_execution: &mut TransactionExecution,
    context: &mut EntityContext<C>,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut TokenP11,
    metadata_url: &MetadataUrl,
) -> BlockStateResult<Result<(), TokenUpdateErrorInternal>> {
    let token_configuration = token.token_base.token_configuration(context)?;

    if !metadata_url.additional.is_empty() {
        return Ok(Err(TokenUpdateErrorInternal::UnsupportedOperation {
            reason: "Unknown additional metadata fields",
        }));
    }
    match check_authorized(
        transaction_execution,
        context,
        token,
        TokenAdminRole::UpdateMetadata,
    )? {
        Ok(()) => (),
        Err(err) => return Ok(Err(err)),
    };
    token.token_base.set_metadata_url(context, metadata_url)?;
    let event = TokenModuleEvent::UpdateMetadata(TokenUpdateMetadataEventDetails {
        metadata_url: metadata_url.clone(),
    });
    let (event_type, details) = event.encode_event();
    events.extend(Some(BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
        token_id: token_configuration.token_id,
        event_type: event_type.to_type_discriminator(),
        details,
    })));

    Ok(Ok(()))
}
