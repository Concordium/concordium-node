use crate::key_value_state;
use crate::token_module::TokenAmountDecimalsMismatchError;
use crate::util;
use concordium_base::base::Energy;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    AddressNotFoundRejectReason, CborHolderAccount, DeserializationFailureRejectReason,
    MintWouldOverflowRejectReason, OperationNotPermittedRejectReason, RawCbor,
    TokenBalanceInsufficientRejectReason, TokenListUpdateDetails, TokenListUpdateEventDetails,
    TokenModuleCborTypeDiscriminator, TokenModuleEvent, TokenModuleEventType,
    TokenModuleRejectReason, TokenOperation, TokenSupplyUpdateDetails, TokenTransfer,
};
use concordium_base::transactions::Memo;
use plt_scheduler_interface::error::{AccountNotFoundByAddressError, OutOfEnergyError};
use plt_scheduler_interface::token_kernel_interface::{
    InsufficientBalanceError, MintWouldOverflowError, TokenBurnError, TokenKernelOperations,
    TokenMintError, TokenStateInvariantError, TokenTransferError,
};
use plt_scheduler_interface::transaction_execution_interface::TransactionExecution;

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

/// Execute a token update transaction using the [`TokenKernelOperations`] to
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
///      sufficient (checked by the kernel).
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
///      within representable range (checked by the kernel).
///
/// - For each burn operation:
///
///    - Check that the burn amount is specified with the correct number of decimals.
///    - Tick energy required for the operation.
///    - Check that the governance account is the sender.
///    - Check that the module is not paused.
///    - Check that the module configuration allows burning.
///    - Burn the amount from the sender, if the sender's balance is
///      sufficient (checked by the kernel).
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
pub fn execute_token_update_transaction<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::AccountWithAddress>,
>(
    transaction_execution: &mut TE,
    kernel: &mut TK,
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
        execute_token_update_operation(transaction_execution, kernel, &operation).map_err(
            |err| match err {
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
                                available_balance: util::to_token_amount(kernel, err.available),
                                required_balance: util::to_token_amount(kernel, err.required),
                            },
                        ),
                    ))
                }
                TokenUpdateErrorInternal::MintWouldOverflow(err) => {
                    TokenUpdateError::TokenModuleReject(make_reject_reason(
                        TokenModuleRejectReason::MintWouldOverflow(MintWouldOverflowRejectReason {
                            index: index as u64,
                            requested_amount: util::to_token_amount(kernel, err.requested_amount),
                            current_supply: util::to_token_amount(kernel, err.current_supply),
                            max_representable_amount: util::to_token_amount(
                                kernel,
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
                            reason: format!(
                                "token operation {} is paused",
                                operation_name(&operation)
                            )
                            .to_string()
                            .into(),
                        },
                    )),
                ),
                TokenUpdateErrorInternal::OperationNotPermitted {
                    account_address,
                    reason,
                } => TokenUpdateError::TokenModuleReject(make_reject_reason(
                    TokenModuleRejectReason::OperationNotPermitted(
                        OperationNotPermittedRejectReason {
                            index: index as u64,
                            address: account_address.map(Into::into),
                            reason: reason.to_string().into(),
                        },
                    ),
                )),
            },
        )?;
    }
    Ok(())
}

fn operation_name(operation: &TokenOperation) -> &'static str {
    match operation {
        TokenOperation::Transfer(_) => "transfer",
        TokenOperation::Mint(_) => "mint",
        TokenOperation::Burn(_) => "burn",
        TokenOperation::AddAllowList(_) => "add-allow-list",
        TokenOperation::RemoveAllowList(_) => "remove-allow-list",
        TokenOperation::AddDenyList(_) => "add-deny-list",
        TokenOperation::RemoveDenyList(_) => "remove-deny-list",
        TokenOperation::Pause(_) => "pause",
        TokenOperation::Unpause(_) => "unpause",
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

fn execute_token_update_operation<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::AccountWithAddress>,
>(
    transaction_execution: &mut TE,
    kernel: &mut TK,
    token_operation: &TokenOperation,
) -> Result<(), TokenUpdateErrorInternal> {
    // Charge energy
    let energy_cost = energy_cost(token_operation);
    transaction_execution.tick_energy(energy_cost)?;

    // Execute operation
    match token_operation {
        TokenOperation::Transfer(transfer) => {
            execute_token_transfer(transaction_execution, kernel, transfer)
        }
        TokenOperation::Mint(mint) => execute_token_mint(transaction_execution, kernel, mint),
        TokenOperation::Burn(burn) => execute_token_burn(transaction_execution, kernel, burn),
        TokenOperation::Pause(_) => execute_token_pause(transaction_execution, kernel),
        TokenOperation::Unpause(_) => execute_token_unpause(transaction_execution, kernel),
        TokenOperation::AddAllowList(list_operation) => {
            execute_add_allow_list(transaction_execution, kernel, list_operation)
        }
        TokenOperation::RemoveAllowList(list_operation) => {
            execute_remove_allow_list(transaction_execution, kernel, list_operation)
        }
        TokenOperation::AddDenyList(list_operation) => {
            execute_add_deny_list(transaction_execution, kernel, list_operation)
        }
        TokenOperation::RemoveDenyList(list_operation) => {
            execute_remove_deny_list(transaction_execution, kernel, list_operation)
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
    }
}

fn check_not_paused<TK: TokenKernelOperations>(
    kernel: &TK,
) -> Result<(), TokenUpdateErrorInternal> {
    if key_value_state::is_paused(kernel) {
        return Err(TokenUpdateErrorInternal::Paused);
    }
    Ok(())
}

fn check_authorized<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::AccountWithAddress>,
>(
    transaction_execution: &mut TE,
    kernel: &TK,
) -> Result<(), TokenUpdateErrorInternal> {
    let sender_index = kernel.account_index(&transaction_execution.sender_account());
    let authorized = key_value_state::get_governance_account_index(kernel)
        .is_ok_and(|gov_index| gov_index == sender_index);

    if !authorized {
        return Err(TokenUpdateErrorInternal::OperationNotPermitted {
            account_address: Some(transaction_execution.sender_account_address()),
            reason: "sender is not the token governance account",
        });
    }
    Ok(())
}

fn execute_token_transfer<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::AccountWithAddress>,
>(
    transaction_execution: &mut TE,
    kernel: &mut TK,
    transfer_operation: &TokenTransfer,
) -> Result<(), TokenUpdateErrorInternal> {
    // preprocessing
    let raw_amount = util::to_raw_token_amount(kernel, transfer_operation.amount)?;

    // operation execution
    check_not_paused(kernel)?;
    let sender = transaction_execution.sender_account();
    let sender_address = transaction_execution.sender_account_address();
    let receiver = kernel.account_by_address(&transfer_operation.recipient.address)?;

    if key_value_state::has_allow_list(kernel) {
        if !key_value_state::get_allow_list_for(kernel, kernel.account_index(&sender)) {
            return Err(TokenUpdateErrorInternal::OperationNotPermitted {
                account_address: Some(sender_address),
                reason: "sender not in allow list",
            });
        }
        if !key_value_state::get_allow_list_for(kernel, kernel.account_index(&receiver)) {
            return Err(TokenUpdateErrorInternal::OperationNotPermitted {
                account_address: Some(transfer_operation.recipient.address),
                reason: "recipient not in allow list",
            });
        }
    }

    if key_value_state::has_deny_list(kernel) {
        if key_value_state::get_deny_list_for(kernel, kernel.account_index(&sender)) {
            return Err(TokenUpdateErrorInternal::OperationNotPermitted {
                account_address: Some(sender_address),
                reason: "sender in deny list",
            });
        }
        if key_value_state::get_deny_list_for(kernel, kernel.account_index(&receiver)) {
            return Err(TokenUpdateErrorInternal::OperationNotPermitted {
                account_address: Some(transfer_operation.recipient.address),
                reason: "recipient in deny list",
            });
        }
    }

    kernel.transfer(
        &sender,
        &receiver,
        raw_amount,
        transfer_operation.memo.clone().map(Memo::from),
    )?;
    Ok(())
}

fn execute_token_mint<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::AccountWithAddress>,
>(
    transaction_execution: &mut TE,
    kernel: &mut TK,
    mint_operation: &TokenSupplyUpdateDetails,
) -> Result<(), TokenUpdateErrorInternal> {
    // preprocessing
    let raw_amount = util::to_raw_token_amount(kernel, mint_operation.amount)?;

    // operation execution
    check_authorized(transaction_execution, kernel)?;
    check_not_paused(kernel)?;
    // TODO: check if feature is enabled as part of PSR-50

    kernel.mint(&transaction_execution.sender_account(), raw_amount)?;
    Ok(())
}

fn execute_token_burn<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::AccountWithAddress>,
>(
    transaction_execution: &mut TE,
    kernel: &mut TK,
    burn_operation: &TokenSupplyUpdateDetails,
) -> Result<(), TokenUpdateErrorInternal> {
    // preprocessing
    let raw_amount = util::to_raw_token_amount(kernel, burn_operation.amount)?;

    // operation execution
    check_authorized(transaction_execution, kernel)?;
    check_not_paused(kernel)?;
    // TODO: check if feature is enabled as part of PSR-50

    kernel.burn(&transaction_execution.sender_account(), raw_amount)?;
    Ok(())
}

fn execute_token_pause<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::AccountWithAddress>,
>(
    transaction_execution: &mut TE,
    kernel: &mut TK,
) -> Result<(), TokenUpdateErrorInternal> {
    check_authorized(transaction_execution, kernel)?;

    key_value_state::set_paused(kernel, true);

    let event_type = TokenModuleEventType::Pause.to_type_discriminator();
    kernel.log_token_event(event_type, vec![].into());
    Ok(())
}

fn execute_token_unpause<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::AccountWithAddress>,
>(
    transaction_execution: &mut TE,
    kernel: &mut TK,
) -> Result<(), TokenUpdateErrorInternal> {
    check_authorized(transaction_execution, kernel)?;

    key_value_state::set_paused(kernel, false);

    let event_type = TokenModuleEventType::Unpause.to_type_discriminator();
    kernel.log_token_event(event_type, vec![].into());
    Ok(())
}

fn execute_add_allow_list<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::AccountWithAddress>,
>(
    transaction_execution: &mut TE,
    kernel: &mut TK,
    list_operation: &TokenListUpdateDetails,
) -> Result<(), TokenUpdateErrorInternal> {
    check_authorized(transaction_execution, kernel)?;
    // TODO: check if feature is enabled as part of PSR-50
    let account = kernel.account_by_address(&list_operation.target.address)?;

    kernel.touch_account(&account);
    key_value_state::set_allow_list_for(kernel, kernel.account_index(&account), true);

    let event_details = TokenListUpdateEventDetails {
        target: list_operation.target.clone(),
    };
    let (event_type, details) = TokenModuleEvent::AddAllowList(event_details).encode_event();
    kernel.log_token_event(event_type.to_type_discriminator(), details);

    Ok(())
}

fn execute_add_deny_list<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::AccountWithAddress>,
>(
    transaction_execution: &mut TE,
    kernel: &mut TK,
    list_operation: &TokenListUpdateDetails,
) -> Result<(), TokenUpdateErrorInternal> {
    check_authorized(transaction_execution, kernel)?;
    // TODO: check if feature is enabled as part of PSR-50
    let account = kernel.account_by_address(&list_operation.target.address)?;

    kernel.touch_account(&account);
    key_value_state::set_deny_list_for(kernel, kernel.account_index(&account), true);

    let event_details = TokenListUpdateEventDetails {
        target: list_operation.target.clone(),
    };
    let (event_type, details) = TokenModuleEvent::AddDenyList(event_details).encode_event();
    kernel.log_token_event(event_type.to_type_discriminator(), details);

    Ok(())
}

fn execute_remove_allow_list<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::AccountWithAddress>,
>(
    transaction_execution: &mut TE,
    kernel: &mut TK,
    list_operation: &TokenListUpdateDetails,
) -> Result<(), TokenUpdateErrorInternal> {
    check_authorized(transaction_execution, kernel)?;
    // TODO: check if feature is enabled as part of PSR-50
    let account = kernel.account_by_address(&list_operation.target.address)?;

    kernel.touch_account(&account);
    key_value_state::set_allow_list_for(kernel, kernel.account_index(&account), false);

    let event_details = TokenListUpdateEventDetails {
        target: list_operation.target.clone(),
    };
    let (event_type, details) = TokenModuleEvent::RemoveAllowList(event_details).encode_event();
    kernel.log_token_event(event_type.to_type_discriminator(), details);

    Ok(())
}
fn execute_remove_deny_list<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::AccountWithAddress>,
>(
    transaction_execution: &mut TE,
    kernel: &mut TK,
    list_operation: &TokenListUpdateDetails,
) -> Result<(), TokenUpdateErrorInternal> {
    check_authorized(transaction_execution, kernel)?;
    // TODO: check if feature is enabled as part of PSR-50
    let account = kernel.account_by_address(&list_operation.target.address)?;

    kernel.touch_account(&account);
    key_value_state::set_deny_list_for(kernel, kernel.account_index(&account), false);

    let event_details = TokenListUpdateEventDetails {
        target: list_operation.target.clone(),
    };
    let (event_type, details) = TokenModuleEvent::RemoveDenyList(event_details).encode_event();
    kernel.log_token_event(event_type.to_type_discriminator(), details);

    Ok(())
}
