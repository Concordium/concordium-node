use crate::module_state::{self, KernelOperationsExt, STATE_KEY_PAUSED};
use crate::token_kernel_interface::{
    InsufficientBalanceError, MintWouldOverflowError, TokenBurnError, TokenKernelOperations,
    TokenMintError, TokenStateInvariantError, TokenTransferError,
};
use crate::token_module::TokenAmountDecimalsMismatchError;
use crate::util;
use concordium_base::base::Energy;
use concordium_base::protocol_level_tokens::{
    AddressNotFoundRejectReason, CborHolderAccount, DeserializationFailureRejectReason,
    MintWouldOverflowRejectReason, OperationNotPermittedRejectReason, RawCbor,
    TokenBalanceInsufficientRejectReason, TokenModuleCborTypeDiscriminator, TokenModuleEventType,
    TokenModuleRejectReasonEnum, TokenOperation, TokenSupplyUpdateDetails, TokenTransfer,
};
use concordium_base::transactions::Memo;
use plt_scheduler_interface::{
    AccountNotFoundByAddressError, OutOfEnergyError, TransactionExecution,
};

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
///    - Check that the module is not paused.
///    - Check that the recipient is valid.
///    - Check allow and deny list restrictions.
///    - Check that the transfer amount is specified with the correct number of decimals.
///    - Transfer the amount from the sender to the recipient, if the sender's balance is
///      sufficient (checked by the kernel).
///
/// - For each list update operation:
///
///    - Check that the governance account is the sender.
///    - Check that the module configuration allows the list operation.
///    - Check that the account to add/remove exists on-chain.
///    - Add or remove the account to/from the list.
///
/// - For each mint operation:
///
///    - Check that the governance account is the sender.
///    - Check that the module is not paused.
///    - Check that the module configuration allows minting.
///    - Check that the mint amount is specified with the correct number of decimals.
///    - Mint the amount to the sender, if the resulting circulating supply is
///      within representable range (checked by the kernel).
///
/// - For each burn operation:
///
///    - Check that the governance account is the sender.
///    - Check that the module is not paused.
///    - Check that the module configuration allows burning.
///    - Check that the burn amount is specified with the correct number of decimals.
///    - Burn the amount from the sender, if the sender's balance is
///      sufficient (checked by the kernel).
///
/// - For each pause/unpause operation:
///
///     - Check that the governance account is the sender.
///     - Pause/unpause the token.
///
/// If the state stored in the token module contains data that breaks the invariants
/// maintained by the token module, the special error [`TokenUpdateError::StateInvariantViolation`]
/// is returned. This is an unrecoverable error and should never happen.
pub fn execute_token_update_transaction<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::Account>,
>(
    transaction_execution: &mut TE,
    kernel: &mut TK,
    token_operations: RawCbor,
) -> Result<(), TokenUpdateError> {
    let operations: Vec<TokenOperation> = util::cbor_decode(&token_operations).map_err(|err| {
        TokenUpdateError::TokenModuleReject(make_reject_reason(
            TokenModuleRejectReasonEnum::DeserializationFailure(
                DeserializationFailureRejectReason {
                    cause: Some(err.to_string()),
                },
            ),
        ))
    })?;

    for (index, operation) in operations.into_iter().enumerate() {
        execute_token_update_operation(transaction_execution, kernel, &operation).map_err(
            |err| match err {
                TokenUpdateErrorInternal::AccountDoesNotExist(err) => {
                    TokenUpdateError::TokenModuleReject(make_reject_reason(
                        TokenModuleRejectReasonEnum::AddressNotFound(AddressNotFoundRejectReason {
                            index: index as u64,
                            address: CborHolderAccount::from(err.0),
                        }),
                    ))
                }
                TokenUpdateErrorInternal::AmountDecimalsMismatch(err) => {
                    TokenUpdateError::TokenModuleReject(make_reject_reason(
                        TokenModuleRejectReasonEnum::DeserializationFailure(
                            DeserializationFailureRejectReason {
                                cause: Some(err.to_string()),
                            },
                        ),
                    ))
                }
                TokenUpdateErrorInternal::InsufficientBalance(err) => {
                    TokenUpdateError::TokenModuleReject(make_reject_reason(
                        TokenModuleRejectReasonEnum::TokenBalanceInsufficient(
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
                        TokenModuleRejectReasonEnum::MintWouldOverflow(
                            MintWouldOverflowRejectReason {
                                index: index as u64,
                                requested_amount: util::to_token_amount(
                                    kernel,
                                    err.requested_amount,
                                ),
                                current_supply: util::to_token_amount(kernel, err.current_supply),
                                max_representable_amount: util::to_token_amount(
                                    kernel,
                                    err.max_representable_amount,
                                ),
                            },
                        ),
                    ))
                }
                TokenUpdateErrorInternal::OutOfEnergy(err) => TokenUpdateError::OutOfEnergy(err),
                TokenUpdateErrorInternal::StateInvariantViolation(err) => {
                    TokenUpdateError::StateInvariantViolation(err)
                }
                TokenUpdateErrorInternal::Paused => TokenUpdateError::TokenModuleReject(
                    make_reject_reason(TokenModuleRejectReasonEnum::OperationNotPermitted(
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
            },
        )?;
    }
    Ok(())
}

fn operation_name(operation: &TokenOperation) -> String {
    match operation {
        TokenOperation::Transfer(_) => "transfer".to_string(),
        TokenOperation::Mint(_) => "mint".to_string(),
        TokenOperation::Burn(_) => "burn".to_string(),
        TokenOperation::AddAllowList(_) => "add-allow-list".to_string(),
        TokenOperation::RemoveAllowList(_) => "remove-allow-list".to_string(),
        TokenOperation::AddDenyList(_) => "add-deny-list".to_string(),
        TokenOperation::RemoveDenyList(_) => "remove-deny-list".to_string(),
        TokenOperation::Pause(_) => "pause".to_string(),
        TokenOperation::Unpause(_) => "unpause".to_string(),
    }
}

fn make_reject_reason(reject_reason: TokenModuleRejectReasonEnum) -> RejectReason {
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
    TE: TransactionExecution<Account = TK::Account>,
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
        _ => todo!(),
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
    if module_state::is_paused(kernel) {
        return Err(TokenUpdateErrorInternal::Paused);
    }
    Ok(())
}

fn execute_token_transfer<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::Account>,
>(
    transaction_execution: &mut TE,
    kernel: &mut TK,
    transfer_operation: &TokenTransfer,
) -> Result<(), TokenUpdateErrorInternal> {
    // preprocessing
    let raw_amount = util::to_raw_token_amount(kernel, transfer_operation.amount)?;

    // operation execution
    let receiver = kernel.account_by_address(&transfer_operation.recipient.address)?;
    check_not_paused(kernel)?;
    // todo implement allow/deny list checks https://linear.app/concordium/issue/PSR-24/implement-allow-and-deny-lists

    kernel.transfer(
        &transaction_execution.sender_account(),
        &receiver,
        raw_amount,
        transfer_operation.memo.clone().map(Memo::from),
    )?;
    Ok(())
}

fn execute_token_mint<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::Account>,
>(
    transaction_execution: &mut TE,
    kernel: &mut TK,
    mint_operation: &TokenSupplyUpdateDetails,
) -> Result<(), TokenUpdateErrorInternal> {
    // preprocessing
    let raw_amount = util::to_raw_token_amount(kernel, mint_operation.amount)?;

    // operation execution
    check_not_paused(kernel)?;

    kernel.mint(&transaction_execution.sender_account(), raw_amount)?;
    Ok(())
}

fn execute_token_burn<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::Account>,
>(
    transaction_execution: &mut TE,
    kernel: &mut TK,
    burn_operation: &TokenSupplyUpdateDetails,
) -> Result<(), TokenUpdateErrorInternal> {
    // preprocessing
    let raw_amount = util::to_raw_token_amount(kernel, burn_operation.amount)?;

    // operation execution
    check_not_paused(kernel)?;

    kernel.burn(&transaction_execution.sender_account(), raw_amount)?;
    Ok(())
}

fn execute_token_pause<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::Account>,
>(
    _transaction_execution: &mut TE,
    kernel: &mut TK,
) -> Result<(), TokenUpdateErrorInternal> {
    // let gov_account = kernel
    //     .get_module_state(STATE_KEY_GOVERNANCE_ACCOUNT)
    //     .unwrap(); // TODO: how do we handle the case of a missing governance account?
    // let gov_account = AccountAddress::try_from(gov_account.as_slice());
    // TODO: authorization implemented as part of PSR-26

    kernel.set_module_state(STATE_KEY_PAUSED, Some(vec![]));

    let event_type = TokenModuleEventType::Pause.to_type_discriminator();
    kernel.log_token_event(event_type, vec![].into());
    Ok(())
}

fn execute_token_unpause<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::Account>,
>(
    _transaction_execution: &mut TE,
    kernel: &mut TK,
) -> Result<(), TokenUpdateErrorInternal> {
    // TODO: authorization implemented as part of PSR-26
    kernel.set_module_state(STATE_KEY_PAUSED, None);

    let event_type = TokenModuleEventType::Unpause.to_type_discriminator();
    kernel.log_token_event(event_type, vec![].into());
    Ok(())
}
