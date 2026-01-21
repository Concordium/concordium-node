use crate::token_kernel_interface::{
    InsufficientBalanceError, TokenKernelOperations, TokenStateInvariantError, TokenTransferError,
};
use crate::token_module::TokenAmountDecimalsMismatchError;
use crate::util;
use concordium_base::protocol_level_tokens::{
    AddressNotFoundRejectReason, CborHolderAccount, DeserializationFailureRejectReason, RawCbor,
    TokenAmount, TokenBalanceInsufficientRejectReason, TokenModuleCborTypeDiscriminator,
    TokenModuleRejectReasonEnum, TokenOperation, TokenTransfer,
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
    #[error("{0}")]
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
        execute_token_update_operation(transaction_execution, kernel, operation).map_err(
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
                                available_balance: TokenAmount::from_raw(
                                    err.available.0,
                                    kernel.decimals(),
                                ),
                                required_balance: TokenAmount::from_raw(
                                    err.required.0,
                                    kernel.decimals(),
                                ),
                            },
                        ),
                    ))
                }
                TokenUpdateErrorInternal::OutOfEnergy(err) => TokenUpdateError::OutOfEnergy(err),
                TokenUpdateErrorInternal::StateInvariantViolation(err) => {
                    TokenUpdateError::StateInvariantViolation(err)
                }
            },
        )?;
    }
    Ok(())
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
}

impl From<TokenTransferError> for TokenUpdateErrorInternal {
    fn from(err: TokenTransferError) -> Self {
        match err {
            TokenTransferError::StateInvariantViolation(err) => Self::StateInvariantViolation(err),
            TokenTransferError::InsufficientBalance(err) => Self::InsufficientBalance(err),
        }
    }
}

fn execute_token_update_operation<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::Account>,
>(
    transaction_execution: &mut TE,
    kernel: &mut TK,
    token_operation: TokenOperation,
) -> Result<(), TokenUpdateErrorInternal> {
    match token_operation {
        TokenOperation::Transfer(transfer) => {
            execute_token_transfer(transaction_execution, kernel, transfer)
        }
        _ => todo!(),
    }
}

fn execute_token_transfer<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::Account>,
>(
    transaction_execution: &mut TE,
    kernel: &mut TK,
    transfer_operation: TokenTransfer,
) -> Result<(), TokenUpdateErrorInternal> {
    let raw_amount = util::to_raw_token_amount(kernel, transfer_operation.amount)?;
    let receiver = kernel.account_by_address(&transfer_operation.recipient.address)?;

    kernel.transfer(
        &transaction_execution.sender_account(),
        &receiver,
        raw_amount,
        transfer_operation.memo.map(Memo::from),
    )?;

    // todo implement allow/deny list checks https://linear.app/concordium/issue/PSR-24/implement-allow-and-deny-lists

    Ok(())
}
