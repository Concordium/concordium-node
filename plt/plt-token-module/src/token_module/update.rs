use crate::token_kernel_interface::{
    AccountNotFoundByAddressError, InsufficientBalanceError, MintWouldOverflowError,
    TokenBurnError, TokenKernelOperations, TokenMintError, TokenStateInvariantError,
    TokenTransferError,
};
use crate::token_module::TokenAmountDecimalsMismatchError;
use crate::util;
use concordium_base::protocol_level_tokens::{
    AddressNotFoundRejectReason, CborHolderAccount, DeserializationFailureRejectReason,
    MintWouldOverflowRejectReason, RawCbor, TokenBalanceInsufficientRejectReason,
    TokenModuleCborTypeDiscriminator, TokenModuleRejectReasonEnum, TokenOperation,
    TokenSupplyUpdateDetails, TokenTransfer,
};
use concordium_base::transactions::Memo;
use plt_scheduler_interface::{OutOfEnergyError, TransactionExecution};

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

/// Execute a token update transaction using the [`TokenKernelOperations`] implementation on `host` to
/// update state and produce events.
///
/// When resulting in an `Err` signals a rejected operation and all of the calls to
/// [`TokenKernelOperations`] must be rolled back y the caller.
///
/// The process is as follows:
///
/// - Decode the transaction CBOR parameter.
/// - Check that amounts are within the representable range.
/// - For each transfer operation:
///
///    - Check that the module is not paused.
///    - Check that the recipient is valid.
///    - Check allowList/denyList restrictions.
///    - Transfer the amount from the sender to the recipient, if the sender's balance is
///      sufficient.
///
/// - For each list update operation:
///
///    - Check that the governance account is the sender.
///    - Check that the module configuration allows the list operation.
///    - Check that the account to add/remove exists on-chain.
///
/// - For each mint operation:
///
///    - Check that the governance account is the sender.
///    - Check that the module is not paused.
///    - Check that the module configuration allows minting.
///    - Check that the minting process was successful.
///
/// - For each burn operation:
///
///    - Check that the governance account is the sender.
///    - Check that the module is not paused.
///    - Check that the module configuration allows burning.
///    - Check that the burning process was successful.
///
/// - For each pause/unpause operation:
///
///     - Check that the governance account is the sender.
///
/// # INVARIANTS:
///
///   - Token module state contains a correctly encoded governance account address.
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
                                current_supply: util::to_token_amount(
                                    kernel,
                                    err.circulating_supply,
                                ),
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
    #[error("{0}")]
    MintWouldOverflow(#[from] MintWouldOverflowError),
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
    token_operation: TokenOperation,
) -> Result<(), TokenUpdateErrorInternal> {
    match token_operation {
        TokenOperation::Transfer(transfer) => {
            execute_token_transfer(transaction_execution, kernel, transfer)
        }
        TokenOperation::Mint(mint) => execute_token_mint(transaction_execution, kernel, mint),
        TokenOperation::Burn(burn) => execute_token_burn(transaction_execution, kernel, burn),
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
    Ok(())
}

fn execute_token_mint<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::Account>,
>(
    transaction_execution: &mut TE,
    kernel: &mut TK,
    mint_operation: TokenSupplyUpdateDetails,
) -> Result<(), TokenUpdateErrorInternal> {
    let raw_amount = util::to_raw_token_amount(kernel, mint_operation.amount)?;

    kernel.mint(&transaction_execution.sender_account(), raw_amount)?;
    Ok(())
}

fn execute_token_burn<
    TK: TokenKernelOperations,
    TE: TransactionExecution<Account = TK::Account>,
>(
    transaction_execution: &mut TE,
    kernel: &mut TK,
    burn_operation: TokenSupplyUpdateDetails,
) -> Result<(), TokenUpdateErrorInternal> {
    let raw_amount = util::to_raw_token_amount(kernel, burn_operation.amount)?;

    kernel.burn(&transaction_execution.sender_account(), raw_amount)?;
    Ok(())
}
