use crate::token_kernel_interface::{
    AccountNotFoundByAddressError, InsufficientBalanceError, TokenKernelOperations,
    TokenStateInvariantError, TokenTransferError,
};
use crate::token_module;
use crate::token_module::TokenAmountDecimalsMismatchError;
use concordium_base::protocol_level_tokens::{TokenOperation, TokenTransfer};
use concordium_base::transactions::Memo;
use plt_scheduler_interface::{OutOfEnergyError, TransactionExecution};

/// Internal variant of `TokenUpdateError` where the reject reason is
/// not encoded as CBOR
#[derive(Debug, thiserror::Error)]
pub enum TokenUpdateErrorInternal {
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

pub fn execute_token_update_operation<
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
    let raw_amount = token_module::to_raw_token_amount(kernel, transfer_operation.amount)?;
    let receiver = kernel.account_by_address(&transfer_operation.recipient.address)?;

    kernel.transfer(
        &transaction_execution.sender_account(),
        &receiver,
        raw_amount,
        transfer_operation.memo.map(Memo::from),
    )?;
    Ok(())
}
