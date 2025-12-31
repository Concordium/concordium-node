use crate::token_kernel_interface::{
    AccountNotFoundByAddressError, InsufficientBalanceError, OutOfEnergyError,
    TokenKernelOperations,
};
use crate::token_module;
use crate::token_module::{TokenAmountDecimalsMismatchError, TransactionContext};
use concordium_base::protocol_level_tokens::{TokenOperation, TokenTransfer};
use concordium_base::transactions::Memo;

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
}

pub fn execute_token_update_operation<Kernel: TokenKernelOperations>(
    kernel: &mut Kernel,
    context: &TransactionContext<Kernel::Account>,
    token_operation: TokenOperation,
) -> Result<(), TokenUpdateErrorInternal> {
    match token_operation {
        TokenOperation::Transfer(transfer) => execute_token_transfer(kernel, context, transfer),
        _ => todo!(),
    }
}

fn execute_token_transfer<Kernel: TokenKernelOperations>(
    kernel: &mut Kernel,
    context: &TransactionContext<Kernel::Account>,
    transfer_operation: TokenTransfer,
) -> Result<(), TokenUpdateErrorInternal> {
    let raw_amount = token_module::to_raw_token_amount(kernel, transfer_operation.amount)?;
    let receiver = kernel.account_by_address(&transfer_operation.recipient.address)?;

    kernel.transfer(
        &context.sender,
        &receiver,
        raw_amount,
        transfer_operation.memo.map(Memo::from),
    )?;
    Ok(())
}
