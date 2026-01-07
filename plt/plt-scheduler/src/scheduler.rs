//! General scheduler logic that is not specific to protocol-level tokens

use crate::block_state_interface::BlockStateOperations;
use crate::plt_scheduler;
use crate::plt_scheduler::{TokenSupplyUpdateEvent, TokenTransferEvent};
use crate::scheduler_interface::{OutOfEnergyError, TransactionExecution};
use concordium_base::base::Energy;
use concordium_base::protocol_level_tokens::TokenId;
use concordium_base::transactions::Payload;
use concordium_base::updates::UpdatePayload;
use plt_token_module::token_kernel_interface::TokenModuleEvent;
use plt_token_module::token_module::TokenModuleRejectReason;

/// A reason for why a transaction was rejected. Rejected means included in a
/// block, but the desired action was not achieved. The only effect of a
/// rejected transaction is paying for the energy used.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TransactionRejectReason {
    /// We ran of out energy to process this transaction.
    OutOfEnergy,
    /// The provided identifier does not match a token currently on chain.
    NonExistentTokenId(TokenId),
    /// The token module rejected the transaction.
    TokenModule(TokenModuleRejectReason),
}

/// Token event. This is an observable effect on the token state.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TransactionEvent {
    /// An event emitted by the token module.
    TokenModule(TokenModuleEvent),
    /// An event emitted when a transfer of tokens is performed.
    TokenTransfer(TokenTransferEvent),
    /// An event emitted when the token supply is updated by minting tokens to a
    /// token holder.
    TokenMint(TokenSupplyUpdateEvent),
    /// An event emitted when the token supply is updated by burning tokens from
    /// the balance of a token holder.
    TokenBurn(TokenSupplyUpdateEvent),
}

/// Tracks the energy remaining and some context during the execution.
struct TransactionExecutionImpl<Account> {
    // /// The remaining energy tracked spent during the execution.
    // remaining_energy: Energy,
    /// The account which signed as the sender of the transaction.
    sender_account: Account,
}

impl<Account: Clone> TransactionExecution for TransactionExecutionImpl<Account> {
    type Account = Account;

    fn sender_account(&self) -> Account {
        self.sender_account.clone()
    }

    fn tick_energy(&mut self, _energy: Energy) -> Result<(), OutOfEnergyError> {
        // implement as part of https://linear.app/concordium/issue/PSR-37/energy-charge
        Ok(())
    }
}

/// Error executing transaction. This does not include reject reasons,
/// which are handled at a higher level.
#[derive(Debug, thiserror::Error)]
pub enum TransactionExecutionError {
    #[error("Unexpected transaction payload that cannot be handled")]
    UnexpectedPayload,
}

/// Execute a transaction payload modifying `transaction_execution` and `block_state` accordingly.
/// Returns the events produced if successful otherwise a reject reason.
///
/// The caller must ensure to rollback state changes in case of the transaction being rejected.
pub fn execute_transaction<BSO: BlockStateOperations>(
    sender_account: BSO::Account,
    block_state: &mut BSO,
    payload: Payload,
) -> Result<Result<Vec<TransactionEvent>, TransactionRejectReason>, TransactionExecutionError>
where
    BSO::Account: Clone,
{
    let mut execution = TransactionExecutionImpl { sender_account };

    // handle energy as part of https://linear.app/concordium/issue/PSR-37/energy-charge

    match payload {
        Payload::TokenUpdate { payload } => Ok(plt_scheduler::execute_plt_transaction(
            &mut execution,
            block_state,
            payload,
        )),
        _ => Err(TransactionExecutionError::UnexpectedPayload),
    }
}

/// Account with given id does not exist
#[derive(Debug, thiserror::Error)]
pub enum UpdateInstructionExecutionError {
    #[error("Unexpected update instruction payload that cannot be handled")]
    UnexpectedPayload,
}

/// Execute an update instruction payload modifying `block_state` accordingly.
/// Returns the events produced if successful.
pub fn execute_update_instruction<BSO: BlockStateOperations>(
    block_state: &mut BSO,
    payload: UpdatePayload,
) -> Result<Vec<TransactionEvent>, UpdateInstructionExecutionError>
where
    BSO::Account: Clone,
{
    match payload {
        UpdatePayload::CreatePlt(create_plt) => {
            plt_scheduler::execute_plt_update_instruction(block_state, create_plt)
        }
        _ => Err(UpdateInstructionExecutionError::UnexpectedPayload),
    }
}
