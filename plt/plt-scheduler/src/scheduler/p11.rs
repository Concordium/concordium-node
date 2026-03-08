use concordium_base::transactions::Payload;
use concordium_base::updates::UpdatePayload;
use plt_block_state::block_state::blob_store;
use plt_block_state::block_state::external::ExternalBlockStateOperations;
use plt_block_state::block_state::p11::PltBlockStateP11;
use plt_scheduler_interface::transaction_execution_interface::TransactionExecution;
use plt_scheduler_types::types::execution::{ChainUpdateOutcome, TransactionExecutionSummary};

use super::{
    ChainUpdateExecutionError, SchedulerOperations, TransactionExecutionError, plt_scheduler,
};

impl SchedulerOperations for PltBlockStateP11 {
    fn execute_chain_update(
        &mut self,
        external: &mut impl ExternalBlockStateOperations,
        payload: concordium_base::updates::UpdatePayload,
    ) -> Result<ChainUpdateOutcome, ChainUpdateExecutionError> {
        match payload {
            UpdatePayload::CreatePlt(create_plt) => plt_scheduler::execute_create_plt_chain_update(
                &mut self.tokens,
                external,
                create_plt,
            ),
            _ => Err(ChainUpdateExecutionError::UnexpectedPayload),
        }
    }

    fn execute_transaction(
        &mut self,
        execution: &mut TransactionExecution,
        _loader: &mut impl blob_store::BackingStoreLoad,
        external: &mut impl ExternalBlockStateOperations,
        payload: Payload,
    ) -> Result<TransactionExecutionSummary, TransactionExecutionError> {
        match payload {
            Payload::TokenUpdate { payload } => {
                let outcome = plt_scheduler::execute_token_update_transaction(
                    execution,
                    &mut self.tokens,
                    external,
                    payload,
                )?;
                Ok(TransactionExecutionSummary {
                    outcome,
                    energy_used: execution.energy_used(),
                })
            }
            _ => Err(TransactionExecutionError::UnexpectedPayload),
        }
    }
}
