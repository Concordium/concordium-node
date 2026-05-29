use concordium_base::base::{AccountIndex, Energy, Nonce};
use concordium_base::contracts_common::{AccountAddress, Timestamp};
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::{RawCbor, TokenId};
use concordium_base::transactions::Payload;
use concordium_base::updates::UpdatePayload;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::persistent::blob_reference;
use plt_scheduler::TransactionContext;
use plt_scheduler::queries::{QueryLockError, QueryTokenInfoError};
use plt_scheduler::scheduler::{ChainUpdateExecutionError, TransactionExecutionError};
use plt_scheduler_types::types::execution::{ChainUpdateOutcome, TransactionExecutionSummary};
use plt_scheduler_types::types::queries::{TokenAccountInfo, TokenAuthorizations, TokenInfo};

/// Operations and queries that the scheduler must support. Must be implemented by all
/// protocol version block states.
pub trait SchedulerOperations {
    fn execute_transaction<C: EntityContextTypes>(
        &mut self,
        context: &mut EntityContext<C>,
        transaction_context: TransactionContext,
        sender_account: AccountIndex,
        payload: Payload,
    ) -> Result<TransactionExecutionSummary, TransactionExecutionError>
    where
        EntityContext<C>: Default;

    fn execute_chain_update<C: EntityContextTypes>(
        &mut self,
        context: &mut EntityContext<C>,
        payload: UpdatePayload,
    ) -> Result<ChainUpdateOutcome, ChainUpdateExecutionError>
    where
        EntityContext<C>: Default;

    fn query_plt_list<C: EntityContextTypes>(&self, context: &EntityContext<C>) -> Vec<TokenId>
    where
        EntityContext<C>: Clone;

    fn query_token_info<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        token_id: &TokenId,
    ) -> Result<TokenInfo, QueryTokenInfoError>
    where
        EntityContext<C>: Clone;

    fn query_token_account_infos<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        account: AccountIndex,
    ) -> Result<Vec<TokenAccountInfo>, QueryTokenInfoError>
    where
        EntityContext<C>: Clone;

    fn query_token_authorizations<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        token_id: &TokenId,
    ) -> Result<TokenAuthorizations, QueryTokenInfoError>
    where
        EntityContext<C>: Clone;

    fn query_lock_list<C: EntityContextTypes>(&self, context: &EntityContext<C>) -> Vec<LockId>
    where
        EntityContext<C>: Clone;

    fn query_lock_info<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        lock_id: &LockId,
    ) -> Result<RawCbor, QueryLockError>
    where
        EntityContext<C>: Clone;
}
