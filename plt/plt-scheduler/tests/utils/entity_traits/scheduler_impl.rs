use super::scheduler::SchedulerOperations;
use concordium_base::base::{AccountIndex, Energy, Nonce};
use concordium_base::contracts_common::{AccountAddress, Timestamp};
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::{RawCbor, TokenId};
use concordium_base::transactions::Payload;
use concordium_base::updates::UpdatePayload;
use plt_block_state::block_state::{ExecutionTimeBlockStateP9, ExecutionTimeBlockStateP11};
use plt_block_state::entity::accounts::Account;
use plt_block_state::entity::block_state::TokenNotFoundByIdError;
use plt_block_state::entity::block_state::p9::BlockStateP9;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_scheduler::protocol_level_tokens;
use plt_scheduler::queries::QueryLockError;
use plt_scheduler::scheduler::{ChainUpdateExecutionError, TransactionExecutionError};
use plt_scheduler::{TransactionContext, queries, scheduler};
use plt_scheduler_types::types::execution::{ChainUpdateOutcome, TransactionExecutionSummary};
use plt_scheduler_types::types::queries::{TokenAccountInfo, TokenAuthorizations, TokenInfo};
use std::mem;

impl SchedulerOperations for BlockStateP9 {
    fn execute_transaction<C: EntityContextTypes>(
        &mut self,
        context: &mut EntityContext<C>,
        transaction_context: TransactionContext,
        sender_account: AccountIndex,
        payload: Payload,
    ) -> Result<TransactionExecutionSummary, TransactionExecutionError> {
        let sender_account = Account::from_existing_account(sender_account);

        scheduler::p9::execute_transaction(
            context,
            self,
            transaction_context,
            sender_account.clone(),
            payload,
        )
    }

    fn execute_chain_update<C: EntityContextTypes>(
        &mut self,
        context: &mut EntityContext<C>,
        payload: UpdatePayload,
    ) -> Result<ChainUpdateOutcome, ChainUpdateExecutionError> {
        scheduler::p9::execute_chain_update(context, self, payload)
    }

    fn query_plt_list<C: EntityContextTypes>(&self, context: &EntityContext<C>) -> Vec<TokenId> {
        protocol_level_tokens::p9::query_plt_list(context, self).unwrap()
    }

    fn query_token_info<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        token_id: &TokenId,
    ) -> Result<TokenInfo, TokenNotFoundByIdError> {
        protocol_level_tokens::p9::query_token_info(context, self, token_id).unwrap()
    }

    fn query_token_account_infos<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        account: AccountIndex,
    ) -> Vec<TokenAccountInfo> {
        protocol_level_tokens::p9::query_token_account_infos(
            context,
            self,
            Account::from_existing_account(account),
        )
        .unwrap()
    }

    fn query_token_authorizations<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        token_id: &TokenId,
    ) -> Result<TokenAuthorizations, TokenNotFoundByIdError> {
        protocol_level_tokens::p9::query_token_authorizations(context, self, token_id).unwrap()
    }

    fn query_lock_list<C: EntityContextTypes>(&self, context: &EntityContext<C>) -> Vec<LockId>
    where
        EntityContext<C>: Clone,
    {
        let exec_block_state = ExecutionTimeBlockStateP9 {
            block_state: self.clone(),
            context: context.clone(),
        };

        queries::query_lock_list(&exec_block_state)
    }

    fn query_lock_info<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        lock_id: &LockId,
    ) -> Result<RawCbor, QueryLockError>
    where
        EntityContext<C>: Clone,
    {
        let exec_block_state = ExecutionTimeBlockStateP9 {
            block_state: self.clone(),
            context: context.clone(),
        };

        queries::query_lock_info(&exec_block_state, lock_id)
    }
}

impl SchedulerOperations for BlockStateP11 {
    fn execute_transaction<C: EntityContextTypes>(
        &mut self,
        context: &mut EntityContext<C>,
        transaction_context: TransactionContext,
        sender_account: AccountIndex,
        payload: Payload,
    ) -> Result<TransactionExecutionSummary, TransactionExecutionError>
    where
        EntityContext<C>: Clone,
    {
        let sender_account = Account::from_existing_account(sender_account);

        scheduler::p11::execute_transaction(
            context,
            self,
            transaction_context,
            sender_account.clone(),
            payload,
        )
    }

    fn execute_chain_update<C: EntityContextTypes>(
        &mut self,
        context: &mut EntityContext<C>,
        payload: UpdatePayload,
    ) -> Result<ChainUpdateOutcome, ChainUpdateExecutionError> {
        scheduler::p11::execute_chain_update(context, self, payload)
    }

    fn query_plt_list<C: EntityContextTypes>(&self, context: &EntityContext<C>) -> Vec<TokenId> {
        protocol_level_tokens::p11::query_plt_list(context, self).unwrap()
    }

    fn query_token_info<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        token_id: &TokenId,
    ) -> Result<TokenInfo, TokenNotFoundByIdError> {
        protocol_level_tokens::p11::query_token_info(context, self, token_id).unwrap()
    }

    fn query_token_account_infos<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        account: AccountIndex,
    ) -> Vec<TokenAccountInfo> {
        protocol_level_tokens::p11::query_token_account_infos(
            context,
            self,
            Account::from_existing_account(account),
        )
        .unwrap()
    }

    fn query_token_authorizations<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        token_id: &TokenId,
    ) -> Result<TokenAuthorizations, TokenNotFoundByIdError> {
        protocol_level_tokens::p11::query_token_authorizations(context, self, token_id).unwrap()
    }

    fn query_lock_list<C: EntityContextTypes>(&self, context: &EntityContext<C>) -> Vec<LockId>
    where
        EntityContext<C>: Clone,
    {
        let exec_block_state = ExecutionTimeBlockStateP11 {
            block_state: self.clone(),
            context: context.clone(),
        };

        queries::query_lock_list(&exec_block_state)
    }

    fn query_lock_info<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        lock_id: &LockId,
    ) -> Result<RawCbor, QueryLockError>
    where
        EntityContext<C>: Clone,
    {
        let exec_block_state = ExecutionTimeBlockStateP11 {
            block_state: self.clone(),
            context: context.clone(),
        };

        queries::query_lock_info(&exec_block_state, lock_id)
    }
}
