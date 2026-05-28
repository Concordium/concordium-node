use super::scheduler::SchedulerOperations;
use concordium_base::base::{AccountIndex, Energy, Nonce};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::{RawCbor, TokenId};
use concordium_base::transactions::Payload;
use concordium_base::updates::UpdatePayload;
use plt_block_state::block_state::{ExecutionTimeBlockStateP9, ExecutionTimeBlockStateP11};
use plt_block_state::entity::accounts::Account;
use plt_block_state::entity::block_state::Accounts;
use plt_block_state::entity::block_state::p9::BlockStateP9;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_scheduler::queries::{QueryLockError, QueryTokenInfoError};
use plt_scheduler::scheduler::{ChainUpdateExecutionError, TransactionExecutionError};
use plt_scheduler::{queries, scheduler};
use plt_scheduler_types::types::execution::{ChainUpdateOutcome, TransactionExecutionSummary};
use plt_scheduler_types::types::queries::{TokenAccountInfo, TokenAuthorizations, TokenInfo};
use std::mem;

impl SchedulerOperations for BlockStateP9 {
    fn execute_transaction<C: EntityContextTypes>(
        &mut self,
        context: &mut EntityContext<C>,
        sender_account: AccountIndex,
        sender_account_address: AccountAddress,
        transaction_sequence_number: Nonce,
        payload: Payload,
        energy_limit: Energy,
    ) -> Result<TransactionExecutionSummary, TransactionExecutionError>
    where
        EntityContext<C>: Default,
    {
        let sender_account = Account::from_existing_account(sender_account);

        let mut exec_block_state = ExecutionTimeBlockStateP9 {
            block_state: mem::take(self),
            context: mem::take(context),
        };

        let res = scheduler::execute_transaction(
            sender_account,
            sender_account_address,
            transaction_sequence_number,
            &mut exec_block_state,
            payload,
            energy_limit,
        );

        mem::swap(context, &mut exec_block_state.context);
        mem::swap(self, &mut exec_block_state.block_state);

        res
    }

    fn execute_chain_update<C: EntityContextTypes>(
        &mut self,
        context: &mut EntityContext<C>,
        payload: UpdatePayload,
    ) -> Result<ChainUpdateOutcome, ChainUpdateExecutionError>
    where
        EntityContext<C>: Default,
    {
        let mut exec_block_state = ExecutionTimeBlockStateP9 {
            block_state: mem::take(self),
            context: mem::take(context),
        };

        let res = scheduler::execute_chain_update(&mut exec_block_state, payload);

        mem::swap(context, &mut exec_block_state.context);
        mem::swap(self, &mut exec_block_state.block_state);

        res
    }

    fn query_plt_list<C: EntityContextTypes>(&self, context: &EntityContext<C>) -> Vec<TokenId>
    where
        EntityContext<C>: Clone,
    {
        let exec_block_state = ExecutionTimeBlockStateP9 {
            block_state: self.clone(),
            context: context.clone(),
        };

        queries::query_plt_list(&exec_block_state)
    }

    fn query_token_info<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        token_id: &TokenId,
    ) -> Result<TokenInfo, QueryTokenInfoError>
    where
        EntityContext<C>: Clone,
    {
        let exec_block_state = ExecutionTimeBlockStateP9 {
            block_state: self.clone(),
            context: context.clone(),
        };

        queries::query_token_info(&exec_block_state, token_id)
    }

    fn query_token_account_infos<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        account: AccountIndex,
    ) -> Vec<TokenAccountInfo>
    where
        EntityContext<C>: Clone,
    {
        let account = self.account_by_index(context, account).unwrap().account;

        let exec_block_state = ExecutionTimeBlockStateP9 {
            block_state: self.clone(),
            context: context.clone(),
        };

        queries::query_token_account_infos(&exec_block_state, account)
            .expect("query_token_account_infos must succeed")
    }

    fn query_token_authorizations<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        token_id: &TokenId,
    ) -> Result<TokenAuthorizations, QueryTokenInfoError>
    where
        EntityContext<C>: Clone,
    {
        let exec_block_state = ExecutionTimeBlockStateP9 {
            block_state: self.clone(),
            context: context.clone(),
        };

        queries::query_token_authorizations(&exec_block_state, token_id)
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
        sender_account: AccountIndex,
        sender_account_address: AccountAddress,
        transaction_sequence_number: Nonce,
        payload: Payload,
        energy_limit: Energy,
    ) -> Result<TransactionExecutionSummary, TransactionExecutionError>
    where
        EntityContext<C>: Default,
    {
        let sender_account = self
            .account_by_index(context, sender_account)
            .unwrap()
            .account;

        let mut exec_block_state = ExecutionTimeBlockStateP11 {
            block_state: mem::take(self),
            context: mem::take(context),
        };

        let res = scheduler::execute_transaction(
            sender_account,
            sender_account_address,
            transaction_sequence_number,
            &mut exec_block_state,
            payload,
            energy_limit,
        );

        mem::swap(context, &mut exec_block_state.context);
        mem::swap(self, &mut exec_block_state.block_state);

        res
    }

    fn execute_chain_update<C: EntityContextTypes>(
        &mut self,
        context: &mut EntityContext<C>,
        payload: UpdatePayload,
    ) -> Result<ChainUpdateOutcome, ChainUpdateExecutionError>
    where
        EntityContext<C>: Default,
    {
        let mut exec_block_state = ExecutionTimeBlockStateP11 {
            block_state: mem::take(self),
            context: mem::take(context),
        };

        let res = scheduler::execute_chain_update(&mut exec_block_state, payload);

        mem::swap(context, &mut exec_block_state.context);
        mem::swap(self, &mut exec_block_state.block_state);

        res
    }

    fn query_plt_list<C: EntityContextTypes>(&self, context: &EntityContext<C>) -> Vec<TokenId>
    where
        EntityContext<C>: Clone,
    {
        let exec_block_state = ExecutionTimeBlockStateP11 {
            block_state: self.clone(),
            context: context.clone(),
        };

        queries::query_plt_list(&exec_block_state)
    }

    fn query_token_info<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        token_id: &TokenId,
    ) -> Result<TokenInfo, QueryTokenInfoError>
    where
        EntityContext<C>: Clone,
    {
        let exec_block_state = ExecutionTimeBlockStateP11 {
            block_state: self.clone(),
            context: context.clone(),
        };

        queries::query_token_info(&exec_block_state, token_id)
    }

    fn query_token_account_infos<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        account: AccountIndex,
    ) -> Vec<TokenAccountInfo>
    where
        EntityContext<C>: Clone,
    {
        let account = Account::from_existing_account(account);

        let exec_block_state = ExecutionTimeBlockStateP11 {
            block_state: self.clone(),
            context: context.clone(),
        };

        queries::query_token_account_infos(&exec_block_state, account)
            .expect("query_token_account_infos must succeed")
    }

    fn query_token_authorizations<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        token_id: &TokenId,
    ) -> Result<TokenAuthorizations, QueryTokenInfoError>
    where
        EntityContext<C>: Clone,
    {
        let exec_block_state = ExecutionTimeBlockStateP11 {
            block_state: self.clone(),
            context: context.clone(),
        };

        queries::query_token_authorizations(&exec_block_state, token_id)
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
