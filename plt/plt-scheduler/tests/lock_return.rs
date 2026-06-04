//! Tests for returning funds from protocol-level token locks.

use crate::utils::entity_traits::scheduler::SchedulerOperations;
use crate::utils::{BlockStateLatest, TokenInitTestParams};
use assert_matches::assert_matches;
use concordium_base::base::Energy;
use concordium_base::common::cbor;
use concordium_base::protocol_level_locks::LockInfo;
use concordium_base::protocol_level_locks::{LockControllerSimpleV0Capability, LockId};
use concordium_base::protocol_level_tokens::meta_operations::{
    MetaUpdateOperations, MetaUpdatePayload, lock_fund, lock_return,
};
use concordium_base::protocol_level_tokens::{
    RawCbor, TokenAmount, TokenId, TokenModuleAccountState,
};
use concordium_base::transactions::Payload;
use plt_block_state::{
    entity::entity_test_stub, persistent::protocol_level_locks::p11::LockControllerSimpleV0Grant,
};
use plt_scheduler::queries::QueryLockError;
use plt_scheduler_types::types::events::{BlockItemEvent, LockDestroyEvent, TokenTransferEvent};
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;
use plt_scheduler_types::types::tokens::{RawTokenAmount, TokenHolder};

mod utils;

macro_rules! execute_meta_update {
    ($context:expr, $block_state:expr, $sender:expr, $timestamp:expr, $operations:expr $(,)?) => {{
        let sender_addr = $context.external.account_canonical_address($sender);
        let payload = Payload::MetaUpdate {
            payload: MetaUpdatePayload {
                operations: RawCbor::from(cbor::cbor_encode(&MetaUpdateOperations {
                    operations: $operations,
                })),
            },
        };

        $block_state
            .execute_transaction(
                $context,
                plt_scheduler::TransactionContext {
                    energy_limit: Energy::from(u64::MAX),
                    sender_account_address: sender_addr,
                    transaction_sequence_number: 1.into(),
                    block_timestamp: $timestamp.into(),
                },
                $sender,
                payload,
            )
            .expect("meta-update transaction must execute")
            .outcome
    }};
}

macro_rules! token_account_info {
    ($context:expr, $block_state:expr, $account:expr, $token_id:expr $(,)?) => {{
        $block_state
            .query_token_account_infos($context, $account)
            .expect("token account query must succeed")
            .into_iter()
            .find(|info| &info.token_id == $token_id)
            .expect("token account info must exist")
    }};
}

macro_rules! token_module_account_state {
    ($info:expr $(,)?) => {{
        cbor::cbor_decode::<TokenModuleAccountState>(
            $info
                .account_state
                .module_state
                .as_ref()
                .expect("token account state must contain token-module state"),
        )
        .expect("token-module account state must decode")
    }};
}

#[test]
fn test_lock_return_deletes_empty_lock_when_keep_alive_is_false() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let sender = context.external.create_account();
    let recipient = context.external.create_account();
    let token_id: TokenId = "pltX".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        sender.account_index(),
        &token_id,
        RawTokenAmount(1000),
    );

    let lock_id = LockId::new(sender.account_index(), 7u64, 0);
    let lock_config = utils::CreateLockSimpleConfig {
        recipients: vec![recipient.account_index()],
        grants: vec![LockControllerSimpleV0Grant {
            account: sender.account_index(),
            roles: vec![
                LockControllerSimpleV0Capability::Fund,
                LockControllerSimpleV0Capability::Return,
            ],
        }],
        tokens: vec![token_id.clone()],
        expiry: 1_804_806_000,
        keep_alive: false,
    };
    utils::create_lock(&mut context, &mut block_state, &lock_id, lock_config);
    execute_meta_update!(
        &mut context,
        &mut block_state,
        sender.account_index(),
        0,
        vec![lock_fund(
            token_id.clone(),
            lock_id.clone(),
            TokenAmount::from_raw(250, 4),
            None,
        )],
    );

    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    let outcome = execute_meta_update!(
        &mut context,
        &mut block_state,
        sender.account_index(),
        0,
        vec![lock_return(
            token_id.clone(),
            lock_id.clone(),
            sender_addr,
            TokenAmount::from_raw(250, 4),
            None,
        )],
    );
    let events = assert_matches!(outcome, TransactionOutcome::Success(events) => events);

    assert_eq!(events.len(), 2);
    assert_matches!(&events[0], BlockItemEvent::TokenTransfer(TokenTransferEvent {
        token_id: event_token_id,
        from,
        to,
        amount,
        from_lock,
        to_lock,
        ..
    }) => {
        assert_eq!(event_token_id, &token_id);
        assert_eq!(from, &TokenHolder::Account(sender_addr));
        assert_eq!(to, &TokenHolder::Account(sender_addr));
        assert_eq!(amount.amount, RawTokenAmount(250));
        assert_eq!(amount.decimals, 4);
        assert_eq!(from_lock, &Some(lock_id.clone()));
        assert_eq!(to_lock, &None);
    });
    assert_matches!(&events[1], BlockItemEvent::LockDestroyed(LockDestroyEvent { lock_id: event_lock_id }) => {
        assert_eq!(event_lock_id, &lock_id);
    });

    let sender_info =
        token_account_info!(&context, &block_state, sender.account_index(), &token_id);
    assert_eq!(
        sender_info.account_state.balance.amount,
        RawTokenAmount(1000)
    );
    let sender_state = token_module_account_state!(&sender_info);
    assert!(sender_state.available.is_none());
    assert!(sender_state.locks.is_empty());

    assert_matches!(
        block_state.query_lock_info(&context, &lock_id),
        Err(QueryLockError::LockDoesNotExist)
    );
}
#[test]
fn test_lock_return_keeps_empty_lock_when_keep_alive_is_true() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let sender = context.external.create_account();
    let recipient = context.external.create_account();
    let token_id: TokenId = "pltX".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        sender.account_index(),
        &token_id,
        RawTokenAmount(1000),
    );

    let lock_id = LockId::new(sender.account_index(), 7u64, 0);
    let lock_config = utils::CreateLockSimpleConfig {
        recipients: vec![recipient.account_index()],
        grants: vec![LockControllerSimpleV0Grant {
            account: sender.account_index(),
            roles: vec![
                LockControllerSimpleV0Capability::Fund,
                LockControllerSimpleV0Capability::Return,
            ],
        }],
        tokens: vec![token_id.clone()],
        expiry: 1_804_806_000,
        keep_alive: true,
    };
    utils::create_lock(&mut context, &mut block_state, &lock_id, lock_config);
    execute_meta_update!(
        &mut context,
        &mut block_state,
        sender.account_index(),
        0,
        vec![lock_fund(
            token_id.clone(),
            lock_id.clone(),
            TokenAmount::from_raw(250, 4),
            None,
        )],
    );

    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    let outcome = execute_meta_update!(
        &mut context,
        &mut block_state,
        sender.account_index(),
        0,
        vec![lock_return(
            token_id.clone(),
            lock_id.clone(),
            sender_addr,
            TokenAmount::from_raw(250, 4),
            None,
        )],
    );
    let events = assert_matches!(outcome, TransactionOutcome::Success(events) => events);

    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenTransfer(..));

    let lock_info: LockInfo = cbor::cbor_decode(
        block_state
            .query_lock_info(&context, &lock_id)
            .expect("lock info query must succeed"),
    )
    .expect("lock info must decode");
    assert!(lock_info.funds.is_empty());
}

#[test]
fn test_lock_return_rejects_unauthorized_sender() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let owner = context.external.create_account();
    let recipient = context.external.create_account();
    let token_id: TokenId = "pltX".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        owner.account_index(),
        &token_id,
        RawTokenAmount(1000),
    );

    let lock_id = LockId::new(owner.account_index(), 7u64, 0);
    let lock_config = utils::CreateLockSimpleConfig {
        recipients: vec![recipient.account_index()],
        grants: vec![LockControllerSimpleV0Grant {
            account: owner.account_index(),
            roles: vec![LockControllerSimpleV0Capability::Fund],
        }],
        tokens: vec![token_id.clone()],
        expiry: 1_804_806_000,
        keep_alive: false,
    };
    utils::create_lock(&mut context, &mut block_state, &lock_id, lock_config);
    execute_meta_update!(
        &mut context,
        &mut block_state,
        owner.account_index(),
        0,
        vec![lock_fund(
            token_id.clone(),
            lock_id.clone(),
            TokenAmount::from_raw(250, 4),
            None,
        )],
    );

    let owner_addr = context
        .external
        .account_canonical_address(owner.account_index());
    let outcome = execute_meta_update!(
        &mut context,
        &mut block_state,
        owner.account_index(),
        0,
        vec![lock_return(
            token_id,
            lock_id.clone(),
            owner_addr,
            TokenAmount::from_raw(1, 4),
            None,
        )],
    );
    assert_matches!(outcome, TransactionOutcome::Rejected(reason) => {
        assert_eq!(reason, TransactionRejectReason::LockReturnNotAuthorized(lock_id, owner_addr));
    });
}

#[test]
fn test_lock_return_rejects_after_expiry() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let owner = context.external.create_account();
    let recipient = context.external.create_account();
    let token_id: TokenId = "pltX".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        owner.account_index(),
        &token_id,
        RawTokenAmount(1000),
    );

    let lock_id = LockId::new(owner.account_index(), 7u64, 0);
    let lock_config = utils::CreateLockSimpleConfig {
        recipients: vec![recipient.account_index()],
        grants: vec![LockControllerSimpleV0Grant {
            account: owner.account_index(),
            roles: vec![
                LockControllerSimpleV0Capability::Fund,
                LockControllerSimpleV0Capability::Return,
            ],
        }],
        tokens: vec![token_id.clone()],
        expiry: 10,
        keep_alive: false,
    };
    utils::create_lock(&mut context, &mut block_state, &lock_id, lock_config);
    execute_meta_update!(
        &mut context,
        &mut block_state,
        owner.account_index(),
        0,
        vec![lock_fund(
            token_id.clone(),
            lock_id.clone(),
            TokenAmount::from_raw(250, 4),
            None,
        )],
    );

    let owner_addr = context
        .external
        .account_canonical_address(owner.account_index());
    let outcome = execute_meta_update!(
        &mut context,
        &mut block_state,
        owner.account_index(),
        20_000,
        vec![lock_return(
            token_id,
            lock_id.clone(),
            owner_addr,
            TokenAmount::from_raw(1, 4),
            None,
        )],
    );
    assert_matches!(outcome, TransactionOutcome::Rejected(reason) => {
        assert_eq!(reason, TransactionRejectReason::LockExpired(lock_id));
    });
}
