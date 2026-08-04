//! Tests for creating a PLT lock.

use crate::utils::BlockStateLatest;
use crate::utils::entity_traits::scheduler::SchedulerOperations;
use assert_matches::assert_matches;
use concordium_base::{
    base::Energy,
    common::{cbor, cbor::value::Value, types::TransactionTime},
    protocol_level_locks::{
        LockConfig, LockController, LockControllerSimpleV0, LockControllerSimpleV0Capability,
        LockControllerSimpleV0Grant, LockId, LockMetadata, LockRecipients,
    },
    protocol_level_tokens::{
        MetadataUrl, RawCbor, TokenAmount, TokenId, TokenModuleInitializationParameters,
        meta_operations::{MetaUpdatePayload, lock_create},
    },
    transactions::Payload,
    updates::{CreatePlt, UpdatePayload},
};
use plt_block_state::entity::accounts::Account;
use plt_block_state::entity::entity_test_stub;
use plt_block_state::persistent::chain_parameters::p11::PersistentChainParametersP11;
use plt_scheduler::TOKEN_MODULE_REF;
use plt_scheduler_types::types::events::{BlockItemEvent, LockCreateEvent};
use std::collections::HashMap;

mod utils;

macro_rules! execute_lock_create_with_duration {
    ($expiry_seconds:expr, $block_timestamp:expr, $max_lock_duration:expr) => {{
        let mut context = entity_test_stub::new_stubbed_context();
        let mut block_state = BlockStateLatest::default();
        let account_index = context.external.create_account().account_index();
        let account = context.external.account_canonical_address(account_index);
        let lock_id = LockId::new(account_index, 1, 0);
        let config = LockConfig {
            recipients: LockRecipients::Any,
            expiry: TransactionTime::from_seconds($expiry_seconds),
            controller: LockController::SimpleV0(LockControllerSimpleV0 {
                grants: vec![],
                tokens: vec![],
                keep_alive: false,
                memo: None,
            }),
            metadata: None,
        };
        let payload = MetaUpdatePayload {
            operations: RawCbor::from(cbor::cbor_encode(&vec![lock_create(config)])),
        };
        let result = plt_scheduler::scheduler::p11::execute_transaction(
            &mut context,
            &mut block_state,
            plt_scheduler::TransactionContext {
                energy_limit: Energy::from(u64::MAX),
                sender_account_address: account,
                transaction_sequence_number: 1.into(),
                block_timestamp: $block_timestamp.into(),
            },
            Account::from_existing_account(account_index),
            Payload::MetaUpdate { payload },
            &PersistentChainParametersP11 {
                max_lock_duration: $max_lock_duration,
            },
        );
        let lock_exists = matches!(block_state.lock_by_id(&context, &lock_id), Ok(Ok(_)));
        (result, lock_exists)
    }};
}

#[test]
fn test_create_simple_lock() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let account_index_1 = context.external.create_account().account_index();
    let account_1 = context.external.account_canonical_address(account_index_1);

    let plt_x: TokenId = "pltX".parse().unwrap();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Test PLT 1".to_owned()),
        metadata: Some(MetadataUrl::from("https://pltX.token".to_string())),
        governance_account: Some(account_1.into()),
        allow_list: None,
        deny_list: None,
        initial_supply: Some(TokenAmount::from_raw(10000, 2)),
        mintable: Some(true),
        burnable: Some(true),
    };
    let initialization_parameters = cbor::cbor_encode(&parameters).into();
    let payload = UpdatePayload::CreatePlt(CreatePlt {
        token_id: plt_x.clone(),
        token_module: TOKEN_MODULE_REF,
        decimals: 2,
        initialization_parameters,
    });
    block_state
        .execute_chain_update(&mut context, payload)
        .expect("create pltX");

    let metadata = LockMetadata {
        name: Some("Test lock".to_string()),
        description: Some("Lock created in scheduler test".to_string()),
        additional: HashMap::from([("issuer".to_string(), Value::Text("Concordium".to_string()))]),
    };
    let config = LockConfig {
        recipients: LockRecipients::Limited(vec![account_1.into()]),
        expiry: TransactionTime::from_seconds(1000),
        controller: LockController::SimpleV0(LockControllerSimpleV0 {
            grants: vec![LockControllerSimpleV0Grant {
                account: account_1.into(),
                roles: vec![
                    LockControllerSimpleV0Capability::Fund,
                    LockControllerSimpleV0Capability::Send,
                ],
            }],
            tokens: vec![plt_x],
            keep_alive: false,
            memo: None,
        }),
        metadata: Some(metadata.encode_raw_cbor()),
    };
    let operations = vec![lock_create(config.clone())];
    let payload = MetaUpdatePayload {
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            plt_scheduler::TransactionContext {
                energy_limit: Energy::from(u64::MAX),
                sender_account_address: account_1,
                transaction_sequence_number: 1.into(),
                block_timestamp: 0.into(),
            },
            account_index_1,
            Payload::MetaUpdate { payload },
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, plt_scheduler_types::types::execution::TransactionOutcome::Success(events) => events);
    assert_eq!(events.len(), 1);
    let lock_id = LockId::new(account_index_1, 1, 0);
    assert_eq!(
        events[0],
        BlockItemEvent::LockCreated(LockCreateEvent {
            lock_id: lock_id.clone(),
            lock_config: RawCbor::from(cbor::cbor_encode(&config))
        })
    );

    let stored_metadata = block_state
        .lock_by_id(&context, &lock_id)
        .unwrap()
        .unwrap()
        .lock_configuration(&context)
        .unwrap()
        .metadata
        .clone();
    assert_eq!(stored_metadata, Some(metadata.encode_raw_cbor()));
}

#[test]
fn test_create_any_recipient_lock() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let account_index_1 = context.external.create_account().account_index();
    let account_1 = context.external.account_canonical_address(account_index_1);

    let plt_x: TokenId = "pltX".parse().unwrap();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Test PLT 1".to_owned()),
        metadata: Some(MetadataUrl::from("https://pltX.token".to_string())),
        governance_account: Some(account_1.into()),
        allow_list: None,
        deny_list: None,
        initial_supply: Some(TokenAmount::from_raw(10000, 2)),
        mintable: Some(true),
        burnable: Some(true),
    };
    let initialization_parameters = cbor::cbor_encode(&parameters).into();
    let payload = UpdatePayload::CreatePlt(CreatePlt {
        token_id: plt_x.clone(),
        token_module: TOKEN_MODULE_REF,
        decimals: 2,
        initialization_parameters,
    });
    block_state
        .execute_chain_update(&mut context, payload)
        .expect("create pltX");

    let config = LockConfig {
        recipients: LockRecipients::Any,
        expiry: TransactionTime::from_seconds(1000),
        controller: LockController::SimpleV0(LockControllerSimpleV0 {
            grants: vec![LockControllerSimpleV0Grant {
                account: account_1.into(),
                roles: vec![LockControllerSimpleV0Capability::Fund],
            }],
            tokens: vec![plt_x],
            keep_alive: false,
            memo: None,
        }),
        metadata: None,
    };
    let operations = vec![lock_create(config.clone())];
    let payload = MetaUpdatePayload {
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            plt_scheduler::TransactionContext {
                energy_limit: Energy::from(u64::MAX),
                sender_account_address: account_1,
                transaction_sequence_number: 1.into(),
                block_timestamp: 0.into(),
            },
            account_index_1,
            Payload::MetaUpdate { payload },
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, plt_scheduler_types::types::execution::TransactionOutcome::Success(events) => events);
    assert_eq!(events.len(), 1);
    assert_eq!(
        events[0],
        BlockItemEvent::LockCreated(LockCreateEvent {
            lock_id: LockId::new(account_index_1, 1, 0),
            lock_config: RawCbor::from(cbor::cbor_encode(&config))
        })
    );

    let lock = block_state
        .lock_by_id(&context, &LockId::new(account_index_1, 1, 0))
        .expect("lock lookup must succeed")
        .expect("lock must exist");
    let configuration = lock
        .lock_configuration(&context)
        .expect("lock configuration must load");
    assert!(configuration.recipients.is_any());
}

#[test]
fn lock_creation_enforces_expiry_and_maximum_duration() {
    let lock_id = LockId::new(0, 1, 0);

    let (result, lock_exists) = execute_lock_create_with_duration!(0, 1, u64::MAX);
    let outcome = result.expect("expired lock creation must execute").outcome;
    assert_matches!(outcome, plt_scheduler_types::types::execution::TransactionOutcome::Rejected(
        plt_scheduler_types::types::reject_reasons::TransactionRejectReason::LockExpired(id)
    ) if id == lock_id);
    assert!(!lock_exists);

    let (result, lock_exists) = execute_lock_create_with_duration!(1, 1_000, 0);
    let outcome = result.expect("boundary lock creation must execute").outcome;
    assert_matches!(
        outcome,
        plt_scheduler_types::types::execution::TransactionOutcome::Success(_)
    );
    assert!(lock_exists);

    let (result, lock_exists) = execute_lock_create_with_duration!(2, 1_000, 999);
    let outcome = result.expect("overlong lock creation must execute").outcome;
    assert_matches!(outcome, plt_scheduler_types::types::execution::TransactionOutcome::Rejected(
        plt_scheduler_types::types::reject_reasons::TransactionRejectReason::LockDurationTooLong(id)
    ) if id == lock_id);
    assert!(!lock_exists);
}

#[test]
fn lock_creation_maximum_deadline_inclusive() {
    let (result, lock_exists) = execute_lock_create_with_duration!(2, 1_500, 500);
    let outcome = result.expect("deadline lock creation must execute").outcome;
    assert_matches!(
        outcome,
        plt_scheduler_types::types::execution::TransactionOutcome::Success(_)
    );
    assert!(lock_exists);

    let (result, lock_exists) = execute_lock_create_with_duration!(2, 1_500, 499);
    let outcome = result.expect("overlong lock creation must execute").outcome;
    assert_matches!(outcome, plt_scheduler_types::types::execution::TransactionOutcome::Rejected(
        plt_scheduler_types::types::reject_reasons::TransactionRejectReason::LockDurationTooLong(_)
    ));
    assert!(!lock_exists);
}

#[test]
fn lock_creation_duration_reject_reports_its_creation_order() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let account_index = context.external.create_account().account_index();
    let account = context.external.account_canonical_address(account_index);
    let config = |expiry| LockConfig {
        recipients: LockRecipients::Any,
        expiry: TransactionTime::from_seconds(expiry),
        controller: LockController::SimpleV0(LockControllerSimpleV0 {
            grants: vec![],
            tokens: vec![],
            keep_alive: false,
            memo: None,
        }),
        metadata: None,
    };
    let payload = MetaUpdatePayload {
        operations: RawCbor::from(cbor::cbor_encode(&vec![
            lock_create(config(1)),
            lock_create(config(2)),
        ])),
    };
    let result = plt_scheduler::scheduler::p11::execute_transaction(
        &mut context,
        &mut block_state,
        plt_scheduler::TransactionContext {
            energy_limit: Energy::from(u64::MAX),
            sender_account_address: account,
            transaction_sequence_number: 1.into(),
            block_timestamp: 0.into(),
        },
        Account::from_existing_account(account_index),
        Payload::MetaUpdate { payload },
        &PersistentChainParametersP11 {
            max_lock_duration: 1_000,
        },
    )
    .expect("multi-operation lock creation must execute");
    assert_matches!(result.outcome, plt_scheduler_types::types::execution::TransactionOutcome::Rejected(
        plt_scheduler_types::types::reject_reasons::TransactionRejectReason::LockDurationTooLong(lock_id)
    ) if lock_id == LockId::new(account_index, 1, 1));
}
