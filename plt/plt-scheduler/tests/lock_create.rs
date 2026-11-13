//! Tests for creating a PLT lock.

use crate::utils::entity_traits::scheduler::SchedulerOperations;
use crate::utils::{BlockStateLatest, TokenInitTestParams};
use assert_matches::assert_matches;
use concordium_base::{
    base::Energy,
    common::{
        cbor::{self, value::Value},
        types::TransactionTime,
    },
    contracts_common::Duration,
    protocol_level_locks::{
        LockConfig, LockConfigSimpleV0, LockControllerSimpleV0Capability,
        LockControllerSimpleV0Grant, LockId, LockMetadata, LockRecipients,
    },
    protocol_level_tokens::{
        MetadataUrl, Operation, OperationsPayload, RawCbor, TokenAmount, TokenId,
        TokenModuleInitializationParameters, operations,
    },
    transactions::{Payload, TokenUpdatePayload, cost},
    updates::{CreatePlt, UpdatePayload},
};
use plt_block_state::entity::accounts::Account;
use plt_block_state::entity::entity_test_stub;
use plt_block_state::persistent::chain_parameters::p11::PersistentChainParametersP11;
use plt_scheduler::{TOKEN_MODULE_REF, scheduler::TransactionExecutionError};
use plt_scheduler_types::types::{
    events::{BlockItemEvent, LockCreateEvent},
    execution::{TransactionExecutionSummary, TransactionOutcome},
    reject_reasons::TransactionRejectReason,
};
use std::collections::HashMap;

mod utils;

fn execute_lock_create_with_duration(
    expiry_seconds: u64,
    block_timestamp: u64,
    max_lock_duration: u64,
) -> (
    Result<TransactionExecutionSummary, TransactionExecutionError>,
    bool,
) {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let account_index = context.external.create_account().account_index();
    let account = context.external.account_canonical_address(account_index);
    let lock_id = LockId::new(account_index, 1, 0);
    let config = LockConfig::SimpleV0(LockConfigSimpleV0 {
        recipients: LockRecipients::Any,
        expiry: TransactionTime::from_seconds(expiry_seconds),
        grants: vec![],
        tokens: vec![],
        memo: None,
        metadata: None,
    });
    let payload = OperationsPayload {
        operations: RawCbor::from(cbor::cbor_encode(&vec![Operation::from(
            operations::create_lock(config),
        )])),
    };
    let result = plt_scheduler::scheduler::p11::execute_transaction(
        &mut context,
        &mut block_state,
        plt_scheduler::TransactionContext {
            energy_limit: Energy::from(u64::MAX),
            sender_account_address: account,
            transaction_sequence_number: 1.into(),
            block_timestamp: block_timestamp.into(),
            block_height: 0.into(),
        },
        Account::from_existing_account(account_index),
        Payload::TokenUpdate {
            payload: TokenUpdatePayload::Unscoped(payload),
        },
        &PersistentChainParametersP11 {
            max_lock_duration: Duration::from_millis(max_lock_duration),
        },
    );
    let lock_exists = matches!(block_state.lock_by_id(&context, &lock_id), Ok(Ok(_)));
    (result, lock_exists)
}

#[test]
fn test_create_lock_rejects_obsolete_keep_alive() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let sender = context.external.create_account().account_index();
    let sender_address = context.external.account_canonical_address(sender);
    let config = LockConfig::SimpleV0(LockConfigSimpleV0 {
        recipients: LockRecipients::Any,
        expiry: TransactionTime::from_seconds(1_000),
        grants: vec![],
        tokens: vec![],
        memo: None,
        metadata: None,
    });
    let operations = vec![Operation::from(operations::create_lock(config))];
    let mut encoded: Value = cbor::cbor_decode(cbor::cbor_encode(&operations)).unwrap();
    let array = assert_matches!(&mut encoded, Value::Array(array) => array);
    let operation = assert_matches!(&mut array[0], Value::Map(map) => map);
    let config = assert_matches!(&mut operation[0].1, Value::Map(map) => map);
    let simple = assert_matches!(&mut config[0].1, Value::Map(map) => map);
    simple.push((Value::Text("keepAlive".into()), Value::Bool(true)));

    let result = block_state
        .execute_transaction(
            &mut context,
            utils::simple_transaction_context(sender_address),
            sender,
            Payload::TokenUpdate {
                payload: TokenUpdatePayload::Unscoped(OperationsPayload {
                    operations: RawCbor::from(cbor::cbor_encode(&encoded)),
                }),
            },
        )
        .expect("transaction internal error");
    assert_matches!(
        result.outcome,
        TransactionOutcome::Rejected(TransactionRejectReason::SerializationFailure)
    );
    assert!(block_state.query_lock_list(&context).is_empty());
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
    let config = LockConfig::SimpleV0(LockConfigSimpleV0 {
        recipients: LockRecipients::Limited(vec![account_1.into()]),
        expiry: TransactionTime::from_seconds(1000),
        grants: vec![LockControllerSimpleV0Grant {
            account: account_1.into(),
            roles: vec![
                LockControllerSimpleV0Capability::Cancel,
                LockControllerSimpleV0Capability::Fund,
                LockControllerSimpleV0Capability::Cancel,
                LockControllerSimpleV0Capability::Send,
                LockControllerSimpleV0Capability::Release,
                LockControllerSimpleV0Capability::Fund,
            ],
        }],
        tokens: vec!["PLTx".parse().unwrap()],
        memo: None,
        metadata: Some(metadata.encode_raw_cbor()),
    });
    let mut canonical_config = config.clone();
    let LockConfig::SimpleV0(controller) = &mut canonical_config;
    controller.tokens = vec![plt_x.clone()];
    controller.grants[0].roles = vec![
        LockControllerSimpleV0Capability::Fund,
        LockControllerSimpleV0Capability::Release,
        LockControllerSimpleV0Capability::Send,
        LockControllerSimpleV0Capability::Cancel,
    ];

    let operations = vec![Operation::from(operations::create_lock(config))];
    let payload = OperationsPayload {
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            plt_scheduler::TransactionContext {
                energy_limit: Energy::from(u64::MAX),
                sender_account_address: account_1,
                transaction_sequence_number: 1.into(),
                block_timestamp: 1234.into(),
                block_height: 5678.into(),
            },
            account_index_1,
            Payload::TokenUpdate {
                payload: TokenUpdatePayload::Unscoped(payload),
            },
        )
        .expect("transaction internal error");
    // Operation charge, excluding transaction overhead.
    assert_eq!(
        result.energy_used - cost::PLT_OPERATIONS_TRANSACTIONS,
        Energy::from(610),
    );
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);
    assert_eq!(events.len(), 1);
    let BlockItemEvent::LockCreated(event) = &events[0] else {
        panic!("expected lock-created event")
    };
    assert_eq!(
        hex::encode(&event.lock_config),
        "a16873696d706c655630a566657870697279c11903e8666772616e747381a265726f6c6573846466756e646772656c656173656473656e646663616e63656c676163636f756e74d99d73a201d99d71a101190397035820000000000000000000000000000000000000000000000000000000000000000066746f6b656e738164706c7458686d65746164617461584ea3646e616d656954657374206c6f636b666973737565726a436f6e636f726469756d6b6465736372697074696f6e781e4c6f636b206372656174656420696e207363686564756c657220746573746a726563697069656e747381d99d73a201d99d71a1011903970358200000000000000000000000000000000000000000000000000000000000000000"
    );
    let config_value: Value = cbor::cbor_decode(&event.lock_config).unwrap();
    let outer = assert_matches!(config_value, Value::Map(map) => map);
    let simple = assert_matches!(&outer[0].1, Value::Map(map) => map);
    assert!(
        !simple
            .iter()
            .any(|(key, _)| key == &Value::Text("keepAlive".into()))
    );
    let lock_id = LockId::new(account_index_1, 1, 0);
    let queried = block_state.query_lock_info(&context, &lock_id).unwrap();
    let info: concordium_base::protocol_level_locks::LockInfo =
        cbor::cbor_decode(&queried).unwrap();
    assert_eq!(info.config, canonical_config);
    let queried_value: Value = cbor::cbor_decode(&queried).unwrap();
    let queried_map = assert_matches!(queried_value, Value::Map(map) => map);
    assert_eq!(
        queried_map
            .iter()
            .find(|(key, _)| key == &Value::Text("config".into()))
            .unwrap()
            .1,
        Value::Map(outer)
    );
    assert_eq!(
        events[0],
        BlockItemEvent::LockCreated(LockCreateEvent {
            lock_id: lock_id.clone(),
            lock_config: RawCbor::from(cbor::cbor_encode(&canonical_config))
        })
    );

    let stored_lock = block_state.lock_by_id(&context, &lock_id).unwrap().unwrap();
    let stored_configuration = stored_lock.lock_configuration(&context).unwrap();
    assert_eq!(
        match stored_configuration.as_ref() {
            plt_block_state::persistent::protocol_level_locks::p11::LockConfig::SimpleV0(
                config,
            ) => config.metadata.clone(),
        },
        Some(metadata.encode_raw_cbor())
    );
    let plt_block_state::persistent::protocol_level_locks::p11::LockConfig::SimpleV0(controller) =
        stored_configuration.as_ref();
    assert_eq!(
        controller.grants()[0].roles(),
        [
            LockControllerSimpleV0Capability::Fund,
            LockControllerSimpleV0Capability::Release,
            LockControllerSimpleV0Capability::Send,
            LockControllerSimpleV0Capability::Cancel,
        ]
    );
    assert_eq!(controller.tokens(), &[plt_x]);
    let index = block_state.lock_expiry_index(&context).unwrap();
    let keys = index
        .iter(&context.store)
        .map(|entry| entry.unwrap().0)
        .collect::<Vec<_>>();
    assert_eq!(keys.len(), 1);
    assert_eq!(keys[0].expiry, TransactionTime::from_seconds(1000));
    assert_eq!(keys[0].created_at_height, 5678.into());
    assert_eq!(keys[0].lock_id, lock_id);
}

#[test]
fn test_create_locks_orders_equal_expiries_by_absolute_height() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let lower_account = context.external.create_account().account_index();
    let higher_account = context.external.create_account().account_index();
    let earlier_lock = LockId::new(higher_account, 1, 0);
    let later_lock = LockId::new(lower_account, 1, 0);
    assert!(later_lock < earlier_lock);

    for (account_index, height) in [(higher_account, 1234_u64), (lower_account, 1235)] {
        let account = context.external.account_canonical_address(account_index);
        let config = LockConfig::SimpleV0(LockConfigSimpleV0 {
            recipients: LockRecipients::Any,
            expiry: TransactionTime::from_seconds(1000),
            grants: vec![],
            tokens: vec![],
            memo: None,
            metadata: None,
        });
        let payload = OperationsPayload {
            operations: RawCbor::from(cbor::cbor_encode(&vec![Operation::from(
                operations::create_lock(config),
            )])),
        };
        let result = block_state
            .execute_transaction(
                &mut context,
                plt_scheduler::TransactionContext {
                    energy_limit: Energy::from(u64::MAX),
                    sender_account_address: account,
                    transaction_sequence_number: 1.into(),
                    block_timestamp: 0.into(),
                    block_height: height.into(),
                },
                account_index,
                Payload::TokenUpdate {
                    payload: TokenUpdatePayload::Unscoped(payload),
                },
            )
            .expect("lock creation must succeed");
        assert_matches!(result.outcome, TransactionOutcome::Success(_));
    }

    let index = block_state.lock_expiry_index(&context).unwrap();
    let keys = index
        .iter(&context.store)
        .map(|entry| entry.unwrap().0)
        .collect::<Vec<_>>();
    assert_eq!(keys.len(), 2);
    assert_eq!(keys[0].expiry, keys[1].expiry);
    assert_eq!(keys[0].created_at_height, 1234.into());
    assert_eq!(keys[1].created_at_height, 1235.into());
    assert_eq!(keys[0].lock_id, earlier_lock);
    assert_eq!(keys[1].lock_id, later_lock);
}

#[test]
fn test_create_lock_with_256_duplicate_roles_persists_and_reloads() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let account_index = context.external.create_account().account_index();
    let account = context.external.account_canonical_address(account_index);
    let other_index = context.external.create_account().account_index();
    let other = context.external.account_canonical_address(other_index);
    let lock_id = LockId::new(account_index, 1, 0);
    let config = LockConfig::SimpleV0(LockConfigSimpleV0 {
        recipients: LockRecipients::Any,
        expiry: TransactionTime::from_seconds(1_000),
        grants: vec![
            LockControllerSimpleV0Grant {
                account: other.into(),
                roles: vec![],
            },
            LockControllerSimpleV0Grant {
                account: account.into(),
                roles: vec![LockControllerSimpleV0Capability::Send],
            },
            LockControllerSimpleV0Grant {
                account: account.into(),
                roles: vec![LockControllerSimpleV0Capability::Fund; 256],
            },
        ],
        tokens: vec![],
        memo: None,
        metadata: None,
    });
    let expected = LockConfig::SimpleV0(LockConfigSimpleV0 {
        grants: vec![
            LockControllerSimpleV0Grant {
                account: account.into(),
                roles: vec![
                    LockControllerSimpleV0Capability::Fund,
                    LockControllerSimpleV0Capability::Send,
                ],
            },
            LockControllerSimpleV0Grant {
                account: other.into(),
                roles: vec![],
            },
        ],
        recipients: LockRecipients::Any,
        expiry: TransactionTime::from_seconds(1_000),
        tokens: vec![],
        memo: None,
        metadata: None,
    });
    let payload = OperationsPayload {
        operations: RawCbor::from(cbor::cbor_encode(&vec![Operation::from(
            operations::create_lock(config),
        )])),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            plt_scheduler::TransactionContext {
                energy_limit: Energy::from(u64::MAX),
                sender_account_address: account,
                transaction_sequence_number: 1.into(),
                block_timestamp: 0.into(),
                block_height: 0.into(),
            },
            account_index,
            Payload::TokenUpdate {
                payload: TokenUpdatePayload::Unscoped(payload),
            },
        )
        .expect("lock creation must succeed");
    // Operation charge, excluding transaction overhead.
    assert_eq!(
        result.energy_used - cost::PLT_OPERATIONS_TRANSACTIONS,
        Energy::from(120),
    );
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);
    assert_eq!(
        events,
        vec![BlockItemEvent::LockCreated(LockCreateEvent {
            lock_id: lock_id.clone(),
            lock_config: RawCbor::from(cbor::cbor_encode(&expected))
        })]
    );
    let info: concordium_base::protocol_level_locks::LockInfo =
        cbor::cbor_decode(block_state.query_lock_info(&context, &lock_id).unwrap()).unwrap();
    assert_eq!(info.config, expected);

    let stored_lock = block_state
        .lock_by_id(&context, &lock_id)
        .expect("lock lookup must succeed")
        .expect("lock must exist");
    let stored_configuration = stored_lock
        .lock_configuration(&context)
        .expect("lock configuration must load");
    let bytes = concordium_base::common::to_bytes(stored_configuration.as_ref());
    let reloaded: plt_block_state::persistent::protocol_level_locks::p11::LockConfig =
        concordium_base::common::from_bytes_complete(&bytes)
            .expect("canonical lock configuration must reload completely");

    assert_eq!(reloaded, *stored_configuration);
    let plt_block_state::persistent::protocol_level_locks::p11::LockConfig::SimpleV0(controller) =
        reloaded;
    assert_eq!(
        controller.grants()[0].roles(),
        [
            LockControllerSimpleV0Capability::Fund,
            LockControllerSimpleV0Capability::Send
        ]
    );
}

#[test]
fn test_create_any_recipient_lock_normalizes_tokens() {
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

    let plt_a: TokenId = "pltA".parse().unwrap();
    let payload = UpdatePayload::CreatePlt(CreatePlt {
        token_id: plt_a.clone(),
        token_module: TOKEN_MODULE_REF,
        decimals: 2,
        initialization_parameters: cbor::cbor_encode(&parameters).into(),
    });
    block_state
        .execute_chain_update(&mut context, payload)
        .expect("create pltA");

    let config = LockConfig::SimpleV0(LockConfigSimpleV0 {
        recipients: LockRecipients::Any,
        expiry: TransactionTime::from_seconds(1000),
        grants: vec![LockControllerSimpleV0Grant {
            account: account_1.into(),
            roles: vec![LockControllerSimpleV0Capability::Fund],
        }],
        tokens: vec![plt_a.clone(), plt_x.clone()],
        memo: None,
        metadata: None,
    });
    for (index, references) in [
        vec!["PLTx", "plta", "pltx", "PLTA"],
        vec!["PLTA", "pltX", "pltA", "PLTX"],
        vec!["pltx", "PLTX", "missing"],
    ]
    .into_iter()
    .enumerate()
    {
        let sequence = index as u64 + 1;
        let mut input = config.clone();
        let LockConfig::SimpleV0(controller) = &mut input;
        controller.tokens = references
            .into_iter()
            .map(|id| id.parse().unwrap())
            .collect();
        let operations: Vec<Operation> = vec![operations::create_lock(input).into()];
        let payload = OperationsPayload {
            operations: RawCbor::from(cbor::cbor_encode(&operations)),
        };

        let result = block_state
            .execute_transaction(
                &mut context,
                plt_scheduler::TransactionContext {
                    energy_limit: Energy::from(u64::MAX),
                    sender_account_address: account_1,
                    transaction_sequence_number: sequence.into(),
                    block_timestamp: 0.into(),
                    block_height: 0.into(),
                },
                account_index_1,
                Payload::TokenUpdate {
                    payload: TokenUpdatePayload::Unscoped(payload),
                },
            )
            .expect("transaction internal error");
        let lock_id = LockId::new(account_index_1, sequence, 0);
        if index == 2 {
            assert_matches!(result.outcome, TransactionOutcome::Rejected(
                TransactionRejectReason::NonExistentTokenId(id)
            ) if id == "missing".parse::<TokenId>().unwrap());
            assert!(block_state.lock_by_id(&context, &lock_id).unwrap().is_err());
            continue;
        }
        let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);
        assert_eq!(events.len(), 1);
        assert_eq!(
            events[0],
            BlockItemEvent::LockCreated(LockCreateEvent {
                lock_id: lock_id.clone(),
                lock_config: RawCbor::from(cbor::cbor_encode(&config))
            })
        );

        let lock = block_state
            .lock_by_id(&context, &lock_id)
            .expect("lock lookup must succeed")
            .expect("lock must exist");
        let configuration = lock
            .lock_configuration(&context)
            .expect("lock configuration must load");
        assert!(
            matches!(configuration.as_ref(), plt_block_state::persistent::protocol_level_locks::p11::LockConfig::SimpleV0(config) if config.recipients.is_any())
        );
        let plt_block_state::persistent::protocol_level_locks::p11::LockConfig::SimpleV0(
            controller,
        ) = configuration.as_ref();
        assert_eq!(controller.tokens(), &[plt_a.clone(), plt_x.clone()]);
        let info: concordium_base::protocol_level_locks::LockInfo =
            cbor::cbor_decode(block_state.query_lock_info(&context, &lock_id).unwrap()).unwrap();
        assert_eq!(info.config, config);
    }
}

#[test]
fn test_expired_creation_charges_configuration_complexity() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let sender = context.external.create_account().account_index();
    let address = context.external.account_canonical_address(sender);
    let token_id: TokenId = "pltX".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(),
        0,
        None,
    );
    let config = LockConfig::SimpleV0(LockConfigSimpleV0 {
        recipients: LockRecipients::Any,
        expiry: TransactionTime::from_seconds(0),
        grants: vec![LockControllerSimpleV0Grant {
            account: address.into(),
            roles: vec![LockControllerSimpleV0Capability::Fund],
        }],
        tokens: vec![token_id],
        memo: None,
        metadata: None,
    });
    let summary = block_state
        .execute_transaction(
            &mut context,
            plt_scheduler::TransactionContext {
                energy_limit: Energy::from(u64::MAX),
                sender_account_address: address,
                transaction_sequence_number: 1.into(),
                block_timestamp: 1.into(),
                block_height: 0.into(),
            },
            sender,
            Payload::TokenUpdate {
                payload: TokenUpdatePayload::Unscoped(OperationsPayload {
                    operations: RawCbor::from(cbor::cbor_encode(&vec![Operation::from(
                        operations::create_lock(config),
                    )])),
                }),
            },
        )
        .expect("expired lock creation must execute");
    assert_eq!(
        summary.energy_used - cost::PLT_OPERATIONS_TRANSACTIONS,
        Energy::from(610), // 100 + 500 × one funder × one token + 10 × one grant.
    );
    let lock_id = LockId::new(sender, 1, 0);
    assert_matches!(summary.outcome, TransactionOutcome::Rejected(
        TransactionRejectReason::LockExpired(id)
    ) if id == lock_id);
    assert!(matches!(
        block_state.lock_by_id(&context, &lock_id),
        Ok(Err(_))
    ));
}

#[test]
fn lock_creation_enforces_expiry_and_maximum_duration() {
    let lock_id = LockId::new(0, 1, 0);

    let (result, lock_exists) = execute_lock_create_with_duration(0, 1, u64::MAX);
    let summary = result.expect("expired lock creation must execute");
    assert_eq!(
        summary.energy_used - cost::PLT_OPERATIONS_TRANSACTIONS,
        Energy::from(100),
    );
    assert_matches!(summary.outcome, TransactionOutcome::Rejected(
        TransactionRejectReason::LockExpired(id)
    ) if id == lock_id);
    assert!(!lock_exists);

    let (result, lock_exists) = execute_lock_create_with_duration(1, 1_000, 0);
    let outcome = result.expect("boundary lock creation must execute").outcome;
    assert_matches!(outcome, TransactionOutcome::Success(_));
    assert!(lock_exists);

    let (result, lock_exists) = execute_lock_create_with_duration(2, 1_000, 999);
    let summary = result.expect("overlong lock creation must execute");
    assert_eq!(
        summary.energy_used - cost::PLT_OPERATIONS_TRANSACTIONS,
        Energy::from(100),
    );
    assert_matches!(summary.outcome, TransactionOutcome::Rejected(
        TransactionRejectReason::LockDurationTooLong(id)
    ) if id == lock_id);
    assert!(!lock_exists);
}

#[test]
fn lock_creation_maximum_deadline_inclusive() {
    let (result, lock_exists) = execute_lock_create_with_duration(2, 1_500, 500);
    let outcome = result.expect("deadline lock creation must execute").outcome;
    assert_matches!(outcome, TransactionOutcome::Success(_));
    assert!(lock_exists);

    let (result, lock_exists) = execute_lock_create_with_duration(2, 1_500, 499);
    let summary = result.expect("overlong lock creation must execute");
    assert_eq!(
        summary.energy_used - cost::PLT_OPERATIONS_TRANSACTIONS,
        Energy::from(100),
    );
    assert_matches!(
        summary.outcome,
        TransactionOutcome::Rejected(TransactionRejectReason::LockDurationTooLong(_))
    );
    assert!(!lock_exists);
}

#[test]
fn lock_creation_duration_reject_reports_its_creation_order() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let account_index = context.external.create_account().account_index();
    let account = context.external.account_canonical_address(account_index);
    let config = |expiry| {
        LockConfig::SimpleV0(LockConfigSimpleV0 {
            recipients: LockRecipients::Any,
            expiry: TransactionTime::from_seconds(expiry),
            grants: vec![],
            tokens: vec![],
            memo: None,
            metadata: None,
        })
    };
    let payload = OperationsPayload {
        operations: RawCbor::from(cbor::cbor_encode(&vec![
            Operation::from(operations::create_lock(config(1))),
            Operation::from(operations::create_lock(config(2))),
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
            block_height: 0.into(),
        },
        Account::from_existing_account(account_index),
        Payload::TokenUpdate {
            payload: TokenUpdatePayload::Unscoped(payload),
        },
        &PersistentChainParametersP11 {
            max_lock_duration: Duration::from_millis(1_000),
        },
    )
    .expect("multi-operation lock creation must execute");
    assert_eq!(
        result.energy_used - cost::PLT_OPERATIONS_TRANSACTIONS,
        Energy::from(200), // two locks with "zero" configuration complexity
    );
    assert_matches!(result.outcome, TransactionOutcome::Rejected(
        TransactionRejectReason::LockDurationTooLong(lock_id)
    ) if lock_id == LockId::new(account_index, 1, 1));
}
