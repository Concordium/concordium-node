//! Tests for the new `query_lock_list` and `query_lock_info` scheduler query functions.

use crate::utils::BlockStateLatest;
use crate::utils::TokenInitTestParams;
use crate::utils::entity_traits::scheduler::SchedulerOperations;
use assert_matches::assert_matches;
use concordium_base::common::cbor;
use concordium_base::common::cbor::value::Value;
use concordium_base::common::types::TransactionTime;
use concordium_base::protocol_level_locks::LockInfo;
use concordium_base::protocol_level_locks::{
    LockCapability, LockConfig, LockConfigVersion, LockCreateConfig, LockId, LockMetadata,
    LockRecipients, LockVariant, SimpleLockConfigV0,
};
use concordium_base::protocol_level_tokens::{CborHolderAccount, RawCbor, TokenId};
use concordium_base::protocol_level_tokens::{Operations, OperationsPayload, operations};
use concordium_base::transactions::{Payload, TokenUpdatePayload};
use plt_block_state::entity::block_state::LockNotFoundByIdError;
use plt_block_state::entity::entity_test_stub;
use plt_block_state::persistent::protocol_level_locks::p11::LockRole;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use std::collections::HashMap;

mod utils;

/// `query_lock_info` produces the canonical CBOR encoding of the assembled
/// `LockInfo` value, and the round-trip via `cbor_decode` recovers an equal value.
#[test]
fn test_query_lock_info_cbor_round_trip_with_funded_balances() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let recipient = context.external.create_account();
    let funding_account = context.external.create_account();
    let recipient_addr = context
        .external
        .account_canonical_address(recipient.account_index());
    let funding_addr = context
        .external
        .account_canonical_address(funding_account.account_index());

    let token_id: TokenId = "TokenLockA".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        Some(RawTokenAmount::from(0)),
    );
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        funding_account.account_index(),
        &token_id,
        RawTokenAmount::from(123_400),
    );

    let lock_id = LockId {
        account_index: funding_account.account_index().into(),
        sequence_number: 1,
        creation_order: 0,
    };
    let lock_config = utils::CreateLockSimpleConfig {
        recipients: vec![recipient.account_index()],
        roles: vec![LockRole::new(
            funding_account.account_index(),
            vec![LockCapability::Fund],
        )],
        tokens: vec![token_id.clone()],
        expiry: 1_804_806_000,
    };
    utils::create_lock(&mut context, &mut block_state, &lock_id, lock_config);
    utils::lock_balance(
        &mut context,
        &mut block_state,
        &lock_id,
        funding_account.account_index(),
        &token_id,
        RawTokenAmount::from(100),
    );

    let bytes = block_state
        .query_lock_info(&context, &lock_id)
        .expect("query_lock_info must succeed for an existing lock");
    assert_eq!(
        hex::encode(&bytes),
        "a3646c6f636bd99fd8830101006566756e647381a2676163636f756e74d99d73a201d99d71a101190397035820000000000000000100000000000000000000000000000000000000000000000067616d6f756e747381a265746f6b656e6a546f6b656e4c6f636b4166616d6f756e74c48221186466636f6e666967a765726f6c657381a265726f6c6573816466756e64676163636f756e74d99d73a201d99d71a101190397035820000000000000000100000000000000000000000000000000000000000000000066657870697279c11a6b93277066746f6b656e73816a546f6b656e4c6f636b4167686f6c6465727381d99d73a201d99d71a10119039703582000000000000000010000000000000000000000000000000000000000000000006776617269616e746873696d706c6556306776657273696f6e006a726563697069656e747381d99d73a201d99d71a1011903970358200000000000000000000000000000000000000000000000000000000000000000"
    );
    let decoded: LockInfo =
        cbor::cbor_decode(&bytes).expect("CBOR encoding produced by query_lock_info must decode");

    assert_eq!(decoded.lock, lock_id);
    assert_eq!(
        decoded.config,
        LockConfig {
            version: LockConfigVersion::V0,
            variant: LockVariant::SimpleV0,
            recipients: LockRecipients::Limited(vec![CborHolderAccount::from(recipient_addr)]),
            holders: vec![CborHolderAccount::from(funding_addr)],
            expiry: TransactionTime::from(1_804_806_000),
            roles: vec![concordium_base::protocol_level_locks::LockRole {
                account: CborHolderAccount::from(funding_addr),
                roles: vec![LockCapability::Fund],
            }],
            tokens: vec![token_id.clone()],
            metadata: None,
        }
    );
    assert_eq!(decoded.funds.len(), 1);
    assert_eq!(
        decoded.funds[0].account,
        CborHolderAccount::from(funding_addr)
    );
    assert_eq!(decoded.funds[0].amounts.len(), 1);
    assert_eq!(decoded.funds[0].amounts[0].token, token_id);
    assert_eq!(decoded.funds[0].amounts[0].amount.value(), 100);
    assert_eq!(decoded.funds[0].amounts[0].amount.decimals(), 2);
}

/// `query_lock_info` preserves unrestricted recipients and metadata.
#[test]
fn test_query_lock_info_any_recipient() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let owner = context.external.create_account();
    let token_id: TokenId = "TokenLockAny".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        Some(RawTokenAmount::from(0)),
    );

    let lock_id = LockId {
        account_index: owner.account_index().into(),
        sequence_number: 1,
        creation_order: 0,
    };
    let sender_addr = context
        .external
        .account_canonical_address(owner.account_index());
    let metadata = LockMetadata {
        name: Some("Any recipient lock".to_string()),
        description: Some("Metadata returned by GetLockInfo".to_string()),
        additional: HashMap::from([("purpose".to_string(), Value::Text("query test".to_string()))]),
    };
    let operations = Operations {
        operations: vec![
            operations::create_lock(LockCreateConfig::SimpleV0(SimpleLockConfigV0 {
                recipients: LockRecipients::Any,
                holders: vec![CborHolderAccount::from(sender_addr)],
                expiry: TransactionTime::from(1_804_806_000u64),
                roles: vec![],
                tokens: vec![token_id.clone()],
                metadata: Some(metadata.encode_raw_cbor()),
            }))
            .into(),
        ],
    };
    block_state
        .execute_transaction(
            &mut context,
            plt_scheduler::TransactionContext {
                energy_limit: concordium_base::base::Energy::from(u64::MAX),
                sender_account_address: sender_addr,
                transaction_sequence_number: 1.into(),
                block_timestamp: 0.into(),
                block_height: 0.into(),
            },
            owner.account_index(),
            Payload::TokenUpdate {
                payload: TokenUpdatePayload::Unscoped(OperationsPayload {
                    operations: RawCbor::from(cbor::cbor_encode(&operations)),
                }),
            },
        )
        .expect("create any-recipient lock must succeed");

    let bytes = block_state
        .query_lock_info(&context, &lock_id)
        .expect("query_lock_info must succeed for an existing lock");
    let decoded: LockInfo =
        cbor::cbor_decode(&bytes).expect("CBOR encoding produced by query_lock_info must decode");

    assert_eq!(decoded.config.recipients, LockRecipients::Any);
    assert_eq!(decoded.config.metadata, Some(metadata.encode_raw_cbor()));
}

#[test]
fn test_query_lock_info_unknown_lock() {
    let context = entity_test_stub::new_stubbed_context();
    let block_state = BlockStateLatest::default();
    let unknown = LockId {
        account_index: 999,
        sequence_number: 999,
        creation_order: 0,
    };
    let result = block_state.query_lock_info(&context, &unknown);
    assert_matches!(result, Err(LockNotFoundByIdError(_)));
}

/// Discovery requires effective operation permissions, not other account relationships.
#[test]
fn test_discovery_effective_roles_exclusions_and_no_matches() {
    use LockCapability::{Cancel, Fund, Release, Send};
    use plt_block_state::persistent::protocol_level_locks::p11 as persistent;
    use plt_scheduler::protocol_level_locks::p11::query_locks_by_account;

    let mut context = entity_test_stub::new_stubbed_context();
    let mut state = BlockStateLatest::default();
    let target = context.external.create_account().account_index();
    let other = context.external.create_account().account_index();
    let absent = context.external.create_account().account_index();
    let balance_holder = context.external.create_account().account_index();
    let roles = [
        vec![Fund],       // A Fund role matches.
        vec![Release],    // A Release role matches.
        vec![Send],       // A Send role matches.
        vec![Cancel],     // A Cancel role matches.
        vec![],           // An empty persistent role does not match.
        vec![Fund, Send], // Multiple capabilities still yield one match.
    ];
    let mut matching = Vec::new();
    // Odd locks have target as creator and recipient, but not in their roles.
    for sequence_number in 0..12 {
        let id = LockId {
            account_index: target.into(),
            sequence_number,
            creation_order: 0,
        };
        let roles = if sequence_number % 2 == 0 {
            let capabilities = roles[sequence_number as usize / 2].clone();
            if !capabilities.is_empty() {
                matching.push(id.clone());
            }
            vec![
                LockRole::new(target, capabilities),
                LockRole::new(other, vec![]),
                // A duplicate account role must not duplicate the returned lock.
                LockRole::new(target, vec![]),
            ]
        } else {
            vec![LockRole::new(other, vec![Fund])]
        };
        let config = persistent::LockConfig::SimpleV0(
            persistent::LockConfigSimpleV0::new(
                vec![target].try_into().unwrap(),
                TransactionTime::from(1_804_806_000),
                roles,
                vec![],
                None,
            )
            .unwrap(),
        );
        state.create_lock(&context, &id, config, 0.into()).unwrap();
    }
    let token_id: TokenId = "DiscoveryToken".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        Some(RawTokenAmount::from(0)),
    );
    utils::lock_balance(
        &mut context,
        &mut state,
        &LockId {
            account_index: target.into(),
            sequence_number: 1,
            creation_order: 0,
        },
        balance_holder,
        &token_id,
        RawTokenAmount::from(100),
    );

    let actual: Vec<_> = query_locks_by_account(&context, &state, target)
        .unwrap()
        .into_iter()
        .map(|(id, _)| id)
        .collect();
    let expected: Vec<_> = state
        .lock_list(&context)
        .unwrap()
        .into_iter()
        .filter(|id| matching.contains(id))
        .collect();
    // Include effective role matches exactly once, preserving lock-list order.
    assert_eq!(actual, expected);
    for account in [
        absent,         // An account with no roles or other relationships has no matches.
        balance_holder, // Holding locked funds alone does not match.
    ] {
        assert!(
            query_locks_by_account(&context, &state, account)
                .unwrap()
                .is_empty()
        );
    }
}
