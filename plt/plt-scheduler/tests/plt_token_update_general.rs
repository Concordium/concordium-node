//! General tests for token update transactions via the scheduler.

use crate::utils::SchedulerOperations;
use crate::utils::TokenInitTestParams;
use assert_matches::assert_matches;
use concordium_base::base::Energy;
use concordium_base::common::cbor;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    AddressNotFoundRejectReason, CborHolderAccount, DeserializationFailureRejectReason, RawCbor,
    TokenAmount, TokenId, TokenModuleRejectReason, TokenOperation, TokenOperationsPayload,
    TokenTransfer,
};
use concordium_base::transactions::Payload;
use plt_block_state::entity::entity_test_stub;
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;
use plt_scheduler_types::types::tokens::RawTokenAmount;

use crate::utils::BlockStateLatest;

mod utils;

const NON_EXISTING_ACCOUNT: AccountAddress = AccountAddress([2u8; 32]);

/// Test failure to decode token operations.
#[test]
fn test_update_token_decode_failure() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(),
        0,
        None,
    );

    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(vec![]),
    };
    let gov_account_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &gov_account,
            gov_account_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(reject_reason, TokenModuleRejectReason::DeserializationFailure(
        DeserializationFailureRejectReason { cause: Some(cause) }) => {
        assert!(cause.contains("IO error"), "cause: {}", cause);
    });
}

/// Test additional fields specified in token update operation.
#[test]
fn test_update_token_additional_fields() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(),
        0,
        None,
    );
    let receiver = context.external.create_account();

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(
            context
                .external
                .account_canonical_address(receiver.account_index()),
        ),
        memo: None,
    })];

    let mut dynamic_operations: cbor::value::Value =
        cbor::cbor_decode(cbor::cbor_encode(&operations)).unwrap();
    let operations_array =
        assert_matches!(&mut dynamic_operations, cbor::value::Value::Array(array) => array);
    let operation0_outer_map =
        assert_matches!(&mut operations_array[0], cbor::value::Value::Map(map) => map);
    let operation0_map =
        assert_matches!(&mut operation0_outer_map[0].1, cbor::value::Value::Map(map) => map);
    operation0_map.push((
        cbor::value::Value::Text("additionalField".to_string()),
        cbor::value::Value::Text("testvalue1".to_string()),
    ));

    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&dynamic_operations)),
    };
    let gov_account_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &gov_account,
            gov_account_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(reject_reason, TokenModuleRejectReason::DeserializationFailure(
        DeserializationFailureRejectReason { cause: Some(cause) }) => {
        assert!(cause.contains("unknown map key"), "cause: {}", cause);
    });
}

/// Test transaction with multiple operations.
#[test]
fn test_multiple_operations() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = context.external.create_account();
    let receiver = context.external.create_account();
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        sender.account_index(),
        &token_id,
        RawTokenAmount(5000),
    );
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        receiver.account_index(),
        &token_id,
        RawTokenAmount(2000),
    );

    let operations = vec![
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(1000, 2),
            recipient: CborHolderAccount::from(
                context
                    .external
                    .account_canonical_address(receiver.account_index()),
            ),
            memo: None,
        }),
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(2000, 2),
            recipient: CborHolderAccount::from(
                context
                    .external
                    .account_canonical_address(receiver.account_index()),
            ),
            memo: None,
        }),
    ];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &sender,
            sender_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");

    assert_matches!(result.outcome, TransactionOutcome::Success(_));
    assert_eq!(
        sender.account_token_balance(&context, token_index),
        RawTokenAmount(2000)
    );
    assert_eq!(
        receiver.account_token_balance(&context, token_index),
        RawTokenAmount(5000)
    );
}

/// Test transaction with multiple operations where one of them fails.
/// The failing operation is placed first so no state changes occur on rejection.
#[test]
fn test_single_failing_operation() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = context.external.create_account();
    let receiver = context.external.create_account();
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        sender.account_index(),
        &token_id,
        RawTokenAmount(5000),
    );

    let operations = vec![
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(2000, 2),
            recipient: CborHolderAccount::from(NON_EXISTING_ACCOUNT),
            memo: None,
        }),
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(1000, 2),
            recipient: CborHolderAccount::from(
                context
                    .external
                    .account_canonical_address(receiver.account_index()),
            ),
            memo: None,
        }),
    ];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &sender,
            sender_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(reject_reason, TokenModuleRejectReason::AddressNotFound(
        AddressNotFoundRejectReason { index, address }) => {
        assert_eq!(address.address, NON_EXISTING_ACCOUNT);
        assert_eq!(index, 0);
    });
}

/// Test that energy is charged for execution of operations.
#[test]
fn test_energy_charge() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = context.external.create_account();
    let receiver = context.external.create_account();
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        sender.account_index(),
        &token_id,
        RawTokenAmount(5000),
    );

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(
            context
                .external
                .account_canonical_address(receiver.account_index()),
        ),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &sender,
            sender_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(1000),
        )
        .expect("transaction internal error");

    assert_matches!(result.outcome, TransactionOutcome::Success(_));
    // 300 (lookup) + 100 (1 operation) = 400 total energy used.
    assert_eq!(result.energy_used.energy, 400);
}

/// Test hitting out of energy error.
#[test]
fn test_out_of_energy_error() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = context.external.create_account();
    let receiver = context.external.create_account();
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        sender.account_index(),
        &token_id,
        RawTokenAmount(5000),
    );

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(
            context
                .external
                .account_canonical_address(receiver.account_index()),
        ),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    // 50 energy is less than the 300 lookup cost.
    let sender_addr = context
        .external
        .account_canonical_address(sender.account_index());
    let result = block_state
        .execute_transaction(
            &mut context,
            &sender,
            sender_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(50),
        )
        .expect("transaction internal error");

    assert_matches!(
        result.outcome,
        TransactionOutcome::Rejected(TransactionRejectReason::OutOfEnergy)
    );
}
