//! General tests for token update transactions via the scheduler.

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
use plt_block_state::block_state_interface::BlockStateQuery;
use plt_scheduler::scheduler;
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;
use plt_scheduler_types::types::tokens::RawTokenAmount;

mod utils;

const NON_EXISTING_ACCOUNT: AccountAddress = AccountAddress([2u8; 32]);

/// Test failure to decode token operations.
#[test]
fn test_update_token_decode_failure() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 0, None);

    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(vec![]),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        1.into(),
        stub.state_mut(),
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
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 0, None);
    let receiver = stub.create_account();

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&receiver)),
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
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        1.into(),
        stub.state_mut(),
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
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, _) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.increment_account_balance(sender, token, RawTokenAmount(5000));
    stub.increment_account_balance(receiver, token, RawTokenAmount(2000));

    let operations = vec![
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(1000, 2),
            recipient: CborHolderAccount::from(stub.account_canonical_address(&receiver)),
            memo: None,
        }),
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(2000, 2),
            recipient: CborHolderAccount::from(stub.account_canonical_address(&receiver)),
            memo: None,
        }),
    ];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        sender,
        stub.account_canonical_address(&sender),
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");

    assert_matches!(result.outcome, TransactionOutcome::Success(_));
    assert_eq!(
        stub.state().account_token_balance(&sender, &token),
        RawTokenAmount(2000)
    );
    assert_eq!(
        stub.state().account_token_balance(&receiver, &token),
        RawTokenAmount(5000)
    );
}

/// Test transaction with multiple operations where one of them fails.
/// The failing operation is placed first so no state changes occur on rejection.
#[test]
fn test_single_failing_operation() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, _gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.increment_account_balance(sender, token, RawTokenAmount(5000));

    let operations = vec![
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(2000, 2),
            recipient: CborHolderAccount::from(NON_EXISTING_ACCOUNT),
            memo: None,
        }),
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(1000, 2),
            recipient: CborHolderAccount::from(stub.account_canonical_address(&receiver)),
            memo: None,
        }),
    ];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        sender,
        stub.account_canonical_address(&sender),
        1.into(),
        stub.state_mut(),
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
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, _gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.increment_account_balance(sender, token, RawTokenAmount(5000));

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&receiver)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        sender,
        stub.account_canonical_address(&sender),
        1.into(),
        stub.state_mut(),
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
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, _gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.increment_account_balance(sender, token, RawTokenAmount(5000));

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&receiver)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    // 50 energy is less than the 300 lookup cost.
    let result = scheduler::execute_transaction(
        sender,
        stub.account_canonical_address(&sender),
        1.into(),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(50),
    )
    .expect("transaction internal error");

    assert_matches!(
        result.outcome,
        TransactionOutcome::Rejected(TransactionRejectReason::OutOfEnergy)
    );
}
