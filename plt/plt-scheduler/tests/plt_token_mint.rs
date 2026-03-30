//! Tests for token mint operations via the scheduler.

use assert_matches::assert_matches;
use concordium_base::base::{Energy, ProtocolVersion};
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, DeserializationFailureRejectReason, MintWouldOverflowRejectReason,
    OperationNotPermittedRejectReason, RawCbor, TokenAdminRole, TokenAmount, TokenId,
    TokenModuleRejectReason, TokenOperation, TokenOperationsPayload, TokenPauseDetails,
    TokenSupplyUpdateDetails, TokenUpdateAdminRolesDetails, UnsupportedOperationRejectReason,
};
use concordium_base::transactions::Payload;
use plt_block_state::block_state_interface::BlockStateQuery;
use plt_scheduler::scheduler;
use plt_scheduler_types::types::events::BlockItemEvent;
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::tokens::{RawTokenAmount, TokenHolder};
use utils::block_state_external_stubbed::{
    BlockStateWithExternalStateStubbed, TokenInitTestParams,
};

mod utils;

/// Test successful mints on P10.
#[test]
fn test_mint_p10() {
    let mut stub = BlockStateWithExternalStateStubbed::new(ProtocolVersion::P10);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );

    // First mint
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    assert_eq!(
        stub.state().account_token_balance(&gov_account, &token),
        RawTokenAmount(1000)
    );

    // Second mint
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(4000, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    assert_eq!(
        stub.state().account_token_balance(&gov_account, &token),
        RawTokenAmount(5000)
    );
}

/// Rejects mint operations from non-governance accounts on protocol version 10.
#[test]
fn test_unauthorized_mint_p10() {
    let mut stub = BlockStateWithExternalStateStubbed::new(ProtocolVersion::P10);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let non_governance_account = stub.create_account();

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        non_governance_account,
        stub.account_canonical_address(&non_governance_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index,
            address,
            ..
        }) => {
            assert_eq!(index, 0);
            assert_eq!(
                address,
                Some(CborHolderAccount::from(
                    stub.account_canonical_address(&non_governance_account)
                ))
            );
        }
    );

    // Assert balances remain unchanged.
    assert_eq!(
        stub.state().account_token_balance(&gov_account, &token),
        RawTokenAmount(0)
    );
    assert_eq!(
        stub.state()
            .account_token_balance(&non_governance_account, &token),
        RawTokenAmount(0)
    );
}

/// Test successful mints.
#[test]
fn test_mint() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );

    // First mint
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    assert_eq!(
        stub.state().account_token_balance(&gov_account, &token),
        RawTokenAmount(1000)
    );

    // Second mint
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(4000, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    assert_eq!(
        stub.state().account_token_balance(&gov_account, &token),
        RawTokenAmount(5000)
    );
}

/// Rejects mint operations from non-governance accounts.
#[test]
fn test_unauthorized_mint() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let non_governance_account = stub.create_account();

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        non_governance_account,
        stub.account_canonical_address(&non_governance_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index,
            address,
            ..
        }) => {
            assert_eq!(index, 0);
            assert_eq!(
                address,
                Some(CborHolderAccount::from(
                    stub.account_canonical_address(&non_governance_account)
                ))
            );
        }
    );

    // Assert balances remain unchanged.
    assert_eq!(
        stub.state().account_token_balance(&gov_account, &token),
        RawTokenAmount(0)
    );
    assert_eq!(
        stub.state()
            .account_token_balance(&non_governance_account, &token),
        RawTokenAmount(0)
    );
}

/// Rejects mint operations from non-governance accounts using alias address.
/// Check that address in reject reason is the alias and not the canonical address.
#[test]
fn test_unauthorized_mint_using_alias() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, _gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let non_gov_account = stub.create_account();
    let non_gov_account_address_alias = stub
        .account_canonical_address(&non_gov_account)
        .get_alias(5)
        .unwrap();

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        non_gov_account,
        non_gov_account_address_alias,
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            address,
            ..
        }) => {
            // Assert the address alias is used in the reject reason.
            assert_eq!(
                address,
                Some(CborHolderAccount::from(non_gov_account_address_alias))
            );
        }
    );
}

/// Test mint that would overflow circulating supply.
#[test]
fn test_mint_overflow() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        Some(RawTokenAmount(1000)),
    );

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(RawTokenAmount::MAX.0 - 500, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(reject_reason, TokenModuleRejectReason::MintWouldOverflow(
        MintWouldOverflowRejectReason {
            requested_amount,
            current_supply,
            max_representable_amount,
            ..
        }) => {
            assert_eq!(requested_amount, TokenAmount::from_raw(RawTokenAmount::MAX.0 - 500, 2));
            assert_eq!(current_supply, TokenAmount::from_raw(1000, 2));
            assert_eq!(max_representable_amount, TokenAmount::from_raw(RawTokenAmount::MAX.0, 2));
    });

    // Supply unchanged
    assert_eq!(
        stub.state().token_circulating_supply(&token),
        RawTokenAmount(1000)
    );
}

/// Test mint with initial supply specified with wrong number of decimals.
#[test]
fn test_mint_decimals_mismatch() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 2, None);

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(reject_reason, TokenModuleRejectReason::DeserializationFailure(
        DeserializationFailureRejectReason {
            cause: Some(cause)
        }) => {
            assert!(cause.contains("decimals mismatch"), "cause: {}", cause);
    });
}

/// Reject "mint" operations while token is paused.
#[test]
fn test_mint_paused() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );

    // Pause the token first
    let pause_ops = vec![TokenOperation::Pause(TokenPauseDetails {})];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&pause_ops)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Now attempt to mint while paused
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index: 0,
            address: None,
            reason: Some(reason),
        }) if reason == "token operation mint is paused"
    );
}

/// Reject "mint" operation if the feature is not enabled.
#[test]
fn test_not_mintable() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 2, None);

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(RawTokenAmount::MAX.0 - 500, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::UnsupportedOperation(UnsupportedOperationRejectReason {
            index: 0,
            operation_type,
            reason: Some(reason)
        }) if reason == "feature not enabled" && operation_type == "mint"
    );
}

/// Test that mint events contain expected data.
#[test]
fn test_mint_event() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 2),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenMint(mint) => {
        assert_eq!(mint.token_id, token_id);
        assert_eq!(mint.amount.amount, RawTokenAmount(1000));
        assert_eq!(mint.amount.decimals, 2);
        assert_eq!(mint.target, TokenHolder::Account(stub.account_canonical_address(&gov_account)));
    });
}

/// Rejects mint when governance account does not hold the mint role.
#[test]
fn test_reject_without_role() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );

    // Revoke mint role from governance account.
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::RevokeAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Mint],
                account: CborHolderAccount::from(stub.account_canonical_address(&gov_account)),
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Attempting to mint as governance account (no longer has mint role).
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Mint(
            TokenSupplyUpdateDetails {
                amount: TokenAmount::from_raw(200, 2),
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(r) => r);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index: 0,
            address: Some(address),
            reason: Some(reason),
        }) => {
            assert_eq!(reason, "sender is not authorized to perform the operation for this token");
            assert_eq!(address, CborHolderAccount::from(stub.account_canonical_address(&gov_account)));
        }
    );
}

/// Succeeds when another account holds the mint role.
#[test]
fn test_new_account_with_role_succeeds_mint() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    let account2 = stub.create_account();

    // Assign mint role to account2.
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::AssignAdminRoles(
            TokenUpdateAdminRolesDetails {
                roles: vec![TokenAdminRole::Mint],
                account: CborHolderAccount::from(stub.account_canonical_address(&account2)),
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Mint as account2.
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&vec![TokenOperation::Mint(
            TokenSupplyUpdateDetails {
                amount: TokenAmount::from_raw(200, 2),
            },
        )])),
    };
    let result = scheduler::execute_transaction(
        account2,
        stub.account_canonical_address(&account2),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    assert_eq!(
        stub.state().account_token_balance(&gov_account, &token),
        RawTokenAmount(0)
    );
    assert_eq!(
        stub.state().account_token_balance(&account2, &token),
        RawTokenAmount(200)
    );
}
