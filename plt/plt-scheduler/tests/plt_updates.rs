//! Test of protocol-level token updates. Detailed tests should generally be implemented in
//! the tests of the token module in the `plt-token-module` crate. In the present file,
//! higher level tests are implemented.

use crate::block_state_stub::{BlockStateStub, TokenInitTestParams};
use assert_matches::assert_matches;
use concordium_base::base::Energy;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, CborMemo, OperationNotPermittedRejectReason, RawCbor, TokenAmount, TokenId,
    TokenListUpdateDetails, TokenListUpdateEventDetails, TokenModuleEventType,
    TokenModuleRejectReason, TokenModuleState, TokenOperation, TokenOperationsPayload,
    TokenPauseDetails, TokenSupplyUpdateDetails, TokenTransfer,
};
use concordium_base::transactions::{Memo, Payload};
use plt_scheduler::block_state_interface::BlockStateQuery;
use plt_scheduler::{queries, scheduler};
use plt_types::types::events::BlockItemEvent;
use plt_types::types::execution::TransactionOutcome;
use plt_types::types::reject_reasons::TransactionRejectReason;
use plt_types::types::tokens::RawTokenAmount;

mod block_state_stub;
mod utils;

/// Test protocol-level token transfer. First transfer from governance account. And then perform
/// a second transfer from the destination of the first transfer.
#[test]
fn test_plt_transfer() {
    let mut stub = BlockStateStub::new();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default(),
        4,
        Some(RawTokenAmount(5000)),
    );
    let account2 = stub.create_account();
    let account3 = stub.create_account();

    // Transfer from governance account to account2

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(3000, 4),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&account2)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // Assert circulating supply unchanged
    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(5000));

    // Assert balance of sender and receiver
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(2000)
    );
    assert_eq!(
        stub.account_token_balance(&account2, &token),
        RawTokenAmount(3000)
    );

    // Assert transfer event
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenTransfer(transfer) => {
        assert_eq!(transfer.token_id, token_id);
        assert_eq!(transfer.amount.amount, RawTokenAmount(3000));
        assert_eq!(transfer.amount.decimals, 4);
        assert_eq!(transfer.from, stub.account_canonical_address(&gov_account));
        assert_eq!(transfer.to, stub.account_canonical_address(&account2));
        assert_eq!(transfer.memo, None);
    });

    // Transfer from account2 to account3 with memo

    let memo = Memo::try_from(cbor::cbor_encode("testvalue")).unwrap();
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 4),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&account3)),
        memo: Some(CborMemo::Cbor(memo.clone())),
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = scheduler::execute_transaction(
        account2,
        stub.account_canonical_address(&account2),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // Assert circulating supply unchanged
    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(5000));

    // Assert balance of sender and receiver
    assert_eq!(
        stub.account_token_balance(&account2, &token),
        RawTokenAmount(2000)
    );
    assert_eq!(
        stub.account_token_balance(&account3, &token),
        RawTokenAmount(1000)
    );

    // Assert transfer event
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenTransfer(transfer) => {
        assert_eq!(transfer.token_id, token_id);
        assert_eq!(transfer.amount.amount, RawTokenAmount(1000));
        assert_eq!(transfer.amount.decimals, 4);
        assert_eq!(transfer.from, stub.account_canonical_address(&account2));
        assert_eq!(transfer.to, stub.account_canonical_address(&account3));
        assert_eq!(transfer.memo, Some(memo));
    });
}

/// Test protocol-level token transfer using address aliases for sender and receiver.
#[test]
fn test_plt_transfer_using_aliases() {
    let mut stub = BlockStateStub::new();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default(),
        4,
        Some(RawTokenAmount(5000)),
    );
    let account2 = stub.create_account();

    let gov_account_address_alias = stub
        .account_canonical_address(&gov_account)
        .get_alias(5)
        .unwrap();
    let account2_alias_address = stub
        .account_canonical_address(&account2)
        .get_alias(10)
        .unwrap();

    // Transfer from governance account to account2

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(3000, 4),
        recipient: CborHolderAccount::from(account2_alias_address),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = scheduler::execute_transaction(
        gov_account,
        gov_account_address_alias,
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // Assert balance of sender and receiver
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(2000)
    );
    assert_eq!(
        stub.account_token_balance(&account2, &token),
        RawTokenAmount(3000)
    );

    // Assert transfer event
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenTransfer(transfer) => {
        assert_eq!(transfer.token_id, token_id);
        assert_eq!(transfer.amount.amount, RawTokenAmount(3000));
        assert_eq!(transfer.amount.decimals, 4);
        assert_eq!(transfer.from, gov_account_address_alias);
        assert_eq!(transfer.to, account2_alias_address);
        assert_eq!(transfer.memo, None);
    });
}

/// Test protocol-level token transfer that is rejected.
#[test]
fn test_plt_transfer_reject() {
    let mut stub = BlockStateStub::new();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default(),
        4,
        Some(RawTokenAmount(5000)),
    );
    let account2 = stub.create_account();

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(10000, 4),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&account2)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(reject_reason) => reject_reason);

    // Assert circulating supply and account balances unchanged
    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(5000));
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(5000)
    );
    assert_eq!(
        stub.account_token_balance(&account2, &token),
        RawTokenAmount(0)
    );

    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::TokenBalanceInsufficient(_)
    );
}

/// Test allow list enforcement with transfer retry.
#[test]
fn test_plt_transfer_allow_list_flow() {
    let mut stub = BlockStateStub::new();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        4,
        Some(RawTokenAmount(5000)),
    );
    let receiver = stub.create_account();

    // Add only the sender to the allow list.
    let operations = vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_canonical_address(&gov_account)),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenModule(event) => {
        assert_eq!(event.token_id, token_id);
        assert_eq!(event.event_type, TokenModuleEventType::AddAllowList.to_type_discriminator());
        let details: TokenListUpdateEventDetails = cbor::cbor_decode(&event.details).unwrap();
        assert_eq!(details.target, CborHolderAccount::from(stub.account_canonical_address(&gov_account)));
    });

    // Transfer fails because the receiver is not allow-listed yet.
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 4),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&receiver)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(reject_reason) => reject_reason);

    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(5000));
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(5000)
    );
    assert_eq!(
        stub.account_token_balance(&receiver, &token),
        RawTokenAmount(0)
    );

    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            address: Some(address),
            reason: Some(reason),
            ..
        }) => {
            assert_eq!(address, CborHolderAccount::from(stub.account_canonical_address(&receiver)));
            assert_eq!(reason, "recipient not in allow list");
        }
    );

    // Add the receiver to the allow list.
    let operations = vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_canonical_address(&receiver)),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenModule(event) => {
        assert_eq!(event.token_id, token_id);
        assert_eq!(event.event_type, TokenModuleEventType::AddAllowList.to_type_discriminator());
        let details: TokenListUpdateEventDetails = cbor::cbor_decode(&event.details).unwrap();
        assert_eq!(details.target, CborHolderAccount::from(stub.account_canonical_address(&receiver)));
    });

    // Transfer succeeds once both accounts are allow-listed.
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 4),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&receiver)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(5000));
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(4000)
    );
    assert_eq!(
        stub.account_token_balance(&receiver, &token),
        RawTokenAmount(1000)
    );

    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenTransfer(transfer) => {
        assert_eq!(transfer.token_id, token_id);
        assert_eq!(transfer.amount.amount, RawTokenAmount(1000));
        assert_eq!(transfer.amount.decimals, 4);
        assert_eq!(transfer.from, stub.account_canonical_address(&gov_account));
        assert_eq!(transfer.to, stub.account_canonical_address(&receiver));
        assert_eq!(transfer.memo, None);
    });
}

/// Test protocol-level token mint.
#[test]
fn test_plt_mint() {
    let mut stub = BlockStateStub::new();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // Assert circulating supply increased
    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(1000));

    // Assert account balance increased
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(1000)
    );

    // Assert mint event
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenMint(mint) => {
        assert_eq!(mint.token_id, token_id);
        assert_eq!(mint.amount.amount, RawTokenAmount(1000));
        assert_eq!(mint.amount.decimals, 4);
        assert_eq!(mint.target, stub.account_canonical_address(&gov_account));
    });
}

/// Test protocol-level token mint using account address alias.
#[test]
fn test_plt_mint_using_alias() {
    let mut stub = BlockStateStub::new();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );

    let gov_account_address_alias = stub
        .account_canonical_address(&gov_account)
        .get_alias(5)
        .unwrap();

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = scheduler::execute_transaction(
        gov_account,
        gov_account_address_alias,
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // Assert circulating supply increased
    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(1000));

    // Assert account balance increased
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(1000)
    );

    // Assert mint event
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenMint(mint) => {
        assert_eq!(mint.token_id, token_id);
        assert_eq!(mint.amount.amount, RawTokenAmount(1000));
        assert_eq!(mint.amount.decimals, 4);
        assert_eq!(mint.target, gov_account_address_alias);
    });
}

/// Test protocol-level token mint that is rejected.
#[test]
fn test_plt_mint_reject() {
    let mut stub = BlockStateStub::new();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        Some(RawTokenAmount(5000)),
    );

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(u64::MAX, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(reject_reason) => reject_reason);

    // Assert circulating supply and account balance unchanged
    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(5000));
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(5000)
    );

    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(reject_reason, TokenModuleRejectReason::MintWouldOverflow(_));
}

/// Test protocol-level token mint from unauthorized sender.
#[test]
fn test_plt_mint_unauthorized() {
    let mut stub = BlockStateStub::new();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, _gov_account) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 4, None);
    let non_governance_account = stub.create_account();

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = scheduler::execute_transaction(
        non_governance_account,
        stub.account_canonical_address(&non_governance_account),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(reject_reason) => reject_reason);

    // Assert circulating supply and account balance unchanged
    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(0));
    assert_eq!(
        stub.account_token_balance(&non_governance_account, &token),
        RawTokenAmount(0)
    );

    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            index,
            address,
            reason,
        }) => {
            assert_eq!(index, 0);
            assert_eq!(
                address,
                Some(CborHolderAccount::from(
                    stub.account_canonical_address(&non_governance_account)
                ))
            );
            assert_eq!(reason.as_deref(), Some("sender is not the token governance account"));
        }
    );
}

/// Test protocol-level token burn.
#[test]
fn test_plt_burn() {
    let mut stub = BlockStateStub::new();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().burnable(),
        4,
        Some(RawTokenAmount(5000)),
    );

    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // Assert circulating supply decreased
    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(4000));

    // Assert account balance decreased
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(4000)
    );

    // Assert burn event
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenBurn(burn) => {
        assert_eq!(burn.token_id, token_id);
        assert_eq!(burn.amount.amount, RawTokenAmount(1000));
        assert_eq!(burn.amount.decimals, 4);
        assert_eq!(burn.target, stub.account_canonical_address(&gov_account));
    });
}

/// Test protocol-level token burn using address alias for governance account
#[test]
fn test_plt_burn_using_alias() {
    let mut stub = BlockStateStub::new();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().burnable(),
        4,
        Some(RawTokenAmount(5000)),
    );

    let gov_account_address_alias = stub
        .account_canonical_address(&gov_account)
        .get_alias(5)
        .unwrap();

    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = scheduler::execute_transaction(
        gov_account,
        gov_account_address_alias,
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // Assert circulating supply decreased
    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(4000));

    // Assert account balance decreased
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(4000)
    );

    // Assert burn event
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenBurn(burn) => {
        assert_eq!(burn.token_id, token_id);
        assert_eq!(burn.amount.amount, RawTokenAmount(1000));
        assert_eq!(burn.amount.decimals, 4);
        assert_eq!(burn.target, gov_account_address_alias);
    });
}

/// Test protocol-level token burn rejection.
#[test]
fn test_plt_burn_reject() {
    let mut stub = BlockStateStub::new();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().burnable(),
        4,
        Some(RawTokenAmount(5000)),
    );

    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(10000, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(reject_reason) => reject_reason);

    // Assert circulating supply and account balance unchanged
    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(5000));
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(5000)
    );

    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::TokenBalanceInsufficient(_)
    );
}

/// Test multiple protocol-level token update operations in one transaction.
#[test]
fn test_plt_multiple_operations() {
    let mut stub = BlockStateStub::new();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    let account2 = stub.create_account();

    // Compose two operations: Mint and then transfer

    let operations = vec![
        TokenOperation::Mint(TokenSupplyUpdateDetails {
            amount: TokenAmount::from_raw(3000, 4),
        }),
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(1000, 4),
            recipient: CborHolderAccount::from(stub.account_canonical_address(&account2)),
            memo: None,
        }),
    ];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // Assert circulating supply and accout balances
    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(3000));
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(2000)
    );
    assert_eq!(
        stub.account_token_balance(&account2, &token),
        RawTokenAmount(1000)
    );

    // Assert two events in right order
    assert_eq!(events.len(), 2);
    assert_matches!(&events[0], BlockItemEvent::TokenMint(mint) => {
        assert_eq!(mint.token_id, token_id);
        assert_eq!(mint.amount.amount, RawTokenAmount(3000));
        assert_eq!(mint.amount.decimals, 4);
        assert_eq!(mint.target, stub.account_canonical_address(&gov_account));
    });
    assert_matches!(&events[1], BlockItemEvent::TokenTransfer(transfer) => {
        assert_eq!(transfer.token_id, token_id);
        assert_eq!(transfer.amount.amount, RawTokenAmount(1000));
        assert_eq!(transfer.amount.decimals, 4);
        assert_eq!(transfer.from, stub.account_canonical_address(&gov_account));
        assert_eq!(transfer.to, stub.account_canonical_address(&account2));
        assert_eq!(transfer.memo, None);
    });
}

/// Test protocol-level token "pause" operation.
#[test]
fn test_plt_pause() {
    let mut stub = BlockStateStub::new();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 4, None);

    // Add the "pause" operation
    let operations = vec![TokenOperation::Pause(TokenPauseDetails {})];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // Assert that the expected pause event is logged
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenModule(event) => {
        assert_eq!(event.token_id, token_id);
        assert_eq!(event.event_type, TokenModuleEventType::Pause.to_type_discriminator());
        assert_eq!(event.details, vec![].into());
    });

    // Assert paused it set in state
    let token_info = queries::query_token_info(&stub, &token_id).unwrap();
    let token_module_state: TokenModuleState =
        cbor::cbor_decode(&token_info.state.module_state).unwrap();
    assert_eq!(token_module_state.paused, Some(true));

    // Test transfer is now rejected
    let account1 = stub.create_account();
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(10000, 4),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&account1)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(reject_reason) => reject_reason);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(not_permitted) => {
            let reason = not_permitted.reason.unwrap();
            assert!(reason.contains("paused"), "reason: {}", reason);
        }
    );
}

/// Test protocol-level token "unpause" operation.
#[test]
fn test_plt_unpause() {
    let mut stub = BlockStateStub::new();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_, gov_account) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 4, None);

    // Add the "unpause" operation
    let operations = vec![TokenOperation::Unpause(TokenPauseDetails {})];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // Assert that the expected unpause event is logged
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenModule(event) => {
        assert_eq!(event.token_id, token_id);
        assert_eq!(event.event_type, TokenModuleEventType::Unpause.to_type_discriminator());
        assert_eq!(event.details, vec![].into());
    });
}

/// Test protocol-level token transfer that is rejected because token does not exist.
#[test]
fn test_non_existing_token_id() {
    let mut stub = BlockStateStub::new();
    let account1 = stub.create_account();
    let account2 = stub.create_account();

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 4),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&account2)),
        memo: None,
    })];
    let token_id: TokenId = "tokenid1".parse().unwrap();
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = scheduler::execute_transaction(
        account1,
        stub.account_canonical_address(&account1),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(reject_reason) => reject_reason);

    assert_matches!(
        reject_reason,
        TransactionRejectReason::NonExistentTokenId(reject_reason_token_id) => {
            assert_eq!(reject_reason_token_id, token_id);
        }
    );
}

/// Test that energy is charged during execution and the correct amount of used energy returned.
#[test]
fn test_energy_charge() {
    let mut stub = BlockStateStub::new();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    let account2 = stub.create_account();
    stub.increment_account_balance(gov_account, token, RawTokenAmount(5000));

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(3000, 4),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&account2)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    // Assert energy used
    assert_eq!(result.energy_used.energy, 300 + 100);
}

/// Test that energy is charged during execution and the correct amount of used energy returned,
/// also if the transaction is rejected.
#[test]
fn test_energy_charge_at_reject() {
    let mut stub = BlockStateStub::new();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    let account2 = stub.create_account();
    stub.increment_account_balance(gov_account, token, RawTokenAmount(5000));

    // Transfer operation with a larger amount than the token balance of the sender `gov_account`,
    // which will be the cause of the rejection.
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(10000, 4),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&account2)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(
        result.outcome,
        TransactionOutcome::Rejected(TransactionRejectReason::TokenUpdateTransactionFailed(_))
    );

    // Assert energy used
    assert_eq!(result.energy_used.energy, 300 + 100);
}

/// Test that an out of energy reject reason is returned if we run out of energy.
#[test]
fn test_out_of_energy_error() {
    let mut stub = BlockStateStub::new();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    let account2 = stub.create_account();
    stub.increment_account_balance(gov_account, token, RawTokenAmount(5000));

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(3000, 4),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&account2)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        &mut stub,
        Payload::TokenUpdate { payload },
        Energy::from(150), // needs 300 + 100 to succeed
    )
    .expect("transaction internal error");

    // Assert out of energy error
    assert_matches!(
        result.outcome,
        TransactionOutcome::Rejected(TransactionRejectReason::OutOfEnergy)
    );

    // Assert all available energy used
    assert_eq!(result.energy_used.energy, 150);
}
