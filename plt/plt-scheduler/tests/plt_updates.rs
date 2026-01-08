//! Test of protocol-level token updates. Detailed tests should generally be implemented in
//! the tests of the token module in the `plt-token-module` crate. In the present file,
//! higher level tests are implemented.

use crate::block_state_stub::{BlockStateStub, TokenInitTestParams};
use assert_matches::assert_matches;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, CborMemo, RawCbor, TokenAmount, TokenModuleRejectReasonEnum, TokenOperation,
    TokenOperationsPayload, TokenSupplyUpdateDetails, TokenTransfer,
};
use concordium_base::transactions::{Memo, Payload};
use plt_scheduler::block_state_interface::BlockStateQuery;
use plt_scheduler::events::TransactionEvent;
use plt_scheduler::scheduler;
use plt_token_module::token_kernel_interface::RawTokenAmount;

mod block_state_stub;
mod utils;

/// Test protocol-level token transfer. First transfer from governance account. And then perform
/// a second transfer from the destination of the first transfer.
/// the
#[test]
fn test_plt_transfer() {
    let mut stub = BlockStateStub::new();
    let (token, gov_account) = stub.create_and_init_token(TokenInitTestParams::default(), 4, None);
    let account2 = stub.create_account();
    let account3 = stub.create_account();
    stub.increment_account_balance(gov_account, token, RawTokenAmount(5000));
    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(5000));

    // Transfer from governance account to account2

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(3000, 4),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&account2)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: stub.token_configuration(&token).token_id,
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let events =
        scheduler::execute_transaction(gov_account, &mut stub, Payload::TokenUpdate { payload })
            .expect("transaction internal error")
            .expect("transfer");

    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(5000));
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(2000)
    );
    assert_eq!(
        stub.account_token_balance(&account2, &token),
        RawTokenAmount(3000)
    );

    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], TransactionEvent::TokenTransfer(transfer) => {
        assert_eq!(transfer.amount, TokenAmount::from_raw(3000, 4));
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
        token_id: stub.token_configuration(&token).token_id,
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let events =
        scheduler::execute_transaction(account2, &mut stub, Payload::TokenUpdate { payload })
            .expect("transaction internal error")
            .expect("transfer");

    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(5000));
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(2000)
    );
    assert_eq!(
        stub.account_token_balance(&account2, &token),
        RawTokenAmount(2000)
    );
    assert_eq!(
        stub.account_token_balance(&account3, &token),
        RawTokenAmount(1000)
    );

    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], TransactionEvent::TokenTransfer(transfer) => {
        assert_eq!(transfer.amount, TokenAmount::from_raw(1000, 4));
        assert_eq!(transfer.from, stub.account_canonical_address(&account2));
        assert_eq!(transfer.to, stub.account_canonical_address(&account3));
        assert_eq!(transfer.memo, Some(memo));
    });
}

/// Test protocol-level token transfer that is rejected.
#[test]
fn test_plt_transfer_reject() {
    let mut stub = BlockStateStub::new();
    let (token, gov_account) = stub.create_and_init_token(TokenInitTestParams::default(), 4, None);
    let account2 = stub.create_account();
    stub.increment_account_balance(gov_account, token, RawTokenAmount(5000));
    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(5000));

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(10000, 4),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&account2)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: stub.token_configuration(&token).token_id,
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let reject_reason =
        scheduler::execute_transaction(gov_account, &mut stub, Payload::TokenUpdate { payload })
            .expect("transaction internal error")
            .expect_err("transfer reject");

    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(5000));
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(5000)
    );
    assert_eq!(
        stub.account_token_balance(&account2, &token),
        RawTokenAmount(0)
    );

    let reject_reason = utils::assert_token_module_reject_reason(reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReasonEnum::TokenBalanceInsufficient(_)
    );
}

/// Test protocol-level token mint.
#[test]
#[ignore = "enable as part of https://linear.app/concordium/issue/PSR-29/implement-mint-and-burn"]
fn test_plt_mint() {
    let mut stub = BlockStateStub::new();
    let (token, gov_account) = stub.create_and_init_token(TokenInitTestParams::default(), 4, None);

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: stub.token_configuration(&token).token_id,
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let events =
        scheduler::execute_transaction(gov_account, &mut stub, Payload::TokenUpdate { payload })
            .expect("transaction internal error")
            .expect("mint");

    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(1000));
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(1000)
    );

    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], TransactionEvent::TokenMint(mint) => {
        assert_eq!(mint.amount, TokenAmount::from_raw(1000, 4));
        assert_eq!(mint.target, stub.account_canonical_address(&gov_account));
    });
}

/// Test protocol-level token mint that is rejected.
#[test]
#[ignore = "enable as part of https://linear.app/concordium/issue/PSR-29/implement-mint-and-burn"]
fn test_plt_mint_reject() {
    let mut stub = BlockStateStub::new();
    let (token, gov_account) = stub.create_and_init_token(
        TokenInitTestParams::default(),
        4,
        Some(RawTokenAmount(5000)),
    );

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(u64::MAX, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: stub.token_configuration(&token).token_id,
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let reject_reason =
        scheduler::execute_transaction(gov_account, &mut stub, Payload::TokenUpdate { payload })
            .expect("transaction internal error")
            .expect_err("mint reject");

    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(5000));
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(5000)
    );

    let reject_reason = utils::assert_token_module_reject_reason(reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReasonEnum::MintWouldOverflow(_)
    );
}

/// Test protocol-level token burn.
#[test]
#[ignore = "enable as part of https://linear.app/concordium/issue/PSR-29/implement-mint-and-burn"]
fn test_plt_burn() {
    let mut stub = BlockStateStub::new();
    let (token, gov_account) = stub.create_and_init_token(
        TokenInitTestParams::default(),
        4,
        Some(RawTokenAmount(5000)),
    );

    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: stub.token_configuration(&token).token_id,
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let events =
        scheduler::execute_transaction(gov_account, &mut stub, Payload::TokenUpdate { payload })
            .expect("transaction internal error")
            .expect("burn");

    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(4000));
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(4000)
    );

    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], TransactionEvent::TokenBurn(burn) => {
        assert_eq!(burn.amount, TokenAmount::from_raw(1000, 4));
        assert_eq!(burn.target, stub.account_canonical_address(&gov_account));
    });
}

/// Test protocol-level token burn rejection.
#[test]
#[ignore = "enable as part of https://linear.app/concordium/issue/PSR-29/implement-mint-and-burn"]
fn test_plt_burn_reject() {
    let mut stub = BlockStateStub::new();
    let (token, gov_account) = stub.create_and_init_token(
        TokenInitTestParams::default(),
        4,
        Some(RawTokenAmount(5000)),
    );

    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(10000, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: stub.token_configuration(&token).token_id,
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let reject_reason =
        scheduler::execute_transaction(gov_account, &mut stub, Payload::TokenUpdate { payload })
            .expect("transaction internal error")
            .expect_err("burn reject");

    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(5000));
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(5000)
    );

    let reject_reason = utils::assert_token_module_reject_reason(reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReasonEnum::TokenBalanceInsufficient(_)
    );
}

/// Test multiple protocol-level token update operations in one transaction.
#[test]
#[ignore = "enable as part of https://linear.app/concordium/issue/PSR-29/implement-mint-and-burn"]
fn test_plt_multiple_operations() {
    let mut stub = BlockStateStub::new();
    let (token, gov_account) = stub.create_and_init_token(TokenInitTestParams::default(), 4, None);
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
        token_id: stub.token_configuration(&token).token_id,
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let events =
        scheduler::execute_transaction(gov_account, &mut stub, Payload::TokenUpdate { payload })
            .expect("transaction internal error")
            .expect("mint");

    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(3000));
    assert_eq!(
        stub.account_token_balance(&gov_account, &token),
        RawTokenAmount(2000)
    );
    assert_eq!(
        stub.account_token_balance(&account2, &token),
        RawTokenAmount(1000)
    );

    assert_eq!(events.len(), 2);
    assert_matches!(&events[0], TransactionEvent::TokenMint(mint) => {
        assert_eq!(mint.amount, TokenAmount::from_raw(1000, 4));
        assert_eq!(mint.target, stub.account_canonical_address(&gov_account));
    });
    assert_matches!(&events[1], TransactionEvent::TokenTransfer(transfer) => {
        assert_eq!(transfer.amount, TokenAmount::from_raw(1000, 4));
        assert_eq!(transfer.from, stub.account_canonical_address(&gov_account));
        assert_eq!(transfer.to, stub.account_canonical_address(&account2));
        assert_eq!(transfer.memo, None);
    });
}
