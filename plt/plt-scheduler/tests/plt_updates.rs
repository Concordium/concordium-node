//! Test of protocol-level token updates. Detailed tests should generally be implemented in
//! the tests of the token module in the `plt-token-module` crate. In the present file,
//! higher level tests are implemented.

use crate::block_state_stub::{BlockStateStub, TokenInitTestParams};
use assert_matches::assert_matches;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, RawCbor, TokenAmount, TokenModuleRejectReasonEnum, TokenOperation,
    TokenOperationsPayload, TokenSupplyUpdateDetails, TokenTransfer,
};
use concordium_base::transactions::Payload;
use plt_scheduler::block_state_interface::BlockStateQuery;
use plt_scheduler::scheduler;
use plt_scheduler::scheduler::TransactionEvent;
use plt_token_module::token_kernel_interface::RawTokenAmount;

mod block_state_stub;
mod utils;

/// Test protocol-level token transfer.
#[test]
fn test_plt_transfer() {
    let mut stub = BlockStateStub::new();
    let (token, _) = stub.init_token(TokenInitTestParams::default(), 4, None);
    let account1 = stub.create_account();
    let account2 = stub.create_account();
    stub.increment_account_balance(account1, token, RawTokenAmount(5000));
    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(5000));

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 4),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&account2)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: stub.token_configuration(&token).token_id,
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let events =
        scheduler::execute_transaction(account1, &mut stub, Payload::TokenUpdate { payload })
            .expect("transaction internal error")
            .expect("transfer");

    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(5000));
    assert_eq!(
        stub.account_token_balance(&account1, &token),
        RawTokenAmount(4000)
    );
    assert_eq!(
        stub.account_token_balance(&account2, &token),
        RawTokenAmount(1000)
    );

    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], TransactionEvent::TokenTransfer(transfer) => {
        assert_eq!(transfer.amount, TokenAmount::from_raw(1000, 4));
        assert_eq!(transfer.from, stub.account_canonical_address(&account1));
        assert_eq!(transfer.to, stub.account_canonical_address(&account2));
        assert_eq!(transfer.memo, None);
    });
}

/// Test protocol-level token transfer that is rejected.
#[test]
fn test_plt_transfer_reject() {
    let mut stub = BlockStateStub::new();
    let (token, _) = stub.init_token(TokenInitTestParams::default(), 4, None);
    let account1 = stub.create_account();
    let account2 = stub.create_account();
    stub.increment_account_balance(account1, token, RawTokenAmount(5000));
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
        scheduler::execute_transaction(account1, &mut stub, Payload::TokenUpdate { payload })
            .expect("transaction internal error")
            .expect_err("transfer reject");

    assert_eq!(stub.token_circulating_supply(&token), RawTokenAmount(5000));
    assert_eq!(
        stub.account_token_balance(&account1, &token),
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
    let (token, gov_account) = stub.init_token(TokenInitTestParams::default(), 4, None);

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
    let (token, gov_account) = stub.init_token(
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
    let (token, gov_account) = stub.init_token(
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
    let (token, gov_account) = stub.init_token(
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
