//! Test of protocol-level token updates. Detailed, functionally complete tests should generally be implemented in
//! the tests of the token module in the `plt-token-module` crate. In the present file,
//! higher level tests are implemented, and they may not in themselves be functionally complete.

use crate::utils::TokenInitTestParams;
use crate::utils::entity_traits::scheduler::SchedulerOperations;
use assert_matches::assert_matches;
use concordium_base::base::Energy;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, CborMemo, OperationNotPermittedRejectReason, RawCbor, TokenAmount, TokenId,
    TokenListUpdateDetails, TokenListUpdateEventDetails, TokenModuleEventType,
    TokenModuleRejectReason, TokenModuleState, TokenOperation, TokenOperationsPayload,
    TokenPauseDetails, TokenPauseEventDetails, TokenSupplyUpdateDetails, TokenTransfer,
    UnsupportedOperationRejectReason,
};
use concordium_base::transactions::{Memo, Payload};
use plt_block_state::entity::entity_test_stub;
use plt_scheduler_types::types::events::BlockItemEvent;
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;
use plt_scheduler_types::types::tokens::{RawTokenAmount, TokenHolder};

use crate::utils::BlockStateLatest;

mod utils;

/// Test protocol-level token transfer. First transfer from governance account. And then perform
/// a second transfer from the destination of the first transfer.
#[test]
fn test_plt_transfer() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(),
        4,
        Some(RawTokenAmount(5000)),
    );
    let account2 = context.external.create_account();
    let account3 = context.external.create_account();

    // Transfer from governance account to account2

    let account2_addr = context
        .external
        .account_canonical_address(account2.account_index());
    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(3000, 4),
        recipient: CborHolderAccount::from(account2_addr),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    let token = block_state
        .token_by_id(&context, &token_id)
        .unwrap()
        .unwrap();

    // Assert circulating supply unchanged
    assert_eq!(
        token.token_base.token_circulating_supply(),
        RawTokenAmount(5000)
    );

    // Assert balance of sender and receiver
    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(2000)
    );
    assert_eq!(
        account2.account_token_balance(&context, token_index),
        RawTokenAmount(3000)
    );

    // Assert transfer event
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenTransfer(transfer) => {
        assert_eq!(transfer.token_id, token_id);
        assert_eq!(transfer.amount.amount, RawTokenAmount(3000));
        assert_eq!(transfer.amount.decimals, 4);
        assert_eq!(transfer.from, TokenHolder::Account(gov_addr));
        assert_eq!(transfer.to, TokenHolder::Account(account2_addr));
        assert_eq!(transfer.memo, None);
    });

    // Transfer from account2 to account3 with memo

    let account3_addr = context
        .external
        .account_canonical_address(account3.account_index());
    let memo = Memo::try_from(cbor::cbor_encode("testvalue")).unwrap();
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 4),
        recipient: CborHolderAccount::from(account3_addr),
        memo: Some(CborMemo::Cbor(memo.clone())),
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            account2.account_index(),
            account2_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    let token = block_state
        .token_by_id(&context, &token_id)
        .unwrap()
        .unwrap();

    // Assert circulating supply unchanged
    assert_eq!(
        token.token_base.token_circulating_supply(),
        RawTokenAmount(5000)
    );

    // Assert balance of sender and receiver
    assert_eq!(
        account2.account_token_balance(&context, token_index),
        RawTokenAmount(2000)
    );
    assert_eq!(
        account3.account_token_balance(&context, token_index),
        RawTokenAmount(1000)
    );

    // Assert transfer event
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenTransfer(transfer) => {
        assert_eq!(transfer.token_id, token_id);
        assert_eq!(transfer.amount.amount, RawTokenAmount(1000));
        assert_eq!(transfer.amount.decimals, 4);
        assert_eq!(transfer.from, TokenHolder::Account(account2_addr));
        assert_eq!(transfer.to, TokenHolder::Account(account3_addr));
        assert_eq!(transfer.memo, Some(memo));
    });
}

/// Test protocol-level token transfer using address aliases for sender and receiver.
#[test]
fn test_plt_transfer_using_aliases() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(),
        4,
        Some(RawTokenAmount(5000)),
    );
    let account2 = context.external.create_account();

    let gov_account_address_alias = context
        .external
        .account_canonical_address(gov_account.account_index())
        .get_alias(5)
        .unwrap();
    let account2_alias_address = context
        .external
        .account_canonical_address(account2.account_index())
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

    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_account_address_alias,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // Assert balance of sender and receiver
    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(2000)
    );
    assert_eq!(
        account2.account_token_balance(&context, token_index),
        RawTokenAmount(3000)
    );

    // Assert transfer event
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenTransfer(transfer) => {
        assert_eq!(transfer.token_id, token_id);
        assert_eq!(transfer.amount.amount, RawTokenAmount(3000));
        assert_eq!(transfer.amount.decimals, 4);
        assert_eq!(transfer.from, TokenHolder::Account(gov_account_address_alias));
        assert_eq!(transfer.to, TokenHolder::Account(account2_alias_address));
        assert_eq!(transfer.memo, None);
    });
}

/// Test protocol-level token transfer that is rejected.
#[test]
fn test_plt_transfer_reject() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(),
        4,
        Some(RawTokenAmount(5000)),
    );
    let account2 = context.external.create_account();

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let account2_addr = context
        .external
        .account_canonical_address(account2.account_index());
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(10000, 4),
        recipient: CborHolderAccount::from(account2_addr),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(reject_reason) => reject_reason);

    let token = block_state
        .token_by_id(&context, &token_id)
        .unwrap()
        .unwrap();

    // Assert circulating supply and account balances unchanged
    assert_eq!(
        token.token_base.token_circulating_supply(),
        RawTokenAmount(5000)
    );
    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(5000)
    );
    assert_eq!(
        account2.account_token_balance(&context, token_index),
        RawTokenAmount(0)
    );

    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::TokenBalanceInsufficient(_)
    );
}

/// Test
/// * transfer without sender being in allow list (rejected)
/// * add sender to allow list
/// * transfer (successful)
#[test]
fn test_plt_transfer_allow_list_flow() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        4,
        Some(RawTokenAmount(5000)),
    );
    let receiver = context.external.create_account();

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let receiver_addr = context
        .external
        .account_canonical_address(receiver.account_index());

    // Add only the sender to the allow list.
    let operations = vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(gov_addr),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
            1.into(),
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
        assert_eq!(details.target, CborHolderAccount::from(gov_addr));
    });

    // Transfer fails because the receiver is not allow-listed yet.
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 4),
        recipient: CborHolderAccount::from(receiver_addr),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(reject_reason) => reject_reason);

    let token = block_state
        .token_by_id(&context, &token_id)
        .unwrap()
        .unwrap();

    assert_eq!(
        token.token_base.token_circulating_supply(),
        RawTokenAmount(5000)
    );
    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(5000)
    );
    assert_eq!(
        receiver.account_token_balance(&context, token_index),
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
            assert_eq!(address, CborHolderAccount::from(receiver_addr));
            assert_eq!(reason, "recipient not in allow list");
        }
    );

    // Add the receiver to the allow list.
    let operations = vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(receiver_addr),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
            1.into(),
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
        assert_eq!(details.target, CborHolderAccount::from(receiver_addr));
    });

    // Transfer succeeds once both accounts are allow-listed.
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 4),
        recipient: CborHolderAccount::from(receiver_addr),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    let token = block_state
        .token_by_id(&context, &token_id)
        .unwrap()
        .unwrap();

    assert_eq!(
        token.token_base.token_circulating_supply(),
        RawTokenAmount(5000)
    );
    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(4000)
    );
    assert_eq!(
        receiver.account_token_balance(&context, token_index),
        RawTokenAmount(1000)
    );

    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenTransfer(transfer) => {
        assert_eq!(transfer.token_id, token_id);
        assert_eq!(transfer.amount.amount, RawTokenAmount(1000));
        assert_eq!(transfer.amount.decimals, 4);
        assert_eq!(transfer.from, TokenHolder::Account(gov_addr));
        assert_eq!(transfer.to, TokenHolder::Account(receiver_addr));
        assert_eq!(transfer.memo, None);
    });
}

/// Test add account to allow list for a token where allow lists are not enabled.
#[test]
fn test_plt_allow_list_disabled() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(),
        4,
        None,
    );

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let operations = vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(gov_addr),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");

    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(reject_reason) => reject_reason);
    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::UnsupportedOperation(UnsupportedOperationRejectReason {
            operation_type,
            reason: Some(reason),
            ..
        }) => {
            assert_eq!(operation_type, "addAllowList");
            assert_eq!(reason, "feature not enabled");
        }
    );
}

/// Test protocol-level token mint.
#[test]
fn test_plt_mint() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    let token = block_state
        .token_by_id(&context, &token_id)
        .unwrap()
        .unwrap();

    // Assert circulating supply increased
    assert_eq!(
        token.token_base.token_circulating_supply(),
        RawTokenAmount(1000)
    );

    // Assert account balance increased
    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(1000)
    );

    // Assert mint event
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenMint(mint) => {
        assert_eq!(mint.token_id, token_id);
        assert_eq!(mint.amount.amount, RawTokenAmount(1000));
        assert_eq!(mint.amount.decimals, 4);
        assert_eq!(mint.target, TokenHolder::Account(gov_addr));
    });
}

/// Test protocol-level token mint using account address alias.
#[test]
fn test_plt_mint_using_alias() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );

    let gov_account_address_alias = context
        .external
        .account_canonical_address(gov_account.account_index())
        .get_alias(5)
        .unwrap();

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_account_address_alias,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    let token = block_state
        .token_by_id(&context, &token_id)
        .unwrap()
        .unwrap();

    // Assert circulating supply increased
    assert_eq!(
        token.token_base.token_circulating_supply(),
        RawTokenAmount(1000)
    );

    // Assert account balance increased
    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(1000)
    );

    // Assert mint event
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenMint(mint) => {
        assert_eq!(mint.token_id, token_id);
        assert_eq!(mint.amount.amount, RawTokenAmount(1000));
        assert_eq!(mint.amount.decimals, 4);
        assert_eq!(mint.target, TokenHolder::Account(gov_account_address_alias));
    });
}

/// Test protocol-level token mint that is rejected.
#[test]
fn test_plt_mint_reject() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        Some(RawTokenAmount(5000)),
    );

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(u64::MAX, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(reject_reason) => reject_reason);

    let token = block_state
        .token_by_id(&context, &token_id)
        .unwrap()
        .unwrap();

    // Assert circulating supply and account balance unchanged
    assert_eq!(
        token.token_base.token_circulating_supply(),
        RawTokenAmount(5000)
    );
    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(5000)
    );

    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(reject_reason, TokenModuleRejectReason::MintWouldOverflow(_));
}

/// Test protocol-level token mint from unauthorized sender.
#[test]
fn test_plt_mint_unauthorized() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    let non_governance_account = context.external.create_account();

    let non_gov_addr = context
        .external
        .account_canonical_address(non_governance_account.account_index());
    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            non_governance_account.account_index(),
            non_gov_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(reject_reason) => reject_reason);

    let token = block_state
        .token_by_id(&context, &token_id)
        .unwrap()
        .unwrap();

    // Assert circulating supply and account balance unchanged
    assert_eq!(token.token_base.token_circulating_supply(), RawTokenAmount(0));
    assert_eq!(
        non_governance_account.account_token_balance(&context, token_index),
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
                Some(CborHolderAccount::from(non_gov_addr))
            );
            assert_eq!(reason.as_deref(), Some("sender is not authorized to perform the operation for this token"));
        }
    );
}

/// Test protocol-level token burn.
#[test]
fn test_plt_burn() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().burnable(),
        4,
        Some(RawTokenAmount(5000)),
    );

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    let token = block_state
        .token_by_id(&context, &token_id)
        .unwrap()
        .unwrap();

    // Assert circulating supply decreased
    assert_eq!(
        token.token_base.token_circulating_supply(),
        RawTokenAmount(4000)
    );

    // Assert account balance decreased
    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(4000)
    );

    // Assert burn event
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenBurn(burn) => {
        assert_eq!(burn.token_id, token_id);
        assert_eq!(burn.amount.amount, RawTokenAmount(1000));
        assert_eq!(burn.amount.decimals, 4);
        assert_eq!(burn.target, TokenHolder::Account(gov_addr));
    });
}

/// Test protocol-level token burn using address alias for governance account
#[test]
fn test_plt_burn_using_alias() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().burnable(),
        4,
        Some(RawTokenAmount(5000)),
    );

    let gov_account_address_alias = context
        .external
        .account_canonical_address(gov_account.account_index())
        .get_alias(5)
        .unwrap();

    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_account_address_alias,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    let token = block_state
        .token_by_id(&context, &token_id)
        .unwrap()
        .unwrap();

    // Assert circulating supply decreased
    assert_eq!(
        token.token_base.token_circulating_supply(),
        RawTokenAmount(4000)
    );

    // Assert account balance decreased
    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(4000)
    );

    // Assert burn event
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenBurn(burn) => {
        assert_eq!(burn.token_id, token_id);
        assert_eq!(burn.amount.amount, RawTokenAmount(1000));
        assert_eq!(burn.amount.decimals, 4);
        assert_eq!(burn.target, TokenHolder::Account(gov_account_address_alias));
    });
}

/// Test protocol-level token burn rejection.
#[test]
fn test_plt_burn_reject() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().burnable(),
        4,
        Some(RawTokenAmount(5000)),
    );

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let operations = vec![TokenOperation::Burn(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(10000, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(reject_reason) => reject_reason);

    let token = block_state
        .token_by_id(&context, &token_id)
        .unwrap()
        .unwrap();

    // Assert circulating supply and account balance unchanged
    assert_eq!(
        token.token_base.token_circulating_supply(),
        RawTokenAmount(5000)
    );
    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
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
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, token_index) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    let account2 = context.external.create_account();

    // Compose two operations: Mint and then transfer

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let account2_addr = context
        .external
        .account_canonical_address(account2.account_index());
    let operations = vec![
        TokenOperation::Mint(TokenSupplyUpdateDetails {
            amount: TokenAmount::from_raw(3000, 4),
        }),
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(1000, 4),
            recipient: CborHolderAccount::from(account2_addr),
            memo: None,
        }),
    ];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    let token = block_state
        .token_by_id(&context, &token_id)
        .unwrap()
        .unwrap();

    // Assert circulating supply and account balances
    assert_eq!(
        token.token_base.token_circulating_supply(),
        RawTokenAmount(3000)
    );
    assert_eq!(
        gov_account.account_token_balance(&context, token_index),
        RawTokenAmount(2000)
    );
    assert_eq!(
        account2.account_token_balance(&context, token_index),
        RawTokenAmount(1000)
    );

    // Assert two events in right order
    assert_eq!(events.len(), 2);
    assert_matches!(&events[0], BlockItemEvent::TokenMint(mint) => {
        assert_eq!(mint.token_id, token_id);
        assert_eq!(mint.amount.amount, RawTokenAmount(3000));
        assert_eq!(mint.amount.decimals, 4);
        assert_eq!(mint.target, TokenHolder::Account(gov_addr));
    });
    assert_matches!(&events[1], BlockItemEvent::TokenTransfer(transfer) => {
        assert_eq!(transfer.token_id, token_id);
        assert_eq!(transfer.amount.amount, RawTokenAmount(1000));
        assert_eq!(transfer.amount.decimals, 4);
        assert_eq!(transfer.from, TokenHolder::Account(gov_addr));
        assert_eq!(transfer.to, TokenHolder::Account(account2_addr));
        assert_eq!(transfer.memo, None);
    });
}

/// Test protocol-level token "pause" operation.
#[test]
fn test_plt_pause() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(),
        4,
        None,
    );

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    // Add the "pause" operation
    let operations = vec![TokenOperation::Pause(TokenPauseDetails {})];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
            1.into(),
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
        let _details: TokenPauseEventDetails = cbor::cbor_decode(&event.details).unwrap();
    });

    // Assert paused is set in state
    let token_info = block_state.query_token_info(&context, &token_id).unwrap();
    let token_module_state: TokenModuleState =
        cbor::cbor_decode(&token_info.state.module_state).unwrap();
    assert_eq!(token_module_state.paused, Some(true));

    // Test transfer is now rejected
    let account1 = context.external.create_account();
    let account1_addr = context
        .external
        .account_canonical_address(account1.account_index());
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(10000, 4),
        recipient: CborHolderAccount::from(account1_addr),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
            1.into(),
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
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(),
        4,
        None,
    );

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    // Add the "unpause" operation
    let operations = vec![TokenOperation::Unpause(TokenPauseDetails {})];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
            1.into(),
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
        let _details: TokenPauseEventDetails = cbor::cbor_decode(&event.details).unwrap();
    });
}

/// Test protocol-level token transfer that is rejected because token does not exist.
#[test]
fn test_non_existing_token_id() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let account1 = context.external.create_account();
    let account2 = context.external.create_account();

    let account1_addr = context
        .external
        .account_canonical_address(account1.account_index());
    let account2_addr = context
        .external
        .account_canonical_address(account2.account_index());
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 4),
        recipient: CborHolderAccount::from(account2_addr),
        memo: None,
    })];
    let token_id: TokenId = "tokenid1".parse().unwrap();
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            account1.account_index(),
            account1_addr,
            1.into(),
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
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    let account2 = context.external.create_account();
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        gov_account.account_index(),
        &token_id,
        RawTokenAmount(5000),
    );

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let account2_addr = context
        .external
        .account_canonical_address(account2.account_index());
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(3000, 4),
        recipient: CborHolderAccount::from(account2_addr),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
            1.into(),
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
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    let account2 = context.external.create_account();
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        gov_account.account_index(),
        &token_id,
        RawTokenAmount(5000),
    );

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let account2_addr = context
        .external
        .account_canonical_address(account2.account_index());
    // Transfer operation with a larger amount than the token balance of the sender `gov_account`,
    // which will be the cause of the rejection.
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(10000, 4),
        recipient: CborHolderAccount::from(account2_addr),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
            1.into(),
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
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    let account2 = context.external.create_account();
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        gov_account.account_index(),
        &token_id,
        RawTokenAmount(5000),
    );

    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let account2_addr = context
        .external
        .account_canonical_address(account2.account_index());
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(3000, 4),
        recipient: CborHolderAccount::from(account2_addr),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            gov_account.account_index(),
            gov_addr,
            1.into(),
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
