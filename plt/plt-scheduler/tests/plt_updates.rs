//! Test of protocol-level token updates. Detailed, functionally complete tests should generally be implemented in
//! the tests of the token module in the `plt-token-module` crate. In the present file,
//! higher level tests are implemented, and they may not in themselves be functionally complete.

use crate::block_state_external_stubbed::TokenInitTestParams;
use assert_matches::assert_matches;
use block_state_external_stubbed::{BlobStoreLoadStub, BlockStateTestImpl, ExternalBlockStateStub};
use concordium_base::base::Energy;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, CborMemo, OperationNotPermittedRejectReason, RawCbor, TokenAmount, TokenId,
    TokenListUpdateDetails, TokenListUpdateEventDetails, TokenModuleEventType,
    TokenModuleRejectReason, TokenModuleState, TokenOperation, TokenOperationsPayload,
    TokenPauseDetails, TokenSupplyUpdateDetails, TokenTransfer, UnsupportedOperationRejectReason,
};
use concordium_base::transactions::{Memo, Payload};
use plt_block_state::block_state::p10;
use plt_scheduler_interface::transaction_execution_interface::TransactionExecution;
use plt_scheduler_types::types::events::BlockItemEvent;
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;
use plt_scheduler_types::types::tokens::{RawTokenAmount, TokenHolder};

mod block_state_external_stubbed;
mod utils;

/// Test protocol-level token transfer. First transfer from governance account. And then perform
/// a second transfer from the destination of the first transfer.
#[test]
fn test_plt_transfer() {
    test_plt_transfer_worker::<p10::PltBlockStateP10>();
}

fn test_plt_transfer_worker<BlockStateVersion>()
where
    BlockStateVersion: BlockStateTestImpl,
{
    let mut block_state = BlockStateVersion::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = block_state.create_and_init_token(
        &mut external,
        token_id.clone(),
        TokenInitTestParams::default(),
        4,
        Some(RawTokenAmount(5000)),
    );
    let account2 = external.create_account();
    let account3 = external.create_account();

    // Transfer from governance account to account2

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(3000, 4),
        recipient: CborHolderAccount::from(external.account_canonical_address(&account2)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        external.account_canonical_address(&gov_account),
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // Assert circulating supply unchanged
    let token_info = block_state.query_token_info(&external, &token_id).unwrap();
    assert_eq!(token_info.state.total_supply.amount, RawTokenAmount(5000));

    // Assert balance of sender and receiver
    assert_eq!(
        block_state
            .token_account_state(&external, &token_id, gov_account)
            .unwrap()
            .balance
            .amount,
        RawTokenAmount(2000)
    );
    assert_eq!(
        block_state
            .token_account_state(&external, &token_id, account2)
            .unwrap()
            .balance
            .amount,
        RawTokenAmount(3000)
    );

    // Assert transfer event
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenTransfer(transfer) => {
        assert_eq!(transfer.token_id, token_id);
        assert_eq!(transfer.amount.amount, RawTokenAmount(3000));
        assert_eq!(transfer.amount.decimals, 4);
        assert_eq!(transfer.from, TokenHolder::Account(external.account_canonical_address(&gov_account)));
        assert_eq!(transfer.to, TokenHolder::Account(external.account_canonical_address(&account2)));
        assert_eq!(transfer.memo, None);
    });

    // Transfer from account2 to account3 with memo

    let memo = Memo::try_from(cbor::cbor_encode("testvalue")).unwrap();
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 4),
        recipient: CborHolderAccount::from(external.account_canonical_address(&account3)),
        memo: Some(CborMemo::Cbor(memo.clone())),
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        account2,
        external.account_canonical_address(&account2),
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // Assert circulating supply unchanged
    let token_info = block_state.query_token_info(&external, &token_id).unwrap();
    assert_eq!(token_info.state.total_supply.amount, RawTokenAmount(5000));

    // Assert balance of sender and receiver
    assert_eq!(
        block_state
            .token_account_state(&external, &token_id, account2)
            .unwrap()
            .balance
            .amount,
        RawTokenAmount(2000)
    );
    assert_eq!(
        block_state
            .token_account_state(&external, &token_id, account3)
            .unwrap()
            .balance
            .amount,
        RawTokenAmount(1000)
    );

    // Assert transfer event
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenTransfer(transfer) => {
        assert_eq!(transfer.token_id, token_id);
        assert_eq!(transfer.amount.amount, RawTokenAmount(1000));
        assert_eq!(transfer.amount.decimals, 4);
        assert_eq!(transfer.from, TokenHolder::Account(external.account_canonical_address(&account2)));
        assert_eq!(transfer.to, TokenHolder::Account(external.account_canonical_address(&account3)));
        assert_eq!(transfer.memo, Some(memo));
    });
}

/// Test protocol-level token transfer using address aliases for sender and receiver.
#[test]
fn test_plt_transfer_using_aliases() {
    test_plt_transfer_using_aliases_worker::<p10::PltBlockStateP10>();
}

fn test_plt_transfer_using_aliases_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = block_state.create_and_init_token(
        &mut external,
        token_id.clone(),
        TokenInitTestParams::default(),
        4,
        Some(RawTokenAmount(5000)),
    );
    let account2 = external.create_account();

    let gov_account_address_alias = external
        .account_canonical_address(&gov_account)
        .get_alias(5)
        .unwrap();
    let account2_alias_address = external
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

    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        gov_account_address_alias,
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // Assert balance of sender and receiver
    assert_eq!(
        block_state
            .token_account_state(&external, &token_id, gov_account)
            .unwrap()
            .balance
            .amount,
        RawTokenAmount(2000)
    );
    assert_eq!(
        block_state
            .token_account_state(&external, &token_id, account2)
            .unwrap()
            .balance
            .amount,
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
    test_plt_transfer_reject_worker::<p10::PltBlockStateP10>();
}

fn test_plt_transfer_reject_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = block_state.create_and_init_token(
        &mut external,
        token_id.clone(),
        TokenInitTestParams::default(),
        4,
        Some(RawTokenAmount(5000)),
    );
    let account2 = external.create_account();

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(10000, 4),
        recipient: CborHolderAccount::from(external.account_canonical_address(&account2)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        external.account_canonical_address(&gov_account),
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(reject_reason) => reject_reason);

    // Assert circulating supply and account balances unchanged
    let token_info = block_state.query_token_info(&external, &token_id).unwrap();
    assert_eq!(token_info.state.total_supply.amount, RawTokenAmount(5000));
    assert_eq!(
        block_state
            .token_account_state(&external, &token_id, gov_account)
            .unwrap()
            .balance
            .amount,
        RawTokenAmount(5000)
    );
    assert_eq!(
        block_state.token_account_state(&external, &token_id, account2),
        None
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
    test_plt_transfer_allow_list_flow_worker::<p10::PltBlockStateP10>();
}

fn test_plt_transfer_allow_list_flow_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = block_state.create_and_init_token(
        &mut external,
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        4,
        Some(RawTokenAmount(5000)),
    );
    let receiver = external.create_account();

    // Add only the sender to the allow list.
    let operations = vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(external.account_canonical_address(&gov_account)),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        external.account_canonical_address(&gov_account),
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenModule(event) => {
        assert_eq!(event.token_id, token_id);
        assert_eq!(event.event_type, TokenModuleEventType::AddAllowList.to_type_discriminator());
        let details: TokenListUpdateEventDetails = cbor::cbor_decode(&event.details).unwrap();
        assert_eq!(details.target, CborHolderAccount::from(external.account_canonical_address(&gov_account)));
    });

    // Transfer fails because the receiver is not allow-listed yet.
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 4),
        recipient: CborHolderAccount::from(external.account_canonical_address(&receiver)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        external.account_canonical_address(&gov_account),
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(reject_reason) => reject_reason);

    let token_info = block_state.query_token_info(&external, &token_id).unwrap();
    assert_eq!(token_info.state.total_supply.amount, RawTokenAmount(5000));
    assert_eq!(
        block_state
            .token_account_state(&external, &token_id, gov_account)
            .unwrap()
            .balance
            .amount,
        RawTokenAmount(5000)
    );
    assert_eq!(
        block_state.token_account_state(&external, &token_id, receiver),
        None
    );

    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(
        reject_reason,
        TokenModuleRejectReason::OperationNotPermitted(OperationNotPermittedRejectReason {
            address: Some(address),
            reason: Some(reason),
            ..
        }) => {
            assert_eq!(address, CborHolderAccount::from(external.account_canonical_address(&receiver)));
            assert_eq!(reason, "recipient not in allow list");
        }
    );

    // Add the receiver to the allow list.
    let operations = vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(external.account_canonical_address(&receiver)),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        external.account_canonical_address(&gov_account),
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenModule(event) => {
        assert_eq!(event.token_id, token_id);
        assert_eq!(event.event_type, TokenModuleEventType::AddAllowList.to_type_discriminator());
        let details: TokenListUpdateEventDetails = cbor::cbor_decode(&event.details).unwrap();
        assert_eq!(details.target, CborHolderAccount::from(external.account_canonical_address(&receiver)));
    });

    // Transfer succeeds once both accounts are allow-listed.
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 4),
        recipient: CborHolderAccount::from(external.account_canonical_address(&receiver)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        external.account_canonical_address(&gov_account),
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    let token_info = block_state.query_token_info(&external, &token_id).unwrap();
    assert_eq!(token_info.state.total_supply.amount, RawTokenAmount(5000));

    assert_eq!(
        block_state
            .token_account_state(&external, &token_id, gov_account)
            .unwrap()
            .balance
            .amount,
        RawTokenAmount(4000)
    );
    assert_eq!(
        block_state
            .token_account_state(&external, &token_id, receiver)
            .unwrap()
            .balance
            .amount,
        RawTokenAmount(1000)
    );

    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenTransfer(transfer) => {
        assert_eq!(transfer.token_id, token_id);
        assert_eq!(transfer.amount.amount, RawTokenAmount(1000));
        assert_eq!(transfer.amount.decimals, 4);
        assert_eq!(transfer.from, TokenHolder::Account(external.account_canonical_address(&gov_account)));
        assert_eq!(transfer.to, TokenHolder::Account(external.account_canonical_address(&receiver)));
        assert_eq!(transfer.memo, None);
    });
}

/// Test add account to allow list for a token where allow lists are not enabled.
#[test]
fn test_plt_allow_list_disabled() {
    test_plt_allow_list_disabled_worker::<p10::PltBlockStateP10>();
}
fn test_plt_allow_list_disabled_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = block_state.create_and_init_token(
        &mut external,
        token_id.clone(),
        TokenInitTestParams::default(),
        4,
        None,
    );

    let operations = vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(external.account_canonical_address(&gov_account)),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        external.account_canonical_address(&gov_account),
    );

    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
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
    test_plt_mint_worker::<p10::PltBlockStateP10>();
}

fn test_plt_mint_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = block_state.create_and_init_token(
        &mut external,
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

    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        external.account_canonical_address(&gov_account),
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // Assert circulating supply increased
    let token_info = block_state.query_token_info(&external, &token_id).unwrap();
    assert_eq!(token_info.state.total_supply.amount, RawTokenAmount(1000));

    // Assert account balance increased
    assert_eq!(
        block_state
            .token_account_state(&external, &token_id, gov_account)
            .unwrap()
            .balance
            .amount,
        RawTokenAmount(1000)
    );

    // Assert mint event
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenMint(mint) => {
        assert_eq!(mint.token_id, token_id);
        assert_eq!(mint.amount.amount, RawTokenAmount(1000));
        assert_eq!(mint.amount.decimals, 4);
        assert_eq!(mint.target, TokenHolder::Account(external.account_canonical_address(&gov_account)));
    });
}

/// Test protocol-level token mint using account address alias.
#[test]
fn test_plt_mint_using_alias() {
    test_plt_mint_using_alias_worker::<p10::PltBlockStateP10>();
}
fn test_plt_mint_using_alias_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = block_state.create_and_init_token(
        &mut external,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );

    let gov_account_address_alias = external
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

    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        gov_account_address_alias,
    );

    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // Assert circulating supply increased
    let token_info = block_state.query_token_info(&external, &token_id).unwrap();
    assert_eq!(token_info.state.total_supply.amount, RawTokenAmount(1000));

    // Assert account balance increased
    assert_eq!(
        block_state
            .token_account_state(&external, &token_id, gov_account)
            .unwrap()
            .balance
            .amount,
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
    test_plt_mint_reject_worker::<p10::PltBlockStateP10>();
}

fn test_plt_mint_reject_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = block_state.create_and_init_token(
        &mut external,
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

    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        external.account_canonical_address(&gov_account),
    );

    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(reject_reason) => reject_reason);

    // Assert circulating supply and account balance unchanged
    let token_info = block_state.query_token_info(&external, &token_id).unwrap();
    assert_eq!(token_info.state.total_supply.amount, RawTokenAmount(5000));

    assert_eq!(
        block_state
            .token_account_state(&external, &token_id, gov_account)
            .unwrap()
            .balance
            .amount,
        RawTokenAmount(5000)
    );

    let reject_reason = utils::assert_token_module_reject_reason(&token_id, reject_reason);
    assert_matches!(reject_reason, TokenModuleRejectReason::MintWouldOverflow(_));
}

/// Test protocol-level token mint from unauthorized sender.
#[test]
fn test_plt_mint_unauthorized() {
    test_plt_mint_unauthorized_worker::<p10::PltBlockStateP10>();
}
fn test_plt_mint_unauthorized_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let _gov_account = block_state.create_and_init_token(
        &mut external,
        token_id.clone(),
        TokenInitTestParams::default(),
        4,
        None,
    );
    let non_governance_account = external.create_account();

    let operations = vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
        amount: TokenAmount::from_raw(1000, 4),
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        non_governance_account,
        external.account_canonical_address(&non_governance_account),
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(reject_reason) => reject_reason);

    // Assert circulating supply and account balance unchanged
    let token_info = block_state.query_token_info(&external, &token_id).unwrap();
    assert_eq!(token_info.state.total_supply.amount, RawTokenAmount(0));
    assert_eq!(
        block_state.token_account_state(&external, &token_id, non_governance_account),
        None
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
                    external.account_canonical_address(&non_governance_account)
                ))
            );
            assert_eq!(reason.as_deref(), Some("sender is not the token governance account"));
        }
    );
}

/// Test protocol-level token burn.
#[test]
fn test_plt_burn() {
    test_plt_burn_worker::<p10::PltBlockStateP10>();
}
fn test_plt_burn_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = block_state.create_and_init_token(
        &mut external,
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

    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        external.account_canonical_address(&gov_account),
    );

    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // Assert circulating supply decreased
    let token_info = block_state.query_token_info(&external, &token_id).unwrap();
    assert_eq!(token_info.state.total_supply.amount, RawTokenAmount(4000));

    // Assert account balance decreased
    assert_eq!(
        block_state
            .token_account_state(&external, &token_id, gov_account)
            .unwrap()
            .balance
            .amount,
        RawTokenAmount(4000)
    );

    // Assert burn event
    assert_eq!(events.len(), 1);
    assert_matches!(&events[0], BlockItemEvent::TokenBurn(burn) => {
        assert_eq!(burn.token_id, token_id);
        assert_eq!(burn.amount.amount, RawTokenAmount(1000));
        assert_eq!(burn.amount.decimals, 4);
        assert_eq!(burn.target, TokenHolder::Account(external.account_canonical_address(&gov_account)));
    });
}

/// Test protocol-level token burn using address alias for governance account
#[test]
fn test_plt_burn_using_alias() {
    test_plt_burn_using_alias_worker::<p10::PltBlockStateP10>()
}

fn test_plt_burn_using_alias_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = block_state.create_and_init_token(
        &mut external,
        token_id.clone(),
        TokenInitTestParams::default().burnable(),
        4,
        Some(RawTokenAmount(5000)),
    );

    let gov_account_address_alias = external
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

    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        gov_account_address_alias,
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // Assert circulating supply decreased
    let token_info = block_state.query_token_info(&external, &token_id).unwrap();
    assert_eq!(token_info.state.total_supply.amount, RawTokenAmount(4000));

    // Assert account balance decreased
    assert_eq!(
        block_state
            .token_account_state(&external, &token_id, gov_account)
            .unwrap()
            .balance
            .amount,
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
    test_plt_burn_reject_worker::<p10::PltBlockStateP10>()
}

fn test_plt_burn_reject_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = block_state.create_and_init_token(
        &mut external,
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

    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        external.account_canonical_address(&gov_account),
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    let reject_reason = assert_matches!(result.outcome, TransactionOutcome::Rejected(reject_reason) => reject_reason);

    // Assert circulating supply and account balance unchanged
    let token_info = block_state.query_token_info(&external, &token_id).unwrap();
    assert_eq!(token_info.state.total_supply.amount, RawTokenAmount(5000));
    assert_eq!(
        block_state
            .token_account_state(&external, &token_id, gov_account)
            .unwrap()
            .balance
            .amount,
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
    test_plt_multiple_operations_worker::<p10::PltBlockStateP10>();
}

fn test_plt_multiple_operations_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = block_state.create_and_init_token(
        &mut external,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    let account2 = external.create_account();

    // Compose two operations: Mint and then transfer

    let operations = vec![
        TokenOperation::Mint(TokenSupplyUpdateDetails {
            amount: TokenAmount::from_raw(3000, 4),
        }),
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(1000, 4),
            recipient: CborHolderAccount::from(external.account_canonical_address(&account2)),
            memo: None,
        }),
    ];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        external.account_canonical_address(&gov_account),
    );

    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);

    // Assert circulating supply and account balances
    let token_info = block_state.query_token_info(&external, &token_id).unwrap();
    assert_eq!(token_info.state.total_supply.amount, RawTokenAmount(3000));

    assert_eq!(
        block_state
            .token_account_state(&external, &token_id, gov_account)
            .unwrap()
            .balance
            .amount,
        RawTokenAmount(2000)
    );
    assert_eq!(
        block_state
            .token_account_state(&external, &token_id, account2)
            .unwrap()
            .balance
            .amount,
        RawTokenAmount(1000)
    );

    // Assert two events in right order
    assert_eq!(events.len(), 2);
    assert_matches!(&events[0], BlockItemEvent::TokenMint(mint) => {
        assert_eq!(mint.token_id, token_id);
        assert_eq!(mint.amount.amount, RawTokenAmount(3000));
        assert_eq!(mint.amount.decimals, 4);
        assert_eq!(mint.target, TokenHolder::Account(external.account_canonical_address(&gov_account)));
    });
    assert_matches!(&events[1], BlockItemEvent::TokenTransfer(transfer) => {
        assert_eq!(transfer.token_id, token_id);
        assert_eq!(transfer.amount.amount, RawTokenAmount(1000));
        assert_eq!(transfer.amount.decimals, 4);
        assert_eq!(transfer.from, TokenHolder::Account(external.account_canonical_address(&gov_account)));
        assert_eq!(transfer.to, TokenHolder::Account(external.account_canonical_address(&account2)));
        assert_eq!(transfer.memo, None);
    });
}

/// Test protocol-level token "pause" operation.
#[test]
fn test_plt_pause() {
    test_plt_pause_worker::<p10::PltBlockStateP10>();
}

fn test_plt_pause_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = block_state.create_and_init_token(
        &mut external,
        token_id.clone(),
        TokenInitTestParams::default(),
        4,
        None,
    );

    // Add the "pause" operation
    let operations = vec![TokenOperation::Pause(TokenPauseDetails {})];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        external.account_canonical_address(&gov_account),
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
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
    let token_info = block_state.query_token_info(&external, &token_id).unwrap();
    let token_module_state: TokenModuleState =
        cbor::cbor_decode(&token_info.state.module_state).unwrap();
    assert_eq!(token_module_state.paused, Some(true));

    // Test transfer is now rejected
    let account1 = external.create_account();
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(10000, 4),
        recipient: CborHolderAccount::from(external.account_canonical_address(&account1)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        external.account_canonical_address(&gov_account),
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
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
    test_plt_unpause_worker::<p10::PltBlockStateP10>();
}

fn test_plt_unpause_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = block_state.create_and_init_token(
        &mut external,
        token_id.clone(),
        TokenInitTestParams::default(),
        4,
        None,
    );

    // Add the "unpause" operation
    let operations = vec![TokenOperation::Unpause(TokenPauseDetails {})];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        external.account_canonical_address(&gov_account),
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
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
    test_non_existing_token_id_worker::<p10::PltBlockStateP10>();
}

fn test_non_existing_token_id_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;

    let account1 = external.create_account();
    let account2 = external.create_account();

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 4),
        recipient: CborHolderAccount::from(external.account_canonical_address(&account2)),
        memo: None,
    })];
    let token_id: TokenId = "tokenid1".parse().unwrap();
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        account1,
        external.account_canonical_address(&account1),
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
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
    test_energy_charge_worker::<p10::PltBlockStateP10>();
}
fn test_energy_charge_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = block_state.create_and_init_token(
        &mut external,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    let account2 = external.create_account();
    block_state.increment_account_balance(
        &mut external,
        &mut blob_store,
        gov_account,
        &token_id,
        RawTokenAmount(5000),
    );

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(3000, 4),
        recipient: CborHolderAccount::from(external.account_canonical_address(&account2)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        external.account_canonical_address(&gov_account),
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
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
    test_energy_charge_at_reject_worker::<p10::PltBlockStateP10>();
}

fn test_energy_charge_at_reject_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = block_state.create_and_init_token(
        &mut external,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    let account2 = external.create_account();
    block_state.increment_account_balance(
        &mut external,
        &mut blob_store,
        gov_account,
        &token_id,
        RawTokenAmount(5000),
    );

    // Transfer operation with a larger amount than the token balance of the sender `gov_account`,
    // which will be the cause of the rejection.
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(10000, 4),
        recipient: CborHolderAccount::from(external.account_canonical_address(&account2)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        external.account_canonical_address(&gov_account),
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
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
    test_out_of_energy_error_worker::<p10::PltBlockStateP10>();
}

fn test_out_of_energy_error_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let gov_account = block_state.create_and_init_token(
        &mut external,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    let account2 = external.create_account();
    block_state.increment_account_balance(
        &mut external,
        &mut blob_store,
        gov_account,
        &token_id,
        RawTokenAmount(5000),
    );

    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(3000, 4),
        recipient: CborHolderAccount::from(external.account_canonical_address(&account2)),
        memo: None,
    })];
    let payload = TokenOperationsPayload {
        token_id: "tokenid1".parse().unwrap(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let mut execution = TransactionExecution::new(
        Energy::from(150), // needs 300 + 100 to succeed
        gov_account,
        external.account_canonical_address(&gov_account),
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
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
