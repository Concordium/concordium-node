//! Test of protocol-level token queries. Notice that detailed test of the token module queries are
//! implemented in the `plt-token-module` crate.

use crate::utils::TokenInitTestParams;
use crate::utils::entity_traits::scheduler::SchedulerOperations;
use assert_matches::assert_matches;
use concordium_base::base::Energy;
use concordium_base::common::cbor;
use concordium_base::protocol_level_locks::{LockControllerSimpleV0Capability, LockId};
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, RawCbor, TokenId, TokenListUpdateDetails, TokenModuleAccountState,
    TokenModuleState, TokenOperation, TokenOperationsPayload,
};
use concordium_base::transactions::Payload;
use plt_block_state::entity::entity_test_stub;
use plt_block_state::persistent::protocol_level_locks::p11::LockControllerSimpleV0Grant;
use plt_scheduler::TOKEN_MODULE_REF;
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::tokens::RawTokenAmount;

use crate::utils::BlockStateLatest;

mod utils;

/// Test query token state
#[test]
fn test_query_plt_list() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id1: TokenId = "TokenId1".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id1.clone(),
        TokenInitTestParams::default(),
        4,
        None,
    );
    let token_id2: TokenId = "TokenId2".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id2.clone(),
        TokenInitTestParams::default(),
        4,
        None,
    );

    let plts = block_state.query_plt_list(&context);
    assert_eq!(plts, vec![token_id1, token_id2]);
}

/// Test query token info
#[test]
fn test_query_token_info() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default(),
        4,
        None,
    );

    let non_canonical_token_id = "toKeniD1".parse().unwrap();
    // Lookup by token id that is not in canonical casing
    let token_info = block_state
        .query_token_info(&context, &non_canonical_token_id)
        .unwrap();
    // Assert that the token id returned is in the canonical casing
    assert_eq!(token_info.token_id, token_id);
    assert_eq!(token_info.state.decimals, 4);
    assert_eq!(token_info.state.total_supply.amount, RawTokenAmount(0));
    assert_eq!(token_info.state.total_supply.decimals, 4);
    assert_eq!(token_info.state.token_module_ref, TOKEN_MODULE_REF);
    let token_module_state: TokenModuleState =
        cbor::cbor_decode(&token_info.state.module_state).unwrap();
    assert_eq!(
        token_module_state.name.as_deref(),
        Some("Protocol-level token")
    );
}

/// Test query token account info
#[test]
fn test_query_token_account_info() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let account = context.external.create_account();
    let token_id1: TokenId = "TokenId1".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id1.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    let token_id2: TokenId = "TokenId2".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id2.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    let token_id3: TokenId = "TokenId3".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id3,
        TokenInitTestParams::default(),
        4,
        None,
    );

    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        account.account_index(),
        &token_id1,
        RawTokenAmount(1000),
    );
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        account.account_index(),
        &token_id2,
        RawTokenAmount(2000),
    );

    // Lookup account token infos
    let token_account_infos =
        block_state.query_token_account_infos(&context, account.account_index());
    assert_eq!(token_account_infos.len(), 2);
    assert_eq!(token_account_infos[0].token_id, token_id1);
    assert_eq!(
        token_account_infos[0].account_state.balance.amount,
        RawTokenAmount(1000)
    );
    assert_eq!(token_account_infos[0].account_state.balance.decimals, 4);
    assert_eq!(token_account_infos[1].token_id, token_id2);
    assert_eq!(
        token_account_infos[1].account_state.balance.amount,
        RawTokenAmount(2000)
    );
    assert_eq!(token_account_infos[1].account_state.balance.decimals, 4);
}

/// Test query token account info reports available balance excluding locked funds.
#[test]
fn test_query_token_account_info_available_with_locked_balance() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let account = context.external.create_account();
    let recipient = context.external.create_account();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );

    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        account.account_index(),
        &token_id,
        RawTokenAmount(1000),
    );

    let lock_id = LockId {
        account_index: account.account_index().into(),
        sequence_number: 1,
        creation_order: 0,
    };
    utils::create_lock(
        &mut context,
        &mut block_state,
        &lock_id,
        vec![recipient.account_index()],
        vec![LockControllerSimpleV0Grant {
            account: account.account_index(),
            roles: vec![LockControllerSimpleV0Capability::Fund],
        }],
        vec![token_id.clone()],
        1_804_806_000,
    );
    utils::lock_balance(
        &mut context,
        &mut block_state,
        &lock_id,
        account.account_index(),
        &token_id,
        RawTokenAmount(250),
    );

    let token_account_infos =
        block_state.query_token_account_infos(&context, account.account_index());
    assert_eq!(token_account_infos.len(), 1);
    assert_eq!(token_account_infos[0].token_id, token_id);
    let module_state: TokenModuleAccountState = cbor::cbor_decode(
        token_account_infos[0]
            .account_state
            .module_state
            .as_ref()
            .unwrap(),
    )
    .unwrap();
    let available = module_state.available.unwrap();
    assert_eq!(available.value(), 750);
    assert_eq!(available.decimals(), 4);
    assert_eq!(module_state.locks.len(), 1);
    assert_eq!(module_state.locks[0].lock, lock_id);
    assert_eq!(module_state.locks[0].amount.value(), 250);
    assert_eq!(module_state.locks[0].amount.decimals(), 4);
}

/// Test query token account info sums multiple locks for the same account/token pair.
#[test]
fn test_query_token_account_info_available_with_multiple_locks() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let account = context.external.create_account();
    let recipient = context.external.create_account();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );

    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        account.account_index(),
        &token_id,
        RawTokenAmount(1000),
    );

    let lock_id1 = LockId {
        account_index: account.account_index().into(),
        sequence_number: 1,
        creation_order: 0,
    };
    utils::create_lock(
        &mut context,
        &mut block_state,
        &lock_id1,
        vec![recipient.account_index()],
        vec![LockControllerSimpleV0Grant {
            account: account.account_index(),
            roles: vec![LockControllerSimpleV0Capability::Fund],
        }],
        vec![token_id.clone()],
        1_804_806_000,
    );
    let lock_id2 = LockId {
        account_index: account.account_index().into(),
        sequence_number: 2,
        creation_order: 0,
    };
    utils::create_lock(
        &mut context,
        &mut block_state,
        &lock_id2,
        vec![recipient.account_index()],
        vec![LockControllerSimpleV0Grant {
            account: account.account_index(),
            roles: vec![LockControllerSimpleV0Capability::Fund],
        }],
        vec![token_id.clone()],
        1_804_806_000,
    );
    utils::lock_balance(
        &mut context,
        &mut block_state,
        &lock_id1,
        account.account_index(),
        &token_id,
        RawTokenAmount(250),
    );
    utils::lock_balance(
        &mut context,
        &mut block_state,
        &lock_id2,
        account.account_index(),
        &token_id,
        RawTokenAmount(300),
    );

    let token_account_infos =
        block_state.query_token_account_infos(&context, account.account_index());
    let module_state: TokenModuleAccountState = cbor::cbor_decode(
        token_account_infos[0]
            .account_state
            .module_state
            .as_ref()
            .unwrap(),
    )
    .unwrap();
    let available = module_state.available.unwrap();
    assert_eq!(available.value(), 450);
    assert_eq!(available.decimals(), 4);
    assert_eq!(module_state.locks.len(), 2);
    assert_eq!(module_state.locks[0].lock, lock_id1);
    assert_eq!(module_state.locks[0].amount.value(), 250);
    assert_eq!(module_state.locks[0].amount.decimals(), 4);
    assert_eq!(module_state.locks[1].lock, lock_id2);
    assert_eq!(module_state.locks[1].amount.value(), 300);
    assert_eq!(module_state.locks[1].amount.decimals(), 4);
}

/// Test query token account info reports zero available balance when all funds are locked.
#[test]
fn test_query_token_account_info_available_zero_when_fully_locked() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let account = context.external.create_account();
    let recipient = context.external.create_account();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );

    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        account.account_index(),
        &token_id,
        RawTokenAmount(1000),
    );

    let lock_id = LockId {
        account_index: account.account_index().into(),
        sequence_number: 1,
        creation_order: 0,
    };
    utils::create_lock(
        &mut context,
        &mut block_state,
        &lock_id,
        vec![recipient.account_index()],
        vec![LockControllerSimpleV0Grant {
            account: account.account_index(),
            roles: vec![LockControllerSimpleV0Capability::Fund],
        }],
        vec![token_id.clone()],
        1_804_806_000,
    );
    utils::lock_balance(
        &mut context,
        &mut block_state,
        &lock_id,
        account.account_index(),
        &token_id,
        RawTokenAmount(1000),
    );

    let token_account_infos =
        block_state.query_token_account_infos(&context, account.account_index());
    let module_state: TokenModuleAccountState = cbor::cbor_decode(
        token_account_infos[0]
            .account_state
            .module_state
            .as_ref()
            .unwrap(),
    )
    .unwrap();
    let available = module_state.available.unwrap();
    assert_eq!(available.value(), 0);
    assert_eq!(available.decimals(), 4);
    assert_eq!(module_state.locks.len(), 1);
    assert_eq!(module_state.locks[0].lock, lock_id);
    assert_eq!(module_state.locks[0].amount.value(), 1000);
    assert_eq!(module_state.locks[0].amount.decimals(), 4);
}

// Test that adding an account to a token list properly touches the account
#[test]
fn test_query_token_account_info_allow_list_no_balance() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let account = context.external.create_account();
    let token_id: TokenId = "TokenId3".parse().unwrap();
    let (gov_account, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        4,
        None,
    );

    let account_addr = context
        .external
        .account_canonical_address(account.account_index());
    let gov_addr = context
        .external
        .account_canonical_address(gov_account.account_index());
    let operations = vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(account_addr),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            plt_scheduler::TransactionContext {
                energy_limit: Energy::from(u64::MAX),
                sender_account_address: gov_addr,
                transaction_sequence_number: 1.into(),
                block_timestamp: 0.into(),
            },
            gov_account.account_index(),
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let token_account_infos =
        block_state.query_token_account_infos(&context, account.account_index());
    assert_eq!(token_account_infos.len(), 1);
    assert_eq!(token_account_infos[0].token_id, token_id);
    assert_eq!(
        token_account_infos[0].account_state.balance.amount,
        RawTokenAmount(0)
    );
    assert_eq!(token_account_infos[0].account_state.balance.decimals, 4);
    let module_state: TokenModuleAccountState = cbor::cbor_decode(
        token_account_infos[0]
            .account_state
            .module_state
            .as_ref()
            .unwrap(),
    )
    .unwrap();
    assert_eq!(module_state.allow_list, Some(true));
    assert_eq!(module_state.deny_list, None);
}
