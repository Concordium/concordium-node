//! Test of protocol-level token queries. Notice that detailed test of the token module queries are
//! implemented in the `plt-token-module` crate.

use assert_matches::assert_matches;
use concordium_base::base::Energy;
use concordium_base::common::cbor;
use concordium_base::protocol_level_locks::{LockControllerSimpleV0Capability, LockId};
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, RawCbor, TokenId, TokenListUpdateDetails, TokenModuleAccountState,
    TokenModuleState, TokenOperation, TokenOperationsPayload,
};
use concordium_base::transactions::Payload;
use plt_block_state::block_state_interface::BlockStateQuery;
use plt_scheduler::TOKEN_MODULE_REF;
use plt_scheduler::{queries, scheduler};
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::locks::LockControllerSimpleV0Grant;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use utils::block_state_external_stubbed::{
    BlockStateWithExternalStateStubbed, TokenInitTestParams,
};

mod utils;

/// Test query token state
#[test]
fn test_query_plt_list() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id1 = "TokenId1".parse().unwrap();
    let (token1, _) =
        stub.create_and_init_token(token_id1, TokenInitTestParams::default(), 4, None);
    let token_id2 = "TokenId2".parse().unwrap();
    let (token2, _) =
        stub.create_and_init_token(token_id2, TokenInitTestParams::default(), 4, None);

    let token_id1 = stub.state().token_configuration(&token1).token_id;
    let token_id2 = stub.state().token_configuration(&token2).token_id;

    let plts = queries::query_plt_list(stub.state());
    assert_eq!(plts, vec![token_id1, token_id2]);
}

/// Test query token info
#[test]
fn test_query_token_info() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, _) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 4, None);

    let non_canonical_token_id = "toKeniD1".parse().unwrap();
    // Lookup by token id that is not in canonical casing
    let token_info = queries::query_token_info(stub.state(), &non_canonical_token_id).unwrap();
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
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let account = stub.create_account();
    let token_id1: TokenId = "TokenId1".parse().unwrap();
    let (token1, _) = stub.create_and_init_token(
        token_id1.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    let token_id2: TokenId = "TokenId2".parse().unwrap();
    let (token2, _) = stub.create_and_init_token(
        token_id2.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    let token_id3 = "TokenId3".parse().unwrap();
    let (_token3, _) =
        stub.create_and_init_token(token_id3, TokenInitTestParams::default(), 4, None);

    stub.increment_account_balance(account, token1, RawTokenAmount(1000));
    stub.increment_account_balance(account, token2, RawTokenAmount(2000));

    // Lookup account token infos
    let token_account_infos = queries::query_token_account_infos(stub.state(), account).unwrap();
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

    let module_state1: TokenModuleAccountState = cbor::cbor_decode(
        token_account_infos[0]
            .account_state
            .module_state
            .as_ref()
            .unwrap(),
    )
    .unwrap();
    assert_eq!(module_state1.available, None);

    let module_state2: TokenModuleAccountState = cbor::cbor_decode(
        token_account_infos[1]
            .account_state
            .module_state
            .as_ref()
            .unwrap(),
    )
    .unwrap();
    assert_eq!(module_state2.available, None);
}

/// Test query token account info reports available balance excluding locked funds.
#[test]
fn test_query_token_account_info_available_with_locked_balance() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let account = stub.create_account();
    let recipient = stub.create_account();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, _) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );

    stub.increment_account_balance(account, token, RawTokenAmount(1000));

    let lock_id = LockId {
        account_index: 7,
        sequence_number: 2,
        creation_order: 0,
    };
    stub.create_lock(
        &lock_id,
        vec![recipient],
        vec![LockControllerSimpleV0Grant {
            account,
            roles: vec![LockControllerSimpleV0Capability::Fund],
        }],
        vec![token_id.clone()],
        1_804_806_000,
    );
    stub.lock_balance(&lock_id, &account, &token, RawTokenAmount(250));

    let token_account_infos = queries::query_token_account_infos(stub.state(), account).unwrap();
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
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let account = stub.create_account();
    let recipient = stub.create_account();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, _) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );

    stub.increment_account_balance(account, token, RawTokenAmount(1000));

    let lock_id1 = LockId {
        account_index: 7,
        sequence_number: 2,
        creation_order: 0,
    };
    let lock_id2 = LockId {
        account_index: 7,
        sequence_number: 2,
        creation_order: 1,
    };
    for lock_id in [&lock_id1, &lock_id2] {
        stub.create_lock(
            lock_id,
            vec![recipient],
            vec![LockControllerSimpleV0Grant {
                account,
                roles: vec![LockControllerSimpleV0Capability::Fund],
            }],
            vec![token_id.clone()],
            1_804_806_000,
        );
    }
    stub.lock_balance(&lock_id1, &account, &token, RawTokenAmount(250));
    stub.lock_balance(&lock_id2, &account, &token, RawTokenAmount(300));

    let token_account_infos = queries::query_token_account_infos(stub.state(), account).unwrap();
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
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let account = stub.create_account();
    let recipient = stub.create_account();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (token, _) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );

    stub.increment_account_balance(account, token, RawTokenAmount(1000));

    let lock_id = LockId {
        account_index: 7,
        sequence_number: 2,
        creation_order: 0,
    };
    stub.create_lock(
        &lock_id,
        vec![recipient],
        vec![LockControllerSimpleV0Grant {
            account,
            roles: vec![LockControllerSimpleV0Capability::Fund],
        }],
        vec![token_id],
        1_804_806_000,
    );
    stub.lock_balance(&lock_id, &account, &token, RawTokenAmount(1000));

    let token_account_infos = queries::query_token_account_infos(stub.state(), account).unwrap();
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
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let account = stub.create_account();
    let token_id: TokenId = "TokenId3".parse().unwrap();
    let (_token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        4,
        None,
    );

    let operations = vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_canonical_address(&account)),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
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
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let token_account_infos = queries::query_token_account_infos(stub.state(), account).unwrap();
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
