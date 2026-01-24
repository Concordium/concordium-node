//! Test of protocol-level token queries. Notice that detailed test of the token module queries are
//! implemented in the `plt-token-module` crate.

use crate::block_state_stub::{BlockStateStub, TokenInitTestParams};
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{TokenAmount, TokenId, TokenModuleState};
use plt_scheduler::block_state_interface::BlockStateQuery;
use plt_scheduler::queries;
use plt_token_module::TOKEN_MODULE_REF;
use plt_types::types::primitives::RawTokenAmount;

mod block_state_stub;

/// Test query token state
#[test]
fn test_query_plt_list() {
    let mut stub = BlockStateStub::new();
    let token_id1 = "TokenId1".parse().unwrap();
    let (token1, _) =
        stub.create_and_init_token(token_id1, TokenInitTestParams::default(), 4, None);
    let token_id2 = "TokenId2".parse().unwrap();
    let (token2, _) =
        stub.create_and_init_token(token_id2, TokenInitTestParams::default(), 4, None);

    let token_id1 = stub.token_configuration(&token1).token_id;
    let token_id2 = stub.token_configuration(&token2).token_id;

    let plts = queries::plt_list(&stub);
    assert_eq!(plts, vec![token_id1, token_id2]);
}

/// Test query token info
#[test]
fn test_query_token_info() {
    let mut stub = BlockStateStub::new();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, _) =
        stub.create_and_init_token(token_id.clone(), TokenInitTestParams::default(), 4, None);

    let non_canonical_token_id = "toKeniD1".parse().unwrap();
    // Lookup by token id that is not in canonical casing
    let token_info = queries::query_token_info(&stub, &non_canonical_token_id).unwrap();
    // Assert that the token id returned is in the canonical casing
    assert_eq!(token_info.token_id, token_id);
    assert_eq!(token_info.state.decimals, 4);
    assert_eq!(token_info.state.total_supply, TokenAmount::from_raw(0, 4));
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
    let mut stub = BlockStateStub::new();
    let account = stub.create_account();
    let token_id1: TokenId = "TokenId1".parse().unwrap();
    let (token1, _) =
        stub.create_and_init_token(token_id1.clone(), TokenInitTestParams::default(), 4, None);
    let token_id2: TokenId = "TokenId2".parse().unwrap();
    let (token2, _) =
        stub.create_and_init_token(token_id2.clone(), TokenInitTestParams::default(), 4, None);
    let token_id3 = "TokenId3".parse().unwrap();
    let (_token3, _) =
        stub.create_and_init_token(token_id3, TokenInitTestParams::default(), 4, None);

    stub.increment_account_balance(account, token1, RawTokenAmount(1000));
    stub.increment_account_balance(account, token2, RawTokenAmount(2000));

    // Lookup account token infos
    let token_account_infos = queries::query_token_account_infos(&stub, account).unwrap();
    assert_eq!(token_account_infos.len(), 2);
    assert_eq!(token_account_infos[0].token_id, token_id1);
    assert_eq!(
        token_account_infos[0].account_state.balance,
        TokenAmount::from_raw(1000, 4)
    );
    assert_eq!(token_account_infos[1].token_id, token_id2);
    assert_eq!(
        token_account_infos[1].account_state.balance,
        TokenAmount::from_raw(2000, 4)
    );
}
