//! Test of protocol-level token queries. Notice that detailed test of the token module queries are
//! implemented in the `plt-token-module` crate.

use crate::block_state_stub::{BlockStateStub, TokenInitTestParams};
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{TokenAmount, TokenModuleState};
use plt_scheduler::block_state_interface::BlockStateQuery;
use plt_scheduler::queries;
use plt_token_module::TOKEN_MODULE_REF;

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
    let token_id = "TokenId1".parse().unwrap();
    let (token, _) = stub.create_and_init_token(token_id, TokenInitTestParams::default(), 4, None);
    let token_id = stub.token_configuration(&token).token_id;

    let non_canonical_token_id = "toKeniD1".parse().unwrap();
    // Lookup by token id that is not in canonical casing
    let token_info = queries::token_info(&stub, &non_canonical_token_id).unwrap();
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
