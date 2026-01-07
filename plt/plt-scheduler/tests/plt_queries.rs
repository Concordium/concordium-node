//! Test of protocol-level token queries

use crate::block_state_stub::{BlockStateStub, TokenInitTestParams};
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{TokenAmount, TokenModuleState};
use plt_scheduler::block_state_interface::BlockStateQuery;
use plt_scheduler::{TOKEN_MODULE_REF, plt_queries};

mod block_state_stub;
mod utils;

/// Test query token state
#[test]
fn test_query_plt_list() {
    let mut stub = BlockStateStub::new();
    let token1 = stub.init_token(TokenInitTestParams::default(), 4);
    let token2 = stub.init_token(TokenInitTestParams::default(), 4);

    let token_id1 = stub.token_configuration(&token1).token_id;
    let token_id2 = stub.token_configuration(&token2).token_id;

    let plts = plt_queries::plt_list(&stub);
    assert_eq!(plts, vec![token_id1, token_id2]);
}

/// Test query token state
#[test]
fn test_query_token_state() {
    let mut stub = BlockStateStub::new();
    let token = stub.init_token(TokenInitTestParams::default(), 4);
    let token_id = stub.token_configuration(&token).token_id;

    let token_state = plt_queries::token_state(&stub, &token_id).unwrap();
    assert_eq!(token_state.decimals, 4);
    assert_eq!(token_state.total_supply, TokenAmount::from_raw(0, 4));
    assert_eq!(token_state.token_module_ref, TOKEN_MODULE_REF);
    let _token_module_state: TokenModuleState =
        cbor::cbor_decode(&token_state.module_state).unwrap();
}
