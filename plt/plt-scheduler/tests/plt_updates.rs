//! Test of protocol-level token updates

use crate::block_state_stub::{BlockStateStub, TokenInitTestParams};
use plt_scheduler::block_state_interface::BlockStateQuery;
use plt_scheduler::plt_queries;

mod block_state_stub;
mod utils;

/// Test protocol-level token transfer.
#[test]
fn test_plt_transfer() {
    let mut stub = BlockStateStub::new();
    let token1 = stub.init_token(TokenInitTestParams::default(), 4);
    let token2 = stub.init_token(TokenInitTestParams::default(), 4);

    let token_id1 = stub.token_configuration(&token1).token_id;
    let token_id2 = stub.token_configuration(&token2).token_id;

    let plts = plt_queries::plt_list(&stub);
    assert_eq!(plts, vec![token_id1, token_id2]);
}
