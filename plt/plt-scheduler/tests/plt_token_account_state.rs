//! Tests for token module account state queries via the scheduler.

use crate::utils::TokenInitTestParams;
use concordium_base::protocol_level_tokens::TokenId;
use plt_scheduler::queries;

mod utils;

/// Test token module account state without lists enabled.
#[test]
fn test_query_token_module_account_state_default() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, _gov_account) =
        stub.create_and_init_token(token_id, TokenInitTestParams::default(), 0, None);
    let account = stub.create_account();

    let token_account_infos = queries::query_token_account_infos(stub.state(), account);
    // Account has no balance and no list entries, so it does not appear in the infos
    assert!(token_account_infos.is_empty());
}
