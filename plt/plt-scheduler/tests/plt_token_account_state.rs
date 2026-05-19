//! Tests for token module account state queries via the scheduler.

use crate::utils::TokenInitTestParams;
use crate::utils::entity_traits::scheduler::SchedulerOperations;
use concordium_base::protocol_level_tokens::TokenId;
use plt_block_state::entity::entity_test_stub;

use crate::utils::BlockStateLatest;

mod utils;

/// Test token module account state without lists enabled.
#[test]
fn test_query_token_module_account_state_default() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id,
        TokenInitTestParams::default(),
        0,
        None,
    );
    let account = context.external.create_account();

    let token_account_infos =
        block_state.query_token_account_infos(&context, account.account_index());
    // Account has no balance and no list entries, so it does not appear in the infos
    assert!(token_account_infos.is_empty());
}
