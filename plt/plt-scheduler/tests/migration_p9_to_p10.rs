//! Scheduler-level smoke test for block state migration from P9 to P10.
//!
//! This complements the block state migration unit test in the `plt-block-state` crate.

use crate::utils::TokenInitTestParams;
use crate::utils::entity_traits::scheduler::SchedulerOperations;
use concordium_base::protocol_level_tokens::{
    TokenAmount, TokenId, TokenOperation, TokenSupplyUpdateDetails,
};
use plt_block_state::entity::block_state::p9::BlockStateP9;
use plt_block_state::entity::entity_test_stub;
use plt_scheduler::TOKEN_MODULE_REF;
use plt_scheduler_types::types::tokens::{RawTokenAmount, TokenAmount as QueryTokenAmount};

mod utils;

const DECIMALS: u8 = 2;

/// Smoke test of migrating a P9 block state to P10 at the scheduler level.
#[test]
fn test_migrate_p9_to_p10() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateP9::default();

    // Create the token on the block state being migrated from (P9).
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _token_index) = utils::create_and_init_token_p9(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        DECIMALS,
        None,
    );

    // Migrate the block state from P9 to P10.
    let (mut context, mut block_state) = utils::migrate_p9_to_p10(&mut context, block_state);

    // Query the migrated state (P10) using the scheduler-implemented queries.
    assert_eq!(block_state.query_plt_list(&context), vec![token_id.clone()]);
    let token_info = block_state
        .query_token_info(&context, &token_id)
        .expect("token info is queryable after migration");
    assert_eq!(token_info.token_id, token_id);
    assert_eq!(token_info.state.token_module_ref, TOKEN_MODULE_REF);
    assert_eq!(token_info.state.decimals, DECIMALS);
    assert_eq!(
        token_info.state.total_supply,
        QueryTokenAmount {
            amount: RawTokenAmount(0),
            decimals: DECIMALS,
        }
    );

    // Execute a transaction on the migrated state.
    utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        gov_account.account_index(),
        vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
            amount: TokenAmount::from_raw(100, DECIMALS),
        })],
    );
}
