//! Scheduler-level smoke test for block state migration from P10 to P11.
//!
//! This complements the block state migration unit test in the `plt-block-state` crate.

use crate::utils::TokenInitTestParams;
use crate::utils::entity_traits::scheduler::SchedulerOperations;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, TokenAmount, TokenAuthorizations, TokenId, TokenOperation,
    TokenSupplyUpdateDetails,
};
use plt_block_state::entity::block_state::migration;
use plt_block_state::entity::block_state::p10::BlockStateP10;
use plt_block_state::entity::entity_test_stub;
use plt_scheduler::TOKEN_MODULE_REF;
use plt_scheduler_types::types::tokens::{RawTokenAmount, TokenAmount as QueryTokenAmount};

mod utils;

const DECIMALS: u8 = 2;

/// Smoke test of migrating a P10 block state to P11 at the scheduler level.
///
/// P11 introduces a new authorization (roles) model which we specifically test.
#[test]
fn test_migrate_p10_to_p11() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateP10::default();

    // Create the token on the block state being migrated from (P10).
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (gov_account, _token_index) = utils::create_and_init_token_p9(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable().burnable(),
        DECIMALS,
        None,
    );

    // Migrate the block state from P10 to P11.
    let (mut context, mut block_state) =
        migration::test_utils::migrate_p10_to_p11(&mut context, block_state);

    // Query the migrated state (P11) using the scheduler-implemented queries.
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
            amount: RawTokenAmount::from(0),
            decimals: DECIMALS,
        }
    );

    // Assert that the governance account was migrated into the new authorization model with the
    // expected roles.
    let authorizations = block_state
        .query_token_authorizations(&context, &token_id)
        .expect("token authorizations are queryable after migration");
    assert_eq!(authorizations.token_id, token_id);
    let details: TokenAuthorizations = cbor::cbor_decode(&authorizations.details).unwrap();
    let gov_holder = CborHolderAccount::from(
        context
            .external
            .account_canonical_address(gov_account.account_index()),
    );
    assert_eq!(
        details.update_admin_roles.unwrap().accounts,
        vec![gov_holder.clone()]
    );
    assert_eq!(
        details.update_metadata.unwrap().accounts,
        vec![gov_holder.clone()]
    );
    assert_eq!(details.pause.unwrap().accounts, vec![gov_holder.clone()]);
    assert_eq!(details.mint.unwrap().accounts, vec![gov_holder.clone()]);
    assert_eq!(details.burn.unwrap().accounts, vec![gov_holder]);
    // No allow/deny list was configured, so those authorizations are absent.
    assert!(details.update_allow_list.is_none());
    assert!(details.update_deny_list.is_none());

    // The governance account must retain the permissions to run the expected transactions under the
    // new authorization model: mint (requires the `Mint` role) and pause (requires the `Pause`
    // role).
    utils::execute_token_operations(
        &mut context,
        &mut block_state,
        &token_id,
        gov_account.account_index(),
        vec![TokenOperation::Mint(TokenSupplyUpdateDetails {
            amount: TokenAmount::from_raw(100, DECIMALS),
        })],
    );
    utils::pause_token(
        &mut context,
        &mut block_state,
        &token_id,
        gov_account.account_index(),
    );
}
