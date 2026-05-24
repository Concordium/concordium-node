//! Tests for the block state stub infrastructure used in the plt-scheduler integration tests.

use crate::utils::{BlockStateLatest, TokenInitTestParams};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::TokenId;
use plt_block_state::entity::accounts::Accounts;
use plt_block_state::entity::entity_test_stub;
use plt_scheduler_types::types::tokens::RawTokenAmount;

mod utils;

/// Test lookup account address and account from address.
#[test]
fn test_account_lookup_address() {
    let mut context = entity_test_stub::new_stubbed_context();

    let account = context.external.create_account();
    let address = context
        .external
        .account_canonical_address(account.account_index());

    context
        .account_by_address(&address)
        .expect("Account is expected to exist");
    assert!(
        context
            .account_by_address(&AccountAddress([2u8; 32]))
            .is_err(),
        "Account is not expected to exist"
    );
}

/// Test lookup account index and account from index.
#[test]
fn test_account_lookup_index() {
    let mut context = entity_test_stub::new_stubbed_context();

    let account = context.external.create_account();

    context
        .account_by_index(account.account_index())
        .expect("Account is expected to exist");
    assert!(
        context.account_by_index(AccountIndex::from(2u64)).is_err(),
        "Account is not expected to exist"
    );
}

/// Test get account token balance.
#[test]
fn test_account_balance() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut block_state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );

    let account0 = context.external.create_account();
    let account1 = context.external.create_account();
    utils::increment_account_balance_p11(
        &mut context,
        &mut block_state,
        account0.account_index(),
        &token_id,
        RawTokenAmount(245),
    );

    let token = block_state
        .token_by_id(&context, &token_id)
        .unwrap()
        .unwrap();

    assert_eq!(
        account0.account_token_balance(&context, token.token_base.token_index()),
        RawTokenAmount(245)
    );
    assert_eq!(
        account1.account_token_balance(&context, token.token_base.token_index()),
        RawTokenAmount(0)
    );
}

/// Test looking up account by alias.
#[test]
fn test_account_by_alias() {
    let mut context = entity_test_stub::new_stubbed_context();

    let account = context.external.create_account();
    let account_address = context
        .external
        .account_canonical_address(account.account_index());
    let account_by_alias = context
        .account_by_address(&account_address.get_alias(0).unwrap())
        .unwrap();

    assert_eq!(account.account_index(), account_by_alias.account_index());
}
