//! Tests for the block state stub infrastructure used in the plt-scheduler integration tests.

use crate::utils::BlockStateLatest;
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use plt_block_state::block_state_interface::BlockStateQuery;
use plt_block_state::entity::block_state::Accounts;
use plt_block_state::entity::entity_test_stub;

mod utils;

/// Test lookup account address and account from address.
#[test]
fn test_account_lookup_address() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let account = context.external.create_account();
    let address = context
        .external
        .account_canonical_address(account.account_index());

    block_state
        .account_by_address(&context, &address)
        .expect("Account is expected to exist");
    assert!(
        block_state
            .account_by_address(&context, &AccountAddress([2u8; 32]))
            .is_err(),
        "Account is not expected to exist"
    );
}

/// Test lookup account index and account from index.
#[test]
fn test_account_lookup_index() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let account = context.external.create_account();

    block_state
        .account_by_index(&context, account.account_index())
        .expect("Account is expected to exist");
    assert!(
        block_state
            .account_by_index(&context, AccountIndex::from(2u64))
            .is_err(),
        "Account is not expected to exist"
    );
}

// todo ar
// /// Test get account token balance.
// #[test]
// fn test_account_balance() {
//     let mut context = entity_test_stub::new_stubbed_context();
//     let mut block_state = BlockStateLatest::default();
//
//     let token_id = "TokenId1".parse().unwrap();
//     utils::create_and_init_token(
//         &mut context,
//         &mut block_state,
//         token_id,
//         TokenInitTestParams::default().mintable(),
//         2,
//         None,
//     );
//
//     let account0 = context.external.create_account();
//     let account1 = context.external.create_account();
//     utils::increment_account_balance(account0, token, RawTokenAmount(245));
//
//     assert_eq!(
//         stub.state().account_token_balance(&account0, &token),
//         RawTokenAmount(245)
//     );
//     assert_eq!(
//         stub.state().account_token_balance(&account1, &token),
//         RawTokenAmount(0)
//     );
// }

/// Test looking up account by alias.
#[test]
fn test_account_by_alias() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let account = context.external.create_account();
    let account_address = context
        .external
        .account_canonical_address(account.account_index());
    let account_by_alias = block_state
        .account_by_address(&context, &account_address.get_alias(0).unwrap())
        .unwrap();

    assert_eq!(account.account_index(), account_by_alias.account_index());
}
