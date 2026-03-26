//! Tests for the block state stub infrastructure used in the plt-scheduler integration tests.

use crate::block_state_external_stubbed::{
    BlockStateWithExternalStateStubbed, TokenInitTestParams,
};
use concordium_base::base::AccountIndex;
use concordium_base::contracts_common::AccountAddress;
use plt_block_state::block_state_interface::BlockStateQuery;
use plt_scheduler_types::types::tokens::RawTokenAmount;

mod block_state_external_stubbed;
mod utils;

/// Test lookup account address and account from address.
#[test]
fn test_account_lookup_address() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let account = stub.create_account();

    let address = stub.account_canonical_address(&account);
    stub.state()
        .account_by_address(&address)
        .expect("Account is expected to exist");
    assert!(
        stub.state()
            .account_by_address(&AccountAddress([2u8; 32]))
            .is_err(),
        "Account is not expected to exist"
    );
}

/// Test lookup account index and account from index.
#[test]
fn test_account_lookup_index() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let account = stub.create_account();

    stub.state()
        .account_by_index(account)
        .expect("Account is expected to exist");
    assert!(
        stub.state()
            .account_by_index(AccountIndex::from(2u64))
            .is_err(),
        "Account is not expected to exist"
    );
}

/// Test get account token balance.
#[test]
fn test_account_balance() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id = "TokenId1".parse().unwrap();
    let (token, _gov_account) =
        stub.create_and_init_token(token_id, TokenInitTestParams::default().mintable(), 2, None);

    let account0 = stub.create_account();
    let account1 = stub.create_account();
    stub.increment_account_balance(account0, token, RawTokenAmount(245));

    assert_eq!(
        stub.state().account_token_balance(&account0, &token),
        RawTokenAmount(245)
    );
    assert_eq!(
        stub.state().account_token_balance(&account1, &token),
        RawTokenAmount(0)
    );
}
