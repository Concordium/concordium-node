use concordium_base::base::ProtocolVersion;
use concordium_base::contracts_common::AccountAddress;
use plt_scheduler_interface::token_kernel_interface::TokenKernelQueries;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use utils::kernel_stub::*;

mod utils;

// Tests for the kernel stub

const TEST_ACCOUNT2: AccountAddress = AccountAddress([2u8; 32]);

/// Test lookup account address and account from address
#[test]
fn test_account_lookup_address() {
    let mut stub = KernelStub::with_decimals(0, utils::LATEST_PROTOCOL_VERSION);
    let account = stub.create_account();

    let address = stub.account_address(&account);
    stub.account_by_address(&address)
        .expect("Account is expected to exist");
    assert!(
        stub.account_by_address(&TEST_ACCOUNT2).is_err(),
        "Account is not expected to exist"
    );
}

/// Test lookup account index and account from index
#[test]
fn test_account_lookup_index() {
    let mut stub = KernelStub::with_decimals(0, utils::LATEST_PROTOCOL_VERSION);
    let account = stub.create_account();

    let index = stub.account_index(&account);
    stub.account_by_index(index)
        .expect("Account is expected to exist");
    assert!(
        stub.account_by_index(2.into()).is_err(),
        "Account is not expected to exist"
    );
}

/// Test get account balance
#[test]
fn test_account_balance() {
    let mut stub = KernelStub::with_decimals(0, utils::LATEST_PROTOCOL_VERSION);
    let account0 = stub.create_account();
    let account1 = stub.create_account();
    stub.set_account_balance(account0, RawTokenAmount(245));

    let balance = stub.account_token_balance(&account0);
    assert_eq!(balance, RawTokenAmount(245));

    let balance = stub.account_token_balance(&account1);
    assert_eq!(balance, RawTokenAmount(0));
}

/// Test looking up account by alias.
#[test]
fn test_account_by_alias() {
    let mut stub = KernelStub::with_decimals(0, utils::LATEST_PROTOCOL_VERSION);

    let account = stub.create_account();
    let account_address = stub.account_address(&account);
    let account_by_alias = stub
        .account_by_address(&account_address.get_alias(0).unwrap())
        .unwrap();

    assert_eq!(
        stub.account_index(&account),
        stub.account_index(&account_by_alias)
    );
}
