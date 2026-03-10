use concordium_base::base::ProtocolVersion;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::TokenModuleAccountState;
use plt_scheduler_interface::token_kernel_interface::TokenKernelQueries;
use plt_token_module::token_module;
use utils::kernel_stub::{KernelStub, TokenInitTestParams};

mod utils;

/// Default protocol version used across the tests.
const PROTOCOL_VERSION: ProtocolVersion = utils::LATEST_PROTOCOL_VERSION;

/// Test token module account state without lists enabled.
#[test]
fn test_query_token_module_account_state_default() {
    let mut stub = KernelStub::with_decimals(0, PROTOCOL_VERSION);
    stub.init_token(TokenInitTestParams::default());
    let account = stub.create_account();

    let cbor = token_module::query_token_module_account_state(&stub, stub.account_index(&account));
    let state: TokenModuleAccountState = cbor::cbor_decode(cbor).unwrap();

    assert_eq!(state.allow_list, None);
    assert_eq!(state.deny_list, None);
}

/// Test token module account state with lists.
#[test]
fn test_query_token_module_account_state_lists() {
    let mut stub = KernelStub::with_decimals(0, PROTOCOL_VERSION);
    stub.init_token(TokenInitTestParams::default().allow_list().deny_list());
    let account = stub.create_account();

    let cbor = token_module::query_token_module_account_state(&stub, stub.account_index(&account));
    let state: TokenModuleAccountState = cbor::cbor_decode(cbor).unwrap();

    assert_eq!(state.allow_list, Some(false));
    assert_eq!(state.deny_list, Some(false));
}
