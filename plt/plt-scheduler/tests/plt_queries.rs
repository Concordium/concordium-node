//! Test of protocol-level token queries

use crate::block_state_stub::{BlockStateStub, TokenInitTestParams};
use concordium_base::contracts_common::AccountAddress;
use plt_token_module::token_kernel_interface::{RawTokenAmount, TokenKernelQueries};

mod block_state_stub;
mod utils;

const NON_EXISTING_ACCOUNT: AccountAddress = AccountAddress([2u8; 32]);

/// Test query token state
#[test]
fn test_query_token_stae() {
    let mut stub = BlockStateStub::new();
    // stub.init_token(TokenInitTestParams::default());
    //
    // assert_eq!(stub.account_token_balance(&sender), RawTokenAmount(4000));
    // assert_eq!(stub.account_token_balance(&receiver), RawTokenAmount(3000));
}
