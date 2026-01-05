//! Test of protocol-level token queries

use crate::block_state_stub::TokenInitTestParams;
use concordium_base::common::cbor;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, RawCbor, TokenAmount, TokenOperation, TokenTransfer,
};
use plt_token_module::token_kernel_interface::{RawTokenAmount, TokenKernelQueries};
use plt_token_module::token_module::{self, TransactionContext};

mod block_state_stub;
mod utils;

const NON_EXISTING_ACCOUNT: AccountAddress = AccountAddress([2u8; 32]);

/// Test successful transfer.
#[test]
fn test_query_token_info() {
    let mut stub = KernelStub::new(2);
    stub.init_token(TokenInitTestParams::default());
    let sender = stub.create_account();
    let receiver = stub.create_account();
    stub.set_account_balance(sender, RawTokenAmount(5000));
    stub.set_account_balance(receiver, RawTokenAmount(2000));

    let context = TransactionContext {
        sender,
        sender_address: stub.account_canonical_address(&sender),
    };
    let operations = vec![TokenOperation::Transfer(TokenTransfer {
        amount: TokenAmount::from_raw(1000, 2),
        recipient: CborHolderAccount::from(stub.account_canonical_address(&receiver)),
        memo: None,
    })];
    token_module::execute_token_update_transaction(
        &mut stub,
        context,
        RawCbor::from(cbor::cbor_encode(&operations)),
    )
    .expect("execute");

    assert_eq!(stub.account_token_balance(&sender), RawTokenAmount(4000));
    assert_eq!(stub.account_token_balance(&receiver), RawTokenAmount(3000));
    let transfer = stub.pop_transfer().expect("transfer");
    assert_eq!(transfer.3, None);
}
