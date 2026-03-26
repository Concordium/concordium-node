//! Tests for token module account state queries via the scheduler.

use crate::block_state_external_stubbed::{
    BlockStateWithExternalStateStubbed, TokenInitTestParams,
};
use assert_matches::assert_matches;
use concordium_base::base::Energy;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, RawCbor, TokenId, TokenListUpdateDetails, TokenModuleAccountState,
    TokenOperation, TokenOperationsPayload,
};
use concordium_base::transactions::Payload;
use plt_scheduler::{queries, scheduler};
use plt_scheduler_types::types::execution::TransactionOutcome;

mod block_state_external_stubbed;
mod utils;

/// Test token module account state without lists enabled.
#[test]
fn test_query_token_module_account_state_default() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, _gov_account) =
        stub.create_and_init_token(token_id, TokenInitTestParams::default(), 0, None);
    let account = stub.create_account();

    let token_account_infos = queries::query_token_account_infos(stub.state(), account);
    // Account has no balance and no list entries, so it does not appear in the infos
    assert!(token_account_infos.is_empty());
}

/// Test token module account state with lists.
#[test]
fn test_query_token_module_account_state_lists() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let (_token, gov_account) = stub.create_and_init_token(
        token_id.clone(),
        TokenInitTestParams::default().allow_list().deny_list(),
        0,
        None,
    );
    let account = stub.create_account();

    // Touch the account by adding it to the allow list
    let operations = vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(stub.account_canonical_address(&account)),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        gov_account,
        stub.account_canonical_address(&gov_account),
        stub.state_mut(),
        Payload::TokenUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let token_account_infos = queries::query_token_account_infos(stub.state(), account);
    assert_eq!(token_account_infos.len(), 1);
    let module_state: TokenModuleAccountState = cbor::cbor_decode(
        token_account_infos[0]
            .account_state
            .module_state
            .as_ref()
            .unwrap(),
    )
    .unwrap();
    assert_eq!(module_state.allow_list, Some(true));
    assert_eq!(module_state.deny_list, Some(false));
}
