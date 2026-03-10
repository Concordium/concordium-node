//! Test of protocol-level token queries. Notice that detailed test of the token module queries are
//! implemented in the `plt-token-module` crate.

use assert_matches::assert_matches;
use block_state_external_stubbed::{
    BlobStoreLoadStub, BlockStateTestImpl, ExternalBlockStateStub, TokenInitTestParams,
};
use concordium_base::base::Energy;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, RawCbor, TokenId, TokenListUpdateDetails, TokenModuleAccountState,
    TokenModuleState, TokenOperation, TokenOperationsPayload,
};
use concordium_base::transactions::Payload;
use plt_block_state::block_state::p10;
use plt_scheduler_interface::transaction_execution_interface::TransactionExecution;
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use plt_token_module::TOKEN_MODULE_REF;

mod block_state_external_stubbed;

/// Test query token state
#[test]
fn test_query_plt_list() {
    test_query_plt_list_worker::<p10::PltBlockStateP10>();
}

fn test_query_plt_list_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();

    let token_id1: TokenId = "TokenId1".parse().unwrap();
    let _ = block_state.create_and_init_token(
        &mut external,
        token_id1.clone(),
        TokenInitTestParams::default(),
        4,
        None,
    );
    let token_id2: TokenId = "TokenId2".parse().unwrap();
    let _ = block_state.create_and_init_token(
        &mut external,
        token_id2.clone(),
        TokenInitTestParams::default(),
        4,
        None,
    );

    let plts = block_state.query_plt_list();
    assert_eq!(plts, vec![token_id1, token_id2]);
}

/// Test query token info
#[test]
fn test_query_token_info() {
    test_query_token_info_worker::<p10::PltBlockStateP10>();
}

fn test_query_token_info_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();

    let token_id: TokenId = "TokenId1".parse().unwrap();
    let _ = block_state.create_and_init_token(
        &mut external,
        token_id.clone(),
        TokenInitTestParams::default(),
        4,
        None,
    );

    let non_canonical_token_id = "toKeniD1".parse().unwrap();
    // Lookup by token id that is not in canonical casing
    let token_info = block_state
        .query_token_info(&external, &non_canonical_token_id)
        .unwrap();
    // Assert that the token id returned is in the canonical casing
    assert_eq!(token_info.token_id, token_id);
    assert_eq!(token_info.state.decimals, 4);
    assert_eq!(token_info.state.total_supply.amount, RawTokenAmount(0));
    assert_eq!(token_info.state.total_supply.decimals, 4);
    assert_eq!(token_info.state.token_module_ref, TOKEN_MODULE_REF);
    let token_module_state: TokenModuleState =
        cbor::cbor_decode(&token_info.state.module_state).unwrap();
    assert_eq!(
        token_module_state.name.as_deref(),
        Some("Protocol-level token")
    );
}

/// Test query token account info
#[test]
fn test_query_token_account_info() {
    test_query_token_account_info_worker::<p10::PltBlockStateP10>();
}

fn test_query_token_account_info_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;

    let account = external.create_account();
    let token_id1: TokenId = "TokenId1".parse().unwrap();
    let _ = block_state.create_and_init_token(
        &mut external,
        token_id1.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    let token_id2: TokenId = "TokenId2".parse().unwrap();
    let _ = block_state.create_and_init_token(
        &mut external,
        token_id2.clone(),
        TokenInitTestParams::default().mintable(),
        4,
        None,
    );
    let token_id3 = "TokenId3".parse().unwrap();
    let _ = block_state.create_and_init_token(
        &mut external,
        token_id3,
        TokenInitTestParams::default(),
        4,
        None,
    );

    block_state.increment_account_balance(
        &mut external,
        &mut blob_store,
        account,
        &token_id1,
        RawTokenAmount(1000),
    );
    block_state.increment_account_balance(
        &mut external,
        &mut blob_store,
        account,
        &token_id2,
        RawTokenAmount(2000),
    );

    // Lookup account token infos
    let token_account_infos = block_state.query_token_account_infos(&external, account);
    assert_eq!(token_account_infos.len(), 2);
    assert_eq!(token_account_infos[0].token_id, token_id1);
    assert_eq!(
        token_account_infos[0].account_state.balance.amount,
        RawTokenAmount(1000)
    );
    assert_eq!(token_account_infos[0].account_state.balance.decimals, 4);
    assert_eq!(token_account_infos[1].token_id, token_id2);
    assert_eq!(
        token_account_infos[1].account_state.balance.amount,
        RawTokenAmount(2000)
    );
    assert_eq!(token_account_infos[1].account_state.balance.decimals, 4);
}

// Test that adding an account to a token list properly touches the account
#[test]
fn test_query_token_account_info_allow_list_no_balance() {
    test_query_token_account_info_allow_list_no_balance_worker::<p10::PltBlockStateP10>()
}

fn test_query_token_account_info_allow_list_no_balance_worker<BlockState>()
where
    BlockState: BlockStateTestImpl,
{
    let mut block_state = BlockState::empty();
    let mut external = ExternalBlockStateStub::empty();
    let mut blob_store = BlobStoreLoadStub;

    let account = external.create_account();
    let token_id: TokenId = "TokenId3".parse().unwrap();
    let gov_account = block_state.create_and_init_token(
        &mut external,
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        4,
        None,
    );

    let operations = vec![TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(external.account_canonical_address(&account)),
    })];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let mut execution = TransactionExecution::new(
        Energy::from(u64::MAX),
        gov_account,
        external.account_canonical_address(&gov_account),
    );
    let result = block_state
        .execute_transaction(
            &mut execution,
            &mut blob_store,
            &mut external,
            Payload::TokenUpdate { payload },
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));

    let token_account_infos = block_state.query_token_account_infos(&external, account);
    assert_eq!(token_account_infos.len(), 1);
    assert_eq!(token_account_infos[0].token_id, token_id);
    assert_eq!(
        token_account_infos[0].account_state.balance.amount,
        RawTokenAmount(0)
    );
    assert_eq!(token_account_infos[0].account_state.balance.decimals, 4);
    let module_state: TokenModuleAccountState = cbor::cbor_decode(
        token_account_infos[0]
            .account_state
            .module_state
            .as_ref()
            .unwrap(),
    )
    .unwrap();
    assert_eq!(module_state.allow_list, Some(true));
    assert_eq!(module_state.deny_list, None);
}
