//! Tests for the meta-update transaction execution logic.

use assert_matches::assert_matches;
use concordium_base::base::Energy;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborMemo, MetadataUrl, RawCbor, TokenAmount, TokenId, TokenListUpdateEventDetails,
    TokenModuleInitializationParameters, TokenPauseDetails, TokenPauseEventDetails,
    meta_operations,
};
use concordium_base::transactions::Payload;
use concordium_base::updates::{CreatePlt, UpdatePayload};
use plt_scheduler::{TOKEN_MODULE_REF, scheduler};
use plt_scheduler_types::types::events::{
    self, BlockItemEvent, EncodedTokenModuleEvent, TokenBurnEvent, TokenTransferEvent,
};
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::tokens::{self, TokenHolder};
use utils::block_state_external_stubbed::BlockStateWithExternalStateStubbed;

mod utils;

#[test]
fn test_meta_update_transaction() {
    let mut stub = BlockStateWithExternalStateStubbed::new(utils::LATEST_PROTOCOL_VERSION);

    // Set up initial accounts.
    let account_index_1 = stub.create_account();
    let account_index_2 = stub.create_account();
    let account_1 = stub.account_canonical_address(account_index_1);
    let account_2 = stub.account_canonical_address(account_index_2);

    // Set up PLT `pltX`.
    let plt_x: TokenId = "pltX".parse().unwrap();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Test PLT 1".to_owned()),
        metadata: Some(MetadataUrl::from("https://pltX.token".to_string())),
        governance_account: Some(account_1.into()),
        allow_list: None,
        deny_list: None,
        initial_supply: Some(TokenAmount::from_raw(10000, 2)),
        mintable: Some(true),
        burnable: Some(true),
    };
    let initialization_parameters = cbor::cbor_encode(&parameters).into();
    let payload = UpdatePayload::CreatePlt(CreatePlt {
        token_id: plt_x.clone(),
        token_module: TOKEN_MODULE_REF,
        decimals: 2,
        initialization_parameters,
    });
    scheduler::execute_chain_update(stub.state_mut(), payload).expect("create pltX");

    // Set up PLT `pltY`.
    let plt_y: TokenId = "pltY".parse().unwrap();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Test PLT 2".to_owned()),
        metadata: Some(MetadataUrl::from("https://pltY.token".to_string())),
        governance_account: Some(account_1.into()),
        allow_list: Some(true),
        deny_list: Some(true),
        initial_supply: None,
        mintable: Some(true),
        burnable: Some(true),
    };
    let initialization_parameters = cbor::cbor_encode(&parameters).into();
    let payload = UpdatePayload::CreatePlt(CreatePlt {
        token_id: plt_y.clone(),
        token_module: TOKEN_MODULE_REF,
        decimals: 0,
        initialization_parameters,
    });
    scheduler::execute_chain_update(stub.state_mut(), payload).expect("create pltY");

    use meta_operations::*;
    let operations = vec![
        transfer_tokens(plt_x.clone(), account_2, TokenAmount::from_raw(100, 2)),
        mint_tokens(plt_y.clone(), TokenAmount::from_raw(100000, 0)),
        pause(plt_x.clone()),
        add_token_allow_list(plt_y.clone(), account_2),
        add_token_deny_list(plt_y.clone(), account_1),
        add_token_allow_list(plt_y.clone(), account_1),
        remove_token_deny_list(plt_y.clone(), account_1),
        transfer_tokens_with_memo(
            plt_y.clone(),
            account_2,
            TokenAmount::from_raw(2200, 0),
            CborMemo::Cbor(vec![0xa0u8].try_into().unwrap()),
        ),
        unpause(plt_x.clone()),
        burn_tokens(plt_x.clone(), TokenAmount::from_raw(10, 2)),
        remove_token_allow_list(plt_y.clone(), account_1),
    ];

    let payload = MetaUpdatePayload {
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = scheduler::execute_transaction(
        account_index_1,
        account_1,
        stub.state_mut(),
        Payload::MetaUpdate { payload },
        Energy::from(u64::MAX),
    )
    .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);
    assert_eq!(events.len(), 11);
    assert_eq!(
        events[0],
        BlockItemEvent::TokenTransfer(TokenTransferEvent {
            token_id: plt_x.clone(),
            from: TokenHolder::Account(account_1),
            to: TokenHolder::Account(account_2),
            amount: tokens::TokenAmount::from_raw(100, 2),
            memo: None,
        })
    );
    assert_eq!(
        events[1],
        BlockItemEvent::TokenMint(events::TokenMintEvent {
            token_id: plt_y.clone(),
            target: TokenHolder::Account(account_1),
            amount: tokens::TokenAmount::from_raw(100000, 0),
        })
    );
    assert_eq!(
        events[2],
        BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
            token_id: plt_x.clone(),
            event_type: "pause".to_string().try_into().unwrap(),
            details: cbor::cbor_encode(&TokenPauseDetails {}).into(),
        })
    );
    assert_eq!(
        events[3],
        BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
            token_id: plt_y.clone(),
            event_type: "addAllowList".to_string().try_into().unwrap(),
            details: cbor::cbor_encode(&TokenListUpdateEventDetails {
                target: account_2.into(),
            })
            .into(),
        })
    );
    assert_eq!(
        events[4],
        BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
            token_id: plt_y.clone(),
            event_type: "addDenyList".to_string().try_into().unwrap(),
            details: cbor::cbor_encode(&TokenListUpdateEventDetails {
                target: account_1.into(),
            })
            .into(),
        })
    );
    assert_eq!(
        events[5],
        BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
            token_id: plt_y.clone(),
            event_type: "addAllowList".to_string().try_into().unwrap(),
            details: cbor::cbor_encode(&TokenListUpdateEventDetails {
                target: account_1.into(),
            })
            .into(),
        })
    );
    assert_eq!(
        events[6],
        BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
            token_id: plt_y.clone(),
            event_type: "removeDenyList".to_string().try_into().unwrap(),
            details: cbor::cbor_encode(&TokenListUpdateEventDetails {
                target: account_1.into(),
            })
            .into(),
        })
    );
    assert_eq!(
        events[7],
        BlockItemEvent::TokenTransfer(TokenTransferEvent {
            token_id: plt_y.clone(),
            from: TokenHolder::Account(account_1),
            to: TokenHolder::Account(account_2),
            amount: tokens::TokenAmount::from_raw(2200, 0),
            memo: Some(vec![0xa0u8].try_into().unwrap()),
        })
    );
    assert_eq!(
        events[8],
        BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
            token_id: plt_x.clone(),
            event_type: "unpause".to_string().try_into().unwrap(),
            details: cbor::cbor_encode(&TokenPauseEventDetails {}).into(),
        })
    );
    assert_eq!(
        events[9],
        BlockItemEvent::TokenBurn(TokenBurnEvent {
            token_id: plt_x.clone(),
            target: TokenHolder::Account(account_1),
            amount: tokens::TokenAmount::from_raw(10, 2),
        })
    );
    assert_eq!(
        events[10],
        BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
            token_id: plt_y.clone(),
            event_type: "removeAllowList".to_string().try_into().unwrap(),
            details: cbor::cbor_encode(&TokenListUpdateEventDetails {
                target: account_1.into(),
            })
            .into(),
        })
    );
}
