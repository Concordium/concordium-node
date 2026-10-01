//! Tests for the unscoped Token Update transaction execution logic.

use std::str::FromStr;

use assert_matches::assert_matches;
use concordium_base::base::Energy;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborMemo, MetadataUrl, OperationsPayload, RawCbor, TokenAmount, TokenId,
    TokenListUpdateEventDetails, TokenModuleInitializationParameters, TokenPauseDetails,
    TokenPauseEventDetails, operations,
};
use concordium_base::transactions::{Payload, TokenUpdatePayload};
use concordium_base::updates::{CreatePlt, UpdatePayload};
use plt_block_state::entity::accounts::Account;
use plt_block_state::entity::entity_test_stub;
use plt_block_state::entity::entity_test_stub::StubbedEntityContext;
use plt_scheduler::TOKEN_MODULE_REF;
use plt_scheduler_types::types::events::{
    self, BlockItemEvent, EncodedTokenModuleEvent, TokenBurnEvent, TokenTransferEvent,
};
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;
use plt_scheduler_types::types::tokens::{self, TokenHolder};

use crate::utils::BlockStateLatest;
use crate::utils::entity_traits::scheduler::SchedulerOperations;

mod utils;

const PLT_X: &str = "pltX";
const PLT_Y: &str = "pltY";

fn setup_test_plts(
    context: &mut StubbedEntityContext,
    block_state: &mut BlockStateLatest,
    account_1: &Account,
) {
    let account_1_addr = context
        .external
        .account_canonical_address(account_1.account_index());

    // Set up PLT `pltX`.
    let plt_x: TokenId = PLT_X.parse().unwrap();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Test PLT 1".to_owned()),
        metadata: Some(MetadataUrl::from("https://pltX.token".to_string())),
        governance_account: Some(account_1_addr.into()),
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
    block_state
        .execute_chain_update(context, payload)
        .expect("create pltX");

    // Set up PLT `pltY`.
    let plt_y: TokenId = PLT_Y.parse().unwrap();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Test PLT 2".to_owned()),
        metadata: Some(MetadataUrl::from("https://pltY.token".to_string())),
        governance_account: Some(account_1_addr.into()),
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
    block_state
        .execute_chain_update(context, payload)
        .expect("create pltY");
}

#[test]
fn test_unscoped_transaction() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    // Set up initial accounts.
    let account_1 = context.external.create_account();
    let account_2 = context.external.create_account();
    let account_1_addr = context
        .external
        .account_canonical_address(account_1.account_index());
    let account_2_addr = context
        .external
        .account_canonical_address(account_2.account_index());
    setup_test_plts(&mut context, &mut block_state, &account_1);
    let plt_x: TokenId = PLT_X.parse().unwrap();
    let plt_y: TokenId = PLT_Y.parse().unwrap();

    let operations = vec![
        operations::transfer_tokens(plt_x.clone(), account_2_addr, TokenAmount::from_raw(100, 2)),
        operations::mint_tokens(plt_y.clone(), TokenAmount::from_raw(100000, 0)),
        operations::pause_token(plt_x.clone()),
        operations::add_token_allow_list(plt_y.clone(), account_2_addr),
        operations::add_token_deny_list(plt_y.clone(), account_1_addr),
        operations::add_token_allow_list(plt_y.clone(), account_1_addr),
        operations::remove_token_deny_list(plt_y.clone(), account_1_addr),
        operations::transfer_tokens_with_memo(
            plt_y.clone(),
            account_2_addr,
            TokenAmount::from_raw(2200, 0),
            CborMemo::Cbor(vec![0xa0u8].try_into().unwrap()),
        ),
        operations::unpause_token(plt_x.clone()),
        operations::burn_tokens(plt_x.clone(), TokenAmount::from_raw(10, 2)),
        operations::remove_token_allow_list(plt_y.clone(), account_1_addr),
    ];

    let payload = OperationsPayload {
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let result = block_state
        .execute_transaction(
            &mut context,
            plt_scheduler::TransactionContext {
                energy_limit: Energy::from(u64::MAX),
                sender_account_address: account_1_addr,
                transaction_sequence_number: 1.into(),
                block_timestamp: 0.into(),
            },
            account_1.account_index(),
            Payload::TokenUpdate {
                payload: TokenUpdatePayload::Unscoped(payload),
            },
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, TransactionOutcome::Success(events) => events);
    assert_eq!(events.len(), 11);
    assert_eq!(
        events[0],
        BlockItemEvent::TokenTransfer(TokenTransferEvent {
            token_id: plt_x.clone(),
            from: TokenHolder::Account(account_1_addr),
            to: TokenHolder::Account(account_2_addr),
            amount: tokens::TokenAmount::from_raw(100, 2),
            memo: None,
        })
    );
    assert_eq!(
        events[1],
        BlockItemEvent::TokenMint(events::TokenMintEvent {
            token_id: plt_y.clone(),
            target: TokenHolder::Account(account_1_addr),
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
                target: account_2_addr.into(),
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
                target: account_1_addr.into(),
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
                target: account_1_addr.into(),
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
                target: account_1_addr.into(),
            })
            .into(),
        })
    );
    assert_eq!(
        events[7],
        BlockItemEvent::TokenTransfer(TokenTransferEvent {
            token_id: plt_y.clone(),
            from: TokenHolder::Account(account_1_addr),
            to: TokenHolder::Account(account_2_addr),
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
            target: TokenHolder::Account(account_1_addr),
            amount: tokens::TokenAmount::from_raw(10, 2),
        })
    );
    assert_eq!(
        events[10],
        BlockItemEvent::TokenModule(EncodedTokenModuleEvent {
            token_id: plt_y.clone(),
            event_type: "removeAllowList".to_string().try_into().unwrap(),
            details: cbor::cbor_encode(&TokenListUpdateEventDetails {
                target: account_1_addr.into(),
            })
            .into(),
        })
    );
}

#[test]
fn test_unscoped_transaction_cbor_extra_fields() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    // Set up initial account.
    let account_1 = context.external.create_account();
    let account_1_addr = context
        .external
        .account_canonical_address(account_1.account_index());
    let payload = OperationsPayload {
        operations: RawCbor::from_str("81a16d746f6b656e5472616e73666572a5646d656d6f440102030465746f6b656e68746f6b656e69643166616d6f756e74c482211a000186a069726563697069656e74d99d73a201d99d71a1011903970358200102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f2064626c616801").unwrap(),
    };
    payload
        .decode_operations()
        .expect("should decode successfully even with extra fields in the CBOR");
    let result = block_state
        .execute_transaction(
            &mut context,
            plt_scheduler::TransactionContext {
                energy_limit: Energy::from(u64::MAX),
                sender_account_address: account_1_addr,
                transaction_sequence_number: 1.into(),
                block_timestamp: 0.into(),
            },
            account_1.account_index(),
            Payload::TokenUpdate {
                payload: TokenUpdatePayload::Unscoped(payload),
            },
        )
        .expect("transaction internal error");
    assert_matches!(
        result.outcome,
        TransactionOutcome::Rejected(TransactionRejectReason::SerializationFailure)
    );
}
