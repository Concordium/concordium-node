//! Tests for token initialization via the scheduler.

use assert_matches::assert_matches;
use concordium_base::common::cbor;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, MetadataUrl, TokenAmount, TokenId, TokenModuleInitializationParameters,
    TokenModuleState,
};
use concordium_base::updates::{CreatePlt, UpdatePayload};
use plt_block_state::entity::entity_test_stub;
use plt_scheduler::TOKEN_MODULE_REF;
use plt_scheduler_types::types::execution::{ChainUpdateOutcome, FailureKind};
use plt_scheduler_types::types::tokens::RawTokenAmount;

use crate::utils::BlockStateLatest;

mod utils;

const NON_EXISTING_ACCOUNT: AccountAddress = AccountAddress([2u8; 32]);

/// In this example, the parameters are not a valid encoding.
#[test]
fn test_initialize_token_parameters_decode_failure() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let outcome = block_state
        .execute_chain_update(
            &mut context,
            UpdatePayload::CreatePlt(CreatePlt {
                token_id,
                token_module: TOKEN_MODULE_REF,
                decimals: 0,
                initialization_parameters: vec![].into(),
            }),
        )
        .unwrap();
    let failure_kind =
        assert_matches!(outcome, ChainUpdateOutcome::Failed(failure_kind) => failure_kind);
    assert_matches!(
        failure_kind,
        FailureKind::TokenInitializeFailure(err) => {
            assert!(err.contains("IO error"), "err: {}", err);
        }
    );
}

/// In this example, a parameter is missing from the required initialization parameters.
#[test]
fn test_initialize_token_parameters_missing() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let gov_account = context.external.create_account();
    let parameters = TokenModuleInitializationParameters {
        name: None,
        metadata: Some("https://plt.token".to_owned().into()),
        governance_account: Some(
            context
                .external
                .account_canonical_address(gov_account.account_index())
                .into(),
        ),
        allow_list: Some(true),
        deny_list: Some(false),
        initial_supply: None,
        mintable: Some(true),
        burnable: Some(true),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).into();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let outcome = block_state
        .execute_chain_update(
            &mut context,
            UpdatePayload::CreatePlt(CreatePlt {
                token_id,
                token_module: TOKEN_MODULE_REF,
                decimals: 0,
                initialization_parameters: encoded_parameters,
            }),
        )
        .unwrap();
    let failure_kind =
        assert_matches!(outcome, ChainUpdateOutcome::Failed(failure_kind) => failure_kind);
    assert_matches!(
        failure_kind,
        FailureKind::TokenInitializeFailure(err) if err.contains("Token name is missing")
    );
}

/// In this example, an unsupported additional parameter is present in the initialization
/// parameters.
#[test]
fn test_initialize_token_additional_parameter() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let gov_account = context.external.create_account();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some("https://plt.token".to_owned().into()),
        governance_account: Some(
            context
                .external
                .account_canonical_address(gov_account.account_index())
                .into(),
        ),
        allow_list: Some(true),
        deny_list: Some(false),
        initial_supply: None,
        mintable: Some(true),
        burnable: Some(true),
    };

    let mut dynamic_parameters: cbor::value::Value =
        cbor::cbor_decode(cbor::cbor_encode(&parameters)).unwrap();
    assert_matches!(&mut dynamic_parameters, cbor::value::Value::Map(map) => {
        map.push((cbor::value::Value::Text("additionalField".to_string()), cbor::value::Value::Text("testvalue1".to_string())));
    });

    let encoded_parameters = cbor::cbor_encode(&dynamic_parameters).into();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let outcome = block_state
        .execute_chain_update(
            &mut context,
            UpdatePayload::CreatePlt(CreatePlt {
                token_id,
                token_module: TOKEN_MODULE_REF,
                decimals: 0,
                initialization_parameters: encoded_parameters,
            }),
        )
        .unwrap();
    let failure_kind =
        assert_matches!(outcome, ChainUpdateOutcome::Failed(failure_kind) => failure_kind);
    assert_matches!(
        failure_kind,
        FailureKind::TokenInitializeFailure(err) => {
            assert!(err.contains("unknown map key"), "err: {}", err);
        }
    );
}

/// In this example, minimal parameters are specified to check defaulting behaviour.
#[test]
fn test_initialize_token_default_values() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let gov_account = context.external.create_account();
    let gov_holder_account = CborHolderAccount::from(
        context
            .external
            .account_canonical_address(gov_account.account_index()),
    );
    let metadata = MetadataUrl::from("https://plt.token".to_string());
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata.clone()),
        governance_account: Some(gov_holder_account.clone()),
        allow_list: None,
        deny_list: None,
        initial_supply: None,
        mintable: None,
        burnable: None,
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).into();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    block_state
        .execute_chain_update(
            &mut context,
            UpdatePayload::CreatePlt(CreatePlt {
                token_id: token_id.clone(),
                token_module: TOKEN_MODULE_REF,
                decimals: 0,
                initialization_parameters: encoded_parameters,
            }),
        )
        .unwrap();

    // Assertions using token module state query
    let token_info = block_state.query_token_info(&context, &token_id).unwrap().unwrap();
    let state: TokenModuleState = cbor::cbor_decode(&token_info.state.module_state).unwrap();
    assert_eq!(state.name, Some("Protocol-level token".to_owned()));
    assert_eq!(state.metadata, Some(metadata));
    assert_eq!(state.governance_account, Some(gov_holder_account));
    assert_eq!(state.allow_list, Some(false));
    assert_eq!(state.deny_list, Some(false));
    assert_eq!(state.mintable, Some(false));
    assert_eq!(state.burnable, Some(false));
    assert_eq!(state.paused, Some(false));

    // Assert governance account balance
    let token = block_state
        .token_by_id(&context, &token_id)
        .unwrap()
        .expect("created token");
    assert_eq!(
        gov_account.account_token_balance(&context, token.token_base.token_index()),
        RawTokenAmount(0)
    );
}

/// In this example, the parameters are valid, no minting.
#[test]
fn test_initialize_token_no_minting() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let gov_account = context.external.create_account();
    let gov_holder_account = CborHolderAccount::from(
        context
            .external
            .account_canonical_address(gov_account.account_index()),
    );
    let metadata = MetadataUrl::from("https://plt.token".to_string());
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata.clone()),
        governance_account: Some(gov_holder_account.clone()),
        allow_list: Some(true),
        deny_list: Some(false),
        initial_supply: None,
        mintable: Some(true),
        burnable: Some(true),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).into();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    block_state
        .execute_chain_update(
            &mut context,
            UpdatePayload::CreatePlt(CreatePlt {
                token_id: token_id.clone(),
                token_module: TOKEN_MODULE_REF,
                decimals: 0,
                initialization_parameters: encoded_parameters,
            }),
        )
        .unwrap();

    // Assertions using token module state query
    let token_info = block_state.query_token_info(&context, &token_id).unwrap().unwrap();
    let state: TokenModuleState = cbor::cbor_decode(&token_info.state.module_state).unwrap();
    assert_eq!(state.name, Some("Protocol-level token".to_owned()));
    assert_eq!(state.metadata, Some(metadata));
    assert_eq!(state.governance_account, Some(gov_holder_account));
    assert_eq!(state.allow_list, Some(true));
    assert_eq!(state.deny_list, Some(false));
    assert_eq!(state.mintable, Some(true));
    assert_eq!(state.burnable, Some(true));
    assert_eq!(state.paused, Some(false));

    // Assert governance account balance
    let token = block_state
        .token_by_id(&context, &token_id)
        .unwrap()
        .expect("created token");
    assert_eq!(
        gov_account.account_token_balance(&context, token.token_base.token_index()),
        RawTokenAmount(0)
    );
}

/// In this example, the parameters are valid, with minting.
#[test]
fn test_initialize_token_with_minting() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let gov_account = context.external.create_account();
    let gov_holder_account = CborHolderAccount::from(
        context
            .external
            .account_canonical_address(gov_account.account_index()),
    );
    let metadata = MetadataUrl::from("https://plt.token".to_string());
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some(metadata.clone()),
        governance_account: Some(gov_holder_account.clone()),
        allow_list: Some(false),
        deny_list: Some(true),
        initial_supply: Some(TokenAmount::from_raw(500000, 2)),
        mintable: Some(false),
        burnable: Some(false),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).into();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    block_state
        .execute_chain_update(
            &mut context,
            UpdatePayload::CreatePlt(CreatePlt {
                token_id: token_id.clone(),
                token_module: TOKEN_MODULE_REF,
                decimals: 2,
                initialization_parameters: encoded_parameters,
            }),
        )
        .unwrap();

    // Assertions using token module state query
    let token_info = block_state.query_token_info(&context, &token_id).unwrap().unwrap();
    let state: TokenModuleState = cbor::cbor_decode(&token_info.state.module_state).unwrap();
    assert_eq!(state.name, Some("Protocol-level token".to_owned()));
    assert_eq!(state.metadata, Some(metadata));
    assert_eq!(state.governance_account, Some(gov_holder_account));
    assert_eq!(state.allow_list, Some(false));
    assert_eq!(state.deny_list, Some(true));
    assert_eq!(state.mintable, Some(false));
    assert_eq!(state.burnable, Some(false));
    assert_eq!(state.paused, Some(false));

    // Assert governance account balance and circulating supply
    let token = block_state
        .token_by_id(&context, &token_id)
        .unwrap()
        .expect("created token");
    assert_eq!(
        gov_account.account_token_balance(&context, token.token_base.token_index()),
        RawTokenAmount(500000)
    );
    assert_eq!(
        token.token_base.token_circulating_supply(),
        RawTokenAmount(500000)
    );
}

/// In this example, the parameters specify an initial supply with higher precision
/// than the token allows.
#[test]
fn test_initialize_token_excessive_mint_decimals() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let gov_account = context.external.create_account();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some("https://plt.token".to_owned().into()),
        governance_account: Some(
            context
                .external
                .account_canonical_address(gov_account.account_index())
                .into(),
        ),
        allow_list: Some(false),
        deny_list: Some(false),
        initial_supply: Some(TokenAmount::from_raw(500000, 6)),
        mintable: Some(false),
        burnable: Some(false),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).into();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let outcome = block_state
        .execute_chain_update(
            &mut context,
            UpdatePayload::CreatePlt(CreatePlt {
                token_id,
                token_module: TOKEN_MODULE_REF,
                decimals: 2,
                initialization_parameters: encoded_parameters,
            }),
        )
        .unwrap();
    let failure_kind =
        assert_matches!(outcome, ChainUpdateOutcome::Failed(failure_kind) => failure_kind);
    assert_matches!(
        failure_kind,
        FailureKind::TokenInitializeFailure(err) => {
            assert!(err.contains("decimals mismatch"), "err: {}", err);
        }
    );
}

/// In this example, the parameters specify an initial supply with less precision
/// than the token allows.
#[test]
fn test_initialize_token_insufficient_mint_decimals() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let gov_account = context.external.create_account();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some("https://plt.token".to_owned().into()),
        governance_account: Some(
            context
                .external
                .account_canonical_address(gov_account.account_index())
                .into(),
        ),
        allow_list: Some(false),
        deny_list: Some(false),
        initial_supply: Some(TokenAmount::from_raw(500000, 2)),
        mintable: Some(false),
        burnable: Some(false),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).into();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let outcome = block_state
        .execute_chain_update(
            &mut context,
            UpdatePayload::CreatePlt(CreatePlt {
                token_id,
                token_module: TOKEN_MODULE_REF,
                decimals: 6,
                initialization_parameters: encoded_parameters,
            }),
        )
        .unwrap();
    let failure_kind =
        assert_matches!(outcome, ChainUpdateOutcome::Failed(failure_kind) => failure_kind);
    assert_matches!(
        failure_kind,
        FailureKind::TokenInitializeFailure(err) => {
            assert!(err.contains("decimals mismatch"), "err: {}", err);
        }
    );
}

/// In this example, the parameters specify a non-existing governance account.
#[test]
fn test_initialize_token_non_existing_governance_account() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();
    let parameters = TokenModuleInitializationParameters {
        name: Some("Protocol-level token".to_owned()),
        metadata: Some("https://plt.token".to_owned().into()),
        governance_account: Some(NON_EXISTING_ACCOUNT.into()),
        allow_list: Some(false),
        deny_list: Some(false),
        initial_supply: Some(TokenAmount::from_raw(500000, 2)),
        mintable: Some(false),
        burnable: Some(false),
    };
    let encoded_parameters = cbor::cbor_encode(&parameters).into();
    let token_id: TokenId = "TokenId1".parse().unwrap();
    let outcome = block_state
        .execute_chain_update(
            &mut context,
            UpdatePayload::CreatePlt(CreatePlt {
                token_id,
                token_module: TOKEN_MODULE_REF,
                decimals: 0,
                initialization_parameters: encoded_parameters,
            }),
        )
        .unwrap();
    let failure_kind =
        assert_matches!(outcome, ChainUpdateOutcome::Failed(failure_kind) => failure_kind);
    assert_matches!(
        failure_kind,
        FailureKind::TokenInitializeFailure(err) => {
            assert!(err.contains("does not exist"), "err: {}", err);
        }
    );
}
