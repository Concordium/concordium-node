//! Tests for creating a PLT lock.

use crate::utils::BlockStateLatest;
use crate::utils::entity_traits::scheduler::SchedulerOperations;
use assert_matches::assert_matches;
use concordium_base::{
    base::Energy,
    common::{cbor, types::TransactionTime},
    protocol_level_locks::{
        LockConfig, LockController, LockControllerSimpleV0, LockControllerSimpleV0Capability,
        LockControllerSimpleV0Grant, LockId,
    },
    protocol_level_tokens::{
        MetadataUrl, RawCbor, TokenAmount, TokenId, TokenModuleInitializationParameters,
        meta_operations::{MetaUpdatePayload, lock_create},
    },
    transactions::Payload,
    updates::{CreatePlt, UpdatePayload},
};
use plt_block_state::entity::entity_test_stub;
use plt_scheduler::TOKEN_MODULE_REF;
use plt_scheduler_types::types::events::{BlockItemEvent, LockCreateEvent};

mod utils;

#[test]
fn test_create_simple_lock() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut block_state = BlockStateLatest::default();

    let account_index_1 = context.external.create_account().account_index();
    let account_1 = context.external.account_canonical_address(account_index_1);

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
    block_state
        .execute_chain_update(&mut context, payload)
        .expect("create pltX");

    let config = LockConfig {
        recipients: vec![account_1.into()],
        expiry: TransactionTime::from_seconds(1000),
        controller: LockController::SimpleV0(LockControllerSimpleV0 {
            grants: vec![LockControllerSimpleV0Grant {
                account: account_1.into(),
                roles: vec![
                    LockControllerSimpleV0Capability::Fund,
                    LockControllerSimpleV0Capability::Send,
                ],
            }],
            tokens: vec![plt_x],
            keep_alive: false,
            memo: None,
        }),
    };
    let operations = vec![lock_create(config.clone())];
    let payload = MetaUpdatePayload {
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let result = block_state
        .execute_transaction(
            &mut context,
            account_index_1,
            account_1,
            1.into(),
            Payload::MetaUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    let events = assert_matches!(result.outcome, plt_scheduler_types::types::execution::TransactionOutcome::Success(events) => events);
    assert_eq!(events.len(), 1);
    assert_eq!(
        events[0],
        BlockItemEvent::LockCreated(LockCreateEvent {
            lock_id: LockId::new(account_index_1, 1, 0),
            lock_config: RawCbor::from(cbor::cbor_encode(&config))
        })
    );
}
