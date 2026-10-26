//! Operation-only charging tests: no transaction rejection or rollback accounting.
use super::*;
use crate::TransactionContext;
use crate::failure::WithBlockStateFailure;
use concordium_base::base::Energy;
use concordium_base::common::types::TransactionTime;
use concordium_base::protocol_level_locks::{
    LockConfig as CborConfig, LockConfigSimpleV0, LockRecipients,
};
use plt_block_state::entity::{accounts::Account, entity_test_stub};

#[test]
fn create_charges_before_expiry_validation() {
    let mut context = entity_test_stub::new_stubbed_context();
    let sender = context.external.create_account().account_index();
    let mut execution = TransactionExecution::new(
        TransactionContext {
            energy_limit: Energy::from(u64::MAX),
            sender_account_address: context.external.account_canonical_address(sender),
            transaction_sequence_number: 1.into(),
            block_timestamp: 1000.into(),
        },
        Account::from_existing_account(sender),
    );
    let result = execute_lock_create(
        &mut context,
        &mut execution,
        &mut BlockStateP11::default(),
        Duration::from_millis(1000),
        LockCreateDetails {
            config: CborConfig::SimpleV0(LockConfigSimpleV0 {
                recipients: LockRecipients::Any,
                expiry: TransactionTime::from_seconds(0),
                grants: vec![],
                tokens: vec![],
                keep_alive: false,
                memo: None,
                metadata: None,
            }),
        },
        &mut vec![],
    );
    assert!(matches!(
        result,
        Err(WithBlockStateFailure::Error(
            TransactionRejectReason::LockExpired(_)
        ))
    ));
    assert_eq!(execution.energy_used(), Energy::from(100));
}

#[test]
fn fixed_operations_charge_before_lock_lookup() {
    let mut context = entity_test_stub::new_stubbed_context();
    let sender = context.external.create_account().account_index();
    let address = context.external.account_canonical_address(sender);
    let lock = LockId::new(sender, 1, 0);
    let token: TokenId = "Token".parse().unwrap();
    let amount = TokenAmount::from_raw(1, 0);
    let operations = [
        LockOperation::Fund(LockFundDetails {
            token: token.clone(),
            lock: lock.clone(),
            amount,
            memo: None,
        }),
        LockOperation::Send(LockSendDetails {
            token: token.clone(),
            lock: lock.clone(),
            source: address.into(),
            recipient: address.into(),
            amount,
            memo: None,
        }),
        LockOperation::Release(LockReleaseDetails {
            token,
            lock,
            source: address.into(),
            amount,
            memo: None,
        }),
    ];
    for operation in operations {
        let mut execution = TransactionExecution::new(
            TransactionContext {
                energy_limit: Energy::from(u64::MAX),
                sender_account_address: address,
                transaction_sequence_number: 1.into(),
                block_timestamp: 0.into(),
            },
            Account::from_existing_account(sender),
        );
        let result = execute_lock_operation(
            &mut context,
            &mut execution,
            &mut BlockStateP11::default(),
            Duration::from_millis(1000),
            0,
            operation,
            &mut vec![],
        );
        assert!(matches!(
            result,
            Err(WithBlockStateFailure::Error(
                TransactionRejectReason::NonExistentLockId(_)
            ))
        ));
        assert_eq!(execution.energy_used(), Energy::from(250));
    }
}

#[test]
fn cancel_charges_current_count_before_authorization() {
    let mut context = entity_test_stub::new_stubbed_context();
    let sender = context.external.create_account().account_index();
    let address = context.external.account_canonical_address(sender);
    let lock_id = LockId::new(sender, 1, 0);
    // Missing lock, then existing locks with zero, one, and three references.
    for count in [None, Some(0), Some(1), Some(3)] {
        let mut state = BlockStateP11::default();
        if let Some(count) = count {
            let config = crate::protocol_level_locks::lock_configuration::from_cbor_config(
                &context,
                &state,
                CborConfig::SimpleV0(LockConfigSimpleV0 {
                    recipients: LockRecipients::Any,
                    expiry: TransactionTime::from_seconds(1000),
                    grants: vec![],
                    tokens: vec![],
                    keep_alive: false,
                    memo: None,
                    metadata: None,
                }),
            )
            .unwrap();
            state.create_lock(&context, &lock_id, config).unwrap();
            let mut lock = state.lock_by_id(&context, &lock_id).unwrap().unwrap();
            for index in 0..count {
                lock.add_lock_balance_ref(
                    &context,
                    AccountIndex::from(index),
                    "Token".parse().unwrap(),
                )
                .unwrap();
            }
            state.update_lock(&context, lock).unwrap();
        }
        let mut execution = TransactionExecution::new(
            TransactionContext {
                energy_limit: Energy::from(u64::MAX),
                sender_account_address: address,
                transaction_sequence_number: 1.into(),
                block_timestamp: 0.into(),
            },
            Account::from_existing_account(sender),
        );
        let result = execute_lock_cancel(
            &mut context,
            &mut execution,
            &mut state,
            LockCancelDetails {
                lock: lock_id.clone(),
                memo: None,
            },
            &mut vec![],
        );
        match count {
            None => assert!(matches!(
                result,
                Err(WithBlockStateFailure::Error(
                    TransactionRejectReason::NonExistentLockId(_)
                ))
            )),
            Some(_) => assert!(matches!(
                result,
                Err(WithBlockStateFailure::Error(
                    TransactionRejectReason::LockCancelNotAuthorized(..)
                ))
            )),
        }
        assert_eq!(
            execution.energy_used(),
            Energy::from(500 * count.unwrap_or(0))
        );
    }
}
