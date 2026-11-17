//! Automatic cancellation uses consensus time, schedule order, and spare block energy.

use assert_matches::assert_matches;
use concordium_base::base::Energy;
use concordium_base::common::cbor;
use concordium_base::protocol_level_locks::{LockControllerSimpleV0Capability, LockId};
use concordium_base::protocol_level_tokens::{TokenAmount, TokenId, operations};
use plt_block_state::entity::entity_test_stub;
use plt_block_state::persistent::protocol_level_locks::p11::{
    LockConfig, LockConfigSimpleV0, LockControllerSimpleV0Grant, LockRecipients,
};
use plt_scheduler::protocol_level_locks::p11::cancel_expired_locks;
use plt_scheduler_types::types::events::{BlockItemEvent, LockDestroyEvent};
use plt_scheduler_types::types::tokens::{RawTokenAmount, TokenHolder};
use utils::entity_traits::scheduler::SchedulerOperations;
use utils::{BlockStateLatest, TokenInitTestParams};

mod utils;

/// Cleanup follows expiry/creation/ID order, skips unaffordable locks, accepts exact
/// energy fits, and retries skipped locks when a later block has enough energy.
#[test]
fn order_skip_exact_fit_and_retry() {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut state = BlockStateLatest::default();
    let holders: Vec<_> = (0..4)
        .map(|_| context.external.create_account().account_index())
        .collect();
    let token_id: TokenId = "Token".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        2,
        None,
    );
    for holder in &holders {
        utils::increment_account_balance_p11(
            &mut context,
            &mut state,
            *holder,
            &token_id,
            1000.into(),
        );
    }

    // IDs/insertion order deliberately disagree with expiry and creation-time order.
    // Equal-expiry, equal-creation locks 3 and 4 must be ordered by ID.
    let mut ids = Vec::new();
    for (sequence, expiry, created_at, references) in [
        (1, 11, 0, 1),
        (2, 10, 2001, 1),
        (4, 10, 2000, 1),
        (3, 10, 2000, 1),
        (5, 10, 1000, 1),
        (6, 9, 9000, 4),
        (7, 11, 1000, 0),
    ] {
        let id = LockId::new(holders[0], sequence, 0);
        let config = LockConfig::SimpleV0(
            LockConfigSimpleV0::new(
                LockRecipients::Any,
                expiry.into(),
                holders
                    .iter()
                    .map(|holder| {
                        LockControllerSimpleV0Grant::new(
                            *holder,
                            vec![LockControllerSimpleV0Capability::Fund],
                        )
                    })
                    .collect(),
                vec![token_id.clone()],
                None,
                None,
            )
            .unwrap(),
        );
        state
            .create_lock(&context, &id, config, created_at.into())
            .unwrap();
        for holder in holders.iter().take(references) {
            utils::execute_operations(
                &mut context,
                &mut state,
                *holder,
                vec![
                    operations::fund_lock(
                        token_id.clone(),
                        id.clone(),
                        TokenAmount::from_raw(10, 2),
                        None,
                    )
                    .into(),
                ],
            );
        }
        ids.push(id);
    }

    // First run: skip ids[5] (2100 NRG), then cancel ids[4], ids[3], and ids[2]
    // (600 NRG each) in creation-time/ID order. ids[1] and ids[0] cannot fit the
    // remaining 100 NRG, but empty ids[6] fits exactly. ids[5], ids[1], and ids[0] remain.
    let expensive = ids[5].clone();
    let expensive_info = state.query_lock_info(&context, &expensive).unwrap();
    // Four permitted funders do not make one-reference cancellations cost 2100 NRG.
    let (energy, events) =
        cancel_expired_locks(&context, &mut state, 11_001.into(), 1900.into()).unwrap();
    assert_eq!(energy, Energy::from(1900));
    assert_eq!(events.len(), 7);
    // The final 100 NRG fits the later empty lock after the funded cancellations.
    assert_eq!(
        events[6],
        BlockItemEvent::LockDestroyed(LockDestroyEvent {
            lock_id: ids[6].clone()
        })
    );
    assert!(state.query_lock_info(&context, &ids[6]).is_err());
    let expected = [ids[4].clone(), ids[3].clone(), ids[2].clone()];
    for (pair, id) in events.as_chunks::<2>().0.iter().zip(&expected) {
        assert_matches!(&pair[0], BlockItemEvent::UnlockAmount(event) => {
            assert_eq!(&event.lock_id, id);
            assert_eq!(event.token_id, token_id);
            assert_eq!(event.amount.amount, RawTokenAmount::from(10));
            assert_eq!(
                event.token_holder,
                TokenHolder::Account(context.external.account_canonical_address(holders[0]))
            );
        });
        assert_eq!(
            pair[1],
            BlockItemEvent::LockDestroyed(LockDestroyEvent {
                lock_id: id.clone()
            })
        );
        assert!(state.query_lock_info(&context, id).is_err());
    }
    assert_eq!(
        state.query_lock_info(&context, &expensive).unwrap(),
        expensive_info
    );
    assert_eq!(state.lock_expiry_index(&context).unwrap().size(), 3);

    // Second run: skip ids[5] again without charging scan energy, then cancel ids[1]
    // before ids[0] because it expires earlier. Their combined 1200 NRG fits exactly;
    // only ids[5] remains scheduled.
    let (energy, events) =
        cancel_expired_locks(&context, &mut state, 12_000.into(), 1200.into()).unwrap();
    assert_eq!(energy, Energy::from(1200));
    assert_eq!(events.len(), 4);
    for (event, id) in [(&events[1], &ids[1]), (&events[3], &ids[0])] {
        assert_eq!(
            event,
            &BlockItemEvent::LockDestroyed(LockDestroyEvent {
                lock_id: id.clone()
            })
        );
    }
    assert_eq!(
        state.query_lock_info(&context, &expensive).unwrap(),
        expensive_info
    );
    assert_eq!(state.lock_expiry_index(&context).unwrap().size(), 1);

    // Third run: the 2100-NRG budget now fits ids[5]'s four balance references exactly.
    // Canceling it emits four unlocks followed by destruction and empties the schedule.
    let (energy, events) =
        cancel_expired_locks(&context, &mut state, 13_000.into(), 2100.into()).unwrap();
    assert_eq!(energy, Energy::from(2100));
    assert_eq!(events.len(), 5);
    assert_eq!(
        events[4],
        BlockItemEvent::LockDestroyed(LockDestroyEvent { lock_id: expensive })
    );
    assert!(state.query_lock_list(&context).is_empty());
    assert_eq!(state.lock_expiry_index(&context).unwrap().size(), 0);
    for holder in holders {
        let info = state
            .query_token_account_infos(&context, holder)
            .unwrap()
            .into_iter()
            .find(|info| info.token_id == token_id)
            .unwrap();
        assert_eq!(
            info.account_state.balance.amount,
            RawTokenAmount::from(1000)
        );
        let module: concordium_base::protocol_level_tokens::TokenModuleAccountState =
            cbor::cbor_decode(info.account_state.module_state.unwrap()).unwrap();
        assert!(module.locks.is_empty());
        assert!(module.available.is_none());
    }
}

/// Empty locks require at least 100 NRG and strict expiry.
#[test]
fn empty_locks_require_energy_budget_and_strict_expiry() {
    // Empty cancellation needs neither a sender nor any external account mutations/queries.
    let context = entity_test_stub::new_no_external_context();
    let mut state = BlockStateLatest::default();
    let mut ids = Vec::new();
    for (sequence, expiry) in [(1, 10), (2, 11)] {
        let id = LockId::new(0, sequence, 0);
        state
            .create_lock(
                &context,
                &id,
                LockConfig::SimpleV0(
                    LockConfigSimpleV0::new(
                        LockRecipients::Any,
                        expiry.into(),
                        vec![],
                        vec![],
                        None,
                        None,
                    )
                    .unwrap(),
                ),
                0.into(),
            )
            .unwrap();
        ids.push(id);
    }
    for timestamp in [9999, 10_000] {
        assert_eq!(
            cancel_expired_locks(&context, &mut state, timestamp.into(), 300.into()).unwrap(),
            (Energy::from(0), vec![])
        );
        assert_eq!(state.lock_expiry_index(&context).unwrap().size(), 2);
    }
    for remaining_energy in [0, 99] {
        assert_eq!(
            cancel_expired_locks(&context, &mut state, 10_001.into(), remaining_energy.into())
                .unwrap(),
            (Energy::from(0), vec![])
        );
        assert_eq!(state.lock_expiry_index(&context).unwrap().size(), 2);
    }
    let (energy, events) =
        cancel_expired_locks(&context, &mut state, 10_001.into(), 100.into()).unwrap();
    assert_eq!(energy, Energy::from(100));
    assert_eq!(
        events,
        vec![BlockItemEvent::LockDestroyed(LockDestroyEvent {
            lock_id: ids[0].clone()
        })]
    );
    assert_eq!(state.lock_list(&context).unwrap(), vec![ids[1].clone()]);
    assert_eq!(state.lock_expiry_index(&context).unwrap().size(), 1);
    assert_eq!(
        cancel_expired_locks(&context, &mut state, 10_001.into(), 0.into()).unwrap(),
        (Energy::from(0), vec![])
    );
}
