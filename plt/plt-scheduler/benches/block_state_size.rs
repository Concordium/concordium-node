//! Scheduler benchmarks isolating block-state size from transaction operation count.
//!
//! Each case prepares increasingly complex state outside the timed closure, then executes one
//! successful operation. The point of the benchmarks is to measure efficiency of transaction
//! execution across varying state complexity of the state which the transaction operates on.

#[path = "../tests/utils/mod.rs"]
mod utils;

use concordium_base::base::AccountIndex;
use concordium_base::common::cbor;
use concordium_base::protocol_level_locks::{
    LockConfig, LockConfigSimpleV0, LockControllerSimpleV0Capability, LockId, LockRecipients,
};
use concordium_base::protocol_level_tokens::meta_operations::{
    MetaUpdateOperation, MetaUpdateOperations, MetaUpdatePayload, lock_cancel as meta_lock_cancel,
    lock_create,
};
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, RawCbor, TokenAmount, TokenId, TokenListUpdateDetails, TokenOperation,
    TokenOperationsPayload, TokenSupplyUpdateDetails,
};
use concordium_base::transactions::Payload;
use divan::Bencher;
use plt_block_state::entity::entity_test_stub::{self, StubbedEntityContext};
use plt_block_state::persistent::protocol_level_locks::p11::LockControllerSimpleV0Grant;
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use utils::entity_traits::scheduler::SchedulerOperations;
use utils::{BlockStateLatest, TokenInitTestParams};

const STATE_SIZES: &[usize] = &[0, 1, 10, 100, 1000];

fn main() {
    divan::main();
}

/// Prepared scheduler input containing one operation to execute.
///
/// Setup has already populated `state`; executing `payload` once as `sender` is the benchmarked
/// action.
///
/// # Examples
///
/// ```ignore
/// let fixture = prepare_mint_with_filler_tokens(10);
/// fixture.state.execute_transaction(
///     &mut fixture.context,
///     fixture.transaction_context,
///     fixture.sender,
///     fixture.payload,
/// );
/// ```
struct PreparedOperation {
    /// Entity context backing account and persistent-state access.
    context: StubbedEntityContext,
    /// Block state populated before timing starts.
    state: BlockStateLatest,
    /// Account authorized to execute the prepared payload.
    sender: AccountIndex,
    /// Transaction context for the single measured operation.
    transaction_context: plt_scheduler::TransactionContext,
    /// Payload containing exactly one measured operation.
    payload: Payload,
}

fn token_payload(token_id: TokenId, operation: TokenOperation) -> Payload {
    Payload::TokenUpdate {
        payload: TokenOperationsPayload {
            token_id,
            operations: RawCbor::from(cbor::cbor_encode(&vec![operation])),
        },
    }
}

/// Prepare one mint against a target token alongside unrelated tokens.
///
/// # Arguments
///
/// - `filler_token_count`: Number of unrelated tokens inserted before the target token.
fn prepare_mint_with_filler_tokens(filler_token_count: usize) -> PreparedOperation {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut state = BlockStateLatest::default();
    for index in 0..filler_token_count {
        utils::create_and_init_token_p11(
            &mut context,
            &mut state,
            format!("benchmark-filler-{index}").parse().unwrap(),
            TokenInitTestParams::default(),
            0,
            None,
        );
    }
    let token_id: TokenId = "benchmark-target".parse().unwrap();
    let (sender, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        0,
        None,
    );
    let sender = sender.account_index();
    PreparedOperation {
        transaction_context: utils::simple_transaction_context(
            context.external.account_canonical_address(sender),
        ),
        payload: token_payload(
            token_id,
            TokenOperation::Mint(TokenSupplyUpdateDetails {
                amount: TokenAmount::from_raw(1, 0),
            }),
        ),
        context,
        state,
        sender,
    }
}

/// Prepare one allow-list insertion against a token with an existing allow list.
///
/// # Arguments
///
/// - `existing_entry_count`: Number of distinct accounts inserted before the measured account.
fn prepare_allow_list_insert(existing_entry_count: usize) -> PreparedOperation {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut state = BlockStateLatest::default();
    let token_id: TokenId = "benchmark-allow-list".parse().unwrap();
    let (sender, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut state,
        token_id.clone(),
        TokenInitTestParams::default().allow_list(),
        0,
        None,
    );
    let sender = sender.account_index();
    let existing = (0..existing_entry_count)
        .map(|_| {
            let account = context.external.create_account();
            TokenOperation::AddAllowList(TokenListUpdateDetails {
                target: CborHolderAccount::from(
                    context
                        .external
                        .account_canonical_address(account.account_index()),
                ),
            })
        })
        .collect();
    utils::execute_token_operations(&mut context, &mut state, &token_id, sender, existing);

    let target = context.external.create_account();
    let operation = TokenOperation::AddAllowList(TokenListUpdateDetails {
        target: CborHolderAccount::from(
            context
                .external
                .account_canonical_address(target.account_index()),
        ),
    });
    PreparedOperation {
        transaction_context: utils::simple_transaction_context(
            context.external.account_canonical_address(sender),
        ),
        payload: token_payload(token_id, operation),
        context,
        state,
        sender,
    }
}

/// Prepare one lock creation against state containing unrelated locks.
///
/// # Arguments
///
/// - `existing_lock_count`: Number of locks created before the measured transaction.
fn prepare_lock_create(existing_lock_count: usize) -> PreparedOperation {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut state = BlockStateLatest::default();
    let sender = context.external.create_account().account_index();
    for sequence_number in 1..=existing_lock_count as u64 {
        let lock_id = LockId::new(sender, sequence_number, 0);
        utils::create_lock(
            &mut context,
            &mut state,
            &lock_id,
            utils::CreateLockSimpleConfig {
                recipients: vec![sender],
                grants: vec![],
                tokens: vec![],
                expiry: 1_804_806_000,
                keep_alive: false,
            },
        );
        assert!(matches!(state.lock_by_id(&context, &lock_id), Ok(Ok(_))));
    }
    let config = LockConfig::SimpleV0(LockConfigSimpleV0 {
        recipients: LockRecipients::Any,
        expiry: 1_804_806_000.into(),
        grants: vec![],
        tokens: vec![],
        keep_alive: false,
        memo: None,
        metadata: None,
    });
    PreparedOperation {
        transaction_context: utils::simple_transaction_context_with_nonce(
            context.external.account_canonical_address(sender),
            existing_lock_count as u64 + 1,
        ),
        payload: Payload::MetaUpdate {
            payload: MetaUpdatePayload {
                operations: RawCbor::from(cbor::cbor_encode(&MetaUpdateOperations {
                    operations: vec![lock_create(config)],
                })),
            },
        },
        context,
        state,
        sender,
    }
}

fn meta_payload(operations: Vec<MetaUpdateOperation>) -> Payload {
    Payload::MetaUpdate {
        payload: MetaUpdatePayload {
            operations: RawCbor::from(cbor::cbor_encode(&MetaUpdateOperations { operations })),
        },
    }
}

#[derive(Clone, Copy)]
enum CancelReferenceDistribution {
    ManyAccountsOneToken,
    OneAccountManyTokens,
    ManyAccountsManyTokens,
}

/// Prepare cancellation of a lock with the requested balance-reference distribution.
///
/// # Arguments
///
/// - `reference_count`: Total number of `(account, token)` balance references.
/// - `distribution`: Whether references vary by account, token, or both.
fn prepare_lock_cancel(
    reference_count: usize,
    distribution: CancelReferenceDistribution,
) -> PreparedOperation {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut state = BlockStateLatest::default();
    let sender = context.external.create_account().account_index();
    let token_count = match distribution {
        CancelReferenceDistribution::ManyAccountsOneToken => 1,
        CancelReferenceDistribution::OneAccountManyTokens
        | CancelReferenceDistribution::ManyAccountsManyTokens => reference_count.max(1),
    };
    let token_ids: Vec<TokenId> = (0..token_count)
        .map(|index| {
            let token_id: TokenId = format!("benchmark-cancel-{index}").parse().unwrap();
            utils::create_and_init_token_p11(
                &mut context,
                &mut state,
                token_id.clone(),
                TokenInitTestParams::default(),
                0,
                None,
            );
            token_id
        })
        .collect();
    let lock_id = LockId::new(sender, 1, 0);
    utils::create_lock(
        &mut context,
        &mut state,
        &lock_id,
        utils::CreateLockSimpleConfig {
            recipients: vec![sender],
            grants: vec![LockControllerSimpleV0Grant::new(
                sender,
                vec![LockControllerSimpleV0Capability::Cancel],
            )],
            tokens: token_ids.clone(),
            expiry: 1_804_806_000,
            keep_alive: true,
        },
    );
    for index in 0..reference_count {
        let account = match distribution {
            CancelReferenceDistribution::OneAccountManyTokens => sender,
            CancelReferenceDistribution::ManyAccountsOneToken
            | CancelReferenceDistribution::ManyAccountsManyTokens => {
                context.external.create_account().account_index()
            }
        };
        let token_id = match distribution {
            CancelReferenceDistribution::ManyAccountsOneToken => &token_ids[0],
            CancelReferenceDistribution::OneAccountManyTokens
            | CancelReferenceDistribution::ManyAccountsManyTokens => &token_ids[index],
        };
        utils::lock_balance(
            &mut context,
            &mut state,
            &lock_id,
            account,
            token_id,
            RawTokenAmount::from(1),
        );
    }
    PreparedOperation {
        transaction_context: utils::simple_transaction_context(
            context.external.account_canonical_address(sender),
        ),
        payload: meta_payload(vec![meta_lock_cancel(lock_id, None)]),
        context,
        state,
        sender,
    }
}

fn execute(mut fixture: PreparedOperation) {
    let result = fixture.state.execute_transaction(
        &mut fixture.context,
        fixture.transaction_context,
        fixture.sender,
        fixture.payload,
    );
    assert!(
        matches!(result, Ok(ref result) if matches!(result.outcome, TransactionOutcome::Success(_)))
    );
    let _ = divan::black_box((result, fixture.state, fixture.context));
}

/// Measure one token mint as unrelated top-level token count grows.
#[divan::bench(args = STATE_SIZES)]
fn mint_by_token_count(bencher: Bencher, filler_token_count: usize) {
    bencher
        .with_inputs(|| prepare_mint_with_filler_tokens(filler_token_count))
        .bench_local_values(execute);
}

/// Measure one allow-list insertion as the target token's existing list grows.
#[divan::bench(args = STATE_SIZES)]
fn allow_list_insert_by_existing_entry_count(bencher: Bencher, existing_entry_count: usize) {
    bencher
        .with_inputs(|| prepare_allow_list_insert(existing_entry_count))
        .bench_local_values(execute);
}

/// Measure one lock creation as unrelated top-level lock count grows.
#[divan::bench(args = STATE_SIZES)]
fn lock_create_by_lock_count(bencher: Bencher, existing_lock_count: usize) {
    bencher
        .with_inputs(|| prepare_lock_create(existing_lock_count))
        .bench_local_values(execute);
}

/// Measure cancellation as account count grows while all references use one token.
#[divan::bench(args = STATE_SIZES)]
fn lock_cancel_many_accounts_one_token(bencher: Bencher, reference_count: usize) {
    bencher
        .with_inputs(|| {
            prepare_lock_cancel(
                reference_count,
                CancelReferenceDistribution::ManyAccountsOneToken,
            )
        })
        .bench_local_values(execute);
}

/// Measure cancellation as token count grows while all references use one account.
#[divan::bench(args = STATE_SIZES)]
fn lock_cancel_one_account_many_tokens(bencher: Bencher, reference_count: usize) {
    bencher
        .with_inputs(|| {
            prepare_lock_cancel(
                reference_count,
                CancelReferenceDistribution::OneAccountManyTokens,
            )
        })
        .bench_local_values(execute);
}

/// Measure cancellation when every reference uses a distinct account and token.
#[divan::bench(args = STATE_SIZES)]
fn lock_cancel_many_accounts_many_tokens(bencher: Bencher, reference_count: usize) {
    bencher
        .with_inputs(|| {
            prepare_lock_cancel(
                reference_count,
                CancelReferenceDistribution::ManyAccountsManyTokens,
            )
        })
        .bench_local_values(execute);
}
