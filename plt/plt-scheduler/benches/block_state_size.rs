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
use concordium_base::protocol_level_tokens::{
    Operation, Operations, OperationsPayload, operations, token_operations,
};
use concordium_base::protocol_level_tokens::{
    RawCbor, TokenAmount, TokenId, TokenOperation, TokenOperationsPayload,
};
use concordium_base::transactions::{Payload, TokenUpdatePayload};
use divan::Bencher;
use plt_block_state::entity::entity_test_stub::{self, StubbedEntityContext};
use plt_block_state::persistent::protocol_level_locks::p11::LockControllerSimpleV0Grant;
use plt_scheduler::scheduler::TransactionExecutionError;
use plt_scheduler_types::types::execution::{TransactionExecutionSummary, TransactionOutcome};
use plt_scheduler_types::types::tokens::RawTokenAmount;
use utils::entity_traits::scheduler::SchedulerOperations;
use utils::{BlockStateLatest, TokenInitTestParams};

const STATE_SIZES: &[usize] = &[0, 1, 10, 100, 1000];

const GRANT_COUNTS: &[usize] = &[1, 10, 100, 1000];

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
        payload: TokenUpdatePayload::Scoped(TokenOperationsPayload {
            token_id,
            operations: RawCbor::from(cbor::cbor_encode(&vec![operation])),
        }),
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
            token_operations::mint_tokens(TokenAmount::from_raw(1, 0)),
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
            token_operations::add_token_allow_list(
                context
                    .external
                    .account_canonical_address(account.account_index()),
            )
        })
        .collect();
    utils::execute_token_operations(&mut context, &mut state, &token_id, sender, existing);

    let target = context.external.create_account();
    let operation = token_operations::add_token_allow_list(
        context
            .external
            .account_canonical_address(target.account_index()),
    );
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
        payload: Payload::TokenUpdate {
            payload: TokenUpdatePayload::Unscoped(OperationsPayload {
                operations: RawCbor::from(cbor::cbor_encode(&Operations {
                    operations: vec![operations::create_lock(config)],
                })),
            }),
        },
        context,
        state,
        sender,
    }
}

fn unscoped_payload(operations: Vec<Operation>) -> Payload {
    Payload::TokenUpdate {
        payload: TokenUpdatePayload::Unscoped(OperationsPayload {
            operations: RawCbor::from(cbor::cbor_encode(&Operations { operations })),
        }),
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
        payload: unscoped_payload(vec![operations::cancel_lock(lock_id, None)]),
        context,
        state,
        sender,
    }
}

fn execute(
    mut fixture: PreparedOperation,
) -> (
    Result<TransactionExecutionSummary, TransactionExecutionError>,
    BlockStateLatest,
    StubbedEntityContext,
) {
    let result = fixture.state.execute_transaction(
        &mut fixture.context,
        fixture.transaction_context,
        fixture.sender,
        fixture.payload,
    );
    assert!(
        matches!(result, Ok(ref result) if matches!(result.outcome, TransactionOutcome::Success(_)))
    );
    // Return owned values so Divan defers teardown until after timing.
    (result, fixture.state, fixture.context)
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

/// Prepare one successful operation.
///
/// `grant_count` distinct ascending accounts all receive `capability`; the executor additionally
/// receives Fund for send/release. Setup is untimed.
fn prepare_lock_grants(
    grant_count: usize,
    capability: LockControllerSimpleV0Capability,
) -> PreparedOperation {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut state = BlockStateLatest::default();
    let token_id: TokenId = "PLT".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        0,
        None,
    );
    let recipient = context.external.create_account().account_index();
    let accounts: Vec<_> = (0..grant_count)
        .map(|_| context.external.create_account().account_index())
        .collect();
    let sender = accounts[grant_count - 1];
    let grants = accounts
        .iter()
        .map(|&account| {
            let mut roles = vec![capability.clone()];
            if account == sender && capability != LockControllerSimpleV0Capability::Fund {
                roles.push(LockControllerSimpleV0Capability::Fund);
            }
            LockControllerSimpleV0Grant::new(account, roles)
        })
        .collect();
    let lock_id = LockId::new(sender, 1, 0);
    utils::create_lock(
        &mut context,
        &mut state,
        &lock_id,
        utils::CreateLockSimpleConfig {
            recipients: vec![recipient],
            grants,
            tokens: vec![token_id.clone()],
            expiry: 1_804_806_000,
            keep_alive: true,
        },
    );
    utils::increment_account_balance_p11(
        &mut context,
        &mut state,
        sender,
        &token_id,
        RawTokenAmount::from(1000),
    );
    utils::lock_balance(
        &mut context,
        &mut state,
        &lock_id,
        sender,
        &token_id,
        RawTokenAmount::from(100),
    );
    let sender_address = context.external.account_canonical_address(sender);
    let amount = TokenAmount::from_raw(10, 0);
    let operation = match capability {
        LockControllerSimpleV0Capability::Fund => {
            operations::fund_lock(token_id, lock_id, amount, None)
        }
        LockControllerSimpleV0Capability::Send => operations::send_locked_tokens(
            token_id,
            lock_id,
            sender_address,
            context.external.account_canonical_address(recipient),
            amount,
            None,
        ),
        LockControllerSimpleV0Capability::Release => {
            operations::release_locked_tokens(token_id, lock_id, sender_address, amount, None)
        }
        _ => unreachable!("only fund/send/release are measured"),
    };
    PreparedOperation {
        transaction_context: utils::simple_transaction_context(sender_address),
        payload: unscoped_payload(vec![operation]),
        context,
        state,
        sender,
    }
}

/// Measure one fund using an existing reference as Fund grant count grows.
#[divan::bench(args = GRANT_COUNTS)]
fn lock_fund_by_grant_count(bencher: Bencher, grant_count: usize) {
    bencher
        .with_inputs(|| prepare_lock_grants(grant_count, LockControllerSimpleV0Capability::Fund))
        .bench_local_values(execute);
}

/// Measure one partial send as Send grant count grows.
#[divan::bench(args = GRANT_COUNTS)]
fn lock_send_by_grant_count(bencher: Bencher, grant_count: usize) {
    bencher
        .with_inputs(|| prepare_lock_grants(grant_count, LockControllerSimpleV0Capability::Send))
        .bench_local_values(execute);
}

/// Measure one partial release as Release grant count grows.
#[divan::bench(args = GRANT_COUNTS)]
fn lock_release_by_grant_count(bencher: Bencher, grant_count: usize) {
    bencher
        .with_inputs(|| prepare_lock_grants(grant_count, LockControllerSimpleV0Capability::Release))
        .bench_local_values(execute);
}
