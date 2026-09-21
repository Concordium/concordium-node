//! Scheduler benchmarks for protocol-level token and lock operations.
//!
//! Each Divan input prepares a fresh entity context, block state, and encoded payload outside the
//! timed closure. The timed part starts at [`BlockStateLatest::execute_transaction`], so it includes
//! payload decoding, scheduler dispatch, validation, event construction, and Rust block-state
//! mutation. It excludes transaction construction and signing, signature and header verification,
//! and block assembly.
//!
//! Token-update and meta-update benchmarks use equivalent operations at several batch sizes. Counts
//! 3 and 5 represent maximum expected transaction sizes; larger counts expose scaling behavior. Their
//! difference indicates the overhead of the meta-update execution path, while the empty cases show
//! its fixed cost. Lock benchmarks instead isolate persistence-relevant transitions: creating locks,
//! adding versus reusing balance references, retaining versus removing references, and canceling
//! locks with different numbers of references.

#[path = "../tests/utils/mod.rs"]
mod utils;

use concordium_base::common::cbor;
use concordium_base::protocol_level_locks::{
    LockConfig, LockConfigSimpleV0, LockControllerSimpleV0Capability, LockId, LockRecipients,
};
use concordium_base::protocol_level_tokens::meta_operations::{
    MetaUpdateOperation, MetaUpdateOperations, MetaUpdatePayload, lock_create as meta_lock_create,
    lock_fund, lock_release as lock_return, lock_send,
};
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, RawCbor, TokenAmount, TokenId, TokenListUpdateDetails, TokenOperation,
    TokenOperationsPayload, TokenSupplyUpdateDetails, TokenTransfer,
};
use concordium_base::transactions::Payload;
use divan::Bencher;
use plt_block_state::entity::entity_test_stub::{self, StubbedEntityContext};
use plt_block_state::persistent::protocol_level_locks::p11::LockControllerSimpleV0Grant;
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use utils::entity_traits::scheduler::SchedulerOperations;
use utils::{BlockStateLatest, TokenInitTestParams};

const COUNTS: &[usize] = &[1, 3, 5, 10, 100, 1000];
const EMPTY_COUNTS: &[usize] = &[0];

fn main() {
    divan::main();
}

struct Fixture {
    context: StubbedEntityContext,
    state: BlockStateLatest,
    sender: concordium_base::base::AccountIndex,
    transaction_context: plt_scheduler::TransactionContext,
    payload: Payload,
}

type TokenFixture = (
    StubbedEntityContext,
    BlockStateLatest,
    concordium_base::base::AccountIndex,
    TokenId,
);

/// Prepare the common context and state for a token-operation benchmark.
///
/// # Arguments
///
/// - `params`: Token configuration required by the operation under test.
/// - `initial_supply`: Optional balance assigned to the benchmark sender.
fn token_fixture(
    params: TokenInitTestParams,
    initial_supply: Option<RawTokenAmount>,
) -> TokenFixture {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut state = BlockStateLatest::default();
    let token_id: TokenId = "benchmark-token".parse().unwrap();
    let (sender, _) = utils::create_and_init_token_p11(
        &mut context,
        &mut state,
        token_id.clone(),
        params,
        0,
        initial_supply,
    );
    (context, state, sender.account_index(), token_id)
}

/// Build a token-update or meta-update fixture from prepared state and operations.
///
/// # Arguments
///
/// - `context`: Entity context containing benchmark accounts.
/// - `state`: Block state containing the benchmark token.
/// - `sender`: Account executing the transaction.
/// - `token_id`: Token affected by every operation.
/// - `operations`: Equivalent operations encoded into the selected payload type.
/// - `meta`: Whether to create a meta-update instead of a token-update payload.
fn prepare_token(
    context: StubbedEntityContext,
    state: BlockStateLatest,
    sender: concordium_base::base::AccountIndex,
    token_id: TokenId,
    operations: Vec<TokenOperation>,
    meta: bool,
) -> Fixture {
    let sender_address = context.external.account_canonical_address(sender);
    let payload = if meta {
        meta_payload(
            operations
                .into_iter()
                .map(|operation| (token_id.clone(), operation).into())
                .collect(),
        )
    } else {
        Payload::TokenUpdate {
            payload: TokenOperationsPayload {
                token_id,
                operations: RawCbor::from(cbor::cbor_encode(&operations)),
            },
        }
    };
    Fixture {
        context,
        state,
        sender,
        transaction_context: utils::simple_transaction_context(sender_address),
        payload,
    }
}

/// Prepare an empty token-update or meta-update baseline.
///
/// # Arguments
///
/// - `_count`: Divan argument fixed at zero for consistent benchmark naming.
/// - `meta`: Whether to create a meta-update instead of a token-update payload.
fn prepare_empty(_count: usize, meta: bool) -> Fixture {
    let (context, state, sender, token_id) = token_fixture(Default::default(), None);
    prepare_token(context, state, sender, token_id, vec![], meta)
}

/// Prepare repeated transfers from the sender to one recipient.
///
/// # Arguments
///
/// - `count`: Number of transfer operations and initial sender balance.
/// - `meta`: Whether to create a meta-update instead of a token-update payload.
fn prepare_transfer(count: usize, meta: bool) -> Fixture {
    let (mut context, state, sender, token_id) =
        token_fixture(Default::default(), Some(RawTokenAmount::from(count as u64)));
    let recipient = context.external.create_account();
    let recipient = context
        .external
        .account_canonical_address(recipient.account_index());
    let operations = (0..count)
        .map(|_| {
            TokenOperation::Transfer(TokenTransfer {
                amount: TokenAmount::from_raw(1, 0),
                recipient: CborHolderAccount::from(recipient),
                memo: None,
            })
        })
        .collect();
    prepare_token(context, state, sender, token_id, operations, meta)
}

/// Prepare repeated mint operations for a mintable token.
///
/// # Arguments
///
/// - `count`: Number of mint operations.
/// - `meta`: Whether to create a meta-update instead of a token-update payload.
fn prepare_mint(count: usize, meta: bool) -> Fixture {
    let (context, state, sender, token_id) =
        token_fixture(TokenInitTestParams::default().mintable(), None);
    let operations = (0..count)
        .map(|_| {
            TokenOperation::Mint(TokenSupplyUpdateDetails {
                amount: TokenAmount::from_raw(1, 0),
            })
        })
        .collect();
    prepare_token(context, state, sender, token_id, operations, meta)
}

/// Prepare repeated burn operations backed by sufficient sender balance.
///
/// # Arguments
///
/// - `count`: Number of burn operations and initial sender balance.
/// - `meta`: Whether to create a meta-update instead of a token-update payload.
fn prepare_burn(count: usize, meta: bool) -> Fixture {
    let (context, state, sender, token_id) = token_fixture(
        TokenInitTestParams::default().burnable(),
        Some(RawTokenAmount::from(count as u64)),
    );
    let operations = (0..count)
        .map(|_| {
            TokenOperation::Burn(TokenSupplyUpdateDetails {
                amount: TokenAmount::from_raw(1, 0),
            })
        })
        .collect();
    prepare_token(context, state, sender, token_id, operations, meta)
}

/// Prepare repeated list updates targeting distinct accounts.
///
/// # Arguments
///
/// - `count`: Number of accounts and list-update operations.
/// - `meta`: Whether to create a meta-update instead of a token-update payload.
/// - `params`: Token configuration enabling the list under test.
/// - `operation`: Constructor for the allow-list or deny-list operation.
fn prepare_list(
    count: usize,
    meta: bool,
    params: TokenInitTestParams,
    operation: fn(TokenListUpdateDetails) -> TokenOperation,
) -> Fixture {
    let (mut context, state, sender, token_id) = token_fixture(params, None);
    let operations = (0..count)
        .map(|_| {
            let account = context.external.create_account();
            operation(TokenListUpdateDetails {
                target: CborHolderAccount::from(
                    context
                        .external
                        .account_canonical_address(account.account_index()),
                ),
            })
        })
        .collect();
    prepare_token(context, state, sender, token_id, operations, meta)
}

/// Prepare add-allow-list operations for distinct accounts.
///
/// # Arguments
///
/// - `count`: Number of accounts added to the allow list.
/// - `meta`: Whether to create a meta-update instead of a token-update payload.
fn prepare_allow_list(count: usize, meta: bool) -> Fixture {
    prepare_list(
        count,
        meta,
        TokenInitTestParams::default().allow_list(),
        TokenOperation::AddAllowList,
    )
}

/// Prepare add-deny-list operations for distinct accounts.
///
/// # Arguments
///
/// - `count`: Number of accounts added to the deny list.
/// - `meta`: Whether to create a meta-update instead of a token-update payload.
fn prepare_deny_list(count: usize, meta: bool) -> Fixture {
    prepare_list(
        count,
        meta,
        TokenInitTestParams::default().deny_list(),
        TokenOperation::AddDenyList,
    )
}

/// Execute only the Rust scheduler path measured by every benchmark.
fn execute(mut fixture: Fixture) {
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

fn bench(bencher: Bencher, prepare: fn(usize, bool) -> Fixture, count: usize, meta: bool) {
    bencher
        .with_inputs(|| prepare(count, meta))
        .bench_local_values(execute);
}

/// Measure fixed token-update execution cost without operations.
#[divan::bench(args = EMPTY_COUNTS)]
fn token_update_empty(bencher: Bencher, count: usize) {
    bench(bencher, prepare_empty, count, false);
}

/// Measure fixed meta-update execution cost without operations.
#[divan::bench(args = EMPTY_COUNTS)]
fn meta_update_empty(bencher: Bencher, count: usize) {
    bench(bencher, prepare_empty, count, true);
}

/// Measure token-update transfer cost as operation count grows.
#[divan::bench(args = COUNTS)]
fn token_update_transfer(bencher: Bencher, count: usize) {
    bench(bencher, prepare_transfer, count, false);
}

/// Measure meta-update transfer cost as operation count grows.
#[divan::bench(args = COUNTS)]
fn meta_update_transfer(bencher: Bencher, count: usize) {
    bench(bencher, prepare_transfer, count, true);
}

/// Measure token-update mint cost as operation count grows.
#[divan::bench(args = COUNTS)]
fn token_update_mint(bencher: Bencher, count: usize) {
    bench(bencher, prepare_mint, count, false);
}

/// Measure meta-update mint cost as operation count grows.
#[divan::bench(args = COUNTS)]
fn meta_update_mint(bencher: Bencher, count: usize) {
    bench(bencher, prepare_mint, count, true);
}

/// Measure token-update burn cost as operation count grows.
#[divan::bench(args = COUNTS)]
fn token_update_burn(bencher: Bencher, count: usize) {
    bench(bencher, prepare_burn, count, false);
}

/// Measure meta-update burn cost as operation count grows.
#[divan::bench(args = COUNTS)]
fn meta_update_burn(bencher: Bencher, count: usize) {
    bench(bencher, prepare_burn, count, true);
}

/// Measure token-update allow-list insertion cost as operation count grows.
#[divan::bench(args = COUNTS)]
fn token_update_allow_list(bencher: Bencher, count: usize) {
    bench(bencher, prepare_allow_list, count, false);
}

/// Measure meta-update allow-list insertion cost as operation count grows.
#[divan::bench(args = COUNTS)]
fn meta_update_allow_list(bencher: Bencher, count: usize) {
    bench(bencher, prepare_allow_list, count, true);
}

/// Measure token-update deny-list insertion cost as operation count grows.
#[divan::bench(args = COUNTS)]
fn token_update_deny_list(bencher: Bencher, count: usize) {
    bench(bencher, prepare_deny_list, count, false);
}

/// Measure meta-update deny-list insertion cost as operation count grows.
#[divan::bench(args = COUNTS)]
fn meta_update_deny_list(bencher: Bencher, count: usize) {
    bench(bencher, prepare_deny_list, count, true);
}

/// Encode lock or token operations as a meta-update payload.
///
/// # Arguments
///
/// - `operations`: Operations included in the transaction payload.
fn meta_payload(operations: Vec<MetaUpdateOperation>) -> Payload {
    Payload::MetaUpdate {
        payload: MetaUpdatePayload {
            operations: RawCbor::from(cbor::cbor_encode(&MetaUpdateOperations { operations })),
        },
    }
}

/// Build the simple lock configuration shared by lock benchmarks.
///
/// # Arguments
///
/// - `sender`: Account receiving the requested controller capabilities.
/// - `token_id`: Token accepted by the lock.
/// - `roles`: Controller capabilities granted to the sender.
/// - `keep_alive`: Whether an empty lock remains in state.
fn lock_config(
    sender: concordium_base::base::AccountIndex,
    token_id: TokenId,
    roles: Vec<LockControllerSimpleV0Capability>,
    keep_alive: bool,
) -> utils::CreateLockSimpleConfig {
    utils::CreateLockSimpleConfig {
        recipients: vec![sender],
        grants: vec![LockControllerSimpleV0Grant::new(sender, roles)],
        tokens: vec![token_id],
        expiry: 1_804_806_000,
        keep_alive,
    }
}

/// Prepare one funded token and one lock for state-transition benchmarks.
///
/// # Arguments
///
/// - `roles`: Controller capabilities granted to the sender.
/// - `keep_alive`: Whether the prepared lock remains after its balances are drained.
fn lock_fixture(
    roles: Vec<LockControllerSimpleV0Capability>,
    keep_alive: bool,
) -> (
    StubbedEntityContext,
    BlockStateLatest,
    concordium_base::base::AccountIndex,
    TokenId,
    LockId,
) {
    let mut context = entity_test_stub::new_stubbed_context();
    let mut state = BlockStateLatest::default();
    let sender = context.external.create_account().account_index();
    let token_id: TokenId = "benchmark-lock-token".parse().unwrap();
    utils::create_and_init_token_p11(
        &mut context,
        &mut state,
        token_id.clone(),
        TokenInitTestParams::default().mintable(),
        0,
        None,
    );
    utils::increment_account_balance_p11(
        &mut context,
        &mut state,
        sender,
        &token_id,
        RawTokenAmount::from(1_000),
    );
    let lock_id = LockId::new(sender, 1, 0);
    utils::create_lock(
        &mut context,
        &mut state,
        &lock_id,
        lock_config(sender, token_id.clone(), roles, keep_alive),
    );
    (context, state, sender, token_id, lock_id)
}

/// Prepare one meta-update containing multiple lock-create operations.
///
/// # Arguments
///
/// - `count`: Number of locks created by the transaction.
fn prepare_lock_create(count: usize) -> Fixture {
    let mut context = entity_test_stub::new_stubbed_context();
    let state = BlockStateLatest::default();
    let sender = context.external.create_account().account_index();
    let sender_address = context.external.account_canonical_address(sender);
    let config = LockConfig::SimpleV0(LockConfigSimpleV0 {
        recipients: LockRecipients::Any,
        expiry: 1_804_806_000.into(),
        grants: vec![],
        tokens: vec![],
        keep_alive: false,
        memo: None,
        metadata: None,
    });
    Fixture {
        context,
        state,
        sender,
        transaction_context: utils::simple_transaction_context(sender_address),
        payload: meta_payload(
            (0..count)
                .map(|_| meta_lock_create(config.clone()))
                .collect(),
        ),
    }
}

/// Prepare a lock-fund operation with optional existing balance reference.
///
/// # Arguments
///
/// - `existing_reference`: Whether setup first funds the same `(account, token)` pair.
fn prepare_lock_fund(existing_reference: bool) -> Fixture {
    let (mut context, mut state, sender, token_id, lock_id) =
        lock_fixture(vec![LockControllerSimpleV0Capability::Fund], true);
    if existing_reference {
        let sender_address = context.external.account_canonical_address(sender);
        let payload = meta_payload(vec![lock_fund(
            token_id.clone(),
            lock_id.clone(),
            TokenAmount::from_raw(100, 0),
            None,
        )]);
        let result = state.execute_transaction(
            &mut context,
            utils::simple_transaction_context(sender_address),
            sender,
            payload,
        );
        assert!(
            matches!(result, Ok(ref result) if matches!(result.outcome, TransactionOutcome::Success(_)))
        );
    }
    Fixture {
        transaction_context: utils::simple_transaction_context(
            context.external.account_canonical_address(sender),
        ),
        payload: meta_payload(vec![lock_fund(
            token_id,
            lock_id,
            TokenAmount::from_raw(100, 0),
            None,
        )]),
        context,
        state,
        sender,
    }
}

/// Prepare a lock send or return against a funded lock.
///
/// # Arguments
///
/// - `return_funds`: Whether to return funds to their owner instead of sending them.
/// - `drain`: Whether the operation consumes the complete balance and removes its reference.
fn prepare_lock_transfer(return_funds: bool, drain: bool) -> Fixture {
    let capability = if return_funds {
        LockControllerSimpleV0Capability::Release
    } else {
        LockControllerSimpleV0Capability::Send
    };
    let (mut context, mut state, sender, token_id, lock_id) = lock_fixture(
        vec![LockControllerSimpleV0Capability::Fund, capability],
        true,
    );
    let sender_address = context.external.account_canonical_address(sender);
    let result = state.execute_transaction(
        &mut context,
        utils::simple_transaction_context(sender_address),
        sender,
        meta_payload(vec![lock_fund(
            token_id.clone(),
            lock_id.clone(),
            TokenAmount::from_raw(200, 0),
            None,
        )]),
    );
    assert!(
        matches!(result, Ok(ref result) if matches!(result.outcome, TransactionOutcome::Success(_)))
    );
    let amount = TokenAmount::from_raw(if drain { 200 } else { 100 }, 0);
    let operation = if return_funds {
        lock_return(
            token_id.clone(),
            lock_id.clone(),
            sender_address,
            amount,
            None,
        )
    } else {
        lock_send(
            token_id.clone(),
            lock_id.clone(),
            sender_address,
            sender_address,
            amount,
            None,
        )
    };
    Fixture {
        transaction_context: utils::simple_transaction_context(sender_address),
        payload: meta_payload(vec![operation]),
        context,
        state,
        sender,
    }
}

/// Measure lock creation and state growth as operation count increases.
#[divan::bench(args = COUNTS)]
fn lock_create(bencher: Bencher, count: usize) {
    bencher
        .with_inputs(|| prepare_lock_create(count))
        .bench_local_values(execute);
}

/// Measure funding that inserts the first `(account, token)` balance reference.
#[divan::bench]
fn lock_fund_absent_reference(bencher: Bencher) {
    bencher
        .with_inputs(|| prepare_lock_fund(false))
        .bench_local_values(execute);
}

/// Measure funding when the `(account, token)` balance reference already exists.
#[divan::bench]
fn lock_fund_existing_reference(bencher: Bencher) {
    bencher
        .with_inputs(|| prepare_lock_fund(true))
        .bench_local_values(execute);
}

/// Measure a partial lock send that retains its balance reference.
#[divan::bench]
fn lock_send_partial_reference_retained(bencher: Bencher) {
    bencher
        .with_inputs(|| prepare_lock_transfer(false, false))
        .bench_local_values(execute);
}

/// Measure a draining lock send that removes its balance reference.
#[divan::bench]
fn lock_send_draining_reference_removed(bencher: Bencher) {
    bencher
        .with_inputs(|| prepare_lock_transfer(false, true))
        .bench_local_values(execute);
}

/// Measure a partial lock return that retains its balance reference.
#[divan::bench]
fn lock_return_partial_reference_retained(bencher: Bencher) {
    bencher
        .with_inputs(|| prepare_lock_transfer(true, false))
        .bench_local_values(execute);
}

/// Measure a draining lock return that removes its balance reference.
#[divan::bench]
fn lock_return_draining_reference_removed(bencher: Bencher) {
    bencher
        .with_inputs(|| prepare_lock_transfer(true, true))
        .bench_local_values(execute);
}
