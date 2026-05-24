use crate::utils::entity_traits::scheduler::SchedulerOperations;
use concordium_base::base::{AccountIndex, Energy};
use concordium_base::common::cbor;
use concordium_base::common::types::TransactionTime;
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::{CborHolderAccount, RawCbor, TokenId};
use concordium_base::transactions::Payload;
use plt_block_state::entity::EntityContext;
use plt_block_state::entity::block_state::Accounts;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::entity_test_stub::StubbedExternalBlockStateTypes;
use plt_block_state::persistent::protocol_level_locks::p11::LockControllerSimpleV0Grant;
use plt_scheduler_types::types::tokens::RawTokenAmount;

/// Create a lock in the block state. The lock controller is hard-coded to the
/// `SimpleV0` variant (the only one currently exposed) with `keep_alive = false`
/// and no memo — individual tests may extend this helper if other variants are
/// needed.
pub fn create_lock(
    context: &mut EntityContext<StubbedExternalBlockStateTypes>,
    block_state: &mut BlockStateP11,
    lock_id: &LockId,
    recipients: Vec<AccountIndex>,
    grants: Vec<LockControllerSimpleV0Grant>,
    tokens: Vec<TokenId>,
    expiry: u64,
) {
    use concordium_base::protocol_level_locks::*;
    use concordium_base::protocol_level_tokens::meta_operations::*;
    let sender = block_state
        .account_by_index(context, lock_id.account_index())
        .expect("sender account must exist");
    let resolve_account = |index: &AccountIndex| {
        CborHolderAccount::from(
            block_state
                .account_by_index(context, *index)
                .unwrap_or_else(|_| panic!("account index {} does not exist", *index))
                .canonical_account_address,
        )
    };
    let recipients = recipients.iter().map(resolve_account).collect();
    let grants = grants
        .iter()
        .map(|grant| LockControllerSimpleV0Grant {
            account: resolve_account(&grant.account),
            roles: grant.roles.clone(),
        })
        .collect();
    let operations = MetaUpdateOperations {
        operations: vec![lock_create(LockConfig {
            recipients,
            expiry: TransactionTime::from(expiry),
            controller: LockController::SimpleV0(LockControllerSimpleV0 {
                grants,
                tokens,
                keep_alive: false,
                memo: None,
            }),
        })],
    };

    block_state
        .execute_transaction(
            context,
            sender.account.account_index(),
            sender.canonical_account_address,
            lock_id.sequence_number(),
            Payload::MetaUpdate {
                payload: MetaUpdatePayload {
                    operations: RawCbor::from(cbor::cbor_encode(&operations)),
                },
            },
            Energy::from(u64::MAX),
        )
        .expect("create lock transaction must succeed");
}

/// Track a `(account, token)` balance reference under the given lock and record the
/// locked `amount` for the account in the token-module key-value state.
///
/// TODO: (COR-2305) Once lock-operation transaction payloads land (the ones that
/// move balances into / out of locks), this helper should drive those payloads
/// through `scheduler::execute_transaction` (mirroring `increment_account_balance`)
/// instead of poking the block state and key-value store directly. At that point
/// the constants duplicated above can also be removed.
pub fn lock_balance(
    context: &mut EntityContext<StubbedExternalBlockStateTypes>,
    block_state: &mut BlockStateP11,
    lock_id: &LockId,
    funder_account: AccountIndex,
    token_id: &TokenId,
    amount: RawTokenAmount,
) {
    // Get the token and determine its index
    let token = block_state
        .token_by_id(context, token_id)
        .unwrap()
        .expect("token must exist");
    let token_index = token.token_base.token_index();

    // Register the (account, token) pair in the lock state
    let mut lock = block_state
        .lock_by_id(context, lock_id)
        .unwrap()
        .expect("lock must exist");
    lock.add_lock_balance_ref(funder_account, token_index);
    block_state.update_lock(context, lock).unwrap();

    // Set the locked amount in the token module KV state
    let mut token = block_state
        .token_by_id(context, token_id)
        .unwrap()
        .expect("token must exist");
    token
        .set_locked_balance_for(context, funder_account, lock_id, amount)
        .unwrap();
    block_state.update_token(context, token).unwrap();
}
