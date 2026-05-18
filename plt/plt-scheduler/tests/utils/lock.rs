// /// Create a lock in the block state. The lock controller is hard-coded to the
// /// `SimpleV0` variant (the only one currently exposed) with `keep_alive = false`
// /// and no memo — individual tests may extend this helper if other variants are
// /// needed.
// ///
// /// TODO: (COR-2302) Once lock-creation transaction payloads land, this helper
// /// should construct and execute that payload via `scheduler::execute_transaction`
// /// (mirroring `create_and_init_token`) instead of poking `BlockStateOperations`
// /// directly, so the test path exercises the same scheduler entry point as
// /// production traffic.
// pub fn create_lock(
//     &mut self,
//     lock_id: &LockId,
//     recipients: Vec<AccountIndex>,
//     grants: Vec<LockControllerSimpleV0Grant>,
//     tokens: Vec<TokenId>,
//     expiry: u64,
// ) {
//     use concordium_base::protocol_level_locks::*;
//     use concordium_base::protocol_level_tokens::meta_operations::*;
//     let sender = self
//         .state()
//         .account_by_index(lock_id.account_index())
//         .expect("sender account must exist");
//     let resolve_account = |index: &AccountIndex| {
//         CborHolderAccount::from(
//             self.state()
//                 .account_by_index(*index)
//                 .unwrap_or_else(|_| panic!("account index {} does not exist", *index))
//                 .canonical_account_address,
//         )
//     };
//     let recipients = recipients.iter().map(resolve_account).collect();
//     let grants = grants
//         .iter()
//         .map(|grant| LockControllerSimpleV0Grant {
//             account: resolve_account(&grant.account),
//             roles: grant.roles.clone(),
//         })
//         .collect();
//     let operations = MetaUpdateOperations {
//         operations: vec![lock_create(
//             concordium_base::protocol_level_locks::LockConfig {
//                 recipients,
//                 expiry: TransactionTime::from(expiry),
//                 controller: LockController::SimpleV0(LockControllerSimpleV0 {
//                     grants,
//                     tokens,
//                     keep_alive: false,
//                     memo: None,
//                 }),
//             },
//         )],
//     };
//
//     scheduler::execute_transaction(
//         sender.account,
//         sender.canonical_account_address,
//         lock_id.sequence_number(),
//         self.state_mut(),
//         Payload::MetaUpdate {
//             payload: MetaUpdatePayload {
//                 operations: RawCbor::from(cbor::cbor_encode(&operations)),
//             },
//         },
//         Energy::from(u64::MAX),
//     )
//         .expect("create lock transaction must succeed");
// }
//
// /// Track a `(account, token)` balance reference under the given lock and record the
// /// locked `amount` for the account in the token-module key-value state.
// ///
// /// TODO: (COR-2305) Once lock-operation transaction payloads land (the ones that
// /// move balances into / out of locks), this helper should drive those payloads
// /// through `scheduler::execute_transaction` (mirroring `increment_account_balance`)
// /// instead of poking the block state and key-value store directly. At that point
// /// the constants duplicated above can also be removed.
// pub fn lock_balance(
//     &mut self,
//     lock_id: &LockId,
//     account: &AccountIndex,
//     token: &Token,
//     amount: RawTokenAmount,
// ) {
//     // 1. Register the (account, token) pair with the lock.
//     self.block_state
//         .add_lock_balance_ref(lock_id, account, token);
//
//     // 2. Write the locked amount into the token-module key-value state.
//     let mut kv_state = self.block_state.mutable_token_key_value_state(token);
//     let key = locked_balance_kv_key(*account, lock_id);
//     let value = if amount == RawTokenAmount(0) {
//         None
//     } else {
//         Some(TokenStateValue(common::to_bytes(&amount)))
//     };
//     self.block_state
//         .update_token_state_value(&mut kv_state, &key, value);
//     self.block_state.set_token_key_value_state(token, kv_state);
// }
//
//
// /// Build the token-module key-value key under which the locked balance for `(account,
// /// lock)` is stored. Mirrors `account_state_key(account, account_quanta_state_key(lock))`
// /// in `plt-scheduler/src/token_module/key_value_state.rs`.
// fn locked_balance_kv_key(account: AccountIndex, lock: &LockId) -> TokenStateKey {
//     use concordium_base::common::Serial;
//     let mut key = Vec::with_capacity(
//         TOKEN_KV_ACCOUNT_STATE_PREFIX.len()
//             + size_of::<AccountIndex>()
//             + TOKEN_KV_ACCOUNT_STATE_KEY_QUANTA.len()
//             + size_of::<LockId>(),
//     );
//     key.extend_from_slice(&TOKEN_KV_ACCOUNT_STATE_PREFIX);
//     account.serial(&mut key);
//     key.extend_from_slice(TOKEN_KV_ACCOUNT_STATE_KEY_QUANTA);
//     lock.serial(&mut key);
//     TokenStateKey(key)
// }
