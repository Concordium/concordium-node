//! [Execution time block state](ExecutionTimeBlockState) where externally (Haskell)
//! managed block stated is stubbed.

// Allow items in this file to be unused. This is needed because it is imported from multiple
// compile targets (each of the integration tests), and some of the targets may not use all
// items in the file.
#![allow(unused)]

use assert_matches::assert_matches;
use concordium_base::base::{AccountIndex, Energy, ProtocolVersion};
use concordium_base::common;
use concordium_base::common::cbor;
use concordium_base::common::types::TransactionTime;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, MetadataUrl, RawCbor, TokenAmount, TokenId,
    TokenModuleInitializationParameters, TokenModuleState, TokenOperation, TokenOperationsPayload,
    TokenPauseDetails, TokenSupplyUpdateDetails, TokenTransfer,
};
use concordium_base::transactions::Payload;
use concordium_base::updates::{CreatePlt, UpdatePayload};
use plt_block_state::block_state::blob_store::test_stub::UnreachableBlobStore;
use plt_block_state::block_state::blob_store::{BlobStoreLoad, BlobStoreLocation};
use plt_block_state::block_state::external::{
    ExternalBlockStateOperations, ExternalBlockStateQuery,
};
use plt_block_state::block_state::types::protocol_level_locks::LockConfiguration;
use plt_block_state::block_state::types::protocol_level_tokens::{
    TokenAccountState, TokenIndex, TokenStateKey, TokenStateValue,
};
use plt_block_state::block_state::{BlockState, ExecutionTimeBlockState, MutableBlockState};
use plt_block_state::block_state_interface::{
    AccountNotFoundByAddressError, AccountNotFoundByIndexError, BlockStateOperations,
    BlockStateQuery, OverflowError, RawTokenAmountDelta, TokenNotFoundByIdError,
};
use plt_scheduler::{TOKEN_MODULE_REF, queries, scheduler};
use plt_scheduler_types::types::events::BlockItemEvent;
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::locks::{
    LockControllerConfig, LockControllerSimpleV0, LockControllerSimpleV0Grant,
};
use plt_scheduler_types::types::tokens::RawTokenAmount;
use std::collections::BTreeMap;

// --- Token module key-value state encoding for locked balances ----------------------
//
// The constants below MUST stay in sync with `plt-scheduler/src/token_module/
// key_value_state.rs`, which owns the canonical encoding. We duplicate them here
// rather than expose them publicly because (a) the encoding is internal to the
// scheduler crate today and (b) this whole helper is a stand-in until COR-2305
// wires lock-balance updates through a real transaction payload, at which point the
// duplicated constants disappear.

const TOKEN_KV_ACCOUNT_STATE_PREFIX: [u8; 2] = 40307u16.to_le_bytes();
const TOKEN_KV_ACCOUNT_STATE_KEY_QUANTA: &[u8] = b"quanta";

type ExecutionTimeBlockStateWithExternalStubbed =
    ExecutionTimeBlockState<MutableBlockState, UnreachableBlobStore, ExternalBlockStateStub>;
type Token = <ExecutionTimeBlockStateWithExternalStubbed as BlockStateQuery>::Token;

/// Block state where external interactions with the Haskell maintained block
/// state is implemented by a stub.
#[derive(Debug)]
pub struct BlockStateWithExternalStateStubbed {
    block_state: ExecutionTimeBlockStateWithExternalStubbed,
}

/// Stubbed block state representing the Haskell maintained part of the block state.
#[derive(Debug)]
pub struct ExternalBlockStateStub {
    /// List of accounts in the stub.
    accounts: Vec<Account<Token>>,
    /// PLT update instruction sequence number
    plt_update_instruction_sequence_number: u64,
}

/// Internal representation of an account in [`BlockStateWithExternalStateStubbed`].
#[derive(Debug)]
pub struct Account<Token> {
    /// The index of the account
    index: AccountIndex,
    /// The canonical account address of the account.
    address: AccountAddress,
    /// Tokens the account is holding
    tokens: BTreeMap<Token, AccountToken>,
}

/// Internal representation of a token in an account.
#[derive(Debug, Default)]
struct AccountToken {
    /// Account balance
    balance: RawTokenAmount,
}

impl BlockStateWithExternalStateStubbed {
    /// Create fresh block state stub
    pub fn new(protocol_version: ProtocolVersion) -> Self {
        let inner_block_state = BlockState::empty().into_mutable();

        let external_block_state = ExternalBlockStateStub {
            accounts: Default::default(),
            plt_update_instruction_sequence_number: 0,
        };

        let block_state = ExecutionTimeBlockState {
            protocol_version,
            internal_block_state: inner_block_state,
            blob_store_load: UnreachableBlobStore,
            external_block_state,
        };

        Self { block_state }
    }

    /// Access to the underlying block state.
    pub fn state(&self) -> &ExecutionTimeBlockStateWithExternalStubbed {
        &self.block_state
    }

    /// Mutable access to the underlying block state.
    pub fn state_mut(&mut self) -> &mut ExecutionTimeBlockStateWithExternalStubbed {
        &mut self.block_state
    }

    /// Create account in the stub and return stub representation of the account.
    pub fn create_account(&mut self) -> AccountIndex {
        let index = self.block_state.external_block_state.accounts.len();
        let mut address = AccountAddress([0u8; 32]);
        address.0[..8].copy_from_slice(&index.to_be_bytes());
        let account_index = AccountIndex::from(index as u64);
        let account = Account {
            index: account_index,
            address,
            tokens: Default::default(),
        };
        let stub_index = AccountIndex::from(index as u64);
        self.block_state.external_block_state.accounts.push(account);

        stub_index
    }

    pub fn account_canonical_address(&self, account: &AccountIndex) -> AccountAddress {
        self.block_state.external_block_state.accounts[account.index as usize].address
    }

    /// Create and initialize token in the stub and return stub representation of the token, together with
    /// the governance account.
    pub fn create_and_init_token(
        &mut self,
        token_id: TokenId,
        params: TokenInitTestParams,
        decimals: u8,
        initial_supply: Option<RawTokenAmount>,
    ) -> (Token, AccountIndex) {
        let gov_account = self.create_account();
        let gov_holder_account =
            CborHolderAccount::from(self.account_canonical_address(&gov_account));
        let metadata = MetadataUrl::from("https://plt.token".to_string());
        let parameters = TokenModuleInitializationParameters {
            name: Some("Protocol-level token".to_owned()),
            metadata: Some(metadata.clone()),
            governance_account: Some(gov_holder_account.clone()),
            allow_list: params.allow_list,
            deny_list: params.deny_list,
            initial_supply: initial_supply.map(|raw| TokenAmount::from_raw(raw.0, decimals)),
            mintable: params.mintable,
            burnable: params.burnable,
        };
        let initialization_parameters = cbor::cbor_encode(&parameters).into();

        let payload = UpdatePayload::CreatePlt(CreatePlt {
            token_id: token_id.clone(),
            token_module: TOKEN_MODULE_REF,
            decimals,
            initialization_parameters,
        });
        scheduler::execute_chain_update(self.state_mut(), payload)
            .expect("create and initialize token");

        let token = self.state().token_by_id(&token_id).expect("created token");

        (token, gov_account)
    }

    /// Add amount to account balance in the stub. This is done by minting
    /// and transferring the given amount
    pub fn increment_account_balance(
        &mut self,
        account: AccountIndex,
        token: Token,
        balance: RawTokenAmount,
    ) {
        let token_configuration = self.state().token_configuration(&token);
        let operations = vec![
            TokenOperation::Mint(TokenSupplyUpdateDetails {
                amount: TokenAmount::from_raw(balance.0, token_configuration.decimals),
            }),
            TokenOperation::Transfer(TokenTransfer {
                amount: TokenAmount::from_raw(balance.0, token_configuration.decimals),
                recipient: CborHolderAccount::from(self.account_canonical_address(&account)),
                memo: None,
            }),
        ];
        let payload = TokenOperationsPayload {
            token_id: token_configuration.token_id.clone(),
            operations: RawCbor::from(cbor::cbor_encode(&operations)),
        };

        let token_info =
            queries::query_token_info(self.state(), &token_configuration.token_id).unwrap();
        let token_module_state: TokenModuleState =
            cbor::cbor_decode(&token_info.state.module_state).unwrap();
        let gov_account = self
            .state()
            .account_by_address(
                &token_module_state
                    .governance_account
                    .as_ref()
                    .unwrap()
                    .address,
            )
            .unwrap();

        let outcome = scheduler::execute_transaction(
            gov_account,
            token_module_state.governance_account.unwrap().address,
            self.state_mut(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
        assert_matches!(outcome.outcome, TransactionOutcome::Success(_));
    }

    /// Pause the given token as the governance account. Panics if the operation fails.
    pub fn pause_token(&mut self, token_id: &TokenId, gov_account: AccountIndex) {
        let operations = vec![TokenOperation::Pause(TokenPauseDetails {})];
        let payload = TokenOperationsPayload {
            token_id: token_id.clone(),
            operations: RawCbor::from(cbor::cbor_encode(&operations)),
        };
        let gov_addr = self.account_canonical_address(&gov_account);
        let result = scheduler::execute_transaction(
            gov_account,
            gov_addr,
            self.state_mut(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
        assert_matches!(result.outcome, TransactionOutcome::Success(_));
    }

    /// Unpause the given token as the governance account. Panics if the operation fails.
    pub fn unpause_token(&mut self, token_id: &TokenId, gov_account: AccountIndex) {
        let operations = vec![TokenOperation::Unpause(TokenPauseDetails {})];
        let payload = TokenOperationsPayload {
            token_id: token_id.clone(),
            operations: RawCbor::from(cbor::cbor_encode(&operations)),
        };
        let gov_addr = self.account_canonical_address(&gov_account);
        let result = scheduler::execute_transaction(
            gov_account,
            gov_addr,
            self.state_mut(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
        assert_matches!(result.outcome, TransactionOutcome::Success(_));
    }

    /// Execute token operations as the given sender account. Returns the block item events on
    /// success, panics if the transaction fails.
    pub fn execute_token_operations(
        &mut self,
        token_id: &TokenId,
        sender: AccountIndex,
        operations: Vec<TokenOperation>,
    ) -> Vec<BlockItemEvent> {
        let payload = TokenOperationsPayload {
            token_id: token_id.clone(),
            operations: RawCbor::from(cbor::cbor_encode(&operations)),
        };
        let sender_addr = self.account_canonical_address(&sender);
        let result = scheduler::execute_transaction(
            sender,
            sender_addr,
            self.state_mut(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
        assert_matches!(result.outcome, TransactionOutcome::Success(events) => events)
    }

    /// Return protocol-level token update instruction sequence number
    pub fn plt_update_instruction_sequence_number(&self) -> u64 {
        self.block_state
            .external_block_state
            .plt_update_instruction_sequence_number
    }

    /// Create a lock in the block state. The lock controller is hard-coded to the
    /// `SimpleV0` variant (the only one currently exposed) with `keep_alive = false`
    /// and no memo — individual tests may extend this helper if other variants are
    /// needed.
    ///
    /// TODO: (COR-2302) Once lock-creation transaction payloads land, this helper
    /// should construct and execute that payload via `scheduler::execute_transaction`
    /// (mirroring `create_and_init_token`) instead of poking `BlockStateOperations`
    /// directly, so the test path exercises the same scheduler entry point as
    /// production traffic.
    pub fn create_lock(
        &mut self,
        lock_id: &LockId,
        recipients: Vec<AccountIndex>,
        grants: Vec<LockControllerSimpleV0Grant>,
        tokens: Vec<TokenId>,
        expiry: u64,
    ) {
        let configuration = LockConfiguration {
            recipients,
            expiry: TransactionTime::from(expiry),
            controller: LockControllerConfig::SimpleV0(LockControllerSimpleV0 {
                grants,
                tokens,
                keep_alive: false,
                memo: None,
            }),
        };
        self.block_state.create_lock(lock_id, &configuration);
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
        &mut self,
        lock_id: &LockId,
        account: &AccountIndex,
        token: &Token,
        amount: RawTokenAmount,
    ) {
        // 1. Register the (account, token) pair with the lock.
        self.block_state
            .add_lock_balance_ref(lock_id, account, token);

        // 2. Write the locked amount into the token-module key-value state.
        let mut kv_state = self.block_state.mutable_token_key_value_state(token);
        let key = locked_balance_kv_key(*account, lock_id);
        let value = if amount == RawTokenAmount(0) {
            None
        } else {
            Some(TokenStateValue(common::to_bytes(&amount)))
        };
        self.block_state
            .update_token_state_value(&mut kv_state, &key, value);
        self.block_state.set_token_key_value_state(token, kv_state);
    }
}

/// Build the token-module key-value key under which the locked balance for `(account,
/// lock)` is stored. Mirrors `account_state_key(account, account_quanta_state_key(lock))`
/// in `plt-scheduler/src/token_module/key_value_state.rs`.
fn locked_balance_kv_key(account: AccountIndex, lock: &LockId) -> TokenStateKey {
    use concordium_base::common::Serial;
    let mut key = Vec::with_capacity(
        TOKEN_KV_ACCOUNT_STATE_PREFIX.len()
            + size_of::<AccountIndex>()
            + TOKEN_KV_ACCOUNT_STATE_KEY_QUANTA.len()
            + size_of::<LockId>(),
    );
    key.extend_from_slice(&TOKEN_KV_ACCOUNT_STATE_PREFIX);
    account.serial(&mut key);
    key.extend_from_slice(TOKEN_KV_ACCOUNT_STATE_KEY_QUANTA);
    lock.serial(&mut key);
    TokenStateKey(key)
}

#[derive(Debug, Clone, Default, Eq, PartialEq)]
pub struct TokenInitTestParams {
    allow_list: Option<bool>,
    deny_list: Option<bool>,
    mintable: Option<bool>,
    burnable: Option<bool>,
}

impl TokenInitTestParams {
    pub fn allow_list(self) -> Self {
        Self {
            allow_list: Some(true),
            ..self
        }
    }

    pub fn deny_list(self) -> Self {
        Self {
            deny_list: Some(true),
            ..self
        }
    }

    pub fn mintable(self) -> Self {
        Self {
            mintable: Some(true),
            ..self
        }
    }

    pub fn burnable(self) -> Self {
        Self {
            burnable: Some(true),
            ..self
        }
    }
}

impl ExternalBlockStateQuery for ExternalBlockStateStub {
    fn read_token_account_balance(
        &self,
        account: AccountIndex,
        token: TokenIndex,
    ) -> RawTokenAmount {
        self.accounts[account.index as usize]
            .tokens
            .get(&token)
            .map(|token| token.balance)
            .unwrap_or_default()
    }

    fn account_canonical_address_by_account_index(
        &self,
        account_index: AccountIndex,
    ) -> Result<AccountAddress, AccountNotFoundByIndexError> {
        if let Some(account) = self.accounts.get(account_index.index as usize) {
            Ok(account.address)
        } else {
            Err(AccountNotFoundByIndexError(account_index))
        }
    }

    fn account_index_by_account_address(
        &self,
        account_address: &AccountAddress,
    ) -> Result<AccountIndex, AccountNotFoundByAddressError> {
        self.accounts
            .iter()
            .enumerate()
            .find_map(|(i, account)| {
                if account.address.is_alias(account_address) {
                    Some(AccountIndex::from(i as u64))
                } else {
                    None
                }
            })
            .ok_or(AccountNotFoundByAddressError(*account_address))
    }

    fn token_account_states(
        &self,
        account_index: AccountIndex,
    ) -> Vec<(TokenIndex, TokenAccountState)> {
        self.accounts[account_index.index as usize]
            .tokens
            .iter()
            .map(|(token, state)| {
                let token_account_state = TokenAccountState {
                    balance: state.balance,
                };

                (*token, token_account_state)
            })
            .collect()
    }
}

impl ExternalBlockStateOperations for ExternalBlockStateStub {
    fn update_token_account_balance(
        &mut self,
        account: AccountIndex,
        token: TokenIndex,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError> {
        let balance = &mut self.accounts[account.index as usize]
            .tokens
            .entry(token)
            .or_default()
            .balance;
        match amount_delta {
            RawTokenAmountDelta::Add(add) => {
                balance.0 = balance.0.checked_add(add.0).ok_or(OverflowError)?;
            }
            RawTokenAmountDelta::Subtract(subtract) => {
                balance.0 = balance.0.checked_sub(subtract.0).ok_or(OverflowError)?;
            }
        }
        Ok(())
    }

    fn touch_token_account(&mut self, account: AccountIndex, token: TokenIndex) {
        self.accounts[account.index as usize]
            .tokens
            .entry(token)
            .or_default();
    }

    fn increment_plt_update_sequence_number(&mut self) {
        self.plt_update_instruction_sequence_number += 1;
    }
}
