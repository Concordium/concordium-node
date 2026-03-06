// Allow items in this file to be unused. This is needed because it is imported from multiple
// compile targets (each of the integration tests), and some of the targets may not use all
// items in the file.
#![allow(unused)]

use assert_matches::assert_matches;
use concordium_base::base::{AccountIndex, Energy};
use concordium_base::common::cbor;
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, MetadataUrl, RawCbor, TokenAmount, TokenId,
    TokenModuleInitializationParameters, TokenModuleState, TokenOperation, TokenOperationsPayload,
    TokenSupplyUpdateDetails, TokenTransfer,
};
use concordium_base::transactions::Payload;
use concordium_base::updates::{CreatePlt, UpdatePayload};
use plt_block_state::block_state::blob_store::{self, BackingStoreLoad, Reference};
use plt_block_state::block_state::external::{
    ExternalBlockStateOperations, ExternalBlockStateQuery,
};
use plt_block_state::block_state::types::{TokenAccountState, TokenConfiguration, TokenIndex};
use plt_block_state::block_state::{
    p10, AccountNotFoundByAddressError, AccountNotFoundByIndexError, BlockStateOperations,
    BlockStateSavepoint,
};
use plt_block_state::block_state_interface::{
    OverflowError, RawTokenAmountDelta, TokenNotFoundByIdError,
};
use plt_scheduler::queries::SchedulerQueries;
use plt_scheduler::scheduler::SchedulerOperations;
use plt_scheduler::{queries, scheduler};
use plt_scheduler_interface::transaction_execution_interface::TransactionExecution;
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::tokens::RawTokenAmount;
use plt_token_module::TOKEN_MODULE_REF;
use std::collections::BTreeMap;

/// Block store load stub for tests.
#[derive(Debug)]
pub struct BlobStoreLoadStub;

impl BackingStoreLoad for BlobStoreLoadStub {
    fn load_raw(&mut self, location: Reference) -> Vec<u8> {
        unimplemented!("should not be called")
    }
}

/// Stubbed block state representing the Haskell maintained part of the block state.
#[derive(Debug)]
pub struct ExternalBlockStateStub {
    /// List of accounts in the stub.
    pub accounts: Vec<Account>,
    /// PLT update instruction sequence number
    pub plt_update_instruction_sequence_number: u64,
}

impl ExternalBlockStateStub {
    pub fn empty() -> Self {
        Self {
            accounts: Default::default(),
            plt_update_instruction_sequence_number: 0,
        }
    }

    /// Create account in the stub and return stub representation of the account.
    pub fn create_account(&mut self) -> AccountIndex {
        let index = self.accounts.len();
        let mut address = AccountAddress([0u8; 32]);
        address.0[..8].copy_from_slice(&index.to_be_bytes());
        let account_index = AccountIndex::from(index as u64);
        let account = Account {
            index: account_index,
            address,
            tokens: Default::default(),
        };
        let index = AccountIndex::from(index as u64);
        self.accounts.push(account);

        index
    }

    pub fn account_canonical_address(&self, account: &AccountIndex) -> AccountAddress {
        self.accounts[account.index as usize].address
    }
}

/// Internal representation of an account in [`BlockStateWithExternalStateStubbed`].
#[derive(Debug)]
pub struct Account {
    /// The index of the account
    index: AccountIndex,
    /// The canonical account address of the account.
    address: AccountAddress,
    /// Tokens the account is holding
    tokens: BTreeMap<TokenIndex, AccountToken>,
}

/// Internal representation of a token in an account.
#[derive(Debug, Default)]
struct AccountToken {
    /// Account balance
    balance: RawTokenAmount,
}

pub trait BlockStateTestImpl:
    SchedulerOperations + SchedulerQueries + BlockStateOperations
{
    /// Map the Token ID to the internal token index.
    fn token_by_id(&self, token_id: &TokenId) -> Result<TokenIndex, TokenNotFoundByIdError>;

    /// Create and initialize token in the stub and return stub representation of the token, together with
    /// the governance account.
    fn create_and_init_token(
        &mut self,
        external: &mut ExternalBlockStateStub,
        token_id: TokenId,
        params: TokenInitTestParams,
        decimals: u8,
        initial_supply: Option<RawTokenAmount>,
    ) -> (TokenIndex, AccountIndex) {
        let gov_account = external.create_account();
        let gov_holder_account =
            CborHolderAccount::from(external.account_canonical_address(&gov_account));
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
            additional: Default::default(),
        };
        let initialization_parameters = cbor::cbor_encode(&parameters).into();

        let payload = UpdatePayload::CreatePlt(CreatePlt {
            token_id: token_id.clone(),
            token_module: TOKEN_MODULE_REF,
            decimals,
            initialization_parameters,
        });

        self.execute_chain_update(external, payload)
            .expect("create and initialize token");

        let token_index = self.token_by_id(&token_id).expect("created token");

        (token_index, gov_account)
    }

    /// Add amount to account balance in the stub. This is done by minting
    /// and transferring the given amount
    fn increment_account_balance(
        &mut self,
        external: &mut ExternalBlockStateStub,
        loader: &mut impl blob_store::BackingStoreLoad,
        account: AccountIndex,
        token_id: &TokenId,
        balance: RawTokenAmount,
    ) {
        let token_info = self
            .query_token_info(external, &token_id)
            .expect("Token assumed to exists");
        let operations = vec![
            TokenOperation::Mint(TokenSupplyUpdateDetails {
                amount: TokenAmount::from_raw(balance.0, token_info.state.decimals),
            }),
            TokenOperation::Transfer(TokenTransfer {
                amount: TokenAmount::from_raw(balance.0, token_info.state.decimals),
                recipient: CborHolderAccount::from(external.account_canonical_address(&account)),
                memo: None,
            }),
        ];
        let payload = TokenOperationsPayload {
            token_id: token_id.clone(),
            operations: RawCbor::from(cbor::cbor_encode(&operations)),
        };
        let token_module_state: TokenModuleState =
            cbor::cbor_decode(&token_info.state.module_state).unwrap();
        let gov_account_address = token_module_state
            .governance_account
            .as_ref()
            .unwrap()
            .address;
        let gov_account = external
            .account_index_by_account_address(&gov_account_address)
            .unwrap();

        let mut execution =
            TransactionExecution::new(Energy::from(u64::MAX), gov_account, gov_account_address);
        let outcome = self
            .execute_transaction(
                &mut execution,
                loader,
                external,
                Payload::TokenUpdate { payload },
            )
            .expect("transaction internal error");
        assert_matches!(outcome.outcome, TransactionOutcome::Success(_));
    }
}

impl BlockStateTestImpl for p10::PltBlockStateP10 {
    fn token_by_id(&self, token_id: &TokenId) -> Result<TokenIndex, TokenNotFoundByIdError> {
        self.tokens.token_by_id(token_id)
    }
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

    fn increment_plt_update_sequence_number(&mut self) {
        self.plt_update_instruction_sequence_number += 1;
    }
}

/// Test looking up account by alias.
#[test]
fn test_account_by_alias() {
    let mut external = ExternalBlockStateStub::empty();

    let account = external.create_account();
    let account_address = external.account_canonical_address(&account);
    let account_by_alias = external
        .account_index_by_account_address(&account_address.get_alias(0).unwrap())
        .unwrap();
    assert_eq!(account, account_by_alias);
    let account_by_alias = external
        .account_index_by_account_address(&account_address.get_alias(1).unwrap())
        .unwrap();
    assert_eq!(account, account_by_alias);
}
