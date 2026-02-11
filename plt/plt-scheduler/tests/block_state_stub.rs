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
use plt_scheduler::block_state::blob_store::{BackingStoreLoad, Reference};
use plt_scheduler::block_state::external::{
    GetAccountIndexByAddress, GetCanonicalAddressByAccountIndex, GetTokenAccountStates,
    IncrementPltUpdateSequenceNumber, ReadTokenAccountBalance, UpdateTokenAccountBalance,
};
use plt_scheduler::block_state::types::{TokenAccountState, TokenConfiguration, TokenIndex};
use plt_scheduler::block_state::{
    ExecutionTimePltBlockState, ExternalBlockStateOperations, ExternalBlockStateQuery,
    PltBlockState, PltBlockStateSavepoint,
};
use plt_scheduler::block_state_interface::{
    BlockStateOperations, BlockStateQuery, OverflowError, RawTokenAmountDelta,
    TokenNotFoundByIdError,
};
use plt_scheduler::{queries, scheduler};
use plt_scheduler_interface::error::{AccountNotFoundByAddressError, AccountNotFoundByIndexError};
use plt_scheduler_interface::token_kernel_interface::{
    AccountWithCanonicalAddress, TokenStateKey, TokenStateValue,
};
use plt_token_module::TOKEN_MODULE_REF;
use plt_types::types::execution::TransactionOutcome;
use plt_types::types::tokens::RawTokenAmount;
use std::collections::BTreeMap;

/// Block store load stub for tests.
#[derive(Debug)]
pub struct BlobStoreLoadStub;

impl BackingStoreLoad for BlobStoreLoadStub {
    fn load_raw(&mut self, location: Reference) -> Vec<u8> {
        unimplemented!("should not be called")
    }
}

type ExecutionTimePltBlockStateWithExternalStateStubbed =
    ExecutionTimePltBlockState<PltBlockState, BlobStoreLoadStub, ExternalBlockStateStub>;
type Token = <ExecutionTimePltBlockStateWithExternalStateStubbed as BlockStateQuery>::Token;

/// Block state where external interactions with the Haskell maintained block
/// state is implemented by a stub.
#[derive(Debug)]
pub struct BlockStateWithExternalStateStubbed {
    block_state: ExecutionTimePltBlockStateWithExternalStateStubbed,
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
    /// Create block state stub
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let inner_block_state = PltBlockStateSavepoint::empty().mutable_state();

        let external_block_state = ExternalBlockStateStub {
            accounts: Default::default(),
            plt_update_instruction_sequence_number: 0,
        };

        let block_state = ExecutionTimePltBlockState {
            inner_block_state,
            backing_store_load: BlobStoreLoadStub,
            external_block_state,
        };

        Self { block_state }
    }

    /// Access to the underlying block state.
    pub fn state(&self) -> &ExecutionTimePltBlockStateWithExternalStateStubbed {
        &self.block_state
    }

    /// Mutable access to the underlying block state.
    pub fn state_mut(&mut self) -> &mut ExecutionTimePltBlockStateWithExternalStateStubbed {
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
            additional: Default::default(),
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

    /// Return protocol-level token update instruction sequence number
    pub fn plt_update_instruction_sequence_number(&self) -> u64 {
        self.block_state
            .external_block_state
            .plt_update_instruction_sequence_number
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

impl ReadTokenAccountBalance for ExternalBlockStateStub {
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
}

impl UpdateTokenAccountBalance for ExternalBlockStateStub {
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
}

impl IncrementPltUpdateSequenceNumber for ExternalBlockStateStub {
    fn increment_plt_update_sequence_number(&mut self) {
        self.plt_update_instruction_sequence_number += 1;
    }
}

impl GetCanonicalAddressByAccountIndex for ExternalBlockStateStub {
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
}

impl GetAccountIndexByAddress for ExternalBlockStateStub {
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
}

impl GetTokenAccountStates for ExternalBlockStateStub {
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

impl ExternalBlockStateQuery for ExternalBlockStateStub {}

impl ExternalBlockStateOperations for ExternalBlockStateStub {}

/// Test looking up account by alias.
#[test]
fn test_account_by_alias() {
    let mut stub = BlockStateWithExternalStateStubbed::new();

    let account = stub.create_account();
    let account_address = stub.account_canonical_address(&account);
    let account_by_alias = stub
        .state()
        .account_by_address(&account_address.get_alias(0).unwrap())
        .unwrap();

    assert_eq!(
        stub.state().account_index(&account),
        stub.state().account_index(&account_by_alias)
    );
}
