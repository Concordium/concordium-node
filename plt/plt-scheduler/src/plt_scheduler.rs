use crate::block_state_interface;
use crate::block_state_interface::{BlockStateOperations, BlockStateQuery, TransactionExecution};
use concordium_base::base::{AccountIndex, Energy};
use concordium_base::contracts_common::AccountAddress;
use concordium_base::protocol_level_tokens::TokenAmount;
use concordium_base::transactions::Memo;
use plt_token_module::token_kernel_interface::{AccountNotFoundByAddressError, AccountNotFoundByIndexError, AmountNotRepresentableError, InsufficientBalanceError,  ModuleStateKey, ModuleStateValue, OutOfEnergyError, RawTokenAmount, TokenKernelOperations, TokenKernelQueries, TokenModuleEvent};

pub struct TransactionRejectReason;

/// Token event
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenEvent {
    /// An event emitted by the token module.
    Module(TokenModuleEvent),
    /// An event emitted when a transfer of tokens is performed.
    Transfer(TokenTransferEvent),
    /// An event emitted when the token supply is updated by minting tokens to a
    /// token holder.
    Mint(TokenSupplyUpdateEvent),
    /// An event emitted when the token supply is updated by burning tokens from
    /// the balance of a token holder.
    Burn(TokenSupplyUpdateEvent),
}

/// An event emitted when a transfer of tokens from `from` to `to` is performed.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TokenTransferEvent {
    /// The token holder from which the tokens are transferred.
    pub from: AccountAddress,
    /// The token holder to which the tokens are transferred.
    pub to: AccountAddress,
    /// The amount of tokens transferred.
    pub amount: TokenAmount,
    /// An optional memo field that can be used to attach a message to the token
    /// transfer.
    pub memo: Option<Memo>,
}

/// An event emitted when the token supply is updated, i.e. by minting/burning
/// tokens to/from the balance of the `target`.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TokenSupplyUpdateEvent {
    /// The token holder the balance update is performed on.
    pub target: AccountAddress,
    /// The balance difference to be applied to the target.
    pub amount: TokenAmount,
}

/// Execute a transaction payload modifying `scheduler` and `block_state` accordingly.
/// Returns the events produce if successful otherwise a reject reason.
///
/// The caller must ensure to rollback state changes in case of the transaction being rejected.
pub fn execute_transaction(
    _scheduler: &mut impl TransactionExecution,
    _block_state: &mut impl BlockStateOperations,
    _payload: &[u8],
) -> Result<Vec<TokenEvent>, TransactionRejectReason> {
    todo!()
}

struct TokenKernelExecutionImpl<'a, BSQ: BlockStateQuery, TE:TransactionExecution> {
    block_state: &'a BSQ,
    transaction_execution: &'a TE,
    token: &'a BSQ::Token,
    token_module_state: &'a mut BSQ::MutableTokenModuleState,
}

impl<BSQ: BlockStateQuery, TE: TransactionExecution> TokenKernelQueries for TokenKernelExecutionImpl<'_, BSQ, TE> {
    type Account = BSQ::Account;

    fn account_by_address(
        &self,
        address: &AccountAddress,
    ) -> Result<Self::Account, AccountNotFoundByAddressError> {
        self.block_state.account_by_address(address).map_err(
            |block_state_interface::AccountNotFoundByAddressError(account_address)| {
                AccountNotFoundByAddressError(account_address)
            },
        )
    }

    fn account_by_index(
        &self,
        index: AccountIndex,
    ) -> Result<Self::Account, AccountNotFoundByIndexError> {
        self.block_state.account_by_index(index).map_err(
            |block_state_interface::AccountNotFoundByIndexError(index)| {
                AccountNotFoundByIndexError(index)
            },
        )
    }

    fn account_index(&self, account: &Self::Account) -> AccountIndex {
        self.block_state.account_index(account)
    }

    fn account_canonical_address(&self, account: &Self::Account) -> AccountAddress {
        self.block_state.account_canonical_address(account)
    }

    fn account_token_balance(&self, account: &Self::Account) -> RawTokenAmount {
        self.block_state.account_token_balance(account, self.token)
    }

    fn circulating_supply(&self) -> RawTokenAmount {
        self.block_state.token_circulating_supply(self.token)
    }

    fn decimals(&self) -> u8 {
        self.block_state.token_configuration(self.token).decimals
    }

    fn lookup_token_module_state_value(&self, key: ModuleStateKey) -> Option<ModuleStateValue> {
        self.block_state
            .lookup_token_module_state_value(self.token_module_state, &key)
    }
}

impl<BSO: BlockStateOperations, TE: TransactionExecution> TokenKernelOperations for TokenKernelExecutionImpl<'_, BSO, TE> {
    fn touch(&mut self, account: &Self::Account) -> bool {
        todo!()
    }

    fn mint(&mut self, account: &Self::Account, amount: RawTokenAmount) -> Result<(), AmountNotRepresentableError> {
        todo!()
    }

    fn burn(&mut self, account: &Self::Account, amount: RawTokenAmount) -> Result<(), InsufficientBalanceError> {
        todo!()
    }

    fn transfer(&mut self, from: &Self::Account, to: &Self::Account, amount: RawTokenAmount, memo: Option<Memo>) -> Result<(), InsufficientBalanceError> {
        todo!()
    }

    fn set_token_module_state_value(&mut self, key: ModuleStateKey, value: Option<ModuleStateValue>) {
        self.block_state
            .update_token_module_state_value(self.token_module_state, &key, value);
    }

    fn tick_energy(&mut self, energy: Energy) -> Result<(), OutOfEnergyError> {
        todo!()
    }

    fn log_token_event(&mut self, event: TokenModuleEvent) {
        todo!()
    }
}