use super::entity_traits::scheduler::SchedulerOperations;
use assert_matches::assert_matches;
use concordium_base::base::{AccountIndex, Energy};
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{
    CborHolderAccount, MetadataUrl, RawCbor, TokenAmount, TokenId,
    TokenModuleInitializationParameters, TokenModuleRejectReason, TokenModuleRejectReasonType,
    TokenModuleState, TokenOperation, TokenOperationsPayload, TokenPauseDetails,
    TokenSupplyUpdateDetails, TokenTransfer,
};
use concordium_base::transactions::Payload;
use concordium_base::updates::{CreatePlt, UpdatePayload};
use plt_block_state::entity::EntityContext;
use plt_block_state::entity::accounts::Account;
use plt_block_state::entity::block_state::Accounts;
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::entity_test_stub::StubbedExternalBlockStateTypes;
use plt_scheduler::TOKEN_MODULE_REF;
use plt_scheduler_types::types::events::BlockItemEvent;
use plt_scheduler_types::types::execution::TransactionOutcome;
use plt_scheduler_types::types::reject_reasons::{
    EncodedTokenModuleRejectReason, TransactionRejectReason,
};
use plt_scheduler_types::types::tokens::RawTokenAmount;

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

/// Create and initialize token in the stub. Returns the governance account for the token.
pub fn create_and_init_token(
    context: &mut EntityContext<StubbedExternalBlockStateTypes>,
    block_state: &mut impl SchedulerOperations,
    token_id: TokenId,
    params: TokenInitTestParams,
    decimals: u8,
    initial_supply: Option<RawTokenAmount>,
) -> Account {
    let gov_account = context.external.create_account();
    let gov_holder_account = CborHolderAccount::from(
        context
            .external
            .account_canonical_address(gov_account.account_index()),
    );
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
    block_state
        .execute_chain_update(context, payload)
        .expect("create and initialize token");

    gov_account
}

// todo ar make generic?

/// Add amount to account balance in the stub. This is done by minting
/// and transferring the given amount
pub fn increment_account_balance_p11(
    context: &mut EntityContext<StubbedExternalBlockStateTypes>,
    block_state: &mut BlockStateP11,
    account_index: AccountIndex,
    token_id: &TokenId,
    balance: RawTokenAmount,
) {
    let token = block_state
        .token_by_id(context, token_id)
        .unwrap()
        .expect("created token");
    let token_configuration = token.token_p9.token_configuration(context).unwrap();
    let operations = vec![
        TokenOperation::Mint(TokenSupplyUpdateDetails {
            amount: TokenAmount::from_raw(balance.0, token_configuration.decimals),
        }),
        TokenOperation::Transfer(TokenTransfer {
            amount: TokenAmount::from_raw(balance.0, token_configuration.decimals),
            recipient: CborHolderAccount::from(
                context.external.account_canonical_address(account_index),
            ),
            memo: None,
        }),
    ];
    let payload = TokenOperationsPayload {
        token_id: token_configuration.token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };

    let token_info = block_state
        .query_token_info(context, &token_configuration.token_id)
        .unwrap();
    let token_module_state: TokenModuleState =
        cbor::cbor_decode(&token_info.state.module_state).unwrap();
    let gov_account = block_state
        .account_by_address(
            context,
            &token_module_state
                .governance_account
                .as_ref()
                .unwrap()
                .address,
        )
        .unwrap();

    let outcome = block_state
        .execute_transaction(
            context,
            gov_account.account_index(),
            token_module_state.governance_account.unwrap().address,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    assert_matches!(outcome.outcome, TransactionOutcome::Success(_));
}

/// Pause the given token as the governance account. Panics if the operation fails.
pub fn pause_token(
    context: &mut EntityContext<StubbedExternalBlockStateTypes>,
    block_state: &mut impl SchedulerOperations,
    token_id: &TokenId,
    gov_account: AccountIndex,
) {
    let operations = vec![TokenOperation::Pause(TokenPauseDetails {})];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let gov_addr = context.external.account_canonical_address(gov_account);
    let result = block_state
        .execute_transaction(
            context,
            gov_account,
            gov_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));
}

/// Unpause the given token as the governance account. Panics if the operation fails.
pub fn unpause_token(
    context: &mut EntityContext<StubbedExternalBlockStateTypes>,
    block_state: &mut impl SchedulerOperations,
    token_id: &TokenId,
    gov_account: AccountIndex,
) {
    let operations = vec![TokenOperation::Unpause(TokenPauseDetails {})];
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let gov_addr = context.external.account_canonical_address(gov_account);
    let result = block_state
        .execute_transaction(
            context,
            gov_account,
            gov_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(_));
}

/// Execute token operations as the given sender account. Returns the block item events on
/// success, panics if the transaction fails.
pub fn execute_token_operations(
    context: &mut EntityContext<StubbedExternalBlockStateTypes>,
    block_state: &mut impl SchedulerOperations,
    token_id: &TokenId,
    sender: AccountIndex,
    operations: Vec<TokenOperation>,
) -> Vec<BlockItemEvent> {
    let payload = TokenOperationsPayload {
        token_id: token_id.clone(),
        operations: RawCbor::from(cbor::cbor_encode(&operations)),
    };
    let sender_addr = context.external.account_canonical_address(sender);
    let result = block_state
        .execute_transaction(
            context,
            sender,
            sender_addr,
            1.into(),
            Payload::TokenUpdate { payload },
            Energy::from(u64::MAX),
        )
        .expect("transaction internal error");
    assert_matches!(result.outcome, TransactionOutcome::Success(events) => events)
}

fn decode_token_module_reject_reason(
    reject_reason: &EncodedTokenModuleRejectReason,
) -> TokenModuleRejectReason {
    let reject_reason_type =
        TokenModuleRejectReasonType::try_from_type_discriminator(&reject_reason.reason_type)
            .unwrap();
    TokenModuleRejectReason::decode_reject_reason(
        reject_reason_type,
        reject_reason.details.as_ref().unwrap(),
    )
    .unwrap()
}

pub fn assert_token_module_reject_reason(
    token_id: &TokenId,
    reject_reason: TransactionRejectReason,
) -> TokenModuleRejectReason {
    let reject_reason = assert_matches!(
        &reject_reason,
        TransactionRejectReason::TokenUpdateTransactionFailed(reject_reason) => reject_reason);
    assert_eq!(reject_reason.token_id, *token_id);
    decode_token_module_reject_reason(reject_reason)
}
