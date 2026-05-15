use concordium_base::base::AccountIndex;
use concordium_base::protocol_level_tokens::TokenId;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::entity::block_state::p11::BlockStateP11;

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




/// Create and initialize token in the stub and return stub representation of the token, together with
/// the governance account.
pub fn create_and_init_token<C: EntityContextTypes>(
    context: &EntityContext<C>,
    block_state: &mut BlockStateP11,
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
        1.into(),
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
        1.into(),
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
        1.into(),
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
        1.into(),
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
