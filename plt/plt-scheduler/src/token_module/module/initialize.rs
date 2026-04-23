use crate::token_context::TokenOperationContext;
use crate::token_module::errors::{
    MintWouldOverflowError, TokenAmountDecimalsMismatchError, TokenMintError,
    TokenStateInvariantError,
};
use crate::token_module::key_value_state;
use crate::token_module::{roles, util};
use concordium_base::common::cbor::CborSerializationError;
use concordium_base::protocol_level_tokens::{
    RawCbor, TokenAdminRole, TokenModuleInitializationParameters,
};
use plt_block_state::block_state_interface::{AccountNotFoundByAddressError, BlockStateOperations};

/// Represents the reasons why [`initialize_token`] can fail.
#[derive(Debug, thiserror::Error)]
pub enum TokenInitializationError {
    #[error("Token initialization parameters could not be deserialized: {0}")]
    InvalidInitializationParameters(String),
    #[error("CBOR serialization error during token initialization: {0}")]
    CborSerialization(#[from] CborSerializationError),
    #[error("The given governance account does not exist: {0}")]
    GovernanceAccountDoesNotExist(#[from] AccountNotFoundByAddressError),
    #[error("The initial mint amount has wrong number of decimals: {0}")]
    MintAmountDecimalsMismatch(#[from] TokenAmountDecimalsMismatchError),
    #[error("The initial mint amount is not representable: {0}")]
    MintAmountNotRepresentable(#[from] MintWouldOverflowError),
    #[error("State invariant violation at token initialization: {0}")]
    StateInvariantViolation(#[from] TokenStateInvariantError),
}

impl From<TokenMintError> for TokenInitializationError {
    fn from(err: TokenMintError) -> Self {
        match err {
            TokenMintError::StateInvariantViolation(err) => Self::StateInvariantViolation(err),
            TokenMintError::MintWouldOverflow(err) => Self::MintAmountNotRepresentable(err),
        }
    }
}

/// Initialize a PLT by recording the relevant configuration parameters in the state and
/// (if necessary) minting the initial supply to the token governance account.
pub fn initialize_token<BSO: BlockStateOperations>(
    context: &mut TokenOperationContext<'_, BSO>,
    initialization_parameters_cbor: RawCbor,
) -> Result<(), TokenInitializationError> {
    let init_params: TokenModuleInitializationParameters =
        util::cbor_decode(&initialization_parameters_cbor)?;
    initialize_token_impl(context, init_params)
}

fn initialize_token_impl<BSO: BlockStateOperations>(
    context: &mut TokenOperationContext<'_, BSO>,
    init_params: TokenModuleInitializationParameters,
) -> Result<(), TokenInitializationError> {
    let name = init_params.name.ok_or_else(|| {
        TokenInitializationError::InvalidInitializationParameters(
            "Token name is missing".to_string(),
        )
    })?;
    let metadata = init_params.metadata.ok_or_else(|| {
        TokenInitializationError::InvalidInitializationParameters(
            "Token metadata is missing".to_string(),
        )
    })?;
    let cbor_governance_account = init_params.governance_account.ok_or_else(|| {
        TokenInitializationError::InvalidInitializationParameters(
            "Token governance account is missing".to_string(),
        )
    })?;
    key_value_state::set_token_name(context, name);
    key_value_state::set_metadata_url(context, &metadata);

    // The governance account should hold every role, except for disabled features, so we build a
    // list of every enabled role and the mandatory roles.
    let mut enabled_roles = Vec::from(roles::UNIVERSAL_ROLES);

    if init_params.allow_list == Some(true) {
        key_value_state::set_allow_list_enabled(context);
        enabled_roles.push(TokenAdminRole::UpdateAllowList);
    }
    if init_params.deny_list == Some(true) {
        key_value_state::set_deny_list_enabled(context);
        enabled_roles.push(TokenAdminRole::UpdateDenyList);
    }
    if init_params.mintable == Some(true) {
        key_value_state::set_mintable_enabled(context);
        enabled_roles.push(TokenAdminRole::Mint);
    }
    if init_params.burnable == Some(true) {
        key_value_state::set_burnable_enabled(context);
        enabled_roles.push(TokenAdminRole::Burn);
    }

    let governance_account = context
        .block_state
        .account_by_address(&cbor_governance_account.address)?;
    let governance_account_index = context.block_state.account_index(&governance_account);
    key_value_state::set_governance_account(context, governance_account_index);
    if context.support_rbac() {
        key_value_state::assign_account_roles(context, governance_account_index, &enabled_roles)?;
    }
    if let Some(initial_supply) = init_params.initial_supply {
        let mint_amount = util::to_raw_token_amount(context.token_configuration, initial_supply)?;

        context.mint(
            &governance_account,
            cbor_governance_account.address,
            mint_amount,
        )?;
    }
    Ok(())
}
