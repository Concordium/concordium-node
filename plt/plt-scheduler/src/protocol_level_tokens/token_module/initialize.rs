use crate::block_state_traits::accounts::AccountsT;
use crate::protocol_level_tokens::token_module::errors::{
    MintWouldOverflowError, TokenAmountDecimalsMismatchError,
};
use crate::protocol_level_tokens::token_module::{balance_operations, util};
use concordium_base::common::cbor::CborSerializationError;
use concordium_base::protocol_level_tokens::{TokenAdminRole, TokenModuleInitializationParameters};
use plt_block_state::entity::protocol_level_tokens::p11::TokenP11;
use plt_block_state::entity::protocol_level_tokens::p9::TokenP9;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::external::AccountNotFoundByAddressError;
use plt_block_state::failure::BlockStateResult;
use plt_scheduler_types::types::events::BlockItemEvent;

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
}

/// List roles which are unaffected by which features are enabled.
const UNIVERSAL_ROLES: &[TokenAdminRole] = &[
    TokenAdminRole::UpdateAdminRoles,
    TokenAdminRole::Pause,
    TokenAdminRole::UpdateMetadata,
];

/// Initialize a PLT by recording the relevant configuration parameters in the state and
/// (if necessary) minting the initial supply to the token governance account.
pub fn initialize_token_p9<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    accounts: &impl AccountsT,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut TokenP9,
    init_params: &TokenModuleInitializationParameters,
) -> BlockStateResult<Result<(), TokenInitializationError>> {
    let token_configuration = token.token_configuration(context)?;

    let Some(name) = init_params.name.as_ref() else {
        return Ok(Err(
            TokenInitializationError::InvalidInitializationParameters(
                "Token name is missing".to_string(),
            ),
        ));
    };
    let Some(metadata) = init_params.metadata.as_ref() else {
        return Ok(Err(
            TokenInitializationError::InvalidInitializationParameters(
                "Token metadata is missing".to_string(),
            ),
        ));
    };
    let Some(cbor_governance_account) = init_params.governance_account.as_ref() else {
        return Ok(Err(
            TokenInitializationError::InvalidInitializationParameters(
                "Token governance account is missing".to_string(),
            ),
        ));
    };
    token.set_token_name(context, name)?;
    token.set_metadata_url(context, &metadata)?;

    if init_params.allow_list == Some(true) {
        token.set_allow_list_enabled(context)?;
    }
    if init_params.deny_list == Some(true) {
        token.set_deny_list_enabled(context)?;
    }
    if init_params.mintable == Some(true) {
        token.set_mintable_enabled(context)?;
    }
    if init_params.burnable == Some(true) {
        token.set_burnable_enabled(context)?;
    }

    let governance_account =
        match accounts.account_by_address(context, &cbor_governance_account.address) {
            Ok(account) => account,
            Err(err) => return Ok(Err(err.into())),
        };
    let governance_account_index = governance_account.account_index();
    token.set_governance_account(context, governance_account_index)?;
    if let Some(initial_supply) = init_params.initial_supply {
        let mint_amount = match util::to_raw_token_amount(&token_configuration, initial_supply) {
            Ok(amount) => amount,
            Err(err) => return Ok(Err(err.into())),
        };

        match balance_operations::mint(
            context,
            events,
            token,
            &governance_account,
            cbor_governance_account.address,
            mint_amount,
        )? {
            Ok(()) => (),
            Err(err) => {
                return Ok(Err(err.into()));
            }
        };
    }
    Ok(Ok(()))
}

/// Initialize a PLT by recording the relevant configuration parameters in the state and
/// (if necessary) minting the initial supply to the token governance account.
pub fn initialize_token_p11<C: EntityContextTypes>(
    context: &mut EntityContext<C>,
    accounts: &impl AccountsT,
    events: &mut impl Extend<BlockItemEvent>,
    token: &mut TokenP11,
    init_params: &TokenModuleInitializationParameters,
) -> BlockStateResult<Result<(), TokenInitializationError>> {
    match initialize_token_p9(context, accounts, events, &mut token.token_p9, init_params)? {
        Ok(()) => (),
        Err(err) => return Ok(Err(err)),
    }

    // The governance account should hold every role, except for disabled features, so we build a
    // list of every enabled role and the mandatory roles.
    let mut enabled_roles = Vec::from(UNIVERSAL_ROLES);

    if init_params.allow_list == Some(true) {
        enabled_roles.push(TokenAdminRole::UpdateAllowList);
    }
    if init_params.deny_list == Some(true) {
        enabled_roles.push(TokenAdminRole::UpdateDenyList);
    }
    if init_params.mintable == Some(true) {
        enabled_roles.push(TokenAdminRole::Mint);
    }
    if init_params.burnable == Some(true) {
        enabled_roles.push(TokenAdminRole::Burn);
    }

    let governance_account_index = token.token_p9.get_governance_account_index(context)?;
    token.assign_account_roles(context, governance_account_index, &enabled_roles)?;

    Ok(Ok(()))
}
