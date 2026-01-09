use crate::module_state;
use crate::token_kernel_interface::TokenKernelQueries;
use crate::token_module::TokenModuleStateInvariantError;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{CborHolderAccount, RawCbor, TokenModuleState};

/// Represents the reasons why a query to the token module can fail.
#[derive(Debug, thiserror::Error)]
pub enum QueryTokenModuleError {
    #[error("Token module state invariant: {0}")]
    StateInvariantViolation(#[from] TokenModuleStateInvariantError),
}

/// Get the CBOR-encoded representation of the token module state.
pub fn query_token_module_state<TK: TokenKernelQueries>(
    kernel: &TK,
) -> Result<RawCbor, QueryTokenModuleError> {
    let state = query_token_module_state_impl(kernel)?;

    Ok(RawCbor::from(cbor::cbor_encode(&state)))
}

/// Get the CBOR-encoded representation of the token module account state.
pub fn query_account_state<TK: TokenKernelQueries>(
    _kernel: &TK,
    _account: TK::Account,
) -> Result<Option<RawCbor>, QueryTokenModuleError> {
    Ok(None)
}

/// Get the CBOR-encoded representation of the token module state.
fn query_token_module_state_impl<TK: TokenKernelQueries>(
    kernel: &TK,
) -> Result<TokenModuleState, QueryTokenModuleError> {
    let name = module_state::get_name(kernel)?;
    let metadata = module_state::get_metadata(kernel)?;
    let allow_list = module_state::has_allow_list(kernel);
    let deny_list = module_state::has_deny_list(kernel);
    let mintable = module_state::is_mintable(kernel);
    let burnable = module_state::is_burnable(kernel);
    let paused = module_state::is_paused(kernel);

    let governance_account_index = module_state::get_governance_account_index(kernel)?;
    let governance_account = kernel
        .account_by_index(governance_account_index)
        .map_err(|_| {
            TokenModuleStateInvariantError(format!(
                "Stored governance account with index {} does not exist",
                governance_account_index
            ))
        })?;
    let governance_account_address = kernel.account_canonical_address(&governance_account);

    let state = TokenModuleState {
        name: Some(name),
        metadata: Some(metadata),
        governance_account: Some(CborHolderAccount::from(governance_account_address)),
        allow_list: Some(allow_list),
        deny_list: Some(deny_list),
        mintable: Some(mintable),
        burnable: Some(burnable),
        paused: Some(paused),
        additional: Default::default(),
    };

    Ok(state)
}
