use crate::token_kernel_interface::TokenKernelQueries;
use crate::token_module;
use crate::token_module::{QueryTokenModuleError, TokenModuleStateInvariantError};
use concordium_base::protocol_level_tokens::{CborHolderAccount, TokenModuleState};

/// Get the CBOR-encoded representation of the token module state.
pub fn query_token_module_state<TK: TokenKernelQueries>(
    kernel: &TK,
) -> Result<TokenModuleState, QueryTokenModuleError> {
    let name = token_module::get_name(kernel)?;
    let metadata = token_module::get_metadata(kernel)?;
    let allow_list = token_module::has_allow_list(kernel);
    let deny_list = token_module::has_deny_list(kernel);
    let mintable = token_module::is_mintable(kernel);
    let burnable = token_module::is_burnable(kernel);
    let paused = token_module::is_paused(kernel);

    let governance_account_index = token_module::get_governance_account_index(kernel)?;
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
