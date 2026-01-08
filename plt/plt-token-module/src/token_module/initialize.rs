use crate::token_kernel_interface::TokenKernelOperations;
use crate::token_module;
use crate::token_module::{
    KernelOperationsExt, STATE_KEY_ALLOW_LIST, STATE_KEY_BURNABLE, STATE_KEY_DENY_LIST,
    STATE_KEY_GOVERNANCE_ACCOUNT, STATE_KEY_METADATA, STATE_KEY_MINTABLE, STATE_KEY_NAME,
    TokenInitializationError,
};
use concordium_base::common;
use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::TokenModuleInitializationParameters;

/// Initialize a PLT by recording the relevant configuration parameters in the state and
/// (if necessary) minting the initial supply to the token governance account.
pub fn initialize_token(
    kernel: &mut impl TokenKernelOperations,
    init_params: TokenModuleInitializationParameters,
) -> Result<(), TokenInitializationError> {
    if !init_params.additional.is_empty() {
        return Err(TokenInitializationError::InvalidInitializationParameters(
            format!(
                "Unknown additional parameters: {}",
                init_params
                    .additional
                    .keys()
                    .map(|k| k.as_str())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        ));
    }
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
    let governance_account = init_params.governance_account.ok_or_else(|| {
        TokenInitializationError::InvalidInitializationParameters(
            "Token governance account is missing".to_string(),
        )
    })?;
    kernel.set_module_state(STATE_KEY_NAME, Some(name.into()));
    let encoded_metadata = cbor::cbor_encode(&metadata);
    kernel.set_module_state(STATE_KEY_METADATA, Some(encoded_metadata));
    if init_params.allow_list == Some(true) {
        kernel.set_module_state(STATE_KEY_ALLOW_LIST, Some(vec![]));
    }
    if init_params.deny_list == Some(true) {
        kernel.set_module_state(STATE_KEY_DENY_LIST, Some(vec![]));
    }
    if init_params.mintable == Some(true) {
        kernel.set_module_state(STATE_KEY_MINTABLE, Some(vec![]));
    }
    if init_params.burnable == Some(true) {
        kernel.set_module_state(STATE_KEY_BURNABLE, Some(vec![]));
    }

    let governance_account = kernel.account_by_address(&governance_account.address)?;
    let governance_account_index = kernel.account_index(&governance_account);
    kernel.set_module_state(
        STATE_KEY_GOVERNANCE_ACCOUNT,
        Some(common::to_bytes(&governance_account_index.index)),
    );
    if let Some(initial_supply) = init_params.initial_supply {
        let mint_amount = token_module::to_raw_token_amount(kernel, initial_supply)?;
        kernel.mint(&governance_account, mint_amount)?;
    }
    Ok(())
}
