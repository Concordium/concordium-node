use crate::module_state::{
    KernelOperationsExt, STATE_KEY_ALLOW_LIST, STATE_KEY_BURNABLE, STATE_KEY_DENY_LIST,
    STATE_KEY_GOVERNANCE_ACCOUNT, STATE_KEY_METADATA, STATE_KEY_MINTABLE, STATE_KEY_NAME,
};
use crate::token_module::TokenAmountDecimalsMismatchError;
use crate::util;
use concordium_base::common;
use concordium_base::common::cbor;
use concordium_base::common::cbor::CborSerializationError;
use concordium_base::protocol_level_tokens::{RawCbor, TokenModuleInitializationParameters};
use plt_scheduler_interface::error::AccountNotFoundByAddressError;
use plt_scheduler_interface::token_kernel_interface::{
    MintWouldOverflowError, TokenKernelOperations, TokenMintError, TokenStateInvariantError,
};

/// Represents the reasons why [`initialize_token`] can fail.
#[derive(Debug, thiserror::Error)]
pub enum TokenInitializationError {
    #[error("Invalid token initialization parameters: {0}")]
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
pub fn initialize_token(
    kernel: &mut impl TokenKernelOperations,
    initialization_parameters_cbor: RawCbor,
) -> Result<(), TokenInitializationError> {
    let init_params: TokenModuleInitializationParameters =
        util::cbor_decode(&initialization_parameters_cbor)?;
    initialize_token_impl(kernel, init_params)
}

fn initialize_token_impl(
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
        let mint_amount = util::to_raw_token_amount(kernel, initial_supply)?;
        kernel.mint(&governance_account, mint_amount)?;
    }
    Ok(())
}
