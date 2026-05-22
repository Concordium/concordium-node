use concordium_base::common::cbor;
use concordium_base::protocol_level_tokens::{RawCbor, TokenId};
use plt_block_state::entity::block_state::p11::BlockStateP11;
use plt_block_state::entity::{EntityContext, EntityContextTypes};
use plt_block_state::entity::accounts::Account;
use plt_block_state::entity::block_state::TokenNotFoundByIdError;
use plt_block_state::failure::BlockStateResult;
use plt_scheduler_types::types::queries::{TokenAccountInfo, TokenAccountState, TokenAuthorizations, TokenInfo, TokenState};
use plt_scheduler_types::types::tokens::TokenAmount;
use crate::protocol_level_tokens::token_module;

/// Get the [`TokenId`]s of all protocol-level tokens registered on the chain.
pub fn query_plt_list<C: EntityContextTypes>(
    context: &EntityContext<C>,
    block_state: &BlockStateP11,
) -> BlockStateResult<Vec<TokenId>> {
    block_state.plt_list(context)
}


/// Get the token state associated with the given token id.
pub fn query_token_info<C: EntityContextTypes>(
    context: &EntityContext<C>,
    block_state: &BlockStateP11,
    token_id: &TokenId,
) -> BlockStateResult<Result<TokenInfo, TokenNotFoundByIdError>> {
    let token = match block_state.token_by_id(context, token_id)? {
        Ok(token) => token,
        Err(err) => return Ok(Err(err)),
    };

    let token_configuration = token.token_p9.token_configuration(context)?;
    let circulating_supply = token.token_p9.token_circulating_supply();

    let total_supply = TokenAmount {
        amount: circulating_supply,
        decimals: token_configuration.decimals,
    };

    let module_state = token_module::query_token_module_state(context, block_state, &token.token_p9)?;

    let token_state = TokenState {
        token_module_ref: token_configuration.module_ref,
        decimals: token_configuration.decimals,
        total_supply,
        module_state: RawCbor::from(cbor::cbor_encode(&module_state)),
    };

    let token_info = TokenInfo {
        // The token configuration contains the canonical token id specified in the original casing
        token_id: token_configuration.token_id,
        state: token_state,
    };

    Ok(Ok(token_info))
}

/// Get the list of tokens on an account
pub fn query_token_account_infos<C: EntityContextTypes>(
    context: &EntityContext<C>,
    block_state: &BlockStateP11,
    account: Account,
) -> BlockStateResult<Vec<TokenAccountInfo>> {
    account
        .token_account_states(context)
        .map(|(token_index, state)| {
            let token = block_state.token_by_index(context, token_index)?;
            let token_configuration =token.token_p9.token_configuration(context)?;


            let module_state = token_module::query_token_module_account_state(
                &context,
                &token.token_p9,
                account.account_index(),
            )?;

            let balance = TokenAmount {
                amount: state.balance,
                decimals: token_configuration.decimals,
            };

            let account_state = TokenAccountState {
                balance,
                module_state: Some(RawCbor::from(cbor::cbor_encode(&module_state))),
            };

            Ok(TokenAccountInfo {
                token_id: token_configuration.token_id,
                account_state,
            })
        })
        .collect()
}

/// Get the authorizations of a token.
pub fn query_token_authorizations<C: EntityContextTypes>(
    context: &EntityContext<C>,
    block_state: &BlockStateP11,
    token_id: &TokenId,
) -> BlockStateResult<Result<TokenAuthorizations, TokenNotFoundByIdError>> {
    let token = match block_state.token_by_id(context, token_id)? {
        Ok(token) => token,
        Err(err) => return Ok(Err(err)),
    };

    let token_configuration = token.token_p9.token_configuration(context)?;

    let details = token_module::query_token_authorizations(context, block_state, &token)?;

    Ok(Ok(TokenAuthorizations {
        token_id: token_configuration.token_id,
        details: RawCbor::from(cbor::cbor_encode(&details)),
    }))
}
