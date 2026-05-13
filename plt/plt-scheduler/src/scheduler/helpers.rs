use concordium_base::{
    base::AccountIndex,
    protocol_level_tokens::{CborHolderAccount, TokenId},
};
use plt_block_state::block_state_interface::{BlockStateQuery, TokenNotFoundByIdError};
use plt_scheduler_types::types::reject_reasons::TransactionRejectReason;

/// Look up the account index for a [`CborHolderAccount`]. If the account does
/// not exist, a [`TransactionRejectReason::InvalidAccountReference`] is
/// returned.
pub fn lookup_account_index<BSQ: BlockStateQuery>(
    block_state: &BSQ,
    holder: CborHolderAccount,
) -> Result<AccountIndex, TransactionRejectReason> {
    match block_state.account_by_address(&holder.address) {
        Ok(account) => Ok(block_state.account_index(&account)),
        Err(_) => Err(TransactionRejectReason::InvalidAccountReference(
            holder.address,
        )),
    }
}

/// Look up the token ID in the block state. If the token ID does not exist, a
/// [`TransactionRejectReason::NonExistentTokenId`] is returned.
/// Otherwise, this returns the canonical representation of the token ID.
///
/// TODO: Consider returning token index instead.
pub fn lookup_token_id<BSQ: BlockStateQuery>(
    block_state: &BSQ,
    token_id: TokenId,
) -> Result<TokenId, TransactionRejectReason> {
    match block_state.token_by_id(&token_id) {
        Ok(token) => Ok(block_state.token_configuration(&token).token_id),
        Err(TokenNotFoundByIdError(_)) => {
            Err(TransactionRejectReason::NonExistentTokenId(token_id))
        }
    }
}
