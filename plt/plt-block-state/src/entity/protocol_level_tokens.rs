//! Protocol-level token types used in the block state.

use crate::block_state::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::block_state::blob_store::{BlobStoreLoad, StoreSerialized};
use crate::block_state::utils::OwnedOrBorrowed;
use crate::block_state::{smart_contract_trie, utils};
use crate::block_state_interface::{BlockStateFailure, BlockStateResult};
use crate::entity::protocol_level_tokens::state_keys::{
    STATE_KEY_ALLOW_LIST, STATE_KEY_BURNABLE, STATE_KEY_DENY_LIST, STATE_KEY_GOVERNANCE_ACCOUNT,
    STATE_KEY_METADATA, STATE_KEY_MINTABLE, STATE_KEY_NAME, STATE_KEY_PAUSED,
};
use crate::persistent;
use crate::persistent::protocol_level_tokens::{PersistentPlToken, PersistentPlTokens, TokenIndex};
use concordium_base::base::AccountIndex;
use concordium_base::common;
use concordium_base::common::Serialize;
use concordium_base::protocol_level_tokens::{MetadataUrl, TokenId, TokenModuleRef};
use plt_scheduler_types::types::tokens::RawTokenAmount;

mod roles;
mod state_keys;

/// Static configuration for a protocol-level token.
///
/// Corresponding Haskell type: `Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens.PLTConfiguration`
#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize)]
pub struct TokenConfiguration {
    /// The token ID in its canonical form. Token IDs are case-insensitive when compared,
    /// but the canonical token ID preserves the original casing specified when
    /// the token was created.
    pub token_id: TokenId,
    /// The token module reference.
    pub module_ref: TokenModuleRef,
    /// The number of decimal places used in the representation of the token.
    pub decimals: u8,
}

pub trait SupportsPlTokens {
    /// Get the [`TokenId`]s of all protocol-level tokens.
    fn plt_list(
        &self,
    ) -> BlockStateResult<impl ExactSizeIterator<Item = BlockStateResult<TokenId>>>;

    /// Get the token associated with a [`TokenId`] (if it exists).
    /// The token ID is case-insensitive when looking up tokens by token ID.
    ///
    /// # Arguments
    ///
    /// - `token_id` The token id to get the [`Self::Token`] of.
    fn token_by_id(
        &self,
        token_id: &TokenId,
    ) -> BlockStateResult<Option<PlTokenEntity<'_, impl BlobStoreLoad>>>;

    /// Create a new token with the given configuration. The initial state will be empty
    /// and the initial supply will be 0. Returns representation of the created token.
    ///
    /// # Arguments
    ///
    /// - `configuration` The configuration for the token.
    fn create_token(
        &mut self,
        configuration: TokenConfiguration,
    ) -> BlockStateResult<PlTokenEntity<'_, impl BlobStoreLoad>>;

    /// Get the token balance of the account.
    fn account_token_balance(&self, account: &Self::Account, token: &Self::Token)
                             -> RawTokenAmount;

    /// Get token account states. It returns states for all tokens
    /// that the account holds.
    fn token_account_states(
        &self,
        account: &Self::Account,
    ) -> impl Iterator<Item = (Self::Token, TokenAccountState)>;

    /// Update the token balance of an account.
    ///
    /// # Arguments
    ///
    /// - `token` The token to update.
    /// - `account` The account to update.
    /// - `amount_delta` The token balance delta.
    ///
    /// # Errors
    ///
    /// - [`OverflowError`] The update would overflow or underflow (result in negative balance)
    ///   the token balance on the account.
    fn update_token_account_balance(
        &mut self,
        token: &Self::Token,
        account: &Self::Account,
        amount_delta: RawTokenAmountDelta,
    ) -> Result<(), OverflowError>;

    /// Initialize the balance of the given account to zero if it didn't have a balance before.
    /// It has the observable effect that the token is then returned when querying the tokens
    /// for an account. Should be called if the token module account state is set,
    /// in order to make sure the token is returned when querying token account info.
    ///
    /// If the account already has a balance for the token in context, the operation has no effect
    ///
    /// # Arguments
    ///
    /// - `token` The token to touch state for in the account.
    /// - `account` The account to touch token state for.
    fn touch_token_account(&mut self, token: &Self::Token, account: &Self::Account);

    /// Increment the update sequence number for Protocol Level Tokens (PLT).
    ///
    /// Unlike the other chain updates this is a separate function, since there is no queue associated with PLTs.
    fn increment_plt_update_instruction_sequence_number(&mut self);
}

/// Protocol-level tokens block state.
#[derive(Debug)]
pub struct PlTokens<'a, L> {
    /// Persistent model of the protocol-level tokens.
    persistent: OwnedOrBorrowed<'a, PersistentPlTokens>,
    /// Blob store loader.
    store_loader: &'a L,
}

impl<'a, L> PlTokens<'a, L> {
    pub fn new(persistent: OwnedOrBorrowed<'a, PersistentPlTokens>, store_loader: &'a L) -> Self {
        Self {
            persistent,
            store_loader,
        }
    }
}

impl<'a, L: BlobStoreLoad> PlTokens<'a, L> {
    pub fn plt_list(&self) -> impl ExactSizeIterator<Item = BlockStateResult<TokenId>> {
        self.persistent
            .tokens
            .values(self.store_loader)
            .map(|item| {
                Ok(match item?.1.configuration.value(self.store_loader)? {
                    OwnedOrBorrowed::Owned(v) => v.0.token_id,
                    OwnedOrBorrowed::Borrowed(r) => r.0.token_id.clone(),
                })
            })
    }

    pub fn token_by_id(
        &self,
        token_id: &TokenId,
    ) -> BlockStateResult<Option<PlTokenEntity<'a, L>>> {
        let token_index_option = *self.persistent.token_id_map.get(
            &persistent::protocol_level_tokens::normalize_token_id(token_id),
        );

        let Some(token_index) = token_index_option else {
            return Ok(None);
        };

        self.thaw_token(token_index).map(Some)
    }

    pub fn create_token(
        &mut self,
        configuration: TokenConfiguration,
    ) -> BlockStateResult<PlTokenEntity<'a, L>> {
        let normalized_token_id =
            persistent::protocol_level_tokens::normalize_token_id(&configuration.token_id);

        let persistent_token = PersistentPlToken {
            configuration: HashedCacheableRef::new(StoreSerialized(configuration)),
            key_value_state: HashedCacheableRef::new(smart_contract_trie::PersistentState::empty()),
            circulating_supply: StoreSerialized(RawTokenAmount(0)),
        };

        let token_index;
        (token_index, self.persistent.to_mut().tokens) = self
            .persistent
            .tokens
            .insert_value(self.store_loader, persistent_token)?;
        self.persistent
            .to_mut()
            .token_id_map
            .insert(normalized_token_id, token_index);

        self.thaw_token(token_index)
    }

    fn thaw_token(&self, token_index: TokenIndex) -> BlockStateResult<PlTokenEntity<'a, L>> {
        // todo ar aliasing check?

        let persistent_token = self
            .persistent
            .tokens
            .lookup_value(self.store_loader, token_index)?
            .ok_or_else(|| {
                BlockStateFailure::Invariant(format!("Token not found by index: {:?}", token_index))
            })?;

        let mutable_key_value_state = persistent_token
            .key_value_state
            .value(self.store_loader)?
            .thaw();

        Ok(PlTokenEntity {
            token_index,
            persistent: persistent_token,
            mutable_key_value_state,
            store_loader: self.store_loader,
        })
    }

    pub fn freeze_token(&mut self, mut token: PlTokenEntity<'a, L>) -> BlockStateResult<()> {
        if token.mutable_key_value_state.is_dirty() {
            token.persistent.to_mut().key_value_state =
                HashedCacheableRef::new(token.mutable_key_value_state.freeze(self.store_loader));
        }

        // todo ar check if token is dirty?
        self.persistent.to_mut().tokens = self
            .persistent
            .tokens
            .update_value(self.store_loader, token.token_index, |_| {
                Ok(token.persistent.into_owned_or_clone())
            })?
            .ok_or_else(|| {
                BlockStateFailure::Invariant(format!(
                    "Token not found by index: {:?}",
                    token.token_index
                ))
            })?;

        Ok(())
    }

    pub fn into_persistent(self) -> PersistentPlTokens {
        self.persistent.into_owned_or_clone()
    }
}

/// Protocol-level token entity.
#[derive(Debug)]
pub struct PlTokenEntity<'a, L> {
    /// Token index
    token_index: TokenIndex,
    /// Persistent model of the protoco-level token. If changed, it must be written
    /// back.
    persistent: OwnedOrBorrowed<'a, PersistentPlToken>,
    /// Token key-value state
    mutable_key_value_state: smart_contract_trie::MutableState,
    /// Blob store loader.
    store_loader: &'a L,
}

impl<L: BlobStoreLoad> PlTokenEntity<'_, L> {
    /// Get the configuration of a protocol-level token.
    pub fn token_configuration(&self) -> BlockStateResult<TokenConfiguration> {
        Ok(self
            .persistent
            .configuration
            .value(self.store_loader)?
            .into_owned_or_clone()
            .0)
    }

    /// Get the circulating supply of a protocol-level token.
    pub fn token_circulating_supply(&self) -> BlockStateResult<RawTokenAmount> {
        Ok(self.persistent.circulating_supply.0)
    }

    /// Set the recorded total circulating supply for a protocol-level token.
    ///
    /// This should always be kept up-to-date with the total balance held in accounts.
    ///
    /// # Arguments
    ///
    /// - `circulation_supply` The new total circulating supply for the token.
    pub fn set_token_circulating_supply(&mut self) {
        self.persistent.to_mut().circulating_supply.0;
    }

    /// Get whether the balance-affecting operations on the token are currently
    /// paused.
    pub fn is_paused(&self) -> bool {
        self.mutable_key_value_state
            .lookup_value(
                &self.store_loader,
                &state_keys::module_state_key(STATE_KEY_PAUSED),
            )
            .is_some()
    }

    /// Sets the paused state of the token module.
    pub fn set_paused(&mut self, value: bool) -> BlockStateResult<()> {
        if value {
            self.mutable_key_value_state.insert_value(
                &self.store_loader,
                &state_keys::module_state_key(STATE_KEY_PAUSED),
                vec![],
            )
        } else {
            self.mutable_key_value_state.delete_value(
                &self.store_loader,
                &state_keys::module_state_key(STATE_KEY_PAUSED),
            )
        }
    }

    /// Get whether the token has allow lists enabled.
    pub fn has_allow_list(&self) -> bool {
        self.mutable_key_value_state
            .lookup_value(
                &self.store_loader,
                &state_keys::module_state_key(STATE_KEY_ALLOW_LIST),
            )
            .is_some()
    }

    /// Enabled 'allowList' feature for the token.
    pub fn set_allow_list_enabled(&mut self) -> BlockStateResult<()> {
        self.mutable_key_value_state.insert_value(
            &self.store_loader,
            &state_keys::module_state_key(STATE_KEY_ALLOW_LIST),
            vec![],
        )
    }

    /// Get whether the token has deny lists enabled.
    pub fn has_deny_list(&self) -> bool {
        self.mutable_key_value_state
            .lookup_value(
                &self.store_loader,
                &state_keys::module_state_key(STATE_KEY_DENY_LIST),
            )
            .is_some()
    }

    /// Enabled 'DenyList' feature for the token.
    pub fn set_deny_list_enabled(&mut self) -> BlockStateResult<()> {
        self.mutable_key_value_state.insert_value(
            &self.store_loader,
            &state_keys::module_state_key(STATE_KEY_DENY_LIST),
            vec![],
        )
    }

    /// Get whether the token allows minting.
    pub fn is_mintable(&self) -> bool {
        self.mutable_key_value_state
            .lookup_value(
                &self.store_loader,
                &state_keys::module_state_key(STATE_KEY_MINTABLE),
            )
            .is_some()
    }

    /// Enabled 'Mintable' feature for the token.
    pub fn set_mintable_enabled(&mut self) -> BlockStateResult<()> {
        self.mutable_key_value_state.insert_value(
            &self.store_loader,
            &state_keys::module_state_key(STATE_KEY_MINTABLE),
            vec![],
        )
    }

    /// Get whether the token allows burning.
    pub fn is_burnable(&self) -> bool {
        self.mutable_key_value_state
            .lookup_value(
                &self.store_loader,
                &state_keys::module_state_key(STATE_KEY_BURNABLE),
            )
            .is_some()
    }

    /// Enabled 'Burnable' feature for the token.
    pub fn set_burnable_enabled(&mut self) -> BlockStateResult<()> {
        self.mutable_key_value_state.insert_value(
            &self.store_loader,
            &state_keys::module_state_key(STATE_KEY_BURNABLE),
            vec![],
        )
    }

    /// Get the name of the token.
    pub fn get_token_name(&self) -> BlockStateResult<String> {
        self.mutable_key_value_state
            .lookup_value(
                &self.store_loader,
                &state_keys::module_state_key(STATE_KEY_NAME),
            )
            .ok_or_else(|| BlockStateFailure::Invariant("Name not present".to_string()))
            .and_then(|value| {
                String::from_utf8(value).map_err(|err| {
                    BlockStateFailure::Invariant(format!("Stored name is invalid UTF-8: {}", err))
                })
            })
    }

    /// Set the token governance account in module state.
    pub fn set_token_name(&mut self, name: String) -> BlockStateResult<()> {
        self.mutable_key_value_state.insert_value(
            &self.store_loader,
            &state_keys::module_state_key(STATE_KEY_NAME),
            name.into(),
        )
    }

    /// Get the account index of the governance account for the token.
    pub fn get_governance_account_index(&self) -> BlockStateResult<AccountIndex> {
        let governance_account_index = AccountIndex::from(
            self.mutable_key_value_state
                .lookup_value(
                    &self.store_loader,
                    &state_keys::module_state_key(STATE_KEY_GOVERNANCE_ACCOUNT),
                )
                .ok_or_else(|| {
                    BlockStateFailure::Invariant("Governance account not present".to_string())
                })
                .and_then(|value| {
                    common::from_bytes_complete::<u64>(value.as_slice()).map_err(|err| {
                        BlockStateFailure::Invariant(format!(
                            "Stored governance account index cannot be decoded: {}",
                            err
                        ))
                    })
                })?,
        );
        Ok(governance_account_index)
    }

    /// Set the token governance account in module state.
    pub fn set_governance_account(&mut self, account: AccountIndex) -> BlockStateResult<()> {
        self.mutable_key_value_state.insert_value(
            &self.store_loader,
            &state_keys::module_state_key(STATE_KEY_GOVERNANCE_ACCOUNT),
            common::to_bytes(&account),
        )
    }

    /// Get the URL metadata of the token.
    pub fn get_metadata(&self) -> BlockStateResult<MetadataUrl> {
        let metadata_cbor = self
            .mutable_key_value_state
            .lookup_value(
                &self.store_loader,
                &state_keys::module_state_key(STATE_KEY_METADATA),
            )
            .ok_or_else(|| BlockStateFailure::Invariant("Metadata not present".to_string()))?;
        let metadata: MetadataUrl = utils::cbor_decode(metadata_cbor).map_err(|err| {
            BlockStateFailure::Invariant(format!("Stored metadata CBOR not decodable: {}", err))
        })?;
        Ok(metadata)
    }

    // todo ar what to do about this?
    /// Set the metadata URL.
    pub fn set_metadata_url(&mut self, metadata: &MetadataUrl) -> BlockStateResult<()> {
        let encoded_metadata = common::cbor::cbor_encode(metadata);
        self.mutable_key_value_state.insert_value(
            &self.store_loader,
            &state_keys::module_state_key(STATE_KEY_METADATA),
            encoded_metadata,
        )
    }

    /// Get the allow-list state for the account at the given account.
    pub fn get_allow_list_for(&self, account: AccountIndex) -> bool {
        self.mutable_key_value_state
            .lookup_value(
                &self.store_loader,
                &state_keys::account_state_key(account, STATE_KEY_ALLOW_LIST),
            )
            .is_some()
    }

    /// Set the allow-list state for the account at the given account.
    pub fn set_allow_list_for(
        &mut self,
        account: AccountIndex,
        value: bool,
    ) -> BlockStateResult<()> {
        if value {
            self.mutable_key_value_state.insert_value(
                &self.store_loader,
                &state_keys::account_state_key(account, STATE_KEY_ALLOW_LIST),
                vec![],
            )
        } else {
            self.mutable_key_value_state.delete_value(
                &self.store_loader,
                &state_keys::account_state_key(account, STATE_KEY_ALLOW_LIST),
            )
        }
    }

    /// Get the deny-list state for the account at the given account.
    pub fn get_deny_list_for(&self, account: AccountIndex) -> bool {
        self.mutable_key_value_state
            .lookup_value(
                &self.store_loader,
                &state_keys::account_state_key(account, STATE_KEY_DENY_LIST),
            )
            .is_some()
    }

    /// Set the deny-list state for the account at the given account.
    pub fn set_deny_list_for(
        &mut self,
        account: AccountIndex,
        value: bool,
    ) -> BlockStateResult<()> {
        if value {
            self.mutable_key_value_state.insert_value(
                &self.store_loader,
                &state_keys::account_state_key(account, STATE_KEY_DENY_LIST),
                vec![],
            )
        } else {
            self.mutable_key_value_state.delete_value(
                &self.store_loader,
                &state_keys::account_state_key(account, STATE_KEY_DENY_LIST),
            )
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use concordium_base::common;
    use concordium_base::protocol_level_tokens::TokenModuleRef;
    use plt_scheduler_types::types::tokens::RawTokenAmount;

    #[test]
    fn test_token_configuration_serial() {
        let token_configuration = TokenConfiguration {
            token_id: "tokenid1".parse().unwrap(),
            module_ref: TokenModuleRef::from([5; 32]),
            decimals: 4,
        };

        let bytes = common::to_bytes(&token_configuration);
        assert_eq!(
            hex::encode(&bytes),
            "08746f6b656e696431050505050505050505050505050505050505050505050505050505050505050504"
        );

        let token_configuration_deserialized: TokenConfiguration =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(token_configuration_deserialized, token_configuration);
    }

    #[test]
    fn test_token_account_state_serial() {
        let state = TokenAccountState {
            balance: RawTokenAmount(10),
        };

        let bytes = common::to_bytes(&state);
        assert_eq!(hex::encode(&bytes), "0a");

        let state_deserialized: TokenAccountState =
            common::from_bytes_complete(bytes.as_slice()).unwrap();
        assert_eq!(state_deserialized, state);
    }
}
