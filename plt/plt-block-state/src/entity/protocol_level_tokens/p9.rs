use crate::block_state_interface::{BlockStateFailure, BlockStateResult};
use crate::entity::protocol_level_tokens::state_keys;
use crate::entity::protocol_level_tokens::state_keys::{
    STATE_KEY_ALLOW_LIST, STATE_KEY_BURNABLE, STATE_KEY_DENY_LIST, STATE_KEY_GOVERNANCE_ACCOUNT,
    STATE_KEY_METADATA, STATE_KEY_MINTABLE, STATE_KEY_NAME, STATE_KEY_PAUSED,
};
use crate::entity::{EntityContext, EntityContextTypes};
use crate::persistent::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::persistent::blob_store::StoreSerialized;
use crate::persistent::protocol_level_tokens::p9::{
    PersistentTokenP9, PersistentTokensP9, TokenConfiguration, TokenIndex,
};
use crate::persistent::smart_contract_trie;
use crate::{persistent, utils};
use concordium_base::base::AccountIndex;
use concordium_base::common;
use concordium_base::protocol_level_tokens::{MetadataUrl, TokenId};
use plt_scheduler_types::types::tokens::RawTokenAmount;

pub(crate) fn plt_list<C: EntityContextTypes>(
    context: &EntityContext<C>,
    persistent_tokens: &PersistentTokensP9,
) -> impl ExactSizeIterator<Item = BlockStateResult<TokenId>> {
    persistent_tokens
        .tokens
        .values(&context.loader)
        .map(|item| {
            Ok(item?
                .1
                .cow_project_configuration()
                .value(&context.loader)?
                .into_owned()
                .0
                .token_id)
        })
}

pub(crate) fn create_token<C: EntityContextTypes>(
    context: &EntityContext<C>,
    persistent_tokens: &mut PersistentTokensP9,
    configuration: TokenConfiguration,
) -> BlockStateResult<TokenIndex> {
    let normalized_token_id =
        persistent::protocol_level_tokens::normalize_token_id(&configuration.token_id);

    let persistent_token = PersistentTokenP9 {
        configuration: HashedCacheableRef::new(StoreSerialized(configuration)),
        key_value_state: HashedCacheableRef::new(smart_contract_trie::PersistentState::empty()),
        circulating_supply: StoreSerialized(RawTokenAmount(0)),
    };

    let token_index;
    (token_index, persistent_tokens.tokens) = persistent_tokens
        .tokens
        .insert_value(&context.loader, persistent_token)?;
    persistent_tokens
        .token_id_map
        .insert(normalized_token_id, token_index);

    Ok(token_index)
}

pub(crate) fn update_token<C: EntityContextTypes>(
    context: &EntityContext<C>,
    persistent_tokens: &mut PersistentTokensP9,
    mut token: TokenP9,
) -> BlockStateResult<()> {
    if token.mutable_key_value_state.is_dirty() {
        token.persistent.key_value_state =
            HashedCacheableRef::new(token.mutable_key_value_state.freeze(&context.loader));
    }

    persistent_tokens.tokens = persistent_tokens
        .tokens
        .update_value(&context.loader, token.token_index, |_| Ok(token.persistent))?
        .ok_or_else(|| {
            BlockStateFailure::Invariant(format!(
                "Token not found by index: {:?}",
                token.token_index
            ))
        })?;

    Ok(())
}

pub(crate) fn token_by_index<C: EntityContextTypes>(
    context: &EntityContext<C>,
    persistent_tokens: &PersistentTokensP9,
    token_index: TokenIndex,
) -> BlockStateResult<TokenP9> {
    let persistent_token = persistent_tokens
        .tokens
        .lookup_value(&context.loader, token_index)?
        .ok_or_else(|| {
            BlockStateFailure::Invariant(format!("Token not found by index: {:?}", token_index))
        })?
        .into_owned();

    let mutable_key_value_state = persistent_token
        .key_value_state
        .value(&context.loader)?
        .thaw();

    Ok(TokenP9 {
        token_index,
        persistent: persistent_token,
        mutable_key_value_state,
    })
}

pub(crate) fn token_index_by_id(
    persistent_tokens: &PersistentTokensP9,
    token_id: &TokenId,
) -> Option<TokenIndex> {
    persistent_tokens
        .token_id_map
        .get(&persistent::protocol_level_tokens::normalize_token_id(
            token_id,
        ))
        .copied()
}

/// Representation of protocol-level token on P9 and later protocols with compatible model.
#[derive(Debug)]
pub struct TokenP9 {
    /// Token index
    pub(crate) token_index: TokenIndex,
    /// Persistent model of the protoco-level token.
    pub(crate) persistent: PersistentTokenP9,
    /// Token key-value state
    pub(crate) mutable_key_value_state: smart_contract_trie::MutableState,
}

impl TokenP9 {
    /// Get the index of the token.
    pub fn token_index(&self) -> TokenIndex {
        self.token_index
    }

    /// Get the configuration of a protocol-level token.
    pub fn token_configuration<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<TokenConfiguration> {
        Ok(self
            .persistent
            .configuration
            .value(&context.loader)?
            .into_owned()
            .0)
    }

    /// Get the circulating supply of a protocol-level token.
    pub fn token_circulating_supply(&self) -> RawTokenAmount {
        self.persistent.circulating_supply.0
    }

    /// Set the recorded total circulating supply for a protocol-level token.
    ///
    /// This should always be kept up-to-date with the total balance held in accounts.
    ///
    /// # Arguments
    ///
    /// - `circulation_supply` The new total circulating supply for the token.
    pub fn set_token_circulating_supply(&mut self, circulation_supply: RawTokenAmount) {
        self.persistent.circulating_supply.0 = circulation_supply;
    }

    /// Get whether the balance-affecting operations on the token are currently
    /// paused.
    pub fn is_paused<C: EntityContextTypes>(&self, context: &EntityContext<C>) -> bool {
        self.mutable_key_value_state
            .lookup_value(
                &context.loader,
                &state_keys::module_state_key(STATE_KEY_PAUSED),
            )
            .is_some()
    }

    /// Sets the paused state of the token module.
    pub fn set_paused<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        value: bool,
    ) -> BlockStateResult<()> {
        if value {
            self.mutable_key_value_state.insert_value(
                &context.loader,
                &state_keys::module_state_key(STATE_KEY_PAUSED),
                vec![],
            )
        } else {
            self.mutable_key_value_state.delete_value(
                &context.loader,
                &state_keys::module_state_key(STATE_KEY_PAUSED),
            )
        }
    }

    /// Get whether the token has allow lists enabled.
    pub fn has_allow_list<C: EntityContextTypes>(&self, context: &EntityContext<C>) -> bool {
        self.mutable_key_value_state
            .lookup_value(
                &context.loader,
                &state_keys::module_state_key(STATE_KEY_ALLOW_LIST),
            )
            .is_some()
    }

    /// Enabled 'allowList' feature for the token.
    pub fn set_allow_list_enabled<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<()> {
        self.mutable_key_value_state.insert_value(
            &context.loader,
            &state_keys::module_state_key(STATE_KEY_ALLOW_LIST),
            vec![],
        )
    }

    /// Get whether the token has deny lists enabled.
    pub fn has_deny_list<C: EntityContextTypes>(&self, context: &EntityContext<C>) -> bool {
        self.mutable_key_value_state
            .lookup_value(
                &context.loader,
                &state_keys::module_state_key(STATE_KEY_DENY_LIST),
            )
            .is_some()
    }

    /// Enabled 'DenyList' feature for the token.
    pub fn set_deny_list_enabled<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<()> {
        self.mutable_key_value_state.insert_value(
            &context.loader,
            &state_keys::module_state_key(STATE_KEY_DENY_LIST),
            vec![],
        )
    }

    /// Get whether the token allows minting.
    pub fn is_mintable<C: EntityContextTypes>(&self, context: &EntityContext<C>) -> bool {
        self.mutable_key_value_state
            .lookup_value(
                &context.loader,
                &state_keys::module_state_key(STATE_KEY_MINTABLE),
            )
            .is_some()
    }

    /// Enabled 'Mintable' feature for the token.
    pub fn set_mintable_enabled<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<()> {
        self.mutable_key_value_state.insert_value(
            &context.loader,
            &state_keys::module_state_key(STATE_KEY_MINTABLE),
            vec![],
        )
    }

    /// Get whether the token allows burning.
    pub fn is_burnable<C: EntityContextTypes>(&self, context: &EntityContext<C>) -> bool {
        self.mutable_key_value_state
            .lookup_value(
                &context.loader,
                &state_keys::module_state_key(STATE_KEY_BURNABLE),
            )
            .is_some()
    }

    /// Enabled 'Burnable' feature for the token.
    pub fn set_burnable_enabled<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<()> {
        self.mutable_key_value_state.insert_value(
            &context.loader,
            &state_keys::module_state_key(STATE_KEY_BURNABLE),
            vec![],
        )
    }

    /// Get the name of the token.
    pub fn get_token_name<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<String> {
        self.mutable_key_value_state
            .lookup_value(
                &context.loader,
                &state_keys::module_state_key(STATE_KEY_NAME),
            )
            .ok_or_else(|| BlockStateFailure::Invariant("Name not present".to_string()))
            .and_then(|value| {
                String::from_utf8(value).map_err(|err| {
                    BlockStateFailure::Invariant(format!("Stored name is invalid UTF-8: {}", err))
                })
            })
    }

    /// Set the name of the token.
    pub fn set_token_name<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        name: String,
    ) -> BlockStateResult<()> {
        self.mutable_key_value_state.insert_value(
            &context.loader,
            &state_keys::module_state_key(STATE_KEY_NAME),
            name.into(),
        )
    }

    /// Get the account index of the governance account for the token.
    pub fn get_governance_account_index<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<AccountIndex> {
        let governance_account_index = AccountIndex::from(
            self.mutable_key_value_state
                .lookup_value(
                    &context.loader,
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
    pub fn set_governance_account<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        account: AccountIndex,
    ) -> BlockStateResult<()> {
        self.mutable_key_value_state.insert_value(
            &context.loader,
            &state_keys::module_state_key(STATE_KEY_GOVERNANCE_ACCOUNT),
            common::to_bytes(&account),
        )
    }

    /// Get the URL metadata of the token.
    pub fn get_metadata<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<MetadataUrl> {
        let metadata_cbor = self
            .mutable_key_value_state
            .lookup_value(
                &context.loader,
                &state_keys::module_state_key(STATE_KEY_METADATA),
            )
            .ok_or_else(|| BlockStateFailure::Invariant("Metadata not present".to_string()))?;
        let metadata: MetadataUrl = utils::cbor_decode(metadata_cbor).map_err(|err| {
            BlockStateFailure::Invariant(format!("Stored metadata CBOR not decodable: {}", err))
        })?;
        Ok(metadata)
    }

    /// Set the metadata URL.
    pub fn set_metadata_url<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        metadata: &MetadataUrl,
    ) -> BlockStateResult<()> {
        let encoded_metadata = common::cbor::cbor_encode(metadata);
        self.mutable_key_value_state.insert_value(
            &context.loader,
            &state_keys::module_state_key(STATE_KEY_METADATA),
            encoded_metadata,
        )
    }

    /// Get the allow-list state for the account at the given account.
    pub fn get_allow_list_for<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        account: AccountIndex,
    ) -> bool {
        self.mutable_key_value_state
            .lookup_value(
                &context.loader,
                &state_keys::account_state_key(account, STATE_KEY_ALLOW_LIST),
            )
            .is_some()
    }

    /// Set the allow-list state for the account at the given account.
    pub fn set_allow_list_for<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,

        account: AccountIndex,
        value: bool,
    ) -> BlockStateResult<()> {
        if value {
            self.mutable_key_value_state.insert_value(
                &context.loader,
                &state_keys::account_state_key(account, STATE_KEY_ALLOW_LIST),
                vec![],
            )
        } else {
            self.mutable_key_value_state.delete_value(
                &context.loader,
                &state_keys::account_state_key(account, STATE_KEY_ALLOW_LIST),
            )
        }
    }

    /// Get the deny-list state for the account at the given account.
    pub fn get_deny_list_for<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        account: AccountIndex,
    ) -> bool {
        self.mutable_key_value_state
            .lookup_value(
                &context.loader,
                &state_keys::account_state_key(account, STATE_KEY_DENY_LIST),
            )
            .is_some()
    }

    /// Set the deny-list state for the account at the given account.
    pub fn set_deny_list_for<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        account: AccountIndex,
        value: bool,
    ) -> BlockStateResult<()> {
        if value {
            self.mutable_key_value_state.insert_value(
                &context.loader,
                &state_keys::account_state_key(account, STATE_KEY_DENY_LIST),
                vec![],
            )
        } else {
            self.mutable_key_value_state.delete_value(
                &context.loader,
                &state_keys::account_state_key(account, STATE_KEY_DENY_LIST),
            )
        }
    }
}
