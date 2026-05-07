use crate::block_state::utils::OwnedOrBorrowed;
use crate::block_state::{smart_contract_trie, utils};
use crate::block_state_interface::{BlockStateFailure, BlockStateResult};
use crate::entity::protocol_level_tokens::state_keys;
use crate::entity::protocol_level_tokens::state_keys::{
    STATE_KEY_ALLOW_LIST, STATE_KEY_BURNABLE, STATE_KEY_DENY_LIST, STATE_KEY_GOVERNANCE_ACCOUNT,
    STATE_KEY_METADATA, STATE_KEY_MINTABLE, STATE_KEY_NAME, STATE_KEY_PAUSED,
};
use crate::entity::{EntityContext, EntityContextTypes};
use crate::persistent::protocol_level_tokens::p9::{PersistentPlTokenP9, TokenIndex};
use concordium_base::base::AccountIndex;
use concordium_base::common;
use concordium_base::common::Serialize;
use concordium_base::protocol_level_tokens::{MetadataUrl, TokenId, TokenModuleRef};
use plt_scheduler_types::types::tokens::RawTokenAmount;

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

/// Protocol-level token entity on P9.
#[derive(Debug)]
pub struct TokenEntityP9<'a> {
    /// Token index
    pub(crate) token_index: TokenIndex,
    /// Persistent model of the protoco-level token. If changed, it must be written
    /// back.
    pub(crate) persistent: OwnedOrBorrowed<'a, PersistentPlTokenP9>,
    /// Token key-value state
    pub(crate) mutable_key_value_state: smart_contract_trie::MutableState,
}

impl<'a> TokenEntityP9<'a> {
    /// Get the configuration of a protocol-level token.
    pub fn token_configuration<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<TokenConfiguration> {
        Ok(self
            .persistent
            .configuration
            .value(&context.loader)?
            .into_owned_or_clone()
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
        self.persistent.to_mut().circulating_supply.0 = circulation_supply;
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
    pub fn set_paused<C: EntityContextTypes>(&mut self, context: &EntityContext<C>, value: bool) -> BlockStateResult<()> {
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
    pub fn set_allow_list_enabled<C: EntityContextTypes>(&mut self, context: &EntityContext<C>) -> BlockStateResult<()> {
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
    pub fn set_deny_list_enabled<C: EntityContextTypes>(&mut self, context: &EntityContext<C>) -> BlockStateResult<()> {
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
    pub fn set_mintable_enabled<C: EntityContextTypes>(&mut self, context: &EntityContext<C>) -> BlockStateResult<()> {
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
    pub fn set_burnable_enabled<C: EntityContextTypes>(&mut self, context: &EntityContext<C>) -> BlockStateResult<()> {
        self.mutable_key_value_state.insert_value(
            &context.loader,
            &state_keys::module_state_key(STATE_KEY_BURNABLE),
            vec![],
        )
    }

    /// Get the name of the token.
    pub fn get_token_name<C: EntityContextTypes>(&self, context: &EntityContext<C>) -> BlockStateResult<String> {
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

    /// Set the token governance account in module state.
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
    pub fn get_metadata<C: EntityContextTypes>(&self, context: &EntityContext<C>) -> BlockStateResult<MetadataUrl> {
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
    pub fn get_allow_list_for<C: EntityContextTypes>(&self, context: &EntityContext<C>, account: AccountIndex) -> bool {
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
    pub fn get_deny_list_for<C: EntityContextTypes>(&self, context: &EntityContext<C>, account: AccountIndex) -> bool {
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::block_state::external::TokenAccountState;
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
