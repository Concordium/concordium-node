use crate::block_state_interface::{BlockStateFailure, BlockStateResult};
use crate::entity::protocol_level_tokens::p9::TokenP9;
use crate::entity::protocol_level_tokens::state_keys;
use crate::entity::protocol_level_tokens::state_keys::ACCOUNT_ROLES_STATE_PREFIX;
use crate::entity::{EntityContext, EntityContextTypes};
use concordium_base::base::AccountIndex;
use concordium_base::common;
use concordium_base::protocol_level_locks::LockId;
use concordium_base::protocol_level_tokens::TokenAdminRole;
use plt_scheduler_types::types::tokens::RawTokenAmount;

/// Representation of protocol-level token on P11 and later protocols with compatible model.
#[derive(Debug)]
pub struct TokenP11 {
    /// P9 token representation
    pub token_p9: TokenP9,
}

impl TokenP11 {
    /// Get the authorization roles for an account from state.
    pub fn get_account_roles<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        account: AccountIndex,
    ) -> BlockStateResult<Roles> {
        Roles::try_from_state_value(
            self.token_p9
                .mutable_key_value_state
                .lookup_value(
                    &context.loader,
                    &state_keys::account_roles_state_key(account),
                )
                .as_deref(),
        )
        .map_err(|err| {
            BlockStateFailure::Invariant(format!(
                "Stored account authorization roles cannot be decoded: {}",
                err
            ))
        })
    }

    /// Update a value in the account section of the token state.
    fn update_account_roles_state<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        account: AccountIndex,
        roles: Roles,
    ) -> BlockStateResult<()> {
        if let Some(value) = roles.into_state_value() {
            self.token_p9.mutable_key_value_state.insert_value(
                &context.loader,
                &state_keys::account_roles_state_key(account),
                value,
            )
        } else {
            self.token_p9.mutable_key_value_state.delete_value(
                &context.loader,
                &state_keys::account_roles_state_key(account),
            )
        }
    }

    /// Assign roles to an account in the state.
    pub fn assign_account_roles<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        account: AccountIndex,
        roles_to_assign: &[TokenAdminRole],
    ) -> BlockStateResult<()> {
        let mut roles = self.get_account_roles(context, account)?;
        for role in roles_to_assign {
            roles.assign(*role)
        }
        self.update_account_roles_state(context, account, roles)
    }

    /// Revoke roles of an account in the state.
    pub fn revoke_account_roles<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        account: AccountIndex,
        roles_to_revoke: &[TokenAdminRole],
    ) -> BlockStateResult<()> {
        let mut roles = self.get_account_roles(context, account)?;
        for role in roles_to_revoke {
            roles.revoke(*role)
        }
        self.update_account_roles_state(context, account, roles)
    }

    /// Get the locked balance for the given account and lock.
    pub fn get_locked_balance_for<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        account_index: AccountIndex,
        lock_id: &LockId,
    ) -> BlockStateResult<RawTokenAmount> {
        let Some(value) = self.token_p9.mutable_key_value_state.lookup_value(
            &context.loader,
            &state_keys::account_quanta_state_key(account_index, lock_id),
        ) else {
            return Ok(RawTokenAmount(0));
        };
        common::from_bytes_complete(value).map_err(|err| {
            BlockStateFailure::BlobStoreDecode(format!(
                "Stored locked balance cannot be decoded: {}",
                err
            ))
        })
    }

    /// Set the locked balance for the given account and lock.
    pub fn set_locked_balance_for<C: EntityContextTypes>(
        &mut self,
        context: &EntityContext<C>,
        account_index: AccountIndex,
        lock_id: &LockId,
        amount: RawTokenAmount,
    ) -> BlockStateResult<()> {
        if amount == RawTokenAmount(0) {
            self.token_p9.mutable_key_value_state.delete_value(
                &context.loader,
                &state_keys::account_quanta_state_key(account_index, lock_id),
            )?;
        } else {
            self.token_p9.mutable_key_value_state.insert_value(
                &context.loader,
                &state_keys::account_quanta_state_key(account_index, lock_id),
                common::to_bytes(&amount),
            )?;
        }
        Ok(())
    }

    /// Iterate all authorization roles assigned for the token, together
    /// with the account they are assigned to.
    pub fn all_roles<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
    ) -> BlockStateResult<Vec<(AccountIndex, Roles)>> {
        self.token_p9
            .mutable_key_value_state
            .iter_prefix(&context.loader, &ACCOUNT_ROLES_STATE_PREFIX)?
            .map(|(key, value)| {
                let account_index_bytes = key
                    .strip_prefix(&ACCOUNT_ROLES_STATE_PREFIX)
                    .ok_or_else(|| {
                        BlockStateFailure::Invariant(
                            "Iterator over account roles state produced invalid key".to_string(),
                        )
                    })?;
                let account_index: AccountIndex = common::from_bytes_complete(account_index_bytes)
                    .map_err(|err| {
                        BlockStateFailure::Invariant(format!(
                            "Stored account index in authorizations cannot be decoded: {}",
                            err
                        ))
                    })?;
                let roles = Roles::try_from_state_value(Some(&value)).map_err(|err| {
                    BlockStateFailure::Invariant(format!(
                        "Stored account authorization roles cannot be decoded: {}",
                        err
                    ))
                })?;

                Ok((account_index, roles))
            })
            .collect()
    }
}

/// List roles which are unaffected by which features are enabled.
pub const UNIVERSAL_ROLES: &[TokenAdminRole] = &[
    TokenAdminRole::UpdateAdminRoles,
    TokenAdminRole::Pause,
    TokenAdminRole::UpdateMetadata,
];

/// List all roles.
const ALL_ROLES: &[TokenAdminRole] = &[
    TokenAdminRole::UpdateAdminRoles,
    TokenAdminRole::Mint,
    TokenAdminRole::Burn,
    TokenAdminRole::UpdateAllowList,
    TokenAdminRole::UpdateDenyList,
    TokenAdminRole::Pause,
    TokenAdminRole::UpdateMetadata,
];

/// Convert a role into the bitmask with 1 in the position of the specific role and zero every else.
const fn role_bitmask(role: TokenAdminRole) -> u16 {
    let bitshift = match role {
        TokenAdminRole::UpdateAdminRoles => 0,
        TokenAdminRole::Mint => 1,
        TokenAdminRole::Burn => 2,
        TokenAdminRole::UpdateAllowList => 3,
        TokenAdminRole::UpdateDenyList => 4,
        TokenAdminRole::Pause => 5,
        TokenAdminRole::UpdateMetadata => 6,
    };
    1u16 << bitshift
}

/// Collection of roles assigned to a single account.
#[derive(Debug, Eq, PartialEq, common::Serialize)]
pub struct Roles {
    bitmap: u16,
}

impl Roles {
    /// Construct the collection with no roles assigned.
    #[inline(always)]
    pub const fn none() -> Self {
        Self { bitmap: 0 }
    }

    /// Test collection for no roles.
    #[inline(always)]
    fn has_none(&self) -> bool {
        self.bitmap == 0
    }

    /// Test for a specific role being assigned.
    #[inline(always)]
    pub fn has(&self, role: TokenAdminRole) -> bool {
        self.bitmap & role_bitmask(role) != 0
    }

    /// Set the specific role to be a assigned.
    #[inline(always)]
    pub const fn assign(&mut self, role: TokenAdminRole) {
        self.bitmap |= role_bitmask(role);
    }

    /// Unset the specific role.
    #[inline(always)]
    pub fn revoke(&mut self, role: TokenAdminRole) {
        self.bitmap &= !role_bitmask(role);
    }

    /// Convert into token state value representation.
    ///
    /// The empty set of roles results in `None`.
    pub fn into_state_value(self) -> Option<Vec<u8>> {
        // The state value is set to none for accounts without any roles.
        if self.has_none() {
            None
        } else {
            Some(common::to_bytes(&self))
        }
    }

    /// Convert from token state value representation.
    ///
    /// State value of `None` results in the empty set of roles.
    pub fn try_from_state_value(value: Option<&[u8]>) -> common::ParseResult<Self> {
        let Some(value) = value else {
            return Ok(Roles::none());
        };
        common::from_bytes_complete(value)
    }

    /// Iterate the roles assigned.
    pub fn iter_assigned(&self) -> impl Iterator<Item = TokenAdminRole> {
        ALL_ROLES.iter().filter(|&role| self.has(*role)).copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_revoke_roles() {
        let mut roles = Roles::none();

        roles.assign(TokenAdminRole::UpdateAdminRoles);
        roles.assign(TokenAdminRole::Mint);
        assert!(roles.has(TokenAdminRole::UpdateAdminRoles));
        assert!(roles.has(TokenAdminRole::Mint));

        roles.revoke(TokenAdminRole::Mint);
        assert!(roles.has(TokenAdminRole::UpdateAdminRoles));
        assert!(!roles.has(TokenAdminRole::Mint));
        assert!(!roles.has(TokenAdminRole::Burn));
    }

    #[test]
    fn test_assign_roles() {
        let mut roles = Roles::none();

        assert!(!roles.has(TokenAdminRole::UpdateAdminRoles));
        roles.assign(TokenAdminRole::UpdateAdminRoles);
        assert!(roles.has(TokenAdminRole::UpdateAdminRoles));

        assert!(!roles.has(TokenAdminRole::Mint));
        roles.assign(TokenAdminRole::Mint);
        assert!(roles.has(TokenAdminRole::Mint));
        assert!(roles.has(TokenAdminRole::UpdateAdminRoles));
    }

    #[test]
    fn test_assign_roles_twice_is_nop() {
        let mut roles = Roles::none();
        roles.assign(TokenAdminRole::UpdateAdminRoles);
        assert!(roles.has(TokenAdminRole::UpdateAdminRoles));
        roles.assign(TokenAdminRole::UpdateAdminRoles);
        assert!(roles.has(TokenAdminRole::UpdateAdminRoles));
    }

    #[test]
    fn test_has_no_roles() {
        let mut roles = Roles::none();
        assert!(roles.has_none());
        roles.assign(TokenAdminRole::UpdateAdminRoles);
        assert!(!roles.has_none());
        roles.revoke(TokenAdminRole::UpdateAdminRoles);
        assert!(roles.has_none());
    }
}
