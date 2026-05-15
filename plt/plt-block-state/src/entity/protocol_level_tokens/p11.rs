use crate::block_state_interface::{BlockStateFailure, BlockStateResult};
use crate::entity::block_state::Accounts;
use crate::entity::protocol_level_tokens::p9::TokenP9;
use crate::entity::protocol_level_tokens::state_keys;
use crate::entity::protocol_level_tokens::state_keys::ACCOUNT_ROLES_STATE_PREFIX;
use crate::entity::{EntityContext, EntityContextTypes};
use concordium_base::base::AccountIndex;
use concordium_base::common;
use concordium_base::protocol_level_tokens::{
    TokenAdminRole, TokenAuthorizations, TokenRoleAuthorizations,
};

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


    // todo ar move to scheduler with accounts trait
    /// Get authorization roles and assigned accounts for the token.
    pub fn get_token_authorizations<C: EntityContextTypes>(
        &self,
        context: &EntityContext<C>,
        accounts: &impl Accounts,
    ) -> BlockStateResult<TokenAuthorizations> {
        let mut update_admin_roles = TokenRoleAuthorizations::default();
        let mut mint = TokenRoleAuthorizations::default();
        let mut burn = TokenRoleAuthorizations::default();
        let mut update_allow_list = TokenRoleAuthorizations::default();
        let mut update_deny_list = TokenRoleAuthorizations::default();
        let mut pause = TokenRoleAuthorizations::default();
        let mut update_metadata = TokenRoleAuthorizations::default();

        for (key, roles) in self
            .token_p9
            .mutable_key_value_state
            .iter_prefix(&context.loader, &ACCOUNT_ROLES_STATE_PREFIX)?
        {
            let account_index_bytes =
                key.strip_prefix(&ACCOUNT_ROLES_STATE_PREFIX)
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
            let account = accounts
                .account_by_index(context, account_index)
                .map_err(|err| {
                    BlockStateFailure::Invariant(format!(
                        "Stored account index in authorizations cannot be found: {}",
                        err
                    ))
                })?
                .canonical_account_address;
            let roles = Roles::try_from_state_value(Some(&roles)).map_err(|err| {
                BlockStateFailure::Invariant(format!(
                    "Stored account authorization roles cannot be decoded: {}",
                    err
                ))
            })?;
            for role in roles.iter_assigned() {
                match role {
                    TokenAdminRole::UpdateAdminRoles => {
                        update_admin_roles.accounts.push(account.into())
                    }
                    TokenAdminRole::Mint => mint.accounts.push(account.into()),
                    TokenAdminRole::Burn => burn.accounts.push(account.into()),
                    TokenAdminRole::UpdateAllowList => {
                        update_allow_list.accounts.push(account.into())
                    }
                    TokenAdminRole::UpdateDenyList => {
                        update_deny_list.accounts.push(account.into())
                    }
                    TokenAdminRole::Pause => pause.accounts.push(account.into()),
                    TokenAdminRole::UpdateMetadata => update_metadata.accounts.push(account.into()),
                }
            }
        }
        Ok(TokenAuthorizations {
            update_admin_roles: Some(update_admin_roles),
            mint: self.token_p9.is_mintable(context).then_some(mint),
            burn: self.token_p9.is_burnable(context).then_some(burn),
            update_allow_list: self
                .token_p9
                .has_allow_list(context)
                .then_some(update_allow_list),
            update_deny_list: self
                .token_p9
                .has_deny_list(context)
                .then_some(update_deny_list),
            pause: Some(pause),
            update_metadata: Some(update_metadata),
        })
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
