//! Contains the collection of roles assigned for a single account.

use concordium_base::common;
use concordium_base::protocol_level_tokens::TokenAdminRole;
use plt_block_state::block_state::types::TokenStateValue;

/// List of every role
pub const ALL: &[TokenAdminRole] = &[
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
#[derive(Debug, common::Serialize)]
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
    pub fn into_state_value(self) -> Option<TokenStateValue> {
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
    pub fn try_from_state_value(value: Option<TokenStateValue>) -> common::ParseResult<Self> {
        let Some(value) = value else {
            return Ok(Roles::none());
        };
        common::from_bytes_complete(value)
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
