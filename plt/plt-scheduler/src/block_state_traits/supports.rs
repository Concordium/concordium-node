use plt_block_state::entity::block_state::p9::BlockStateP9;
use plt_block_state::entity::block_state::p11::BlockStateP11;

// todo ar remove?

/// Trait that allows to access if certain features are supported or not.
pub trait SupportsT {
    /// Whether the protocol version of the block supports RBAC token feature.
    fn support_rbac(&self) -> bool;

    /// Whether the protocol version of the block supports updating the token metadata.
    fn support_updating_metadata(&self) -> bool;
}

impl SupportsT for BlockStateP9 {
    fn support_rbac(&self) -> bool {
        false
    }

    fn support_updating_metadata(&self) -> bool {
        false
    }
}

impl SupportsT for BlockStateP11 {
    fn support_rbac(&self) -> bool {
        true
    }

    fn support_updating_metadata(&self) -> bool {
        true
    }
}
