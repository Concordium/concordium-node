use crate::entity::block_state::p9::BlockStateP9;

/// P10 block state.
#[derive(Debug, Default)]
pub struct BlockStateP10 {
    /// P9 block state
    pub p9_block_state: BlockStateP9,
}
