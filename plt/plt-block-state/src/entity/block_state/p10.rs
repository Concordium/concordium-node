use crate::entity::block_state::p9::BlockStateP9;

/// P10 block state.
#[derive(Debug)]
pub struct BlockStateP10<'a> {
    /// P9 block state
    pub p9_block_state: BlockStateP9<'a>,
}
