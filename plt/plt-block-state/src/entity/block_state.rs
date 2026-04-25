/// Mutable block state. In contrast to the immutable block state [`BlockState`],
/// operations on the mutable block state changes the state that
/// the value represents.
#[derive(Debug, Clone)]
pub struct MutableBlockState {
    /// Persistent block state value. The block state represented by [`MutableBlockState`] is
    /// mutated simply by setting a new value for the persistent block state [`PersistentBlockState`].
    persistent: PersistentBlockState,
}

impl MutableBlockState {
    /// Create mutable block state from immutable block state.
    fn new(mutable_state: BlockState) -> Self {
        Self {
            immutable_state: mutable_state,
        }
    }

    /// Consume the mutable block state and create an immutable block state.
    pub fn into_immutable(self) -> BlockState {
        self.immutable_state
    }

    /// Update the block state using `update` closure and return
    /// the additional value of type `T` returned by the closure.
    fn update_block_state<T>(
        &mut self,
        update: impl FnOnce(BlockStateData) -> BlockStateResult<(T, BlockStateData)>,
    ) -> BlockStateResult<T> {
        let ret;
        (ret, self.immutable_state.data) = update(mem::take(&mut self.immutable_state.data))?;
        Ok(ret)
    }

    /// Update the block state using `update` closure.
    fn update_block_state_(
        &mut self,
        update: impl FnOnce(BlockStateData) -> BlockStateResult<BlockStateData>,
    ) -> BlockStateResult<()> {
        self.immutable_state.data = update(mem::take(&mut self.immutable_state.data))?;
        Ok(())
    }
}
