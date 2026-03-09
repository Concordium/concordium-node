pub mod shared;

use crate::block_state::blob_store::BackingStoreLoad;
use crate::block_state::state_dump::shared::OutputFiles;
use crate::block_state::PltBlockStateSavepoint;

pub fn dump_plt_block_state(
    output: &mut OutputFiles,
    _load_callback: impl BackingStoreLoad,
    block_state: &PltBlockStateSavepoint,
) {
    shared::build_state_data(output, &block_state.block_state)
}
