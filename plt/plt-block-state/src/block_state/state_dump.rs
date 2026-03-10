pub mod shared;

use crate::block_state::PltBlockStateSavepoint;
use crate::block_state::blob_store::BackingStoreLoad;
use crate::block_state::state_dump::shared::{Context, NodeId};

pub fn dump_plt_block_state(
    context: &mut Context,
    _load_callback: impl BackingStoreLoad,
    parent_node: NodeId,
    block_state: &PltBlockStateSavepoint,
) {
    shared::build_state_data(context, &block_state.block_state);

    shared::build_comp_node(context, parent_node, "pltstate", "pltstate", None);
}
