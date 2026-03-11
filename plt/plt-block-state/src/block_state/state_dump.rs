pub mod shared;

use crate::block_state::PltBlockStateSavepoint;
use crate::block_state::blob_store::BackingStoreLoad;
use crate::block_state::state_dump::shared::{NodeId, StateDumpBuilder};

pub fn dump_plt_block_state(
    builder: &mut StateDumpBuilder,
    mut loader: impl BackingStoreLoad,
    parent_node: NodeId,
    block_state: &PltBlockStateSavepoint,
) {
    let hash = block_state.hash(& mut loader);
    builder.build_state_data(block_state.blob_ref.expect("blob reference not set"), hash.into_pure(), &block_state.block_state);

    builder.build_comp_node(parent_node, "pltstate", "pltstate", Some(hash.into_pure()));
}
