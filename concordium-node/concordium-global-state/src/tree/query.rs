use crate::{
    block::*,
    common::{HashBytes, Slot},
    finalization::*,
};

use std::sync::Arc;

use super::{
    messaging::{GlobalStateError, GlobalStateResult},
    GlobalData, GlobalState,
};

use concordium_common::blockchain_types::BlockHash;

impl GlobalState {
    pub fn get_stored_block(self, hash: &BlockHash) -> GlobalStateResult {
        let kvs = self.data.kvs_env.read().unwrap(); // infallible
        let reader = kvs.read().unwrap();
        let block = self
            .data
            .finalized_block_store
            .get(&reader, hash)
            .expect("Can't obtain a block from the store!")
            .map(|blob| blob.to_bytes().unwrap().into_boxed_slice()); // infallible

        if let Some(block) = block {
            GlobalStateResult::SuccessfulQuery(block)
        } else {
            GlobalStateResult::Error(GlobalStateError::MissingBlock(hash.to_owned()))
        }
    }
}

impl GlobalData {
    pub fn get_block(&self, hash: &HashBytes, _delta: Delta) -> Option<&Arc<BlockPtr>> {
        self.live_blocks.get(hash)
    }

    pub fn finalization_span(&self) -> u64 {
        // FIXME: when we fully deserialize the genesis data again, use its finalization
        // span value instead of a hardcoded value
        10
    }

    pub fn get_last_finalized_slot(&self) -> Slot { self.last_finalized.block.slot() }

    pub fn get_last_finalized_height(&self) -> BlockHeight { self.last_finalized.height }

    pub fn get_next_finalization_index(&self) -> FinalizationIndex {
        self.get_last_finalized_height() + 1
    }
}
