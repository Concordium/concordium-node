use std::rc::Rc;

use crate::{
    block::*,
    common::{HashBytes, SerializeToBytes, Slot},
    finalization::*,
};

use super::{
    messaging::{GlobalMetadata, GlobalStateError, GlobalStateResult},
    GlobalData, GlobalState,
};

impl<'a> GlobalState<'a> {
    pub fn get_serialized_metadata(&self) -> GlobalStateResult {
        let metadata = self.get_metadata().serialize();
        GlobalStateResult::SuccessfulQuery(metadata)
    }

    pub fn get_metadata(&self) -> GlobalMetadata {
        GlobalMetadata {
            finalized_height: self.data.get_last_finalized_height(),
            n_pending_blocks: self.data.live_blocks.len() as u64,
            state:            self.data.state,
        }
    }

    pub fn is_peer_metadata_better(&self, peer_metadata: GlobalMetadata) -> bool {
        let our_metadata = self.get_metadata();

        peer_metadata > our_metadata
    }

    pub fn get_stored_block(&'a self, hash: &BlockHash) -> GlobalStateResult {
        let reader = self.data.kvs_env.read().unwrap(); // infallible
        let block = self
            .data
            .block_store
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

impl<'a> GlobalData<'a> {
    pub fn get_block(&self, hash: &HashBytes, delta: Delta) -> Option<&Rc<BlockPtr>> {
        let target_block = self
            .live_blocks
            .get(hash)
            .or_else(|| self.finalized_blocks.get(hash));

        if delta == 0 {
            target_block
        } else {
            // obtain the block's descendant
            let reference_block = target_block?;
            self.get_blocks_at_height(reference_block.height + delta)
                .find(|&candidate| reference_block.is_ancestor_of(candidate))
        }
    }

    fn get_blocks_at_height(&self, height: BlockHeight) -> impl Iterator<Item = &Rc<BlockPtr>> {
        self.live_blocks
            .values()
            .filter(move |ptr| ptr.height == height)
            .chain(
                self.finalized_blocks
                    .values()
                    .filter(move |ptr| ptr.height == height),
            )
    }

    fn _get_finalization_record_by_hash(&self, hash: &HashBytes) -> Option<&FinalizationRecord> {
        self.finalization_records
            .iter()
            .rev()
            .filter_map(|e| e.as_ref())
            .find(|&rec| rec.block_pointer == *hash)
    }

    fn _get_finalization_record_by_idx(
        &self,
        idx: FinalizationIndex,
    ) -> Option<&FinalizationRecord> {
        self.finalization_records.get(idx as usize)
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

    pub(super) fn iter_tree_since(
        &self,
        since: BlockHeight,
    ) -> impl Iterator<Item = (&Block, Option<&FinalizationRecord>)> {
        let fs = self.finalization_span();

        let finalized_blocks = self
            .finalized_blocks
            .values()
            .skip(since as usize + 1)
            .map(|ptr| (ptr.height, &ptr.block));

        let mut finalization_records = self
            .finalization_records
            .into_iter()
            .skip((since / fs) as usize + 1)
            .filter_map(|rec| rec.as_ref());

        let pending_blocks = self.live_blocks.values().map(|ptr| &ptr.block);

        finalized_blocks
            .map(move |(height, block)| {
                if height % fs == 0 {
                    (block, finalization_records.next())
                } else {
                    (block, None)
                }
            })
            .chain(pending_blocks.map(|pb| (pb, None)))
    }
}
