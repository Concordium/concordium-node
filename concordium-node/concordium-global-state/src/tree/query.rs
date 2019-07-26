use std::rc::Rc;

use crate::{
    block::*,
    common::{HashBytes, SerializeToBytes, Slot},
    finalization::*,
};

use super::{
    messaging::{SkovMetadata, SkovResult},
    Skov, SkovData,
};

impl<'a> Skov<'a> {
    pub fn get_serialized_metadata(&self) -> SkovResult {
        SkovResult::SuccessfulQuery(self.get_metadata().serialize())
    }

    pub fn get_metadata(&self) -> SkovMetadata {
        SkovMetadata {
            finalized_height: self.data.get_last_finalized_height(),
            n_pending_blocks: self.data.tree_candidates.len() as u64,
            state:            self.data.state,
        }
    }

    pub fn is_peer_metadata_better(&self, peer_metadata: SkovMetadata) -> bool {
        let our_metadata = self.get_metadata();

        peer_metadata > our_metadata
    }
}

impl<'a> SkovData<'a> {
    pub fn get_block(&self, hash: &HashBytes, delta: Delta) -> Option<&Rc<BlockPtr>> {
        let target_block = self
            .tree_candidates
            .get(hash)
            .or_else(|| self.block_tree.get(hash));

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
        self.tree_candidates
            .values()
            .filter(move |ptr| ptr.height == height)
            .chain(
                self.block_tree
                    .values()
                    .filter(move |ptr| ptr.height == height),
            )
    }

    fn _get_finalization_record_by_hash(&self, hash: &HashBytes) -> Option<&FinalizationRecord> {
        self.finalization_list
            .iter()
            .rev()
            .filter_map(|e| e.as_ref())
            .find(|&rec| rec.block_pointer == *hash)
    }

    fn _get_finalization_record_by_idx(
        &self,
        idx: FinalizationIndex,
    ) -> Option<&FinalizationRecord> {
        self.finalization_list.get(idx as usize)
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
            .block_tree
            .values()
            .skip(since as usize + 1)
            .map(|ptr| (ptr.height, &ptr.block));

        let mut finalization_records = self
            .finalization_list
            .into_iter()
            .skip((since / fs) as usize + 1)
            .filter_map(|rec| rec.as_ref());

        let pending_blocks = self.tree_candidates.values().map(|ptr| &ptr.block);

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
