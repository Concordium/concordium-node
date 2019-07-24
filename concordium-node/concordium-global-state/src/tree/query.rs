use chrono::prelude::Utc;

use std::rc::Rc;

use crate::{
    block::*,
    common::{HashBytes, SerializeToBytes, Slot},
    finalization::*,
};

use super::{
    messaging::{SkovError, SkovResult},
    Skov, SkovData,
};

/// Creates a function to obtain an object from Skov.
macro_rules! get_object {
    ($(#[$doc:meta])*$query_foo:ident($($query_arg:ident: $query_arg_ty:ty),+), $err_kind:ident, $query_stat:ident) => {
        $(#[$doc])*
        pub fn $query_foo(&mut self, $($query_arg: $query_arg_ty),+) -> SkovResult {
            let (result, query_duration) = timed!(self.data.$query_foo($($query_arg),+));

            self.stats.$query_stat.push(query_duration as u64);

            if let Some(result) = result {
                SkovResult::SuccessfulQuery(result.serialize())
            } else {
                SkovResult::Error(SkovError::$err_kind($($query_arg.to_owned()),+))
            }
        }
    }
}

impl<'a> Skov<'a> {
    get_object!(
        /// Queries Skov for a block with the given hash or, if the given
        /// `Delta` is greater than zero, for its descendant that is `delta`
        /// generations below it.
        get_block(hash: &HashBytes, delta: Delta),
        MissingBlock,
        query_block_timings
    );

    get_object!(
        /// Queries Skov for the finalization record of a block with the given
        /// hash.
        get_finalization_record_by_hash(hash: &HashBytes),
        MissingFinalizationRecordByHash,
        query_finalization_timings
    );

    get_object!(
        /// Queries Skov for the finalization record of the given finalization
        /// round index.
        get_finalization_record_by_idx(idx: FinalizationIndex),
        MissingFinalizationRecordByIdx,
        query_finalization_timings
    );
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

    fn get_finalization_record_by_hash(&self, hash: &HashBytes) -> Option<&FinalizationRecord> {
        self.finalization_list
            .iter()
            .rev()
            .filter_map(|e| e.as_ref())
            .find(|&rec| rec.block_pointer == *hash)
    }

    fn get_finalization_record_by_idx(
        &self,
        idx: FinalizationIndex,
    ) -> Option<&FinalizationRecord> {
        self.finalization_list.get(idx as usize)
    }

    pub fn get_last_finalized_slot(&self) -> Slot { self.last_finalized.block.slot() }

    pub fn get_last_finalized_height(&self) -> BlockHeight { self.last_finalized.height }

    pub fn get_next_finalization_index(&self) -> FinalizationIndex {
        self.get_last_finalized_height() + 1
    }
}
