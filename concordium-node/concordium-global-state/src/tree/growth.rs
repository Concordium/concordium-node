use chrono::prelude::Utc;
use concordium_common::PacketType;
use rkv::Value;

use std::rc::Rc;

use crate::{
    block::*,
    common::{sha256, HashBytes, SerializeToBytes},
    finalization::*,
};

use super::{
    messaging::{GlobalStateError, GlobalStateResult},
    GlobalData, GlobalState,
    PendingQueueType::{self, *},
};

/// Creates a function to add a new object to GlobalState.
macro_rules! add_entry {
    ($(#[$doc:meta])*$entry_foo:ident, $entry_type:ty, $addition_stat:ident, $timestamp_stat:ident) => {
        $(#[$doc])*
        pub fn $entry_foo(&mut self, entry: $entry_type) -> GlobalStateResult {
            let timestamp_entry = Utc::now();
            let (result, addition_duration) = timed!(self.data.$entry_foo(entry));

            self.stats.$addition_stat.push(addition_duration as u64);

            if let GlobalStateResult::SuccessfulEntry(_) = result {
                self.stats.$timestamp_stat.push(timestamp_entry);
            };

            result
        }
    };
}

impl<'a> GlobalState<'a> {
    add_entry!(
        /// Attempts to include a `PendingBlock` in the block tree candidate
        /// queue.
        // This operation can succeed only if the parent block and last
        // finalized block of the candidate entry are already in the tree.
        // Successful inclusion can result in existing pending blocks being
        // promoted to the candidate queue as well.
        add_block,
        PendingBlock,
        add_block_timings,
        block_arrival_times
    );

    add_entry!(
        /// Attempts to finalize a block referenced by the given
        /// `FinalizationRecord`.
        // A successful finalization results in the target block being moved
        // from the tree candidate queue to the block tree; it is also followed
        // by a housekeeping operation that involves checking whether any
        // existing tree candidates are not waiting for that block to be
        // finalized.
        add_finalization,
        FinalizationRecord,
        add_finalization_timings,
        finalization_times
    );

    pub fn store_serialized_block(&mut self, serialized_block: &[u8]) {
        self.data.store_serialized_block(serialized_block);
    }
}

impl<'a> GlobalData<'a> {
    pub(crate) fn store_block(&mut self, block_ptr: &BlockPtr) {
        let mut kvs_writer = self.kvs_env.write().unwrap(); // infallible

        self.finalized_block_store
            .put(
                &mut kvs_writer,
                block_ptr.hash.clone(),
                &Value::Blob(&block_ptr.serialize()),
            )
            .expect("Can't store a block!");
    }

    fn store_serialized_block(&mut self, serialized_block: &[u8]) {
        let mut kvs_writer = self.kvs_env.write().unwrap(); // infallible

        self.finalized_block_store
            .put(
                &mut kvs_writer,
                sha256(serialized_block),
                &Value::Blob(serialized_block),
            )
            .expect("Can't store a block!");
    }

    fn add_block(&mut self, pending_block: PendingBlock) -> GlobalStateResult {
        // verify that the pending block's parent block is among tree candidates
        // or already in the tree
        let parent_hash = pending_block.block.pointer().unwrap(); // safe

        let parent_block = if let Some(parent_ptr) = self.get_block(&parent_hash, 0) {
            parent_ptr
        } else {
            let error = GlobalStateError::MissingParentBlock(
                parent_hash.clone(),
                pending_block.hash.clone(),
            );

            self.queue_pending_block(AwaitingParentBlock, parent_hash.to_owned(), pending_block);

            return GlobalStateResult::Error(error);
        };

        let last_finalized = pending_block.block.last_finalized().unwrap(); // safe

        // verify that the pending block's last finalized block is in the block tree
        // (which entails that it had been finalized); if not, check the tree candidate
        // queue
        if self.finalized_blocks.get(&last_finalized).is_some() {
            // nothing to do here
        } else if self.live_blocks.get(&last_finalized).is_some() {
            let error = GlobalStateError::LastFinalizedNotFinalized(
                last_finalized.clone(),
                pending_block.hash.clone(),
            );

            self.queue_pending_block(
                AwaitingLastFinalizedFinalization,
                last_finalized.clone(),
                pending_block,
            );

            return GlobalStateResult::Error(error);
        } else {
            let error = GlobalStateError::MissingLastFinalizedBlock(
                last_finalized.clone(),
                pending_block.hash.clone(),
            );

            self.queue_pending_block(
                AwaitingLastFinalizedBlock,
                last_finalized.clone(),
                pending_block,
            );

            return GlobalStateResult::Error(error);
        }

        // verify if the pending block's last finalized block is actually the last
        // finalized one
        if *last_finalized != self.last_finalized.hash {
            let error = GlobalStateError::InvalidLastFinalized(
                last_finalized.clone(),
                pending_block.hash.clone(),
            );

            // for now, don't break on unaligned last finalized block
            warn!("{:?}", error);
        }

        // if the above checks pass, a BlockPtr can be created
        let block_ptr = BlockPtr::new(
            pending_block,
            Rc::clone(parent_block),
            Rc::clone(&self.last_finalized),
            Utc::now(),
        );

        let housekeeping_hash = block_ptr.hash.clone();

        // put the new block pointer in the tree candidate queue where it will await a
        // finalization record
        let insertion_result = self
            .live_blocks
            .insert(block_ptr.hash.clone(), Rc::new(block_ptr));

        // the block is now in the tree candidate queue; run housekeeping that
        // can possibly promote some other queued pending blocks
        self.refresh_finalization_record_queue(&housekeeping_hash);
        self.refresh_pending_queue(AwaitingParentBlock, &housekeeping_hash);
        self.refresh_pending_queue(AwaitingLastFinalizedBlock, &housekeeping_hash);

        if insertion_result.is_none() {
            GlobalStateResult::SuccessfulEntry(PacketType::Block)
        } else {
            GlobalStateResult::DuplicateEntry
        }
    }

    fn add_finalization(&mut self, record: FinalizationRecord) -> GlobalStateResult {
        let last_finalized_idx = self.get_last_finalized_height(); // safe, always there

        if record.block_pointer == self.last_finalized.hash {
            // we always get N-1 duplicate finalization records from the last round
            return GlobalStateResult::SuccessfulEntry(PacketType::FinalizationRecord);
        }

        // check if the record's index is in the future; if it is, keep the record
        // for later and await further blocks
        if record.index > last_finalized_idx + 1 {
            let error =
                GlobalStateError::FutureFinalizationRecord(record.index, last_finalized_idx);
            self.inapplicable_finalization_records
                .insert(record.block_pointer.clone(), record);
            return GlobalStateResult::Error(error);
        }

        let housekeeping_hash = record.block_pointer.clone();

        let target_block = self.live_blocks.remove(&record.block_pointer);

        let (target_hash, target_block) = if target_block.is_some() {
            (record.block_pointer.clone(), target_block.unwrap())
        } else {
            let error = GlobalStateError::MissingBlockToFinalize(record.block_pointer.clone());

            self.inapplicable_finalization_records
                .insert(record.block_pointer.clone(), record);

            return GlobalStateResult::Error(error);
        };

        // drop the now-redundant old pending finalization records
        self.inapplicable_finalization_records
            .retain(|_, rec| rec.index > record.index);

        // we don't want to break the natural order when catching up
        if target_block.height > self.last_finalized.height {
            self.last_finalized = Rc::clone(&target_block);
        }
        self.finalization_records
            .insert(record.index as usize, record);

        // prune the tree candidate queue, as some of the blocks can probably be dropped
        // now
        self.refresh_candidate_list();

        // store the directly finalized block after the indirectly finalized ones
        self.finalized_blocks
            .insert(target_hash.clone(), Rc::clone(&target_block));
        self.store_block(&target_block);

        // a new finalization record was registered; check for any blocks pending their
        // last finalized block's finalization
        self.refresh_pending_queue(AwaitingLastFinalizedFinalization, &housekeeping_hash);

        GlobalStateResult::SuccessfulEntry(PacketType::FinalizationRecord)
    }

    fn refresh_pending_queue(&mut self, queue: PendingQueueType, target_hash: &HashBytes) {
        if let Some(affected_blocks) = self.pending_queue_mut(queue).remove(target_hash) {
            for pending_block in affected_blocks {
                debug!("Reattempted to add block {:?} to the tree", target_hash);
                // silence errors here, as it is a housekeeping operation
                let _ = self.add_block(pending_block);
            }
        }
    }

    fn refresh_finalization_record_queue(&mut self, target_hash: &HashBytes) {
        if let Some(applicable_record) = self.inapplicable_finalization_records.remove(target_hash)
        {
            debug!(
                "Reattempted to apply finalization record for block {:?}",
                target_hash
            );
            // silence errors here, as it is a housekeeping operation
            let _ = self.add_finalization(applicable_record);
        }
    }

    fn refresh_candidate_list(&mut self) {
        // after a finalization round, the blocks that were not directly finalized, but
        // are a part of the tree, need to be promoted to the tree
        let mut finalized_parent = self.last_finalized.block.pointer().unwrap().to_owned();
        let mut indirectly_finalized_blocks = Vec::with_capacity(self.finalization_span() as usize);
        while let Some(ptr) = self.live_blocks.remove(&finalized_parent) {
            let parent_hash = ptr.block.pointer().unwrap().to_owned(); // safe, always available
            indirectly_finalized_blocks.push(ptr);
            finalized_parent = parent_hash;
        }

        // store the blocks in the correct order
        for ptr in indirectly_finalized_blocks.into_iter().rev() {
            self.store_block(&ptr);
            self.finalized_blocks.insert(ptr.hash.clone(), ptr);
        }
        // afterwards, as long as a catch-up phase is complete, the candidates with
        // surplus height can be removed
        // if self.state == ProcessingState::Complete {
        // let current_height = self.last_finalized.height;
        //
        // self.live_blocks
        // .retain(|_, candidate| candidate.height >= current_height)
        // }
    }

    fn queue_pending_block(
        &mut self,
        queue: PendingQueueType,
        missing_entry: HashBytes,
        pending_block: PendingBlock,
    ) {
        let queued = self
            .pending_queue_mut(queue)
            .entry(missing_entry)
            .or_default();

        queued.insert(pending_block);
    }
}
