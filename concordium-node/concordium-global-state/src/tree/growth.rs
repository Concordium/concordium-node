use concordium_common::PacketType;

use std::sync::Arc;

use crate::{block::*, transaction::BareTransaction};

use super::{messaging::GlobalStateResult, GlobalData, GlobalState};

impl GlobalState {
    pub fn add_transaction(
        &mut self,
        transaction: BareTransaction,
        finalized: bool,
    ) -> GlobalStateResult {
        if !finalized {
            self.data.add_non_finalized_transaction(transaction)
        } else {
            self.data.finalize_transaction(transaction)
        }
    }

    pub fn mutate_pending_to_pointer(
        &mut self,
        pending_block: &PendingBlock,
        height: u64,
    ) -> Arc<BlockPtr> {
        self.data.mutate_pending_to_pointer(&pending_block, height)
    }
}

impl GlobalData {
    pub(crate) fn mutate_pending_to_pointer(
        &mut self,
        pending_block: &PendingBlock,
        height: u64,
    ) -> Arc<BlockPtr> {
        let block_ptr = BlockPtr::new(&pending_block, height).unwrap();
        Arc::new(block_ptr)
    }

    fn add_non_finalized_transaction(&mut self, transaction: BareTransaction) -> GlobalStateResult {
        self.transaction_table.insert(transaction, false);
        GlobalStateResult::SuccessfulEntry(PacketType::Transaction)
    }

    fn finalize_transaction(&mut self, transaction: BareTransaction) -> GlobalStateResult {
        self.transaction_table.insert(transaction, true);
        GlobalStateResult::SuccessfulEntry(PacketType::Transaction)
    }
}
