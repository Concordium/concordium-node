use chrono::prelude::{DateTime, Utc};

use crate::{block::*, common::*, finalization::FinalizationRecord};

#[allow(dead_code)]
#[derive(Debug)]
pub struct BlockPtr {
    hash:           BlockHash,
    block:          Block,
    parent:         Option<Box<BlockPtr>>,
    last_finalized: Box<BlockPtr>,
    height:         BlockHeight,
    // state:       BlockState,
    received:  DateTime<Utc>,
    validated: DateTime<Utc>,
}

impl BlockPtr {
    pub fn new(
        pb: PendingBlock,
        parent: Self,
        last_finalized: Self,
        validated: DateTime<Utc>,
    ) -> Self {
        assert_eq!(parent.hash, pb.block.pointer());
        assert_eq!(last_finalized.hash, pb.block.last_finalized());

        let height = parent.height + 1;

        Self {
            hash: pb.hash,
            block: pb.block,
            parent: Some(Box::new(parent)),
            last_finalized: Box::new(last_finalized),
            height,
            // state: ,
            received: pb.received,
            validated,
        }
    }
}

#[derive(Debug)]
pub enum BlockStatus {
    Alive,
    Dead,
    Finalized(FinalizationRecord),
}

#[derive(Debug)]
pub struct PendingBlock {
    hash:     BlockHash,
    block:    Block,
    received: DateTime<Utc>,
}

impl PendingBlock {
    pub fn new(block: Block, received: DateTime<Utc>) -> Self {
        let hash = sha256(&block.serialize());

        Self {
            hash,
            block,
            received,
        }
    }
}

#[derive(Debug)]
pub struct Branches(Vec<BlockPtr>);
