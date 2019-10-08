use byteorder::{ByteOrder, NetworkEndian, ReadBytesExt, WriteBytesExt};
use failure::Fallible;

use crate::{block::BlockHeight, common::read_ty};

use concordium_common::{
    blockchain_types::BlockHash,
    serial::{NoParam, Serial},
};

#[derive(Debug)]
pub struct CatchUpStatus {
    is_request:              bool,
    last_finalized_block:    BlockHash,
    last_finalized_height:   BlockHeight,
    best_block:              BlockHash,
    finalization_justifiers: Box<[BlockHash]>,
}

impl Serial for CatchUpStatus {
    type Param = NoParam;

    fn deserial<R: ReadBytesExt>(source: &mut R) -> Fallible<Self> {
        let is_request = read_ty!(source, bool)[0] != 0;
        let last_finalized_block = BlockHash::from(read_ty!(source, BlockHash));
        let last_finalized_height = BlockHeight::deserial(source)?;
        let best_block = BlockHash::from(read_ty!(source, BlockHash));

        let finalization_justifiers = read_multiple!(
            source,
            BlockHash::from(read_ty!(source, BlockHash)),
            4,
            1024
        );

        let status = CatchUpStatus {
            is_request,
            last_finalized_block,
            last_finalized_height,
            best_block,
            finalization_justifiers,
        };

        Ok(status)
    }

    fn serial<W: WriteBytesExt>(&self, target: &mut W) -> Fallible<()> {
        target.write_u8(self.is_request as u8)?;
        target.write_all(&self.last_finalized_block)?;
        self.last_finalized_height.serial(target)?;
        target.write_all(&self.best_block)?;

        target.write_u32::<NetworkEndian>(self.finalization_justifiers.len() as u32)?;
        for fj in &*self.finalization_justifiers {
            target.write_all(fj)?;
        }

        Ok(())
    }
}
