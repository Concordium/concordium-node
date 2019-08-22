use byteorder::{ByteOrder, NetworkEndian, WriteBytesExt};
use failure::Fallible;

use std::{
    io::{Cursor, Read, Write},
    mem::size_of,
};

use crate::{
    block::{BlockHash, BlockHeight},
    common::{create_serialization_cursor, SerializeToBytes},
};

#[derive(Debug)]
pub struct CatchUpStatus {
    is_request:              bool,
    last_finalized_block:    BlockHash,
    last_finalized_height:   BlockHeight,
    best_block:              BlockHash,
    finalization_justifiers: Box<[BlockHash]>,
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for CatchUpStatus {
    type Source = &'a [u8];

    fn deserialize(bytes: Self::Source) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let is_request = read_ty!(&mut cursor, bool)[0] != 0;
        let last_finalized_block = BlockHash::from(read_ty!(&mut cursor, BlockHash));
        let last_finalized_height = NetworkEndian::read_u64(&read_ty!(&mut cursor, BlockHeight));
        let best_block = BlockHash::from(read_ty!(&mut cursor, BlockHash));

        const ALLOCATION_LIMIT: usize = 1024;
        let finalization_justifiers = read_multiple!(
            cursor,
            "finalization justifiers",
            BlockHash::from(read_ty!(&mut cursor, BlockHash)),
            4
        );

        let status = CatchUpStatus {
            is_request,
            last_finalized_block,
            last_finalized_height,
            best_block,
            finalization_justifiers,
        };

        check_serialization!(status, cursor);

        Ok(status)
    }

    fn serialize(&self) -> Box<[u8]> {
        let justifiers_len = self.finalization_justifiers.len() * size_of::<BlockHash>();

        let mut cursor = create_serialization_cursor(
            size_of::<bool>()
                + size_of::<BlockHash>()
                + size_of::<BlockHeight>()
                + size_of::<BlockHash>()
                + size_of::<u32>()
                + justifiers_len,
        );

        let _ = cursor.write_u8(self.is_request as u8);
        let _ = cursor.write_all(&self.last_finalized_block);
        let _ = cursor.write_u64::<NetworkEndian>(self.last_finalized_height);
        let _ = cursor.write_all(&self.best_block);

        let _ = cursor.write_u32::<NetworkEndian>(self.finalization_justifiers.len() as u32);
        for fj in &*self.finalization_justifiers {
            let _ = cursor.write_all(fj);
        }

        cursor.into_inner()
    }
}
