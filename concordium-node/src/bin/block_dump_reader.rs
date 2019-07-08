use std::{
    env,
    fs::File,
    io::{self, Cursor, Read},
};

use concordium_global_state::{
    block::{Block, BlockData},
    common::SerializeToBytes,
};

// for now it only reads genesis data, as only that is currently being dumped
fn read_block_dump(bytes: &[u8]) {
    let mut cursor = Cursor::new(bytes);

    let genesis_data = BlockData::deserialize((&mut cursor, 0))
        .expect("Can't serialize the provided data as a GenesisData object!");
    assert_eq!(*BlockData::serialize(&genesis_data), *bytes);

    let genesis_block = Block {
        slot: 0,
        data: genesis_data,
    };

    println!(
        "{:#?}\n\n{:?}",
        genesis_block.genesis_data(),
        genesis_block
    );
}

fn main() -> io::Result<()> {
    let mut args = env::args();
    let _ = args.next();
    let filename = args
        .next()
        .expect("No arguments provided (expected a filename)!");
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();

    file.read_to_end(&mut buffer)?;

    read_block_dump(&buffer);

    Ok(())
}
