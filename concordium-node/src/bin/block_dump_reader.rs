use std::{
    env,
    fs::File,
    io::{self, Cursor, Read},
};

use concordium_common::serial::Serial;
use globalstate_rust::block::{Block, BlockData};

// for now it only reads genesis data, as only that is currently being dumped
fn read_block_dump(bytes: &[u8]) {
    let mut cursor = Cursor::new(bytes);

    let genesis_data = BlockData::deserial_with_param(&mut cursor, 0)
        .expect("Can't deserialize the provided data as a GenesisData object!");
    let mut serialized = Vec::new();
    genesis_data.serial(&mut serialized).unwrap();

    assert_eq!(serialized.as_slice(), bytes);

    let genesis_block = Block {
        slot: 0,
        data: genesis_data,
    };

    println!("{:#?}\n\n{:?}", genesis_block.genesis_data(), genesis_block);
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
