use std::{
    env,
    fs::File,
    io::{self, Read},
};

use concordium_consensus::{
    block::GenesisData,
    common::{sha256, SerializeToBytes},
};

// for now it only reads genesis data, as only that is currently being dumped
fn read_block_dump(bytes: &[u8]) {
    let data = GenesisData::deserialize(bytes)
        .expect("Can't serialize the provided data as a GenesisData object!");

    println!("{:#?}\n\nshort hash: {:?}", data, sha256(bytes));
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
