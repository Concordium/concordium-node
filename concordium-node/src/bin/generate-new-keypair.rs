use std::result::Result;
use p2p_client::crypto::KeyPair;

fn main() -> Result<(), &'static str>
{
    let keys = KeyPair::new();

    println!( "Private key: {}", keys.private_key_as_base64());
    println!( "Public key: {}", keys.public_key_as_base64());
    println!( "Address: {}", keys.address());


    Ok(())
}

