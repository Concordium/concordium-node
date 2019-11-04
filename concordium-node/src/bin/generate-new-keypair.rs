use concordium_common::blockchain_types::{AccountAddress, SchemeId};
use ed25519_dalek::Keypair;
use rand::thread_rng;
use std::result::Result;

// FIXME: Once the crypto id library exports relevant functions use them instead
// of using the dalek library directly.
fn main() -> Result<(), &'static str> {
    let mut csprng = thread_rng();
    let keys = Keypair::generate(&mut csprng);

    println!("Private key: {}", base64::encode(keys.secret.as_bytes()));
    println!("Public key: {}", base64::encode(keys.public.as_bytes()));
    println!(
        "Address: {}",
        AccountAddress::from((&keys.public.as_bytes()[..], SchemeId::Ed25519))
    );

    Ok(())
}
