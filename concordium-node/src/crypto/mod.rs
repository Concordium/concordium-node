use snow::params::{CipherChoice, DHChoice, HashChoice, NoiseParams};

pub fn generate_snow_config(config: &crate::configuration::CryptoConfig) -> NoiseParams {
    let dh_choice = match config.dh_choice {
        DHChoice::Curve25519 => "25519",
        DHChoice::Ed448 => "448",
    };
    let cipher_choice = match config.cipher_choice {
        CipherChoice::ChaChaPoly => "ChaChaPoly",
        CipherChoice::AESGCM => "AESGCM",
    };
    let hash_choice = match config.hash_choice {
        HashChoice::SHA256 => "SHA256",
        HashChoice::SHA512 => "SHA512",
        HashChoice::Blake2s => "BLAKE2s",
        HashChoice::Blake2b => "BLAKE2b",
    };
    format!(
        "Noise_IKpsk2_{}_{}_{}",
        dh_choice, cipher_choice, hash_choice
    )
    .parse()
    .unwrap()
}
