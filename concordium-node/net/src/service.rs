// TODO : impl network with tls layer

#[cfg(test)]
pub mod test {
    use rand::OsRng;
    use hacl_star::curve25519::*;

    #[test]
    fn test_sign_test() {
        // Basic curve test to see HACL* C binding works
        let (mut sk1, mut pk1) = (SecretKey::default(), PublicKey::default());
        let (mut sk2, mut pk2) = (SecretKey::default(), PublicKey::default());
        let (mut out1, mut out2) = ([0; 32], [0; 32]);

        keypair(OsRng::new().unwrap(), &mut sk1, &mut pk1);
        keypair(OsRng::new().unwrap(), &mut sk2, &mut pk2);

        sk1.exchange(&pk2, &mut out1);
        sk2.exchange(&pk1, &mut out2);

        assert_eq!(out1, out2);
    }
}