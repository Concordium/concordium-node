use p2p_client::utils::from_hex_string;
use snow::{params::NoiseParams, Builder};

// TODO Hard-coded keys, should be replace by random values?a
const PROLOGUE: &str = "CONCORDIUMP2P";
const PRE_SHARED_KEY: &str = "54686973206973206d7920417573747269616e20706572737065637469766521";

#[test]
fn ikpsk2_random_keys() {
    let params: NoiseParams = "Noise_IKpsk2_25519_AESGCM_SHA256".parse().unwrap();

    // Initiator
    let mut init_buf = vec![0u8; 65_535];
    let init_builder = Builder::new(params.clone());
    let init_keypair = init_builder.generate_keypair().unwrap();

    // Responsder
    let mut resp_buf = vec![0u8; 65_535];
    let resp_builder = Builder::new(params);
    let resp_keypair = resp_builder.generate_keypair().unwrap();

    let mut init_session = init_builder
        // .psk(2, &[32u8; 32])
        .prologue( PROLOGUE.as_bytes())
        .psk(2, &PRE_SHARED_KEY.as_bytes()[..32])
        .local_private_key( &init_keypair.private)
        .remote_public_key( &resp_keypair.public)
        .build_initiator().unwrap();

    let mut resp_session = resp_builder
        //.psk(2, &[32u8; 32])
        .prologue( PROLOGUE.as_bytes())
        .psk(2, &PRE_SHARED_KEY.as_bytes()[..32])
        .local_private_key( &resp_keypair.private)
        // .remote_public_key( &init_keypair.public)
        .build_responder().unwrap();

    // IKpsk2:
    // <- s

    // A -> e,es,s,ss
    let len = init_session.write_message(&[], &mut init_buf).unwrap();
    resp_session
        .read_message(&init_buf[..len], &mut resp_buf)
        .unwrap();

    // B -> e,ee,se,psk
    let len = resp_session.write_message(&[], &mut init_buf).unwrap();
    init_session
        .read_message(&init_buf[..len], &mut resp_buf)
        .unwrap();

    // Handshake done
    let mut init_session = init_session.into_transport_mode().unwrap();
    let mut resp_session = resp_session.into_transport_mode().unwrap();

    // 1. Initiator Send Handshake
    let clear_data = from_hex_string("434f4e434f524449554d503250010006e847e46a0100000000").unwrap();
    let len = init_session
        .write_message(&clear_data, &mut init_buf)
        .unwrap();
    let len = resp_session
        .read_message(&init_buf[..len], &mut resp_buf)
        .unwrap();
    assert_eq!(clear_data.as_slice(), &resp_buf[..len]);

    // 2. Initiator
    let clear_data = from_hex_string("434f4e434f524449554d503250010008e847e46a0100000201400000003836643635666561383461623562353735303837323262366461376662356461343337633334353835613364393362643939306439376233323833346163396264000300000000000000486579").unwrap();
    let len = init_session
        .write_message(&clear_data, &mut init_buf)
        .unwrap();
    let len = resp_session
        .read_message(&init_buf[..len], &mut resp_buf)
        .unwrap();
    assert_eq!(clear_data.as_slice(), &resp_buf[..len]);

    // 3.  Responder
    let clear_data =
        from_hex_string("434f4e434f524449554d5032500100446148e46a0100000003010000006400").unwrap();
    let len = resp_session
        .write_message(&clear_data, &mut resp_buf)
        .unwrap();
    let len = init_session
        .read_message(&resp_buf[..len], &mut init_buf)
        .unwrap();
    assert_eq!(clear_data.as_slice(), &init_buf[..len]);
}
