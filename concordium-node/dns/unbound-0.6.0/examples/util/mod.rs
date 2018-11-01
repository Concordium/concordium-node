use std::net::Ipv4Addr;

pub fn data_to_ipv4(data: &[u8]) -> Ipv4Addr {
    assert_eq!(data.len(), 4);
    let mut octets = [0; 4];
    octets[..].copy_from_slice(data);
    Ipv4Addr::from(octets)
}
