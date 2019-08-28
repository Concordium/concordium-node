use crate::sys::*;
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};

const DNS_ANCHOR_1: &str = ". IN DNSKEY 257 3 8 AwEAAaz/tAm8yTn4Mfeh5eyI96WSVexTBAvkMgJzkKTOiW1vkIbzxeF3+/4RgWOq7HrxRixHlFlExOLAJr5emLvN7SWXgnLh4+B5xQlNVz8Og8kvArMtNROxVQuCaSnIDdD5LKyWbRd2n9WGe2R8PzgCmr3EgVLrjyBxWezF0jLHwVN8efS3rCj/EWgvIWgb9tarpVUDK/b58Da+sqqls3eNbuv7pr+eoZG+SrDK6nWeL3c6H5Apxz7LjVc1uTIdsIXxuOLYA4/ilBmSVIzuDWfdRUfhHdY6+cn8HFRm+2hM8AnXGXws9555KrUB5qihylGa8subX2Nn6UwNR1AkUTV74bU=";
const DNS_ANCHOR_2: &str = ". IN DNSKEY 256 3 8 AwEAAYvxrQOOujKdZz+37P+oL4l7e35/0diH/mZITGjlp4f81ZGQK42HNxSfkiSahinPR3t0YQhjC393NX4TorSiTJy76TBWddNOkC/IaGqcb4erU+nQ75k2Lf0oIpA7qTCk3UkzYBqhKDHHAr2UditE7uFLDcoX4nBLCoaH5FtfxhUqyTlRu0RBXAEuKO+rORTFP0XgA5vlzVmXtwCkb9G8GknHuO1jVAwu3syPRVHErIbaXs1+jahvWWL+Do4wd+lA+TL3+pUk+zKTD2ncq7ZbJBZddo9T7PZjvntWJUzIHIMWZRFAjpi+V7pgh0o1KYXZgDUbiA1s9oLAL1KLSdmoIYM=";
const DNS_ANCHOR_3: &str = ". IN DNSKEY 257 3 8 AwEAAagAIKlVZrpC6Ia7gEzahOR+9W29euxhJhVVLOyQbSEW0O8gcCjFFVQUTf6v58fLjwBd0YI0EzrAcQqBGCzh/RStIoO8g0NfnfL2MTJRkxoXbfDaUeVPQuYEhg37NZWAJQ9VnMVDxP/VHL496M/QZxkjf5/Efucp2gaDX6RS6CXpoY68LsvPVjR0ZSwzz1apAzvN9dlzEheX7ICJBBtuA6G3LQpzW5hOA2hzCTMjJPJ8LbqF6dsV6DoBQzgul0sGIcGOYl7OyQdXfZ57relSQageu+ipAdTTJ25AsRTAoub8ONGcLmqrAmRLKBP1dfwhYB4N7knNnulqQxA+Uk1ihz0=";

#[derive(Copy, Clone, Debug)]
enum LookupType {
    A    = 1,
    AAAA = 28,
    TXT  = 16,
}

pub fn resolve_dns_txt_record(
    entry: &str,
    dns_servers: &[IpAddr],
    no_dnssec_fail: bool,
) -> Result<Vec<String>, String> {
    debug!(
        "Attempting to resolve TXT record {} using DNS server {:?}",
        entry, dns_servers
    );
    resolve_dns_record(entry, dns_servers, no_dnssec_fail, LookupType::TXT)
}

pub fn resolve_dns_a_record(
    entry: &str,
    dns_servers: &[IpAddr],
    no_dnssec_fail: bool,
) -> Result<Vec<String>, String> {
    debug!(
        "Attempting to resolve A record {} using DNS server {:?}",
        entry, dns_servers
    );
    resolve_dns_record(entry, dns_servers, no_dnssec_fail, LookupType::A)
}

pub fn resolve_dns_aaaa_record(
    entry: &str,
    dns_servers: &[IpAddr],
    no_dnssec_fail: bool,
) -> Result<Vec<String>, String> {
    debug!(
        "Attempting to resolve AAAA record {} using DNS server {:?}",
        entry, dns_servers
    );
    resolve_dns_record(entry, dns_servers, no_dnssec_fail, LookupType::AAAA)
}

fn resolve_dns_record(
    entry: &str,
    dns_servers: &[IpAddr],
    no_dnssec_fail: bool,
    record_type: LookupType,
) -> Result<Vec<String>, String> {
    let mut res = vec![];

    let ctx = Context::new().unwrap();

    if let Err(err) = ctx.add_ta(DNS_ANCHOR_1) {
        error!("error adding key 1: {}", err);
        return Err("Error adding key 1!".to_string());
    }

    if let Err(err) = ctx.add_ta(DNS_ANCHOR_2) {
        error!("error adding key 2: {}", err);
        return Err("Error adding key 2!".to_string());
    }

    if let Err(err) = ctx.add_ta(DNS_ANCHOR_3) {
        error!("error adding key 3: {}", err);
        return Err("Error adding key 3!".to_string());
    }

    // Add forward resolvers
    for ip in dns_servers {
        debug!("Using DNS resolver: {}", ip);
        if let Err(err) = ctx.set_fwd(ip) {
            error!("error adding forwarder: {}", err);
            return Err("Error adding forwarder!".to_string());
        }
    }

    match ctx.resolve(entry, record_type as u16, 1) {
        Ok(ans) => {
            if !no_dnssec_fail && !ans.secure() {
                error!("DNSSEC validation failed!");
                return Err("DNSSEC validation failed!".to_string());
            }

            match record_type {
                LookupType::A => {
                    res.extend(
                        ans.data()
                            .map(|data| data_to_ipv4(data).to_string())
                            .inspect(|ip| debug!("The address is {}", ip)),
                    );
                }
                LookupType::AAAA => {
                    res.extend(
                        ans.data()
                            .map(|data| data_to_ipv6(data).to_string())
                            .inspect(|ip| debug!("The address is {}", ip)),
                    );
                }
                LookupType::TXT => {
                    for data in ans.data() {
                        match String::from_utf8(data[1..].to_vec()) {
                            Ok(read_s) => res.push(read_s),
                            Err(e) => error!("Can't read UTF8 string due to {}", e.utf8_error()),
                        }
                    }
                }
            }
        }
        Err(err) => {
            error!("resolve error: {}", err);
            return Err("Couldn't resolve!".to_string());
        }
    }

    debug!("The following records were found: {:?}", res);

    Ok(res)
}

fn data_to_ipv4(data: &[u8]) -> Ipv4Addr {
    assert_eq!(data.len(), 4);
    let mut octets = [0; 4];
    octets.copy_from_slice(data);

    let ip = Ipv4Addr::from(octets);

    debug!("Got the following IPv4 address: {}", ip);

    ip
}

fn data_to_ipv6(data: &[u8]) -> Ipv6Addr {
    assert_eq!(data.len(), 16);
    let mut octets = [0; 16];
    octets.copy_from_slice(data);

    let ip = Ipv6Addr::from(octets);

    debug!("Got the following IPv6 address: {}", ip);

    ip
}

#[cfg(test)]
mod tests {
    use crate::dns::*;
    use std::str::FromStr;

    #[test]
    pub fn test_googledns_resolve_dns() {
        let res = resolve_dns_txt_record(
            "concordium.com",
            &[IpAddr::from_str("8.8.8.8").unwrap()],
            false,
        );
        match res {
            Ok(ref resps) => assert_eq!(resps.len(), 5),
            Err(e) => panic!("{}", e),
        }
    }

    #[test]
    pub fn test_googledns_resolve_dns_fail() {
        let res = resolve_dns_txt_record(
            "www.dnssec-failed.org",
            &[IpAddr::from_str("8.8.8.8").unwrap()],
            false,
        );
        if res.is_ok() {
            panic!("This shouldn't happen - we got a valid response");
        }
    }

    #[test]
    #[ignore]
    pub fn _test_quadnine_resolve_dns_fail() {
        let res = resolve_dns_txt_record(
            "www.dnssec-failed.org",
            &[IpAddr::from_str("9.9.9.9").unwrap()],
            false,
        );
        if res.is_ok() {
            panic!("This shouldn't happen - we got a valid response");
        }
    }

    #[test]
    #[ignore]
    pub fn _test_norton_resolve_dns() {
        let res = resolve_dns_txt_record(
            "concordium.com",
            &[IpAddr::from_str("199.85.126.20").unwrap()],
            false,
        );
        match res {
            Ok(ref resps) => assert_eq!(resps.len(), 5),
            Err(e) => panic!("{}", e),
        }
    }

    #[test]
    #[ignore]
    pub fn _test_norton_resolve_dns_fail() {
        let res = resolve_dns_txt_record(
            "www.dnssec-failed.org",
            &[IpAddr::from_str("199.85.126.20").unwrap()],
            false,
        );
        if res.is_ok() {
            panic!("This shouldn't happen - we got a valid response");
        }
    }

    #[test]
    #[ignore]
    pub fn _test_quadnine_resolve_dns() {
        let res = resolve_dns_txt_record(
            "concordium.com",
            &[IpAddr::from_str("9.9.9.9").unwrap()],
            false,
        );
        match res {
            Ok(ref resps) => assert_eq!(resps.len(), 4),
            Err(e) => panic!("{}", e),
        }
    }

    #[test]
    pub fn test_cloudflare_resolve_dns_fail() {
        let res = resolve_dns_txt_record(
            "www.dnssec-failed.org",
            &[IpAddr::from_str("1.1.1.1").unwrap()],
            false,
        );
        if res.is_ok() {
            panic!("This shouldn't happen - we got a valid response");
        }
    }

    #[test]
    pub fn test_cloudflare_resolve_dns() {
        let res = resolve_dns_txt_record(
            "concordium.com",
            &[IpAddr::from_str("1.1.1.1").unwrap()],
            false,
        );
        match res {
            Ok(ref resps) => assert_eq!(resps.len(), 5),
            Err(e) => panic!("{}", e),
        }
    }

    #[test]
    #[ignore]
    pub fn _test_comodo_resolve_dns_fail() {
        let res = resolve_dns_txt_record(
            "www.dnssec-failed.org",
            &[IpAddr::from_str("8.26.56.26").unwrap()],
            false,
        );
        if res.is_ok() {
            panic!("This shouldn't happen - we got a valid response");
        }
    }

    #[test]
    pub fn test_cleanbrowsing_resolve_dns() {
        let res = resolve_dns_txt_record(
            "concordium.com",
            &[IpAddr::from_str("185.228.168.168").unwrap()],
            false,
        );
        match res {
            Ok(ref resps) => assert_eq!(resps.len(), 5),
            Err(e) => panic!("{}", e),
        }
    }

    #[test]
    #[ignore]
    pub fn _test_comodo_resolve_dns() {
        let res = resolve_dns_txt_record(
            "concordium.com",
            &[IpAddr::from_str("8.26.56.26").unwrap()],
            false,
        );
        match res {
            Ok(ref resps) => assert_eq!(resps.len(), 5),
            Err(e) => panic!("{}", e),
        }
    }

    #[test]
    pub fn test_cleanbrowsing_resolve_dns_fail() {
        let res = resolve_dns_txt_record(
            "www.dnssec-failed.org",
            &[IpAddr::from_str("185.228.168.168").unwrap()],
            false,
        );
        if res.is_ok() {
            panic!("This shouldn't happen - we got a valid response");
        }
    }

    #[test]
    pub fn test_opendns_resolve_nodnssec_fail() {
        let res = resolve_dns_txt_record(
            "concordium.com",
            &[IpAddr::from_str("208.67.220.220").unwrap()],
            false,
        );
        if res.is_ok() {
            panic!("This shouldn't happen - we got a valid response");
        }
    }

    #[test]
    pub fn test_yandex_resolve_nodnssec_fail() {
        let res = resolve_dns_txt_record(
            "concordium.com",
            &[IpAddr::from_str("77.88.8.7").unwrap()],
            false,
        );
        if res.is_ok() {
            panic!("This shouldn't happen - we got a valid response");
        }
    }

    #[test]
    pub fn test_yandex_resolve_nodnssec_fail_nodnssec_test() {
        let res = resolve_dns_txt_record(
            "concordium.com",
            &[IpAddr::from_str("77.88.8.7").unwrap()],
            true,
        );
        match res {
            Ok(ref resps) => assert_eq!(resps.len(), 5),
            Err(e) => panic!("{}", e),
        }
    }

    #[test]
    pub fn test_googledns_resolve_a_record() {
        let res = resolve_dns_a_record("google.com", &[IpAddr::from_str("8.8.8.8").unwrap()], true);
        match res {
            Ok(ref resps) => assert!(!resps.is_empty()),
            Err(e) => panic!("{}", e),
        }
    }

    #[test]
    pub fn test_googledns_resolve_aaaa_record() {
        let res =
            resolve_dns_aaaa_record("google.com", &[IpAddr::from_str("8.8.8.8").unwrap()], true);
        match res {
            Ok(ref resps) => assert!(!resps.is_empty()),
            Err(e) => panic!("{}", e),
        }
    }
}
