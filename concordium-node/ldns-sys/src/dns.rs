#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
use std::ffi::{CStr, CString};
use std::net::IpAddr;
use std::ptr;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

const DNS_ANCHOR_1: &'static str = ". IN DNSKEY 257 3 8 AwEAAaz/tAm8yTn4Mfeh5eyI96WSVexTBAvkMgJzkKTOiW1vkIbzxeF3+/4RgWOq7HrxRixHlFlExOLAJr5emLvN7SWXgnLh4+B5xQlNVz8Og8kvArMtNROxVQuCaSnIDdD5LKyWbRd2n9WGe2R8PzgCmr3EgVLrjyBxWezF0jLHwVN8efS3rCj/EWgvIWgb9tarpVUDK/b58Da+sqqls3eNbuv7pr+eoZG+SrDK6nWeL3c6H5Apxz7LjVc1uTIdsIXxuOLYA4/ilBmSVIzuDWfdRUfhHdY6+cn8HFRm+2hM8AnXGXws9555KrUB5qihylGa8subX2Nn6UwNR1AkUTV74bU=";
const DNS_ANCHOR_2: &'static str = ". IN DNSKEY 256 3 8 AwEAAYvxrQOOujKdZz+37P+oL4l7e35/0diH/mZITGjlp4f81ZGQK42HNxSfkiSahinPR3t0YQhjC393NX4TorSiTJy76TBWddNOkC/IaGqcb4erU+nQ75k2Lf0oIpA7qTCk3UkzYBqhKDHHAr2UditE7uFLDcoX4nBLCoaH5FtfxhUqyTlRu0RBXAEuKO+rORTFP0XgA5vlzVmXtwCkb9G8GknHuO1jVAwu3syPRVHErIbaXs1+jahvWWL+Do4wd+lA+TL3+pUk+zKTD2ncq7ZbJBZddo9T7PZjvntWJUzIHIMWZRFAjpi+V7pgh0o1KYXZgDUbiA1s9oLAL1KLSdmoIYM=";
const DNS_ANCHOR_3: &'static str = ". IN DNSKEY 257 3 8 AwEAAagAIKlVZrpC6Ia7gEzahOR+9W29euxhJhVVLOyQbSEW0O8gcCjFFVQUTf6v58fLjwBd0YI0EzrAcQqBGCzh/RStIoO8g0NfnfL2MTJRkxoXbfDaUeVPQuYEhg37NZWAJQ9VnMVDxP/VHL496M/QZxkjf5/Efucp2gaDX6RS6CXpoY68LsvPVjR0ZSwzz1apAzvN9dlzEheX7ICJBBtuA6G3LQpzW5hOA2hzCTMjJPJ8LbqF6dsV6DoBQzgul0sGIcGOYl7OyQdXfZ57relSQageu+ipAdTTJ25AsRTAoub8ONGcLmqrAmRLKBP1dfwhYB4N7knNnulqQxA+Uk1ihz0=";

#[derive(Copy, Clone, Debug)]
enum LookupType {
    ARecord,
    AAAARecord,
    TXTRecord,
}

pub fn resolve_dns_txt_record(entry: &str,
                              dns_servers: &Vec<IpAddr>,
                              no_dnssec_fail: bool)
                              -> Result<Vec<String>, String> {
    resolve_dns_record(entry, dns_servers, no_dnssec_fail, LookupType::TXTRecord)
}

pub fn resolve_dns_a_record(entry: &str,
                            dns_servers: &Vec<IpAddr>,
                            no_dnssec_fail: bool)
                            -> Result<Vec<String>, String> {
    resolve_dns_record(entry, dns_servers, no_dnssec_fail, LookupType::ARecord)
}

pub fn resolve_dns_aaaa_record(entry: &str,
                               dns_servers: &Vec<IpAddr>,
                               no_dnssec_fail: bool)
                               -> Result<Vec<String>, String> {
    resolve_dns_record(entry, dns_servers, no_dnssec_fail, LookupType::AAAARecord)
}

fn resolve_dns_record(entry: &str,
                      dns_servers: &Vec<IpAddr>,
                      no_dnssec_fail: bool,
                      record_type: LookupType)
                      -> Result<Vec<String>, String> {
    let mut res: Vec<String> = vec![];
    let mut err: Option<String> = None;
    unsafe {
        match CString::new(entry) {
            Ok(ref s) => {
                let mut domain = ldns_dname_new_frm_str(s.as_ptr());
                if !domain.is_null() {
                    if ldns_dname_str_absolute(s.as_ptr()) != 0 && ldns_dname_absolute(domain) == 0
                    {
                        ldns_rdf_set_size(domain, ldns_rdf_size(domain) - 1);
                    }
                    let mut resolver = ldns_resolver_new();
                    for ns in dns_servers {
                        match ns {
                            IpAddr::V4(ip) => {
                                let nameserver =
                                    ldns_rdf_new_frm_str(ldns_enum_rdf_type_LDNS_RDF_TYPE_A,
                                                         CString::new(ip.to_string()).unwrap()
                                                                                     .as_ptr());
                                ldns_resolver_push_nameserver(resolver, ldns_rdf_clone(nameserver));
                                ldns_rdf_deep_free(nameserver);
                            }
                            IpAddr::V6(ip) => {
                                let nameserver =
                                    ldns_rdf_new_frm_str(ldns_enum_rdf_type_LDNS_RDF_TYPE_AAAA,
                                                         CString::new(ip.to_string()).unwrap()
                                                                                     .as_ptr());
                                ldns_resolver_push_nameserver(resolver, ldns_rdf_clone(nameserver));
                                ldns_rdf_deep_free(nameserver);
                            }
                        }
                    }
                    let mut keys = ldns_rr_list_new();
                    let mut dns_anchor_one = ptr::null_mut();
                    let dns_anchor_one_str = CString::new(DNS_ANCHOR_1).unwrap();
                    ldns_rr_new_frm_str(&mut dns_anchor_one as *mut _,
                                        dns_anchor_one_str.as_ptr(),
                                        0,
                                        ptr::null(),
                                        ptr::null_mut());
                    ldns_rr_list_push_rr(keys, dns_anchor_one);
                    let mut dns_anchor_two = ptr::null_mut();
                    let dns_anchor_two_str = CString::new(DNS_ANCHOR_2).unwrap();
                    ldns_rr_new_frm_str(&mut dns_anchor_two as *mut _,
                                        dns_anchor_two_str.as_ptr(),
                                        0,
                                        ptr::null(),
                                        ptr::null_mut());
                    ldns_rr_list_push_rr(keys, dns_anchor_two);
                    let mut dns_anchor_three = ptr::null_mut();
                    let dns_anchor_three_str = CString::new(DNS_ANCHOR_3).unwrap();
                    ldns_rr_new_frm_str(&mut dns_anchor_three as *mut _,
                                        dns_anchor_three_str.as_ptr(),
                                        0,
                                        ptr::null(),
                                        ptr::null_mut());
                    ldns_rr_list_push_rr(keys, dns_anchor_three);
                    ldns_resolver_set_dnssec_anchors(resolver, ldns_rr_list_clone(keys));
                    ldns_rr_list_deep_free(keys);
                    ldns_resolver_set_dnssec(resolver, 1);
                    ldns_resolver_set_dnssec_cd(resolver, 1);
                    let mut pkt = ldns_resolver_query(resolver,
                                                      domain,
                                                      match record_type {
                                                          LookupType::TXTRecord => {
                                                              ldns_enum_rr_type_LDNS_RR_TYPE_TXT
                                                          }
                                                          LookupType::ARecord => {
                                                              ldns_enum_rr_type_LDNS_RR_TYPE_A
                                                          }
                                                          LookupType::AAAARecord => {
                                                              ldns_enum_rr_type_LDNS_RR_TYPE_AAAA
                                                          }
                                                      },
                                                      ldns_enum_rr_class_LDNS_RR_CLASS_IN,
                                                      LDNS_RD as u16);
                    ldns_rdf_deep_free(domain);
                    if !pkt.is_null() {
                        let mut rr_res =
                            ldns_pkt_rr_list_by_type(pkt,
                                                     match record_type {
                                                         LookupType::TXTRecord => {
                                                             ldns_enum_rr_type_LDNS_RR_TYPE_TXT
                                                         }
                                                         LookupType::ARecord => {
                                                             ldns_enum_rr_type_LDNS_RR_TYPE_A
                                                         }
                                                         LookupType::AAAARecord => {
                                                             ldns_enum_rr_type_LDNS_RR_TYPE_AAAA
                                                         }
                                                     },
                                                     ldns_enum_pkt_section_LDNS_SECTION_ANSWER);
                        if ldns_rr_list_rr_count(rr_res) == 0 {
                            err = Some("SERVFAIL no results".to_string());
                        } else {
                            // we're the ultimate resolver, so we ignore CD-bit checking
                            let mut rrsigs =
                                ldns_pkt_rr_list_by_type(pkt,
                                                         ldns_enum_rr_type_LDNS_RR_TYPE_RRSIG,
                                                         ldns_enum_pkt_section_LDNS_SECTION_ANSWER);
                            if rrsigs.is_null()
                               || ldns_rr_list_rr_count(rrsigs) == 0
                               || no_dnssec_fail
                            {
                                if no_dnssec_fail {
                                    parse_results_internal(rr_res, &mut res, record_type);
                                } else {
                                    err = Some("Insecure missing RRsigs".to_string());
                                }
                            } else {
                                let mut signame = ldns_rr_rrsig_signame(ldns_rr_list_rr(rrsigs, 0));
                                if signame.is_null() {
                                    err = Some("Signature name error".to_string());
                                } else {
                                    let mut res_status =
                                        ldns_verify(rr_res,
                                                    rrsigs,
                                                    ldns_resolver_dnssec_anchors(resolver),
                                                    ptr::null_mut());
                                    if res_status == ldns_enum_status_LDNS_STATUS_OK {
                                        parse_results_internal(rr_res, &mut res, record_type);
                                    } else {
                                        let mut domain_keys =
                                            ldns_fetch_valid_domain_keys(resolver,
                                                                         signame,
                                                                         ldns_resolver_dnssec_anchors(resolver),
                                                                         &mut res_status as *mut _);
                                        if res_status != ldns_enum_status_LDNS_STATUS_OK {
                                            err = Some(format!("Can't fetch keys for domain {}",
                                                               res_status));
                                        } else if ldns_rr_list_rr_count(domain_keys) == 0 {
                                            err = Some("Insecure island, no keys received".to_string());
                                        } else {
                                            res_status = ldns_verify(rr_res,
                                                                     rrsigs,
                                                                     domain_keys,
                                                                     ptr::null_mut());
                                            if res_status == ldns_enum_status_LDNS_STATUS_OK {
                                                parse_results_internal(rr_res,
                                                                       &mut res,
                                                                       record_type);
                                            } else {
                                                err = Some("Key verification failed".to_string());
                                            }
                                        }
                                        if !domain_keys.is_null() {
                                            ldns_rr_list_deep_free(domain_keys);
                                        }
                                    }
                                }
                            }
                            if !rrsigs.is_null() {
                                ldns_rr_list_deep_free(rrsigs);
                            }
                        }

                        if !rr_res.is_null() {
                            ldns_rr_list_deep_free(rr_res);
                        }
                    }
                    ldns_pkt_free(pkt);
                    ldns_resolver_deep_free(resolver);
                }
            }
            Err(_) => {
                err = Some("Invalid DNS entry given".to_string());
            }
        }
    }
    if err.is_some() {
        Err(err.unwrap())
    } else {
        Ok(res)
    }
}

unsafe fn parse_results_internal(rr_res: *mut ldns_rr_list,
                                 res: &mut Vec<String>,
                                 record_type: LookupType) {
    ldns_rr_list_sort(rr_res);
    let count = ldns_rr_list_rr_count(rr_res);
    for i in 0..count {
        let rr_ele = ldns_rr_list_rr(rr_res, i);
        let buffer = ldns_buffer_new(1024);
        let rdf = *((*rr_ele)._rdata_fields);
        let rdf_type = ldns_rdf_get_type(rdf);
        match record_type {
            LookupType::TXTRecord => {
                if rdf_type == ldns_enum_rdf_type_LDNS_RDF_TYPE_STR {
                    ldns_rdf2buffer_str_str(buffer, rdf);
                    let buffer_res = ldns_buffer2str(buffer);
                    let c_str: &CStr = CStr::from_ptr(buffer_res);
                    match c_str.to_str() {
                        Ok(ref rr_ele_rust) => {
                            res.push(rr_ele_rust.replace("\"", "").to_owned());
                        }
                        Err(_) => {}
                    }
                    ldns_buffer_free(buffer);
                }
            }
            LookupType::ARecord => {
                if rdf_type == ldns_enum_rdf_type_LDNS_RDF_TYPE_A {
                    ldns_rdf2buffer_str_a(buffer, rdf);
                    let buffer_res = ldns_buffer2str(buffer);
                    let c_str: &CStr = CStr::from_ptr(buffer_res);
                    match c_str.to_str() {
                        Ok(ref rr_ele_rust) => {
                            res.push(rr_ele_rust.replace("\"", "").to_owned());
                        }
                        Err(_) => {}
                    }
                    ldns_buffer_free(buffer);
                }
            }
            LookupType::AAAARecord => {
                if rdf_type == ldns_enum_rdf_type_LDNS_RDF_TYPE_AAAA {
                    ldns_rdf2buffer_str_aaaa(buffer, rdf);
                    let buffer_res = ldns_buffer2str(buffer);
                    let c_str: &CStr = CStr::from_ptr(buffer_res);
                    match c_str.to_str() {
                        Ok(ref rr_ele_rust) => {
                            res.push(rr_ele_rust.replace("\"", "").to_owned());
                        }
                        Err(_) => {}
                    }
                    ldns_buffer_free(buffer);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use dns::*;
    use std::str::FromStr;

    #[test]
    pub fn test_googledns_resolve_dns() {
        let res = resolve_dns_txt_record(&"concordium.com".to_string(),
                                         &vec![IpAddr::from_str("8.8.8.8").unwrap()],
                                         false);
        match res {
            Ok(ref resps) => {
                assert_eq!(resps.len(), 3);
            }
            Err(e) => panic!("{}", e),
        }
    }

    #[test]
    pub fn test_googledns_resolve_dns_fail() {
        let res = resolve_dns_txt_record(&"www.dnssec-failed.org".to_string(),
                                         &vec![IpAddr::from_str("8.8.8.8").unwrap()],
                                         false);
        match res {
            Ok(_) => panic!("This shouldn't happen - we got a valid response"),
            Err(_) => {}
        }
    }

    // #[test] - does not behave identical geographically
    pub fn _test_quadnine_resolve_dns_fail() {
        let res = resolve_dns_txt_record(&"www.dnssec-failed.org".to_string(),
                                         &vec![IpAddr::from_str("9.9.9.9").unwrap()],
                                         false);
        match res {
            Ok(_) => panic!("This shouldn't happen - we got a valid response"),
            Err(_) => {}
        }
    }

    // #[test] - does not behave identical geographically
    pub fn _test_norton_resolve_dns() {
        let res = resolve_dns_txt_record(&"concordium.com".to_string(),
                                         &vec![IpAddr::from_str("199.85.126.20").unwrap()],
                                         false);
        match res {
            Ok(ref resps) => {
                assert_eq!(resps.len(), 3);
            }
            Err(e) => panic!("{}", e),
        }
    }

    // #[test] - does not behave identical geographically
    pub fn _test_norton_resolve_dns_fail() {
        let res = resolve_dns_txt_record(&"www.dnssec-failed.org".to_string(),
                                         &vec![IpAddr::from_str("199.85.126.20").unwrap()],
                                         false);
        match res {
            Ok(_) => panic!("This shouldn't happen - we got a valid response"),
            Err(_) => {}
        }
    }

    // #[test] - does not behave identical geographically
    pub fn _test_quadnine_resolve_dns() {
        let res = resolve_dns_txt_record(&"concordium.com".to_string(),
                                         &vec![IpAddr::from_str("9.9.9.9").unwrap()],
                                         false);
        match res {
            Ok(ref resps) => {
                assert_eq!(resps.len(), 3);
            }
            Err(e) => panic!("{}", e),
        }
    }

    #[test]
    pub fn test_cloudflare_resolve_dns_fail() {
        let res = resolve_dns_txt_record(&"www.dnssec-failed.org".to_string(),
                                         &vec![IpAddr::from_str("1.1.1.1").unwrap()],
                                         false);
        match res {
            Ok(_) => panic!("This shouldn't happen - we got a valid response"),
            Err(_) => {}
        }
    }

    #[test]
    pub fn test_cloudflare_resolve_dns() {
        let res = resolve_dns_txt_record(&"concordium.com".to_string(),
                                         &vec![IpAddr::from_str("1.1.1.1").unwrap()],
                                         false);
        match res {
            Ok(ref resps) => {
                assert_eq!(resps.len(), 3);
            }
            Err(e) => panic!("{}", e),
        }
    }

    // #[test] - does not behave identical geographically
    pub fn _test_comodo_resolve_dns_fail() {
        let res = resolve_dns_txt_record(&"www.dnssec-failed.org".to_string(),
                                         &vec![IpAddr::from_str("8.26.56.26").unwrap()],
                                         false);
        match res {
            Ok(_) => panic!("This shouldn't happen - we got a valid response"),
            Err(_) => {}
        }
    }

    #[test]
    pub fn test_cleanbrowsing_resolve_dns() {
        let res = resolve_dns_txt_record(&"concordium.com".to_string(),
                                         &vec![IpAddr::from_str("185.228.168.168").unwrap()],
                                         false);
        match res {
            Ok(ref resps) => {
                assert_eq!(resps.len(), 3);
            }
            Err(e) => panic!("{}", e),
        }
    }

    // #[test] - does not behave identical geographically
    pub fn _test_comodo_resolve_dns() {
        let res = resolve_dns_txt_record(&"concordium.com".to_string(),
                                         &vec![IpAddr::from_str("8.26.56.26").unwrap()],
                                         false);
        match res {
            Ok(ref resps) => {
                assert_eq!(resps.len(), 3);
            }
            Err(e) => panic!("{}", e),
        }
    }

    #[test]
    pub fn test_cleanbrowsing_resolve_dns_fail() {
        let res = resolve_dns_txt_record(&"www.dnssec-failed.org".to_string(),
                                         &vec![IpAddr::from_str("185.228.168.168").unwrap()],
                                         false);
        match res {
            Ok(_) => panic!("This shouldn't happen - we got a valid response"),
            Err(_) => {}
        }
    }

    #[test]
    pub fn test_opendns_resolve_nodnssec_fail() {
        let res = resolve_dns_txt_record(&"concordium.com".to_string(),
                                         &vec![IpAddr::from_str("208.67.220.220").unwrap()],
                                         false);
        match res {
            Ok(_) => panic!("This shouldn't happen - we got a valid response"),
            Err(_) => {}
        }
    }

    #[test]
    pub fn test_yandex_resolve_nodnssec_fail() {
        let res = resolve_dns_txt_record(&"concordium.com".to_string(),
                                         &vec![IpAddr::from_str("77.88.8.7").unwrap()],
                                         false);
        match res {
            Ok(_) => panic!("This shouldn't happen - we got a valid response"),
            Err(_) => {}
        }
    }

    #[test]
    pub fn test_yandex_resolve_nodnssec_fail_nodnssec_test() {
        let res = resolve_dns_txt_record(&"concordium.com".to_string(),
                                         &vec![IpAddr::from_str("77.88.8.7").unwrap()],
                                         true);
        match res {
            Ok(ref resps) => {
                assert_eq!(resps.len(), 3);
            }
            Err(e) => panic!("{}", e),
        }
    }

    #[test]
    pub fn test_googledns_resolve_a_record() {
        let res = resolve_dns_a_record(&"google.com".to_string(),
                                       &vec![IpAddr::from_str("8.8.8.8").unwrap()],
                                       true);
        match res {
            Ok(ref resps) => {
                assert!(resps.len() > 0);
            }
            Err(e) => panic!("{}", e),
        }
    }

    #[test]
    pub fn test_googledns_resolve_aaaa_record() {
        let res = resolve_dns_aaaa_record(&"google.com".to_string(),
                                          &vec![IpAddr::from_str("8.8.8.8").unwrap()],
                                          true);
        match res {
            Ok(ref resps) => {
                assert!(resps.len() > 0);
            }
            Err(e) => panic!("{}", e),
        }
    }

}
