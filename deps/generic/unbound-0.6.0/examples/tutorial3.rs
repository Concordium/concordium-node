use std::process::exit;

extern crate unbound;

fn examine_result(query: &str, result: &unbound::Answer) {
    println!("The query is for: {}", query);
    println!("The result has:");
    println!("qname: {}", result.qname());
    println!("qtype: {}", result.qtype());
    println!("qclass: {}", result.qclass());
    println!("canonical name: {}", result.canonname().unwrap_or("<none>"));
    match result.havedata() {
        true => println!("has data"),
        false => println!("has no data"),
    }
    match result.nxdomain() {
        true => println!("nxdomain (name does not exist)"),
        false => println!("not an nxdomain (name exists)"),
    }
    match result.secure() {
        true => println!("validated to be secure"),
        false => println!("not validated as secure"),
    }
    match result.bogus() {
        true => println!("a security failure! (bogus)"),
        false => println!("not a security failure (not bogus)"),
    }
    println!("DNS rcode: {}", result.rcode());
    let mut i = 0;
    for data in result.data() {
        println!("result data element {} has length: {}", i, data.len());
        println!("result data element {} is: {:?}", i, data);
        i += 1;
    }
    println!("result has {} data element(s)", i);
}

fn main() {
    let ctx = unbound::Context::new().unwrap();
    if let Err(err) = ctx.resolvconf_path("/etc/resolv.conf") {
        println!("error reading resolv.conf: {}", err);
        exit(1)
    }
    if let Err(err) = ctx.hosts_path("/etc/hosts") {
        println!("error reading hosts: {}", err);
        exit(1)
    }
    let mut exit_code = 0;
    let mut first = true;
    for name in std::env::args().skip(1) {
        if first {
            first = false;
        } else {
            println!("");
        }
        match ctx.resolve(&name, 1, 1) {
            Ok(answer) => examine_result(&name, &answer),
            Err(err) => {
                println!("error resolving {}: {}", name, err);
                exit_code = 1;
            }
        }
    }
    exit(exit_code);
}
