use std::process::exit;

extern crate unbound;

mod util;

fn main() {
    let ctx = unbound::Context::new().unwrap();
    if let Err(err) = ctx.resolvconf_path("/etc/resolv.conf") {
        println!("Error reading resolv.conf: {}", err);
        exit(1)
    }
    if let Err(err) = ctx.hosts_path("/etc/hosts") {
        println!("Error reading hosts: {}", err);
        exit(1)
    }
    match ctx.resolve("www.nlnetlabs.nl", 1, 1) {
        Ok(ans) => {
            if ans.havedata() {
                for ip in ans.data().map(util::data_to_ipv4) {
                    println!("The address is {}", ip);
                }
            }
        }
        Err(err) => {
            println!("resolve error: {}", err);
            exit(1)
        }
    }
}
