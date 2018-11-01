use std::process::exit;
use std::sync::Arc;
use std::thread;

extern crate unbound;

mod util;

fn main() {
    let ctx = Arc::new(unbound::Context::new().unwrap());
    if let Err(err) = ctx.resolvconf_path("/etc/resolv.conf") {
        println!("error reading resolv.conf: {}", err);
        exit(1);
    }
    if let Err(err) = ctx.hosts_path("/etc/hosts") {
        println!("error reading hosts: {}", err);
        exit(1);
    }
    let mut handles: Vec<_> = Vec::new();
    for (i, name) in ["www.nlnetlabs.nl", "www.google.nl"].iter().enumerate() {
        let ctx = ctx.clone();
        let name = name.to_string();
        handles.push(thread::spawn(move || {
            match ctx.resolve(&name, 1, 1) {
                Err(err) => println!("thread {} - error resolving {}: {}", i, name, err),
                Ok(ans) => {
                    for ip in ans.data().map(util::data_to_ipv4) {
                        println!("thread {} -  address of {} is {}", i, name, ip);
                    }
                }
            }
        }));
    }
    for handle in handles {
        handle.join().unwrap()
    }
}
