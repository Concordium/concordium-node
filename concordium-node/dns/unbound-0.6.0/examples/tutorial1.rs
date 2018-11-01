use std::process::exit;

extern crate unbound;

mod util;

fn main() {
    let ctx = unbound::Context::new().unwrap();
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
