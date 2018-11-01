use std::process::exit;
use std::sync::mpsc;
use std::thread::sleep;
use std::time::Duration;

extern crate unbound;

mod util;

fn main() {
    let ctx = unbound::Context::new().unwrap();
    let mut i = 0;
    let (tx, rx) = mpsc::channel();

    let mycallback = move |_: unbound::AsyncID, result: unbound::Result<unbound::Answer>| {
        match result {
            Err(err) => println!("resolve error: {}", err),
            Ok(ans) => {
                for ip in ans.data().map(util::data_to_ipv4) {
                    println!("The address of {} is {}", ans.qname(), ip);
                }
                tx.send(true).unwrap();
            }
        }
    };

    match ctx.resolve_async("www.nlnetlabs.nl", 1, 1, mycallback) {
        Err(err) => {
            println!("resolve error: {}", err);
            exit(1)
        }
        Ok(_id) => (),
    }

    while rx.try_recv() == Err(mpsc::TryRecvError::Empty) {
        sleep(Duration::new(1, 0) / 10);
        i += 1;
        println!("time passed ({}) ..", i);
        if let Err(err) = ctx.process() {
            println!("resolve error: {}", err);
            exit(1)
        }
    }

    println!("done")
}
