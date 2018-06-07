extern crate p2p_client;
use p2p_client::configuration;
use p2p_client::ffi;

fn main() {
    let conf = configuration::parse_config();
    println!("I'm your new coin! Network enabled: {}, should connect to {}", conf.network, conf.connect_to.unwrap_or(String::from("nothing")));
    println!("calling lib in c {} version found is {}", ffi::is_present(), ffi::version());

}
