#[macro_use]
extern crate structopt;

use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name="DNS Record Generator")]
struct ConfigCli {
    #[structopt(raw(required = "true"), long = "add-peers")]
    peers: Vec<String>,
    #[structopt(long = "keyfile", raw(required = "true"))]
    keyfile: String,
}

pub fn main() {
    let _conf = ConfigCli::from_args();
    /* 
        TODO: 
            - read key
            - generate pubkey using HACL*
            - generate text in format <bytes_total><pubkey><peers: IP400100100100108888..n><signature>
            - pack in base64
            - split into 255 bytes chunks
            - write out IN TXT chunk0\nIN TXT chunk1\n..
    */
}