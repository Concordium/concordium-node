use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "config")]
pub struct Config {
    #[structopt(long="network", short="net", help = "Enable network")]
    pub network: bool,
    #[structopt(long="connect-to", short="c", help = "Peer to connect to upon startup")]
    pub connect_to: Option<String>
}

pub fn parse_config() -> Config  {
    return Config::from_args();
}