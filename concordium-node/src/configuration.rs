use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "config")]
pub struct Config {
    #[structopt(long="network", short="net", help = "Enable network")]
    pub network: bool,
    #[structopt(long="connect-to", short="c", help = "Peer to connect to upon startup")]
    pub remote_ip: Option<String>,
    #[structopt(long="port", short="p", help = "Remote port")]
    pub remote_port: Option<u16>,
    #[structopt(long="listen-port", short="l", help = "Port to listen on")]
    pub listen_port: Option<u16>,
    #[structopt(long="id", short="i", help = "Wanted ID")]
    pub id: Option<String>,
}

pub fn parse_config() -> Config  {
    return Config::from_args();
}