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
    #[structopt(long="debug", help = "Debug mode")]
    pub debug: bool,
    #[structopt(long="no-rpc-server", help ="Disable the built-in RPC server")]
    pub no_rpc_server: bool,
    #[structopt(long="rpc-server-port", help = "RPC server port", default_value="10000")]
    pub rpc_server_port: u16,
    #[structopt(long="rpc-server-addr", help = "RPC server listen address", default_value="127.0.0.1")]
    pub rpc_server_addr: String,
}

pub fn parse_config() -> Config  {
    return Config::from_args();
}