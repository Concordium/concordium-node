use errors::*;
use iron::headers::ContentType;
use iron::prelude::*;
use iron::status;
use prometheus;
use prometheus::{Encoder, IntCounter, IntGauge, Opts, Registry, TextEncoder};
use router::Router;
use std::sync::Arc;
use std::thread;
use std::time;

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum PrometheusMode {
    NodeMode,
    IpDiscoveryMode,
}

#[derive(Clone)]
pub struct PrometheusServer {
    mode: PrometheusMode,
    registry: Registry,
    pkts_received_counter: IntCounter,
    pkts_sent_counter: IntCounter,
    peers_gauge: IntGauge,
    connections_received: IntCounter,
    unique_ips_seen: IntCounter,
    invalid_packets_received: IntCounter,
    unknown_packets_received: IntCounter,
    invalid_network_packets_received: IntCounter,
}

impl PrometheusServer {
    pub fn new(mode: PrometheusMode) -> Self {
        let registry = Registry::new();
        let pg_opts = Opts::new("peer_number", "current peers connected");
        let pg = IntGauge::with_opts(pg_opts).unwrap();
        if mode == PrometheusMode::NodeMode {
            registry.register(Box::new(pg.clone())).unwrap();
        }

        let cr_opts = Opts::new("conn_received", "connections received");
        let cr = IntCounter::with_opts(cr_opts).unwrap();
        registry.register(Box::new(cr.clone())).unwrap();

        let uis_opts = Opts::new("unique_ips_seen", "unique IPs seen");
        let uis = IntCounter::with_opts(uis_opts).unwrap();
        if mode == PrometheusMode::IpDiscoveryMode {
            registry.register(Box::new(uis.clone())).unwrap();
        }

        let prc_opts = Opts::new("pkts_received", "packets received");
        let prc = IntCounter::with_opts(prc_opts).unwrap();
        registry.register(Box::new(prc.clone())).unwrap();

        let psc_opts = Opts::new("pkts_sent", "packets sent");
        let psc = IntCounter::with_opts(psc_opts).unwrap();
        registry.register(Box::new(psc.clone())).unwrap();

        let ipr_opts = Opts::new("invalid_pkts_received", "invalid packets received");
        let ipr = IntCounter::with_opts(ipr_opts).unwrap();
        if mode == PrometheusMode::NodeMode {
            registry.register(Box::new(ipr.clone())).unwrap();
        }

        let upr_opts = Opts::new("unknown_packets_received", "unknown packets received");
        let upr = IntCounter::with_opts(upr_opts).unwrap();
        if mode == PrometheusMode::NodeMode {
            registry.register(Box::new(upr.clone())).unwrap();
        }

        let inpr_opts = Opts::new("invalid_network_packets_received", "invalid network packets received");
        let inpr = IntCounter::with_opts(inpr_opts).unwrap();
        if mode == PrometheusMode::NodeMode {
            registry.register(Box::new(inpr.clone())).unwrap();
        }

        PrometheusServer { mode: mode,
                           registry: registry.clone(),
                           pkts_received_counter: prc.clone(),
                           pkts_sent_counter: psc.clone(),
                           peers_gauge: pg.clone(),
                           connections_received: cr.clone(),
                           unique_ips_seen: uis.clone(),
                           invalid_packets_received: ipr.clone(), 
                           unknown_packets_received: upr.clone(),
                           invalid_network_packets_received: inpr.clone(),}
    }

    pub fn peers_inc(&mut self) -> ResultExtWrapper<()> {
        &self.peers_gauge.inc();
        Ok(())
    }

    pub fn unique_ips_inc(&mut self) -> ResultExtWrapper<()> {
        &self.unique_ips_seen.inc();
        Ok(())
    }

    pub fn peers_dec(&mut self) -> ResultExtWrapper<()> {
        &self.peers_gauge.dec();
        Ok(())
    }

    pub fn pkt_received_inc(&mut self) -> ResultExtWrapper<()> {
        &self.pkts_received_counter.inc();
        Ok(())
    }

    pub fn pkt_received_inc_by(&mut self, to_add: i64) -> ResultExtWrapper<()> {
        &self.pkts_received_counter.inc_by(to_add);
        Ok(())
    }

    pub fn pkt_sent_inc(&mut self) -> ResultExtWrapper<()> {
        &self.pkts_sent_counter.inc();
        Ok(())
    }

    pub fn pkt_sent_inc_by(&mut self, to_add: i64) -> ResultExtWrapper<()> {
        &self.pkts_sent_counter.inc_by(to_add);
        Ok(())
    }

    pub fn conn_received_inc(&mut self) -> ResultExtWrapper<()> {
        &self.connections_received.inc();
        Ok(())
    }

    pub fn invalid_pkts_received_inc(&mut self) -> ResultExtWrapper<()> {
        &self.invalid_packets_received.inc();
        Ok(())
    }

    pub fn invalid_network_pkts_received_inc(&mut self) -> ResultExtWrapper<()> {
        &self.invalid_network_packets_received.inc();
        Ok(())
    }

    pub fn unknown_pkts_received_inc(&mut self) -> ResultExtWrapper<()> {
        &self.unknown_packets_received.inc();
        Ok(())
    }

    fn index(&self) -> IronResult<Response> {
        let mut resp = Response::with((status::Ok,
                          format!("<html><body><h1>Prometheus for {} v{}</h1>Operational!</p></body></html>",
                                   super::APPNAME,
                                   super::VERSION)));
        resp.headers.set(ContentType::html());
        Ok(resp)
    }

    fn metrics(&self) -> IronResult<Response> {
        let encoder = TextEncoder::new();
        let metric_familys = self.registry.gather();
        let mut buffer = vec![];
        encoder.encode(&metric_familys, &mut buffer).unwrap();
        let mut resp = Response::with((status::Ok, String::from_utf8(buffer).unwrap()));
        resp.headers.set(ContentType::plaintext());
        Ok(resp)
    }

    pub fn start_server(&mut self, listen_ip: &String, port: u16) -> ResultExtWrapper<()> {
        let mut router = Router::new();
        let _self_clone = Arc::new(self.clone());
        let _self_clone_2 = _self_clone.clone();
        router.get("/",
                   move |_: &mut Request| _self_clone.clone().index(),
                   "index");
        router.get("/metrics",
                   move |_: &mut Request| _self_clone_2.clone().metrics(),
                   "metrics");
        let _listen = listen_ip.clone();
        let _th = thread::spawn(move || {
                                    Iron::new(router).http(format!("{}:{}", _listen, port))
                                                     .unwrap();
                                });
        Ok(())
    }

    pub fn start_push_to_gateway(&self,
                                 prometheus_push_gateway: String,
                                 prometheus_push_interval: u64,
                                 prometheus_job_name: String,
                                 prometheus_instance_name: String,
                                 prometheus_push_username: Option<String>,
                                 prometheus_push_password: Option<String>)
                                 -> ResultExtWrapper<()> {
        let _registry = self.registry.clone();
        let _th = thread::spawn(move || {
                                    loop {
                                        let username_pass = if prometheus_push_username.is_some()
                                                               && prometheus_push_password.is_some()
                                        {
                                            Some(prometheus::BasicAuthentication { username: prometheus_push_username.clone().unwrap()
                                                                                     .to_owned(),
                                                   password: prometheus_push_password.clone().unwrap()
                                                                                     .to_owned(), })
                                        } else {
                                            None
                                        };
                                        debug!("Pushing data to push gateway");
                                        thread::sleep(time::Duration::from_secs(prometheus_push_interval));
                                        let metrics_families = _registry.gather();
                                        prometheus::push_metrics(&prometheus_job_name, labels!{"instance".to_owned() => prometheus_instance_name.clone(),}, &prometheus_push_gateway, metrics_families, username_pass).map_err(|e| error!("{}", e)).ok();
                                    }
                                });
        Ok(())
    }
}
