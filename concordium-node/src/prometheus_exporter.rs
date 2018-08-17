use prometheus::{Registry, IntGauge, Opts, IntCounter, Encoder, TextEncoder};
use iron::prelude::*;
use iron::status;
use std::sync::Arc;
use router::Router;
use errors::*;
use std::thread;

#[derive(Clone)]
pub struct PrometheusServer {
    registry: Registry,
    pkts_received_counter: IntCounter,
    pkts_sent_counter: IntCounter,
    peers_gauge: IntGauge,
    connections_received: IntCounter,
}

impl PrometheusServer {
    pub fn new() -> Self {
        let registry = Registry::new();
        let pg_opts = Opts::new("peer_number", "current peers connected");
        let pg = IntGauge::with_opts(pg_opts).unwrap();
        registry
            .register(Box::new(pg.clone()))
            .unwrap();

        let cr_opts = Opts::new("conn_received", "connections received");
        let cr = IntCounter::with_opts(cr_opts).unwrap();
        registry
            .register(Box::new(cr.clone()))
            .unwrap();

        let prc_opts = Opts::new("pkts_received", "packets received");
        let prc = IntCounter::with_opts(prc_opts).unwrap();
        registry
            .register(Box::new(prc.clone()))
            .unwrap();

        let psc_opts = Opts::new("pkts_sent", "packets sent");
        let psc = IntCounter::with_opts(psc_opts).unwrap();
        registry
            .register(Box::new(psc.clone()))
            .unwrap();

        PrometheusServer {
            registry: registry.clone(),
            pkts_received_counter: prc.clone(),
            pkts_sent_counter: psc.clone(),
            peers_gauge: pg.clone(),
            connections_received: cr.clone(),
        }
    }

    pub fn peers_inc(&mut self) -> ResultExtWrapper<()> {
        &self.peers_gauge.inc();
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

    fn index(&self) -> IronResult<Response> {
        Ok(Response::with((status::Ok, 
        format!("<html><body><h1>Prometheus for {} v{}</h1>Operational!</p></body></html>", super::APPNAME, super::VERSION
        ))))
    }

    fn metrics(&self) -> IronResult<Response> {
        let encoder = TextEncoder::new();
        let metric_familys = self.registry.gather();
        let mut buffer = vec![];
        encoder.encode(&metric_familys, &mut buffer).unwrap();
       Ok(Response::with((status::Ok, 
            String::from_utf8(buffer).unwrap()
        )))
    }

    pub fn start_server(&mut self, listen_ip: &String, port: u16) -> ResultExtWrapper<()> {
        let mut router = Router::new();
        let _self_clone = Arc::new(self.clone());
        let _self_clone_2 = _self_clone.clone();
        router.get("/", move |_ : &mut Request| _self_clone.clone().index(), "index");
        router.get("/metrics", move| _: &mut Request | _self_clone_2.clone().metrics(), "metrics");
        let _listen = listen_ip.clone();
        let _th = thread::spawn(move || {
            Iron::new(router).http(format!("{}:{}", _listen, port)).unwrap();
        });
        Ok(())
    }
}