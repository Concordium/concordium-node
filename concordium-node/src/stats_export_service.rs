//! Node's statistics and their exposure.

use crate::{common::p2p_node_id::P2PNodeId, read_or_die, spawn_or_die};
use anyhow::Context;
use gotham::{
    handler::IntoResponse,
    helpers::http::response::create_response,
    middleware::state::StateMiddleware,
    pipeline::{single::single_pipeline, single_middleware},
    router::{builder::*, Router},
    state::{FromState, State},
};
use http::{status::StatusCode, Response};
use hyper::Body;
use prometheus::{
    self,
    core::{AtomicI64, AtomicU64, GenericGauge},
    Encoder, IntCounter, IntGauge, Opts, Registry, TextEncoder,
};
use std::{net::SocketAddr, sync::RwLock, thread, time};

use crate::configuration;
use std::sync::Arc;

struct HTMLStringResponse(pub String);

impl IntoResponse for HTMLStringResponse {
    fn into_response(self, state: &State) -> Response<Body> {
        create_response(state, StatusCode::OK, mime::TEXT_HTML, self.0)
    }
}

#[derive(Clone, gotham_derive::StateData)]
struct PrometheusStateData {
    registry: Arc<RwLock<Registry>>,
}

impl PrometheusStateData {
    fn new(registry: Registry) -> Self {
        Self {
            registry: Arc::new(RwLock::new(registry)),
        }
    }
}

/// Collects statistics pertaining to the node.
pub struct StatsExportService {
    registry: Registry,
    pkts_received_counter: IntCounter,
    pkts_sent_counter: IntCounter,
    peers_gauge: IntGauge,
    connections_received: IntCounter,
    inbound_high_priority_consensus_drops_counter: IntCounter,
    inbound_low_priority_consensus_drops_counter: IntCounter,
    inbound_high_priority_consensus_counter: IntCounter,
    inbound_low_priority_consensus_counter: IntCounter,
    inbound_high_priority_consensus_size: IntGauge,
    inbound_low_priority_consensus_size: IntGauge,
    outbound_high_priority_consensus_size: IntGauge,
    outbound_low_priority_consensus_size: IntGauge,
    last_throughput_measurement_timestamp: GenericGauge<AtomicI64>,
    bytes_received: GenericGauge<AtomicU64>,
    bytes_sent: GenericGauge<AtomicU64>,
    avg_bps_in: GenericGauge<AtomicU64>,
    avg_bps_out: GenericGauge<AtomicU64>,
}

impl StatsExportService {
    /// Creates a new instance of the starts export service object.
    pub fn new() -> anyhow::Result<Self> {
        let registry = Registry::new();
        let pg_opts = Opts::new("peer_number", "current peers connected");
        let pg = IntGauge::with_opts(pg_opts)?;
        registry.register(Box::new(pg.clone()))?;

        let qs_opts = Opts::new("queue_size", "current queue size");
        let qs = IntGauge::with_opts(qs_opts)?;
        registry.register(Box::new(qs))?;

        let rqs_opts = Opts::new("resend_queue_size", "current queue size");
        let rqs = IntGauge::with_opts(rqs_opts)?;
        registry.register(Box::new(rqs))?;

        let dp_opts = Opts::new("packets_dropped", "dropped packets");
        let dp = IntCounter::with_opts(dp_opts)?;
        registry.register(Box::new(dp))?;

        let cr_opts = Opts::new("conn_received", "connections received");
        let cr = IntCounter::with_opts(cr_opts)?;
        registry.register(Box::new(cr.clone()))?;

        let prc_opts = Opts::new("packets_received", "packets received");
        let prc = IntCounter::with_opts(prc_opts)?;
        registry.register(Box::new(prc.clone()))?;

        let psc_opts = Opts::new("packets_sent", "packets sent");
        let psc = IntCounter::with_opts(psc_opts)?;
        registry.register(Box::new(psc.clone()))?;

        let ipr_opts = Opts::new("invalid_packets_received", "invalid packets received");
        let ipr = IntCounter::with_opts(ipr_opts)?;
        registry.register(Box::new(ipr))?;

        let upr_opts = Opts::new("unknown_packets_received", "unknown packets received");
        let upr = IntCounter::with_opts(upr_opts)?;
        registry.register(Box::new(upr))?;

        let inpr_opts =
            Opts::new("invalid_network_packets_received", "invalid network packets received");
        let inpr = IntCounter::with_opts(inpr_opts)?;
        registry.register(Box::new(inpr))?;

        let rs_opts = Opts::new("packets_resend", "items in queue that needed to be resend");
        let rs = IntCounter::with_opts(rs_opts)?;
        registry.register(Box::new(rs))?;

        let inbound_high_priority_consensus_drops_opts = Opts::new(
            "inbound_high_priority_consensus_drops",
            "inbound high priority consensus messages dropped",
        );
        let inbound_high_priority_consensus_drops_counter =
            IntCounter::with_opts(inbound_high_priority_consensus_drops_opts)?;
        registry.register(Box::new(inbound_high_priority_consensus_drops_counter.clone()))?;

        let inbound_low_priority_consensus_drops_opts = Opts::new(
            "inbound_low_priority_consensus_drops",
            "inbound low priority consensus messages dropped",
        );
        let inbound_low_priority_consensus_drops_counter =
            IntCounter::with_opts(inbound_low_priority_consensus_drops_opts)?;
        registry.register(Box::new(inbound_low_priority_consensus_drops_counter.clone()))?;

        let inbound_high_priority_consensus_counter_opts = Opts::new(
            "inbound_high_priority_consensus_counter",
            "inbound high priority consensus messages received",
        );
        let inbound_high_priority_consensus_counter =
            IntCounter::with_opts(inbound_high_priority_consensus_counter_opts)?;
        registry.register(Box::new(inbound_high_priority_consensus_counter.clone()))?;

        let inbound_low_priority_consensus_counter_opts = Opts::new(
            "inbound_low_priority_consensus_counter",
            "inbound low priority consensus messages received",
        );
        let inbound_low_priority_consensus_counter =
            IntCounter::with_opts(inbound_low_priority_consensus_counter_opts)?;
        registry.register(Box::new(inbound_low_priority_consensus_counter.clone()))?;

        let inbound_high_priority_consensus_size_opts = Opts::new(
            "inbound_high_priority_consensus_size",
            "inbound high priority consensus queue size",
        );
        let inbound_high_priority_consensus_size =
            IntGauge::with_opts(inbound_high_priority_consensus_size_opts)?;
        registry.register(Box::new(inbound_high_priority_consensus_size.clone()))?;

        let inbound_low_priority_consensus_size_opts = Opts::new(
            "inbound_low_priority_consensus_size",
            "inbound low priority consensus queue size",
        );
        let inbound_low_priority_consensus_size =
            IntGauge::with_opts(inbound_low_priority_consensus_size_opts)?;
        registry.register(Box::new(inbound_low_priority_consensus_size.clone()))?;

        let outbound_high_priority_consensus_size_opts = Opts::new(
            "outbound_high_priority_consensus_size",
            "outbound high priority consensus queue size",
        );
        let outbound_high_priority_consensus_size =
            IntGauge::with_opts(outbound_high_priority_consensus_size_opts)?;
        registry.register(Box::new(outbound_high_priority_consensus_size.clone()))?;

        let outbound_low_priority_consensus_size_opts = Opts::new(
            "outbound_low_priority_consensus_size",
            "outbound low priority consensus queue size",
        );
        let outbound_low_priority_consensus_size =
            IntGauge::with_opts(outbound_low_priority_consensus_size_opts)?;
        registry.register(Box::new(outbound_low_priority_consensus_size.clone()))?;

        let last_throughput_measurement_timestamp_opts = Opts::new(
            "last_throughput_measurement_timestamp",
            "last_throughput_measurement_timestamp",
        );
        let ltm = GenericGauge::with_opts(last_throughput_measurement_timestamp_opts)?;
        registry.register(Box::new(ltm.clone()))?;

        let brc_opts = Opts::new("bytes_received", "bytes received");
        let brc = GenericGauge::with_opts(brc_opts)?;
        registry.register(Box::new(brc.clone()))?;

        let bsc_opts = Opts::new("bytes_sent", "bytes sent");
        let bsc = GenericGauge::with_opts(bsc_opts)?;
        registry.register(Box::new(bsc.clone()))?;

        let avg_bps_in_opts = Opts::new("avg_bps_in", "average inbound througput");
        let avg_bps_in = GenericGauge::with_opts(avg_bps_in_opts)?;
        registry.register(Box::new(avg_bps_in.clone()))?;

        let avg_bps_out_opts = Opts::new("avg_bps_out", "average outbound througput");
        let avg_bps_out = GenericGauge::with_opts(avg_bps_out_opts)?;
        registry.register(Box::new(avg_bps_out.clone()))?;

        Ok(StatsExportService {
            registry,
            pkts_received_counter: prc,
            pkts_sent_counter: psc,
            peers_gauge: pg,
            connections_received: cr,
            inbound_high_priority_consensus_drops_counter,
            inbound_low_priority_consensus_drops_counter,
            inbound_high_priority_consensus_counter,
            inbound_low_priority_consensus_counter,
            inbound_high_priority_consensus_size,
            inbound_low_priority_consensus_size,
            outbound_high_priority_consensus_size,
            outbound_low_priority_consensus_size,
            last_throughput_measurement_timestamp: ltm,
            bytes_received: brc,
            bytes_sent: bsc,
            avg_bps_in,
            avg_bps_out,
        })
    }

    /// Increases the peer count.
    pub fn peers_inc(&self) { self.peers_gauge.inc(); }

    /// Decreases the peer count.
    pub fn peers_dec(&self) { self.peers_gauge.dec(); }

    /// Increases the number of received packets.
    pub fn pkt_received_inc(&self) { self.pkts_received_counter.inc(); }

    /// Increases the number of sent packets.
    pub fn pkt_sent_inc(&self) { self.pkts_sent_counter.inc(); }

    /// Increases the number of received connections.
    pub fn conn_received_inc(&self) { self.connections_received.inc(); }

    /// Increases the number of high priority consensus messages dropped due to
    /// the queue being full.
    pub fn inbound_high_priority_consensus_drops_inc(&self) {
        self.inbound_high_priority_consensus_drops_counter.inc();
    }

    /// Increases the number of low priority consensus messages dropped due to
    /// the queue being full.
    pub fn inbound_low_priority_consensus_drops_inc(&self) {
        self.inbound_low_priority_consensus_drops_counter.inc();
    }

    /// Increases the number of received high priority consensus messages.
    pub fn inbound_high_priority_consensus_inc(&self) {
        self.inbound_high_priority_consensus_counter.inc();
    }

    /// Increases the number of received low priority consensus messages.
    pub fn inbound_low_priority_consensus_inc(&self) {
        self.inbound_low_priority_consensus_counter.inc();
    }

    /// Sets the size value of the high priority inbound consensus queue.
    pub fn set_inbound_high_priority_consensus_size(&self, value: i64) {
        self.inbound_high_priority_consensus_size.set(value);
    }

    /// Sets the size value of the low priority inbound consensus queue.
    pub fn set_inbound_low_priority_consensus_size(&self, value: i64) {
        self.inbound_low_priority_consensus_size.set(value);
    }

    /// Sets the size value of the high priority outbound consensus queue.
    pub fn set_outbound_high_priority_consensus_size(&self, value: i64) {
        self.outbound_high_priority_consensus_size.set(value);
    }

    /// Sets the size value of the low priority outbound consensus queue.
    pub fn set_outbound_low_priority_consensus_size(&self, value: i64) {
        self.outbound_low_priority_consensus_size.set(value);
    }

    /// Gets the timestamp for the last throughput check.
    pub fn get_last_throughput_measurement_timestamp(&self) -> i64 {
        self.last_throughput_measurement_timestamp.get()
    }

    /// Sets the value of throughput timestamp.
    pub fn set_last_throughput_measurement_timestamp(&self, value: i64) {
        self.last_throughput_measurement_timestamp.set(value);
    }

    /// Gets the count of received bytes.
    pub fn get_bytes_received(&self) -> u64 { self.bytes_received.get() }

    /// Gets the count of sent bytes.
    pub fn get_bytes_sent(&self) -> u64 { self.bytes_sent.get() }

    /// Sets the value of received bytes.
    pub fn set_bytes_received(&self, value: u64) { self.bytes_received.set(value); }

    /// Sets the value of sent bytes.
    pub fn set_bytes_sent(&self, value: u64) { self.bytes_sent.set(value); }

    /// Gets the value of average inbound throughput.
    pub fn get_avg_bps_in(&self) -> u64 { self.avg_bps_in.get() }

    /// Gets the value of average outbound throughput.
    pub fn get_avg_bps_out(&self) -> u64 { self.avg_bps_out.get() }

    /// Sets the value of average inbound throughput.
    pub fn set_avg_bps_in(&self, value: u64) { self.avg_bps_in.set(value); }

    /// Sets the value of average outbound throughput.
    pub fn set_avg_bps_out(&self, value: u64) { self.avg_bps_out.set(value); }

    fn metrics(state: State) -> (State, String) {
        let state_data = PrometheusStateData::borrow_from(&state);
        let encoder = TextEncoder::new();
        let metric_families = read_or_die!(state_data.registry).gather();
        let mut buffer = vec![];
        assert!(encoder.encode(&metric_families, &mut buffer).is_ok());
        match String::from_utf8(buffer) {
            Ok(buf) => (state, buf),
            Err(_) => (state, "".to_string()),
        }
    }

    fn index(state: State) -> (State, HTMLStringResponse) {
        let message = HTMLStringResponse(format!(
            "<html><body><h1>Prometheus for {} v{}</h1>Operational!</p></body></html>",
            super::APPNAME,
            super::VERSION
        ));
        (state, message)
    }

    fn router(&self) -> Router {
        let state_data = PrometheusStateData::new(self.registry.clone());
        let middleware = StateMiddleware::new(state_data);
        let pipeline = single_middleware(middleware);
        let (chain, pipelines) = single_pipeline(pipeline);
        build_router(chain, pipelines, |route| {
            route.get("/").to(Self::index);
            route.get("/metrics").to(Self::metrics);
        })
    }

    /// Starts the statistics server.
    pub async fn start_server(&self, listen_addr: SocketAddr) -> Result<(), ()> {
        gotham::plain::init_server(listen_addr, self.router()).await
    }

    fn start_push_to_gateway(
        &self,
        prometheus_push_gateway: String,
        prometheus_push_interval: u64,
        prometheus_job_name: String,
        prometheus_instance_name: String,
        prometheus_push_username: Option<String>,
        prometheus_push_password: Option<String>,
    ) {
        let metrics_families = self.registry.gather();
        let _th = spawn_or_die!("Prometheus", move || loop {
            debug!("Pushing data to push gateway");
            let username_pass = prometheus_push_username.clone().and_then(|username| {
                prometheus_push_password.clone().map(|password| prometheus::BasicAuthentication {
                    username,
                    password,
                })
            });
            thread::sleep(time::Duration::from_secs(prometheus_push_interval));
            prometheus::push_metrics(
                &prometheus_job_name,
                prometheus::labels! {
                    "instance".to_owned() => prometheus_instance_name.clone(),
                },
                &prometheus_push_gateway,
                metrics_families.clone(),
                username_pass,
            )
            .map_err(|e| error!("Can't push to prometheus push gateway {}", e))
            .ok();
        });
    }
}

/// Starts the stats export engine.
pub fn instantiate_stats_export_engine(
    conf: &configuration::Config,
) -> anyhow::Result<Arc<StatsExportService>> {
    if conf.prometheus.prometheus_listen_port.is_some() {
        info!("Enabling prometheus server");
    } else if let Some(ref push_gateway) = conf.prometheus.prometheus_push_gateway {
        info!("Enabling prometheus push gateway at {}", push_gateway);
    };
    let prom =
        StatsExportService::new().context("Could not start statistics collection engine.")?;
    Ok(Arc::new(prom))
}

/// Starts the push gateway to Prometheus if the configuration specifies it.
pub fn start_push_gateway(
    conf: &configuration::PrometheusConfig,
    service: &StatsExportService,
    id: P2PNodeId,
) {
    if let Some(prom_push_addy) = conf.prometheus_push_gateway.as_ref() {
        let instance_name = if let Some(ref instance_id) = conf.prometheus_instance_name {
            instance_id.clone()
        } else {
            id.to_string()
        };
        service.start_push_to_gateway(
            prom_push_addy.clone(),
            conf.prometheus_push_interval,
            conf.prometheus_job_name.clone(),
            instance_name,
            conf.prometheus_push_username.clone(),
            conf.prometheus_push_password.clone(),
        );
    }
}
