//! Node's statistics and their exposure.

cfg_if! {
    if #[cfg(feature = "instrumentation")] {
        use prometheus::{self, Encoder, core::{AtomicI64, AtomicU64, GenericGauge}, IntCounter, IntGauge, Opts, Registry, TextEncoder};
        use crate::{common::NodeShutdownCause, common::p2p_node_id::P2PNodeId, spawn_or_die, read_or_die};
        use std::{net::SocketAddr, thread, time, sync::RwLock};
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
    } else {
        use std::sync::atomic::{AtomicI64, AtomicU64, AtomicUsize, Ordering};
    }
}
use crate::configuration;
use std::sync::Arc;

cfg_if! {
    if #[cfg(feature = "instrumentation")] {
        struct HTMLStringResponse(pub String);

        impl IntoResponse for HTMLStringResponse {
            fn into_response(self, state: &State) -> Response<Body> {
                create_response(state, StatusCode::OK, mime::TEXT_HTML, self.0)
            }
        }

        #[derive(Clone, StateData)]
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
    }
}

/// Collects statistics pertaining to the node.
#[cfg(not(feature = "instrumentation"))]
#[derive(Default)]
pub struct StatsExportService {
    pkts_received_counter: AtomicUsize,
    pkts_sent_counter: AtomicUsize,
    peers_gauge: AtomicUsize,
    connections_received: AtomicUsize,
    inbound_high_priority_consensus_drops_counter: AtomicUsize,
    inbound_low_priority_consensus_drops_counter: AtomicUsize,
    inbound_high_priority_consensus_counter: AtomicUsize,
    inbound_low_priority_consensus_counter: AtomicUsize,
    inbound_high_priority_consensus_size: AtomicUsize,
    inbound_low_priority_consensus_size: AtomicUsize,
    outbound_high_priority_consensus_size: AtomicUsize,
    outbound_low_priority_consensus_size: AtomicUsize,
    last_throughput_measurement_timestamp: AtomicI64,
    bytes_received: AtomicU64,
    bytes_sent: AtomicU64,
    avg_bps_in: AtomicU64,
    avg_bps_out: AtomicU64,
}

impl StatsExportService {
    /// Creates a new instance of the starts export service object.
    #[cfg(feature = "instrumentation")]
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

    /// Creates a new instance of the starts export service object.
    #[cfg(not(feature = "instrumentation"))]
    pub fn new() -> anyhow::Result<Self> { Ok(Default::default()) }

    /// Increases the peer count.
    pub fn peers_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.peers_gauge.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.peers_gauge.fetch_add(1, Ordering::Relaxed);
    }

    /// Decreases the peer count.
    pub fn peers_dec(&self) {
        #[cfg(feature = "instrumentation")]
        self.peers_gauge.dec();
        #[cfg(not(feature = "instrumentation"))]
        self.peers_gauge.fetch_sub(1, Ordering::Relaxed);
    }

    /// Increases the number of received packets.
    pub fn pkt_received_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.pkts_received_counter.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.pkts_received_counter.fetch_add(1, Ordering::Relaxed);
    }

    /// Increases the number of sent packets.
    pub fn pkt_sent_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.pkts_sent_counter.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.pkts_sent_counter.fetch_add(1, Ordering::Relaxed);
    }

    /// Increases the number of received connections.
    pub fn conn_received_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.connections_received.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.connections_received.fetch_add(1, Ordering::Relaxed);
    }

    /// Increases the number of high priority consensus messages dropped due to
    /// the queue being full.
    pub fn inbound_high_priority_consensus_drops_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.inbound_high_priority_consensus_drops_counter.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.inbound_high_priority_consensus_drops_counter.fetch_add(1, Ordering::Relaxed);
    }

    /// Increases the number of low priority consensus messages dropped due to
    /// the queue being full.
    pub fn inbound_low_priority_consensus_drops_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.inbound_low_priority_consensus_drops_counter.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.inbound_low_priority_consensus_drops_counter.fetch_add(1, Ordering::Relaxed);
    }

    /// Increases the number of received high priority consensus messages.
    pub fn inbound_high_priority_consensus_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.inbound_high_priority_consensus_counter.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.inbound_high_priority_consensus_counter.fetch_add(1, Ordering::Relaxed);
    }

    /// Increases the number of received low priority consensus messages.
    pub fn inbound_low_priority_consensus_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.inbound_low_priority_consensus_counter.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.inbound_low_priority_consensus_counter.fetch_add(1, Ordering::Relaxed);
    }

    /// Sets the size value of the high priority inbound consensus queue.
    pub fn set_inbound_high_priority_consensus_size(&self, value: i64) {
        #[cfg(feature = "instrumentation")]
        self.inbound_high_priority_consensus_size.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.inbound_high_priority_consensus_size.store(value as usize, Ordering::Relaxed);
    }

    /// Sets the size value of the low priority inbound consensus queue.
    pub fn set_inbound_low_priority_consensus_size(&self, value: i64) {
        #[cfg(feature = "instrumentation")]
        self.inbound_low_priority_consensus_size.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.inbound_low_priority_consensus_size.store(value as usize, Ordering::Relaxed);
    }

    /// Sets the size value of the high priority outbound consensus queue.
    pub fn set_outbound_high_priority_consensus_size(&self, value: i64) {
        #[cfg(feature = "instrumentation")]
        self.outbound_high_priority_consensus_size.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.outbound_high_priority_consensus_size.store(value as usize, Ordering::Relaxed);
    }

    /// Sets the size value of the low priority outbound consensus queue.
    pub fn set_outbound_low_priority_consensus_size(&self, value: i64) {
        #[cfg(feature = "instrumentation")]
        self.outbound_low_priority_consensus_size.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.outbound_low_priority_consensus_size.store(value as usize, Ordering::Relaxed);
    }

    /// Gets the timestamp for the last throughput check.
    pub fn get_last_throughput_measurement_timestamp(&self) -> i64 {
        #[cfg(feature = "instrumentation")]
        {
            self.last_throughput_measurement_timestamp.get()
        }
        #[cfg(not(feature = "instrumentation"))]
        {
            self.last_throughput_measurement_timestamp.load(std::sync::atomic::Ordering::Relaxed)
        }
    }

    /// Sets the value of throughput timestamp.
    pub fn set_last_throughput_measurement_timestamp(&self, value: i64) {
        #[cfg(feature = "instrumentation")]
        self.last_throughput_measurement_timestamp.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.last_throughput_measurement_timestamp.store(value, Ordering::Relaxed);
    }

    /// Gets the count of received bytes.
    pub fn get_bytes_received(&self) -> u64 {
        #[cfg(feature = "instrumentation")]
        {
            self.bytes_received.get()
        }
        #[cfg(not(feature = "instrumentation"))]
        {
            self.bytes_received.load(std::sync::atomic::Ordering::Relaxed)
        }
    }

    /// Gets the count of sent bytes.
    pub fn get_bytes_sent(&self) -> u64 {
        #[cfg(feature = "instrumentation")]
        {
            self.bytes_sent.get()
        }
        #[cfg(not(feature = "instrumentation"))]
        {
            self.bytes_sent.load(std::sync::atomic::Ordering::Relaxed)
        }
    }

    /// Sets the value of received bytes.
    pub fn set_bytes_received(&self, value: u64) {
        #[cfg(feature = "instrumentation")]
        self.bytes_received.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.bytes_received.store(value, Ordering::Relaxed);
    }

    /// Sets the value of sent bytes.
    pub fn set_bytes_sent(&self, value: u64) {
        #[cfg(feature = "instrumentation")]
        self.bytes_sent.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.bytes_sent.store(value, Ordering::Relaxed);
    }

    /// Gets the value of average inbound throughput.
    pub fn get_avg_bps_in(&self) -> u64 {
        #[cfg(feature = "instrumentation")]
        {
            self.avg_bps_in.get()
        }
        #[cfg(not(feature = "instrumentation"))]
        self.avg_bps_in.load(std::sync::atomic::Ordering::Relaxed)
    }

    /// Gets the value of average outbound throughput.
    pub fn get_avg_bps_out(&self) -> u64 {
        #[cfg(feature = "instrumentation")]
        {
            self.avg_bps_out.get()
        }
        #[cfg(not(feature = "instrumentation"))]
        self.avg_bps_out.load(std::sync::atomic::Ordering::Relaxed)
    }

    /// Sets the value of average inbound throughput.
    pub fn set_avg_bps_in(&self, value: u64) {
        #[cfg(feature = "instrumentation")]
        self.avg_bps_in.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.avg_bps_in.store(value, Ordering::Relaxed);
    }

    /// Sets the value of average outbound throughput.
    pub fn set_avg_bps_out(&self, value: u64) {
        #[cfg(feature = "instrumentation")]
        self.avg_bps_out.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.avg_bps_out.store(value, Ordering::Relaxed);
    }

    #[cfg(feature = "instrumentation")]
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

    #[cfg(feature = "instrumentation")]
    fn index(state: State) -> (State, HTMLStringResponse) {
        let message = HTMLStringResponse(format!(
            "<html><body><h1>Prometheus for {} v{}</h1>Operational!</p></body></html>",
            super::APPNAME,
            super::VERSION
        ));
        (state, message)
    }

    #[cfg(feature = "instrumentation")]
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
    #[cfg(feature = "instrumentation")]
    pub async fn start_server(
        &self,
        listen_addr: SocketAddr,
        error_sender: tokio::sync::broadcast::Sender<NodeShutdownCause>,
    ) -> Result<(), ()> {
        let result = gotham::plain::init_server(listen_addr, self.router()).await;
        if let Err(_) = result {
            // Log an error and notify main thread that an error occured.
            error!("A runtime error occurred in the statistics server.");
            error_sender.send(NodeShutdownCause::StatsServer).expect(
                "A runtime error occurred in the statistics server, but it was unable to notify \
                 the owning thread.",
            );
        }
        result
    }

    #[cfg(feature = "instrumentation")]
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
                labels! {
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
#[cfg(feature = "instrumentation")]
pub fn instantiate_stats_export_engine(
    conf: &configuration::Config,
) -> anyhow::Result<Arc<StatsExportService>> {
    let prom = if conf.prometheus.prometheus_server {
        info!("Enabling prometheus server");
        StatsExportService::new()?
    } else if let Some(ref push_gateway) = conf.prometheus.prometheus_push_gateway {
        info!("Enabling prometheus push gateway at {}", push_gateway);
        StatsExportService::new()?
    } else {
        unreachable!(); // ensured in configuration.rs
    };
    Ok(Arc::new(prom))
}

/// Starts the stats export engine.
#[cfg(not(feature = "instrumentation"))]
pub fn instantiate_stats_export_engine(
    _: &configuration::Config,
) -> anyhow::Result<Arc<StatsExportService>> {
    Ok(Arc::new(StatsExportService::new()?))
}

/// Starts the push gateway to Prometheus.
#[cfg(feature = "instrumentation")]
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
