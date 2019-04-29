cfg_if! {
    if #[cfg(feature = "instrumentation")] {
        use failure::Fallible;
        use prometheus::{self, Encoder, IntCounter, IntGauge, Opts, Registry, TextEncoder};
        use std::{net::SocketAddr, thread, time, sync::Mutex};
        use gotham::{
            handler::IntoResponse,
            helpers::http::response::create_response,
            middleware::state::StateMiddleware,
            pipeline::{single::single_pipeline, single_middleware},
            router::{builder::*, Router},
            state::{FromState, State},
        };
        use hyper::{Body, Response, StatusCode};
    } else {
        use std::sync::atomic::{AtomicUsize, Ordering};
    }
}
use std::{fmt, sync::Arc};

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum StatsServiceMode {
    BootstrapperMode,
    NodeMode,
}

impl fmt::Display for StatsServiceMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            StatsServiceMode::BootstrapperMode => write!(f, "bootstrapper"),
            StatsServiceMode::NodeMode => write!(f, "node"),
        }
    }
}

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
            registry: Arc<Mutex<Registry>>,
        }

        impl PrometheusStateData {
            fn new(registry: Registry) -> Self {
                Self {
                    registry: Arc::new(Mutex::new(registry)),
                }
            }
        }

        #[derive(Clone)]
        pub struct StatsExportService {
            mode: StatsServiceMode,
            registry: Registry,
            pkts_received_counter: IntCounter,
            pkts_sent_counter: IntCounter,
            peers_gauge: IntGauge,
            connections_received: IntCounter,
            invalid_packets_received: IntCounter,
            unknown_packets_received: IntCounter,
            invalid_network_packets_received: IntCounter,
            queue_size: IntGauge,
            queue_resent: IntCounter,
        }
    }
}

#[cfg(not(feature = "instrumentation"))]
#[derive(Clone)]
pub struct StatsExportService {
    mode: StatsServiceMode,
    pkts_received_counter: Arc<AtomicUsize>,
    pkts_sent_counter: Arc<AtomicUsize>,
    peers_gauge: Arc<AtomicUsize>,
    connections_received: Arc<AtomicUsize>,
    invalid_packets_received: Arc<AtomicUsize>,
    unknown_packets_received: Arc<AtomicUsize>,
    invalid_network_packets_received: Arc<AtomicUsize>,
    queue_size: Arc<AtomicUsize>,
    queue_resent: Arc<AtomicUsize>,
}

impl StatsExportService {
    #[cfg(feature = "instrumentation")]
    pub fn new(mode: StatsServiceMode) -> Fallible<Self> {
        let registry = Registry::new();
        let pg_opts = Opts::new("peer_number", "current peers connected");
        let pg = IntGauge::with_opts(pg_opts)?;
        if mode == StatsServiceMode::NodeMode || mode == StatsServiceMode::BootstrapperMode {
            registry.register(Box::new(pg.clone()))?;
        }

        let qs_opts = Opts::new("queue_size", "current queue size");
        let qs = IntGauge::with_opts(qs_opts)?;
        if mode == StatsServiceMode::NodeMode || mode == StatsServiceMode::BootstrapperMode {
            registry.register(Box::new(qs.clone()))?;
        }

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
        if mode == StatsServiceMode::NodeMode || mode == StatsServiceMode::BootstrapperMode {
            registry.register(Box::new(ipr.clone()))?;
        }

        let upr_opts = Opts::new("unknown_packets_received", "unknown packets received");
        let upr = IntCounter::with_opts(upr_opts)?;
        if mode == StatsServiceMode::NodeMode || mode == StatsServiceMode::BootstrapperMode {
            registry.register(Box::new(upr.clone()))?;
        }

        let inpr_opts = Opts::new(
            "invalid_network_packets_received",
            "invalid network packets received",
        );
        let inpr = IntCounter::with_opts(inpr_opts)?;
        if mode == StatsServiceMode::NodeMode || mode == StatsServiceMode::BootstrapperMode {
            registry.register(Box::new(inpr.clone()))?;
        }

        let qrs_opts = Opts::new("queue_resent", "items in queue that needed to be resent");
        let qrs = IntCounter::with_opts(qrs_opts)?;
        if mode == StatsServiceMode::NodeMode || mode == StatsServiceMode::BootstrapperMode {
            registry.register(Box::new(qrs.clone()))?;
        }

        Ok(StatsExportService {
            mode,
            registry,
            pkts_received_counter: prc,
            pkts_sent_counter: psc,
            peers_gauge: pg,
            connections_received: cr,
            invalid_packets_received: ipr,
            unknown_packets_received: upr,
            invalid_network_packets_received: inpr,
            queue_size: qs,
            queue_resent: qrs,
        })
    }

    #[cfg(not(feature = "instrumentation"))]
    pub fn new(mode: StatsServiceMode) -> Self {
        StatsExportService {
            mode,
            pkts_received_counter: Arc::new(AtomicUsize::new(0)),
            pkts_sent_counter: Arc::new(AtomicUsize::new(0)),
            peers_gauge: Arc::new(AtomicUsize::new(0)),
            connections_received: Arc::new(AtomicUsize::new(0)),
            invalid_packets_received: Arc::new(AtomicUsize::new(0)),
            unknown_packets_received: Arc::new(AtomicUsize::new(0)),
            invalid_network_packets_received: Arc::new(AtomicUsize::new(0)),
            queue_size: Arc::new(AtomicUsize::new(0)),
            queue_resent: Arc::new(AtomicUsize::new(0)),
        }
    }

    pub fn peers_inc(&mut self) {
        #[cfg(feature = "instrumentation")]
        self.peers_gauge.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.peers_gauge.fetch_add(1, Ordering::Relaxed);
    }

    pub fn peers_dec(&mut self) {
        #[cfg(feature = "instrumentation")]
        self.peers_gauge.dec();
        #[cfg(not(feature = "instrumentation"))]
        self.peers_gauge.fetch_sub(1, Ordering::Relaxed);
    }

    pub fn pkt_received_inc(&mut self) {
        #[cfg(feature = "instrumentation")]
        self.pkts_received_counter.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.pkts_received_counter.fetch_add(1, Ordering::Relaxed);
    }

    pub fn pkt_sent_inc(&mut self) {
        #[cfg(feature = "instrumentation")]
        self.pkts_sent_counter.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.pkts_sent_counter.fetch_add(1, Ordering::Relaxed);
    }

    pub fn pkt_sent_inc_by(&mut self, to_add: i64) {
        #[cfg(feature = "instrumentation")]
        self.pkts_sent_counter.inc_by(to_add);
        #[cfg(not(feature = "instrumentation"))]
        self.pkts_sent_counter
            .fetch_add(to_add as usize, Ordering::Relaxed);
    }

    pub fn conn_received_inc(&mut self) {
        #[cfg(feature = "instrumentation")]
        self.connections_received.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.connections_received.fetch_add(1, Ordering::Relaxed);
    }

    pub fn invalid_pkts_received_inc(&mut self) {
        #[cfg(feature = "instrumentation")]
        self.invalid_packets_received.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.invalid_packets_received
            .fetch_add(1, Ordering::Relaxed);
    }

    pub fn invalid_network_pkts_received_inc(&mut self) {
        #[cfg(feature = "instrumentation")]
        self.invalid_network_packets_received.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.invalid_network_packets_received
            .fetch_add(1, Ordering::Relaxed);
    }

    pub fn unknown_pkts_received_inc(&mut self) {
        #[cfg(feature = "instrumentation")]
        self.unknown_packets_received.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.unknown_packets_received
            .fetch_add(1, Ordering::Relaxed);
    }

    pub fn queue_size_inc(&mut self) {
        #[cfg(feature = "instrumentation")]
        self.queue_size.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.queue_size.fetch_add(1, Ordering::Relaxed);
    }

    pub fn queue_size_dec(&mut self) {
        #[cfg(feature = "instrumentation")]
        self.queue_size.dec();
        #[cfg(not(feature = "instrumentation"))]
        self.queue_size.fetch_sub(1, Ordering::Relaxed);
    }

    pub fn queue_size_inc_by(&mut self, to_add: i64) {
        #[cfg(feature = "instrumentation")]
        self.queue_size.add(to_add);
        #[cfg(not(feature = "instrumentation"))]
        self.queue_size
            .fetch_add(to_add as usize, Ordering::Relaxed);
    }

    pub fn queue_resent_inc_by(&mut self, to_add: i64) {
        #[cfg(feature = "instrumentation")]
        self.queue_resent.inc_by(to_add);
        #[cfg(not(feature = "instrumentation"))]
        self.queue_resent
            .fetch_add(to_add as usize, Ordering::Relaxed);
    }

    pub fn queue_size(&self) -> i64 {
        #[cfg(feature = "instrumentation")]
        return self.queue_size.get();
        #[cfg(not(feature = "instrumentation"))]
        return self.queue_size.load(Ordering::Relaxed) as i64;
    }

    #[cfg(feature = "instrumentation")]
    fn metrics(state: State) -> (State, String) {
        let state_data = PrometheusStateData::borrow_from(&state);
        let encoder = TextEncoder::new();
        let metric_familys = lock_or_die!(state_data.registry).gather();
        let mut buffer = vec![];
        assert!(encoder.encode(&metric_familys, &mut buffer).is_ok());
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

    #[cfg(feature = "instrumentation")]
    pub fn start_server(&mut self, listen_addr: SocketAddr) -> thread::JoinHandle<()> {
        let self_clone = self.clone();
        thread::spawn(move || {
            gotham::start(listen_addr, self_clone.router());
        })
    }

    #[cfg(feature = "instrumentation")]
    pub fn start_push_to_gateway(
        &self,
        prometheus_push_gateway: String,
        prometheus_push_interval: u64,
        prometheus_job_name: String,
        prometheus_instance_name: String,
        prometheus_push_username: Option<String>,
        prometheus_push_password: Option<String>,
    ) {
        let metrics_families = self.registry.gather();
        let _mode = self.mode.to_string();
        let _th = thread::spawn(move || loop {
            debug!("Pushing data to push gateway");
            let username_pass = prometheus_push_username.clone().and_then(|username| {
                prometheus_push_password.clone().and_then(|password| {
                    Some(prometheus::BasicAuthentication { username, password })
                })
            });
            thread::sleep(time::Duration::from_secs(prometheus_push_interval));
            prometheus::push_metrics(
                &prometheus_job_name,
                labels! {
                    "instance".to_owned() => prometheus_instance_name.clone(),
                    "mode".to_owned() => _mode.clone(),
                },
                &prometheus_push_gateway,
                metrics_families.clone(),
                username_pass,
            )
            .map_err(|e| error!("Can't push to prometheus push gateway {}", e))
            .ok();
        });
    }

    #[cfg(not(feature = "instrumentation"))]
    pub fn start_push_to_gateway(
        &self,
        _: String,
        _: u64,
        _: String,
        _: String,
        _: Option<String>,
        _: Option<String>,
    ) {
    }
}

#[cfg(test)]
mod tests {
    use crate::stats_export_service::*;

    #[test]
    pub fn test_node_mode() {
        let _prom_inst = StatsExportService::new(StatsServiceMode::NodeMode);
    }

    #[test]
    pub fn test_bootstrapper_mode() {
        let _prom_inst = StatsExportService::new(StatsServiceMode::BootstrapperMode);
    }
}
