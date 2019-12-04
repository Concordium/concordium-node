cfg_if! {
    if #[cfg(feature = "instrumentation")] {
        use prometheus::{self, Encoder, core::{AtomicU64, GenericGauge}, IntCounter, IntGauge, Opts, Registry, TextEncoder};
        use crate::common::p2p_node_id::P2PNodeId;
        use std::{net::SocketAddr, thread, time, sync::{Arc, RwLock}};
        use gotham::{
            handler::IntoResponse,
            helpers::http::response::create_response,
            middleware::state::StateMiddleware,
            pipeline::{single::single_pipeline, single_middleware},
            router::{builder::*, Router},
            state::{FromState, State},
        };
        use hyper::{Body, Response, StatusCode};
        use tokio::runtime::{self, Runtime};
    } else {
        use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
    }
}
use crate::configuration;
use failure::Fallible;
use std::fmt;

#[derive(Debug, PartialEq)]
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

impl Default for StatsServiceMode {
    fn default() -> Self { Self::NodeMode }
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
            registry: Arc<RwLock<Registry>>,
        }

        impl PrometheusStateData {
            fn new(registry: Registry) -> Self {
                Self {
                    registry: Arc::new(RwLock::new(registry)),
                }
            }
        }

        pub struct StatsExportService {
            pub mode: StatsServiceMode,
            registry: Registry,
            pkts_received_counter: IntCounter,
            pkts_sent_counter: IntCounter,
            pkts_dropped_counter: IntCounter,
            pkts_resend_counter: IntCounter,
            peers_gauge: IntGauge,
            connections_received: IntCounter,
            invalid_packets_received: IntCounter,
            invalid_network_packets_received: IntCounter,
            queue_size: IntGauge,
            resend_queue_size: IntGauge,
            tokio_runtime: RwLock<Option<Runtime>>,
            gs_block_receipt: IntGauge,
            gs_block_entry: IntGauge,
            gs_block_query: IntGauge,
            gs_finalization_receipt: IntGauge,
            gs_finalization_entry: IntGauge,
            gs_finalization_query: IntGauge,
            inbound_high_priority_consensus_drops_counter: IntCounter,
            inbound_low_priority_consensus_drops_counter: IntCounter,
            inbound_high_priority_consensus_counter: IntCounter,
            inbound_low_priority_consensus_counter: IntCounter,
            inbound_high_priority_consensus_size: IntGauge,
            inbound_low_priority_consensus_size: IntGauge,
            outbound_high_priority_consensus_size: IntGauge,
            outbound_low_priority_consensus_size: IntGauge,
            bytes_received: GenericGauge<AtomicU64>,
            bytes_sent: GenericGauge<AtomicU64>,
            avg_bps_in: GenericGauge<AtomicU64>,
            avg_bps_out: GenericGauge<AtomicU64>,
        }
    }
}

#[cfg(not(feature = "instrumentation"))]
#[derive(Default)]
pub struct StatsExportService {
    pub mode: StatsServiceMode,
    pkts_received_counter: AtomicUsize,
    pkts_sent_counter: AtomicUsize,
    pkts_dropped_counter: AtomicUsize,
    pkts_resend_counter: AtomicUsize,
    peers_gauge: AtomicUsize,
    connections_received: AtomicUsize,
    invalid_packets_received: AtomicUsize,
    invalid_network_packets_received: AtomicUsize,
    queue_size: AtomicUsize,
    resend_queue_size: AtomicUsize,
    gs_block_receipt: AtomicUsize,
    gs_block_entry: AtomicUsize,
    gs_block_query: AtomicUsize,
    gs_finalization_receipt: AtomicUsize,
    gs_finalization_entry: AtomicUsize,
    gs_finalization_query: AtomicUsize,
    inbound_high_priority_consensus_drops_counter: AtomicUsize,
    inbound_low_priority_consensus_drops_counter: AtomicUsize,
    inbound_high_priority_consensus_counter: AtomicUsize,
    inbound_low_priority_consensus_counter: AtomicUsize,
    inbound_high_priority_consensus_size: AtomicUsize,
    inbound_low_priority_consensus_size: AtomicUsize,
    outbound_high_priority_consensus_size: AtomicUsize,
    outbound_low_priority_consensus_size: AtomicUsize,
    bytes_received: AtomicU64,
    bytes_sent: AtomicU64,
    avg_bps_in: AtomicU64,
    avg_bps_out: AtomicU64,
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

        let rqs_opts = Opts::new("resend_queue_size", "current queue size");
        let rqs = IntGauge::with_opts(rqs_opts)?;
        if mode == StatsServiceMode::NodeMode || mode == StatsServiceMode::BootstrapperMode {
            registry.register(Box::new(rqs.clone()))?;
        }

        let dp_opts = Opts::new("packets_dropped", "dropped packets");
        let dp = IntCounter::with_opts(dp_opts)?;
        if mode == StatsServiceMode::NodeMode || mode == StatsServiceMode::BootstrapperMode {
            registry.register(Box::new(dp.clone()))?;
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

        let rs_opts = Opts::new("packets_resend", "items in queue that needed to be resend");
        let rs = IntCounter::with_opts(rs_opts)?;
        if mode == StatsServiceMode::NodeMode || mode == StatsServiceMode::BootstrapperMode {
            registry.register(Box::new(rs.clone()))?;
        }

        let sbr_opts = Opts::new("gs_block_receipt", "global state block receipt");
        let sbr = IntGauge::with_opts(sbr_opts)?;
        registry.register(Box::new(sbr.clone()))?;

        let sbe_opts = Opts::new("gs_block_entry", "global state block entry");
        let sbe = IntGauge::with_opts(sbe_opts)?;
        registry.register(Box::new(sbe.clone()))?;

        let sbq_opts = Opts::new("gs_block_query", "global state block query");
        let sbq = IntGauge::with_opts(sbq_opts)?;
        registry.register(Box::new(sbq.clone()))?;

        let sfr_opts = Opts::new(
            "gs_finalization_receipt",
            "global state finalization receipt",
        );
        let sfr = IntGauge::with_opts(sfr_opts)?;
        registry.register(Box::new(sfr.clone()))?;

        let sfe_opts = Opts::new("gs_finalization_entry", "global state finalization receipt");
        let sfe = IntGauge::with_opts(sfe_opts)?;
        registry.register(Box::new(sfe.clone()))?;

        let sfq_opts = Opts::new("gs_finalization_query", "global state finalization receipt");
        let sfq = IntGauge::with_opts(sfq_opts)?;
        registry.register(Box::new(sfq.clone()))?;

        let inbound_high_priority_consensus_drops_opts = Opts::new(
            "inbound_high_priority_consensus_drops",
            "inbound high priority consensus messages dropped",
        );
        let inbound_high_priority_consensus_drops_counter =
            IntCounter::with_opts(inbound_high_priority_consensus_drops_opts)?;
        registry.register(Box::new(
            inbound_high_priority_consensus_drops_counter.clone(),
        ))?;

        let inbound_low_priority_consensus_drops_opts = Opts::new(
            "inbound_low_priority_consensus_drops",
            "inbound low priority consensus messages dropped",
        );
        let inbound_low_priority_consensus_drops_counter =
            IntCounter::with_opts(inbound_low_priority_consensus_drops_opts)?;
        registry.register(Box::new(
            inbound_low_priority_consensus_drops_counter.clone(),
        ))?;

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
            mode,
            registry,
            pkts_received_counter: prc,
            pkts_sent_counter: psc,
            pkts_dropped_counter: dp,
            pkts_resend_counter: rs,
            peers_gauge: pg,
            connections_received: cr,
            invalid_packets_received: ipr,
            invalid_network_packets_received: inpr,
            queue_size: qs,
            resend_queue_size: rqs,
            tokio_runtime: RwLock::new(None),
            gs_block_receipt: sbr,
            gs_block_entry: sbe,
            gs_block_query: sbq,
            gs_finalization_receipt: sfr,
            gs_finalization_entry: sfe,
            gs_finalization_query: sfq,
            inbound_high_priority_consensus_drops_counter,
            inbound_low_priority_consensus_drops_counter,
            inbound_high_priority_consensus_counter,
            inbound_low_priority_consensus_counter,
            inbound_high_priority_consensus_size,
            inbound_low_priority_consensus_size,
            outbound_high_priority_consensus_size,
            outbound_low_priority_consensus_size,
            bytes_received: brc,
            bytes_sent: bsc,
            avg_bps_in,
            avg_bps_out,
        })
    }

    #[cfg(not(feature = "instrumentation"))]
    pub fn new(mode: StatsServiceMode) -> Fallible<Self> {
        Ok(Self {
            mode,
            ..Default::default()
        })
    }

    pub fn peers_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.peers_gauge.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.peers_gauge.fetch_add(1, Ordering::Relaxed);
    }

    pub fn peers_dec(&self) {
        #[cfg(feature = "instrumentation")]
        self.peers_gauge.dec();
        #[cfg(not(feature = "instrumentation"))]
        self.peers_gauge.fetch_sub(1, Ordering::Relaxed);
    }

    pub fn pkt_received_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.pkts_received_counter.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.pkts_received_counter.fetch_add(1, Ordering::Relaxed);
    }

    pub fn pkt_sent_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.pkts_sent_counter.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.pkts_sent_counter.fetch_add(1, Ordering::Relaxed);
    }

    pub fn pkt_dropped_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.pkts_dropped_counter.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.pkts_dropped_counter.fetch_add(1, Ordering::Relaxed);
    }

    pub fn pkt_sent_inc_by(&self, to_add: i64) {
        #[cfg(feature = "instrumentation")]
        self.pkts_sent_counter.inc_by(to_add);
        #[cfg(not(feature = "instrumentation"))]
        self.pkts_sent_counter
            .fetch_add(to_add as usize, Ordering::Relaxed);
    }

    pub fn conn_received_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.connections_received.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.connections_received.fetch_add(1, Ordering::Relaxed);
    }

    pub fn invalid_pkts_received_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.invalid_packets_received.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.invalid_packets_received
            .fetch_add(1, Ordering::Relaxed);
    }

    pub fn invalid_network_pkts_received_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.invalid_network_packets_received.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.invalid_network_packets_received
            .fetch_add(1, Ordering::Relaxed);
    }

    pub fn queue_size_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.queue_size.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.queue_size.fetch_add(1, Ordering::Relaxed);
    }

    pub fn queue_size_dec(&self) {
        #[cfg(feature = "instrumentation")]
        self.queue_size.dec();
        #[cfg(not(feature = "instrumentation"))]
        self.queue_size.fetch_sub(1, Ordering::Relaxed);
    }

    pub fn resend_queue_size_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.resend_queue_size.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.resend_queue_size.fetch_add(1, Ordering::Relaxed);
    }

    pub fn resend_queue_size_dec(&self) {
        #[cfg(feature = "instrumentation")]
        self.resend_queue_size.dec();
        #[cfg(not(feature = "instrumentation"))]
        self.resend_queue_size.fetch_sub(1, Ordering::Relaxed);
    }

    pub fn queue_size_inc_by(&self, to_add: i64) {
        #[cfg(feature = "instrumentation")]
        self.queue_size.add(to_add);
        #[cfg(not(feature = "instrumentation"))]
        self.queue_size
            .fetch_add(to_add as usize, Ordering::Relaxed);
    }

    pub fn pkt_resend_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.pkts_resend_counter.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.pkts_resend_counter.fetch_add(1, Ordering::Relaxed);
    }

    pub fn queue_size(&self) -> i64 {
        #[cfg(feature = "instrumentation")]
        return self.queue_size.get();
        #[cfg(not(feature = "instrumentation"))]
        return self.queue_size.load(Ordering::Relaxed) as i64;
    }

    pub fn set_gs_block_receipt(&self, value: i64) {
        #[cfg(feature = "instrumentation")]
        return self.gs_block_receipt.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.gs_block_receipt
            .store(value as usize, Ordering::Relaxed);
    }

    pub fn set_gs_block_entry(&self, value: i64) {
        #[cfg(feature = "instrumentation")]
        return self.gs_block_entry.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.gs_block_entry.store(value as usize, Ordering::Relaxed);
    }

    pub fn set_gs_block_query(&self, value: i64) {
        #[cfg(feature = "instrumentation")]
        return self.gs_block_query.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.gs_block_query.store(value as usize, Ordering::Relaxed);
    }

    pub fn set_gs_finalization_receipt(&self, value: i64) {
        #[cfg(feature = "instrumentation")]
        return self.gs_finalization_receipt.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.gs_finalization_receipt
            .store(value as usize, Ordering::Relaxed);
    }

    pub fn set_gs_finalization_entry(&self, value: i64) {
        #[cfg(feature = "instrumentation")]
        return self.gs_finalization_entry.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.gs_finalization_entry
            .store(value as usize, Ordering::Relaxed);
    }

    pub fn set_gs_finalization_query(&self, value: i64) {
        #[cfg(feature = "instrumentation")]
        return self.gs_finalization_query.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.gs_finalization_query
            .store(value as usize, Ordering::Relaxed);
    }

    pub fn inbound_high_priority_consensus_drops_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.inbound_high_priority_consensus_drops_counter.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.inbound_high_priority_consensus_drops_counter
            .fetch_add(1, Ordering::Relaxed);
    }

    pub fn inbound_low_priority_consensus_drops_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.inbound_low_priority_consensus_drops_counter.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.inbound_low_priority_consensus_drops_counter
            .fetch_add(1, Ordering::Relaxed);
    }

    pub fn inbound_high_priority_consensus_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.inbound_high_priority_consensus_counter.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.inbound_high_priority_consensus_counter
            .fetch_add(1, Ordering::Relaxed);
    }

    pub fn inbound_low_priority_consensus_inc(&self) {
        #[cfg(feature = "instrumentation")]
        self.inbound_low_priority_consensus_counter.inc();
        #[cfg(not(feature = "instrumentation"))]
        self.inbound_low_priority_consensus_counter
            .fetch_add(1, Ordering::Relaxed);
    }

    pub fn set_inbound_high_priority_consensus_size(&self, value: i64) {
        #[cfg(feature = "instrumentation")]
        self.inbound_high_priority_consensus_size.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.inbound_high_priority_consensus_size
            .store(value as usize, Ordering::Relaxed);
    }

    pub fn set_inbound_low_priority_consensus_size(&self, value: i64) {
        #[cfg(feature = "instrumentation")]
        self.inbound_low_priority_consensus_size.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.inbound_low_priority_consensus_size
            .store(value as usize, Ordering::Relaxed);
    }

    pub fn set_outbound_high_priority_consensus_size(&self, value: i64) {
        #[cfg(feature = "instrumentation")]
        self.outbound_high_priority_consensus_size.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.outbound_high_priority_consensus_size
            .store(value as usize, Ordering::Relaxed);
    }

    pub fn set_outbound_low_priority_consensus_size(&self, value: i64) {
        #[cfg(feature = "instrumentation")]
        self.outbound_low_priority_consensus_size.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.outbound_low_priority_consensus_size
            .store(value as usize, Ordering::Relaxed);
    }

    pub fn get_bytes_received(&self) -> u64 {
        #[cfg(feature = "instrumentation")]
        {
            self.bytes_received.get()
        }
        #[cfg(not(feature = "instrumentation"))]
        {
            self.bytes_received
                .load(std::sync::atomic::Ordering::Relaxed)
        }
    }

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

    pub fn set_bytes_received(&self, value: u64) {
        #[cfg(feature = "instrumentation")]
        self.bytes_received.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.bytes_received.store(value, Ordering::Relaxed);
    }

    pub fn set_bytes_sent(&self, value: u64) {
        #[cfg(feature = "instrumentation")]
        self.bytes_sent.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.bytes_sent.store(value, Ordering::Relaxed);
    }

    pub fn get_avg_bps_in(&self) -> u64 {
        #[cfg(feature = "instrumentation")]
        {
            self.avg_bps_in.get()
        }
        #[cfg(not(feature = "instrumentation"))]
        self.avg_bps_in.load(std::sync::atomic::Ordering::Relaxed)
    }

    pub fn get_avg_bps_out(&self) -> u64 {
        #[cfg(feature = "instrumentation")]
        {
            self.avg_bps_out.get()
        }
        #[cfg(not(feature = "instrumentation"))]
        self.avg_bps_out.load(std::sync::atomic::Ordering::Relaxed)
    }

    pub fn set_avg_bps_in(&self, value: u64) {
        #[cfg(feature = "instrumentation")]
        self.avg_bps_in.set(value);
        #[cfg(not(feature = "instrumentation"))]
        self.avg_bps_in.store(value, Ordering::Relaxed);
    }

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

    #[cfg(feature = "instrumentation")]
    pub fn start_server(&self, listen_addr: SocketAddr) {
        let runtime = runtime::Builder::new()
            .core_threads(num_cpus::get())
            .name_prefix("gotham-worker-")
            .build()
            .unwrap();
        gotham::start_on_executor(listen_addr, self.router(), runtime.executor());
        if let Ok(mut locked_tokio) = self.tokio_runtime.write() {
            *locked_tokio = Some(runtime);
        }
    }

    #[cfg(feature = "instrumentation")]
    pub fn stop_server(&self) {
        if let Ok(mut locked_tokio) = self.tokio_runtime.write() {
            if (&*locked_tokio).is_some() {
                let old_v = std::mem::replace(&mut *locked_tokio, None);
                old_v.map(tokio::runtime::Runtime::shutdown_now);
            }
        }
    }

    #[cfg(not(feature = "instrumentation"))]
    pub fn stop_server(&self) {}

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
        let _th = spawn_or_die!("Prometheus push", move || loop {
            debug!("Pushing data to push gateway");
            let username_pass = prometheus_push_username.clone().and_then(|username| {
                prometheus_push_password
                    .clone()
                    .map(|password| prometheus::BasicAuthentication { username, password })
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

    #[cfg(feature = "instrumentation")]
    pub fn get_gs_stats(&self) -> (u32, u32, u32, u32, u32, u32) {
        (
            self.gs_block_receipt.get() as u32,
            self.gs_block_query.get() as u32,
            self.gs_block_entry.get() as u32,
            self.gs_finalization_receipt.get() as u32,
            self.gs_finalization_query.get() as u32,
            self.gs_finalization_entry.get() as u32,
        )
    }

    #[cfg(not(feature = "instrumentation"))]
    pub fn get_gs_stats(&self) -> (u32, u32, u32, u32, u32, u32) {
        (
            self.gs_block_receipt.load(Ordering::Relaxed) as u32,
            self.gs_block_query.load(Ordering::Relaxed) as u32,
            self.gs_block_entry.load(Ordering::Relaxed) as u32,
            self.gs_finalization_receipt.load(Ordering::Relaxed) as u32,
            self.gs_finalization_query.load(Ordering::Relaxed) as u32,
            self.gs_finalization_entry.load(Ordering::Relaxed) as u32,
        )
    }
}

#[cfg(feature = "instrumentation")]
pub fn instantiate_stats_export_engine(
    conf: &configuration::Config,
    mode: StatsServiceMode,
) -> Fallible<StatsExportService> {
    let prom = if conf.prometheus.prometheus_server {
        info!("Enabling prometheus server");
        let srv = StatsExportService::new(mode)?;
        srv.start_server(SocketAddr::new(
            conf.prometheus.prometheus_listen_addr.parse()?,
            conf.prometheus.prometheus_listen_port,
        ));
        srv
    } else if let Some(ref push_gateway) = conf.prometheus.prometheus_push_gateway {
        info!("Enabling prometheus push gateway at {}", push_gateway);
        StatsExportService::new(mode)?
    } else {
        panic!("Couldn't instantiate prometheus");
    };
    Ok(prom)
}

#[cfg(not(feature = "instrumentation"))]
pub fn instantiate_stats_export_engine(
    _: &configuration::Config,
    mode: StatsServiceMode,
) -> Fallible<StatsExportService> {
    Ok(StatsExportService::new(mode)?)
}

#[cfg(feature = "instrumentation")]
pub fn start_push_gateway(
    conf: &configuration::PrometheusConfig,
    service: &StatsExportService,
    id: P2PNodeId,
) {
    conf.prometheus_push_gateway
        .as_ref()
        .and_then(|prom_push_addy| {
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
            Some(())
        });
}

#[cfg(feature = "instrumentation")]
pub fn stop_stats_export_engine(conf: &configuration::Config, srv: &Option<StatsExportService>) {
    if conf.prometheus.prometheus_server {
        if let Some(srv) = srv {
            info!("Stopping prometheus server");
            srv.stop_server();
        }
    }
}

#[cfg(not(feature = "instrumentation"))]
pub fn stop_stats_export_engine(_: &configuration::Config, _: &Option<StatsExportService>) {}

#[cfg(test)]
mod tests {
    use crate::stats_export_service::*;

    #[test]
    pub fn test_node_mode() -> Fallible<()> {
        let prom_inst = StatsExportService::new(StatsServiceMode::NodeMode)?;
        assert_eq!(StatsServiceMode::NodeMode, prom_inst.mode);
        Ok(())
    }

    #[test]
    pub fn test_bootstrapper_mode() -> Fallible<()> {
        let prom_inst = StatsExportService::new(StatsServiceMode::BootstrapperMode)?;
        assert_eq!(StatsServiceMode::BootstrapperMode, prom_inst.mode);
        Ok(())
    }
}
