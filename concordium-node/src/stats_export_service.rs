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
    core::{Atomic, AtomicI64, AtomicU64, GenericGauge},
    Encoder, HistogramOpts, HistogramVec, IntCounter, IntCounterVec, IntGauge, Opts, Registry,
    TextEncoder,
};
use std::{net::SocketAddr, sync::RwLock, thread, time};
use tower_http::metrics::in_flight_requests::InFlightRequestsCounter;

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

/// Wrapper for implementing the prometheus::Collector trait for
/// InFlightRequestsCounter, which syncs the prometheus gauge with the counter
/// on scrape.
struct GrpcInFlightRequestsCollector {
    /// The prometheus gauge exposed.
    pub gauge:   IntGauge,
    /// Counter used to track in flight requests by
    /// `tower_http::metrics::InFlightRequestsLayer`.
    pub counter: InFlightRequestsCounter,
}

impl prometheus::core::Collector for GrpcInFlightRequestsCollector {
    fn desc(&self) -> Vec<&prometheus::core::Desc> { self.gauge.desc() }

    fn collect(&self) -> Vec<prometheus::proto::MetricFamily> {
        let in_flight_requests = self.counter.get();
        self.gauge.set(in_flight_requests as i64);
        self.gauge.collect()
    }
}

/// Collects statistics pertaining to the node.
pub struct StatsExportService {
    /// The Prometheus registry. Every metric which should be exposed via the
    /// Prometheus exporter should be registered in this registry.
    registry: Registry,
    /// Total number of network packets received.
    pub packets_received: IntCounter,
    /// Total number of network packets sent.
    pub packets_sent: IntCounter,
    /// Current number of connected peers.
    pub connected_peers: IntGauge,
    /// Total number of connections received.
    pub connections_received: IntCounter,
    /// Current number of inbound high priority messages in queue.
    pub inbound_high_priority_message_queue_size: IntGauge,
    /// Current number of inbound low priority messages in queue.
    pub inbound_low_priority_message_queue_size: IntGauge,
    /// Current number of outbound high priority messages in queue.
    pub outbound_high_priority_message_queue_size: IntGauge,
    /// Current number of outbound low priority messages in queue.
    pub outbound_low_priority_message_queue_size: IntGauge,
    /// Total number of bytes received.
    pub received_bytes: IntCounter,
    /// Total number of bytes sent.
    pub sent_bytes: IntCounter,
    /// The block height of the last finalized block.
    pub last_finalized_block_height: GenericGauge<AtomicU64>,
    /// Timestamp of receiving last finalized block (Unix time in milliseconds).
    pub last_finalized_block_timestamp: IntGauge,
    /// Timestamp of receiving last arrived block (Unix time in milliseconds).
    pub last_arrived_block_timestamp: IntGauge,
    /// The block height of the last finalized block.
    pub last_arrived_block_height: GenericGauge<AtomicU64>,
    /// Total number of consensus messages received. Labelled with message type
    /// (`message=<type>`) and the outcome (`result=<outcome>`).
    ///
    /// Possible values of `message` are:
    /// - `"block"`
    /// - `"transaction"`
    /// - `"finalization message"`
    /// - `"catch-up status message"`
    ///
    /// Possible values of `result` are:
    /// - `"valid"` Successful outcome.
    /// - `"invalid"` Messages being rejected as invalid.
    /// - `"dropped"` Messages being dropped due to a full queue.
    /// - `"duplicate"` Duplicate consensus messages. These are duplicate
    ///   messages determined so by consensus, **after** the message has already
    ///   been deduplicated at the network layer.
    pub received_consensus_messages: IntCounterVec,
    /// Total number of consensus messages sent. Labelled with message type
    /// (`message=<type>`).
    ///
    /// Possible values of `message` are:
    /// - `"block"`
    /// - `"transaction"`
    /// - `"finalization message"`
    /// - `"catch-up status message"`
    pub sent_consensus_messages: IntCounterVec,
    /// Current number of soft banned peers.
    pub soft_banned_peers: IntGauge,
    /// Total number of peers connected since startup.
    pub total_peers: IntCounter,
    /// Information of the node software. Contains a label `version` with the
    /// version of the node.
    pub node_info: IntGauge,
    /// Timestamp of starting up the node (Unix time in milliseconds).
    pub node_startup_timestamp: IntGauge,
    /// Histogram tracking response time of gRPC requests. Labelled with the
    /// gRPC method name (`method=<name>`) and the gRPC response status
    /// (`status=<status>`).
    pub grpc_request_response_time: HistogramVec,
    /// Counter for tracking inflight requests.
    /// This is passed to the Tower layer `InFlightRequestLayer` provided by
    /// `tower_http::metrics` and then synced with the prometheus gauge on each
    /// scrape.
    pub grpc_pending_requests_counter: InFlightRequestsCounter,
    /// Total number of bytes received at the point of last
    /// throughput_measurement.
    ///
    /// This is not exposed in the prometheus exporter, but we use the value
    /// when calculating `avg_bps_in` and `avg_bps_out`.
    pub last_throughput_measurement_received_bytes: AtomicU64,
    /// Total number of bytes sent at the point of last throughput_measurement.
    ///
    /// This is not exposed in the prometheus exporter, but we use the value
    /// when calculating `avg_bps_in` and `avg_bps_out`.
    pub last_throughput_measurement_sent_bytes: AtomicU64,
    /// Timestamp for the last calculation of throughput.
    ///
    /// This is not exposed in the prometheus exporter, but we use the value
    /// when calculating `avg_bps_in` and `avg_bps_out` (Unix time in
    /// milliseconds).
    pub last_throughput_measurement_timestamp: AtomicI64,
    /// Average bytes per second received between the two last values of
    /// last_throughput_measurement_timestamp.
    ///
    /// This is not exposed in the prometheus exporter, but is exposed by the
    /// gRPC API.
    pub avg_bps_in: AtomicU64,
    /// Average bytes per second sent between the two last values of
    /// last_throughput_measurement_timestamp.
    ///
    /// This is not exposed in the prometheus exporter, but is exposed by the
    /// gRPC API.
    pub avg_bps_out: AtomicU64,
}

impl StatsExportService {
    /// Creates a new instance of the stats export service object.
    pub fn new(grpc_duration_buckets: Vec<f64>) -> anyhow::Result<Self> {
        let registry = Registry::new();

        let packets_received = IntCounter::with_opts(Opts::new(
            "network_packets_received_total",
            "Total number of network packets received",
        ))?;
        registry.register(Box::new(packets_received.clone()))?;

        let packets_sent = IntCounter::with_opts(Opts::new(
            "network_packets_sent_total",
            "Total number of network packets sent",
        ))?;
        registry.register(Box::new(packets_sent.clone()))?;

        let connected_peers = IntGauge::with_opts(Opts::new(
            "network_connected_peers",
            "Current number of connected peers",
        ))?;
        registry.register(Box::new(connected_peers.clone()))?;

        let connections_received = IntCounter::with_opts(Opts::new(
            "network_connections_received_total",
            "Total number of connections received",
        ))?;
        registry.register(Box::new(connections_received.clone()))?;

        let inbound_high_priority_message_queue_size = IntGauge::with_opts(Opts::new(
            "network_inbound_high_priority_message_queue_size",
            "Current number of inbound high priority messages in queue",
        ))?;
        registry.register(Box::new(inbound_high_priority_message_queue_size.clone()))?;

        let inbound_low_priority_message_queue_size = IntGauge::with_opts(Opts::new(
            "network_inbound_low_priority_message_queue_size",
            "Current number of inbound low priority messages in queue",
        ))?;
        registry.register(Box::new(inbound_low_priority_message_queue_size.clone()))?;

        let outbound_high_priority_message_queue_size = IntGauge::with_opts(Opts::new(
            "network_outbound_high_priority_message_queue_size",
            "Current number of outbound high priority messages in queue",
        ))?;
        registry.register(Box::new(outbound_high_priority_message_queue_size.clone()))?;

        let outbound_low_priority_message_queue_size = IntGauge::with_opts(Opts::new(
            "network_outbound_low_priority_message_queue_size",
            "Current number of outbound low priority messages in queue",
        ))?;
        registry.register(Box::new(outbound_low_priority_message_queue_size.clone()))?;

        let received_bytes = IntCounter::with_opts(Opts::new(
            "network_received_bytes",
            "Total number of bytes received",
        ))?;
        registry.register(Box::new(received_bytes.clone()))?;

        let sent_bytes =
            IntCounter::with_opts(Opts::new("network_sent_bytes", "Total number of bytes sent"))?;
        registry.register(Box::new(sent_bytes.clone()))?;

        let last_finalized_block_height = GenericGauge::with_opts(Opts::new(
            "consensus_last_finalized_block_height",
            "The block height of the last finalized block",
        ))?;
        registry.register(Box::new(last_finalized_block_height.clone()))?;

        let last_finalized_block_timestamp = IntGauge::with_opts(Opts::new(
            "consensus_last_finalized_block_timestamp",
            "Timestamp for processing the last finalized block (Unix time in milliseconds)",
        ))?;
        registry.register(Box::new(last_finalized_block_timestamp.clone()))?;

        let last_arrived_block_height = GenericGauge::with_opts(Opts::new(
            "consensus_last_arrived_block_height",
            "The block height of the last arrived block",
        ))?;
        registry.register(Box::new(last_arrived_block_height.clone()))?;

        let last_arrived_block_timestamp = IntGauge::with_opts(Opts::new(
            "consensus_last_arrived_block_timestamp",
            "Timestamp for processing the last arrived block (Unix time in milliseconds)",
        ))?;
        registry.register(Box::new(last_arrived_block_timestamp.clone()))?;

        let received_consensus_messages = IntCounterVec::new(
            Opts::new(
                "consensus_received_messages_total",
                "Total number of received messages labelled by the type of messages and the \
                 resulting outcome",
            )
            .variable_label("message")
            .variable_label("result"),
            &["message", "result"],
        )?;
        registry.register(Box::new(received_consensus_messages.clone()))?;

        let sent_consensus_messages = IntCounterVec::new(
            Opts::new(
                "consensus_sent_messages_total",
                "Total number of sent messages labelled by the type of messages",
            )
            .variable_label("message"),
            &["message"],
        )?;
        registry.register(Box::new(sent_consensus_messages.clone()))?;

        let soft_banned_peers = IntGauge::with_opts(Opts::new(
            "network_soft_banned_peers",
            "Current number of soft banned peers",
        ))?;
        registry.register(Box::new(soft_banned_peers.clone()))?;

        let total_peers = IntCounter::with_opts(Opts::new(
            "network_peers_total",
            "Total number of peers since startup",
        ))?;
        registry.register(Box::new(total_peers.clone()))?;

        let node_info = IntGauge::with_opts(
            Opts::new(
                "node_info",
                "Node software information. Provides the node version using a label \
                 (`version=<version>`). Always has the value 1",
            )
            .const_labels(prometheus::labels! {
                "version".to_owned() => crate::VERSION.to_owned()
            }),
        )?;
        registry.register(Box::new(node_info.clone()))?;
        node_info.set(1);

        let node_startup_timestamp = IntGauge::with_opts(Opts::new(
            "node_startup_timestamp",
            "Timestamp of starting up the node (Unix time in milliseconds).",
        ))?;
        registry.register(Box::new(node_startup_timestamp.clone()))?;

        let grpc_request_response_time = HistogramVec::new(
            HistogramOpts::new(
                "grpc_request_response_time_seconds",
                "Response time of gRPC requests in seconds",
            )
            .variable_label("method")
            .variable_label("status")
            .buckets(grpc_duration_buckets),
            &["method", "status"],
        )?;
        registry.register(Box::new(grpc_request_response_time.clone()))?;

        let grpc_pending_requests_gauge = IntGauge::with_opts(Opts::new(
            "grpc_pending_requests",
            "Current number of gRPC requests being handled by the node",
        ))?;
        let grpc_pending_requests_counter = InFlightRequestsCounter::new();
        let grpc_pending_requests = GrpcInFlightRequestsCollector {
            gauge:   grpc_pending_requests_gauge,
            counter: grpc_pending_requests_counter.clone(),
        };
        registry.register(Box::new(grpc_pending_requests))?;

        let last_throughput_measurement_timestamp = AtomicI64::new(0);
        let last_throughput_measurement_sent_bytes = AtomicU64::new(0);
        let last_throughput_measurement_received_bytes = AtomicU64::new(0);
        let avg_bps_in = AtomicU64::new(0);
        let avg_bps_out = AtomicU64::new(0);

        Ok(StatsExportService {
            registry,
            packets_received,
            packets_sent,
            connected_peers,
            connections_received,
            inbound_high_priority_message_queue_size,
            inbound_low_priority_message_queue_size,
            outbound_high_priority_message_queue_size,
            outbound_low_priority_message_queue_size,
            received_bytes,
            sent_bytes,
            last_finalized_block_height,
            last_finalized_block_timestamp,
            last_arrived_block_height,
            last_arrived_block_timestamp,
            received_consensus_messages,
            sent_consensus_messages,
            soft_banned_peers,
            total_peers,
            node_info,
            node_startup_timestamp,
            grpc_request_response_time,
            grpc_pending_requests_counter,
            last_throughput_measurement_timestamp,
            last_throughput_measurement_sent_bytes,
            last_throughput_measurement_received_bytes,
            avg_bps_in,
            avg_bps_out,
        })
    }

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
    pub async fn start_server(
        &self,
        listen_addr: SocketAddr,
        error_sender: tokio::sync::broadcast::Sender<()>,
    ) -> Result<(), ()> {
        log::info!("Starting Prometheus exporter listening on {}", listen_addr);
        let result = gotham::plain::init_server(listen_addr, self.router()).await;
        if let Err(()) = result {
            // Log an error and notify main thread that an error occured.
            error!("A runtime error occurred in the Prometheus exporter.");
            if error_sender.send(()).is_err() {
                error!("An error occurred while trying to signal the main node thread.")
            }
        }
        result
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
    conf: &configuration::PrometheusConfig,
) -> anyhow::Result<Arc<StatsExportService>> {
    let prom =
        StatsExportService::new(conf.prometheus_metric_grpc_response_time_buckets.to_owned())
            .context("Could not start statistics collection engine.")?;
    Ok(Arc::new(prom))
}

/// Starts the push gateway to Prometheus if the configuration specifies it.
pub fn start_push_gateway(
    conf: &configuration::PrometheusConfig,
    service: &StatsExportService,
    id: P2PNodeId,
) {
    if let Some(prom_push_addy) = conf.prometheus_push_gateway.as_ref() {
        info!("Starting Prometheus push to gateway at {}", prom_push_addy);
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
