//! Node's statistics and their exposure.

use crate::{
    common::p2p_node_id::P2PNodeId, consensus_ffi::ffi::NotificationHandlers, read_or_die,
    spawn_or_die,
};
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
    /// Total inbound high priority consensus messages dropped due to a full
    /// queue.
    pub inbound_high_priority_message_drops: IntCounter,
    /// Total inbound low priority consensus messages dropped due to a full
    /// queue.
    pub inbound_low_priority_message_drops: IntCounter,
    /// Total inbound high priority consensus messages received.
    pub inbound_high_priority_messages: IntCounter,
    /// Total inbound low priority consensus messages received.
    pub inbound_low_priority_messages: IntCounter,
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
    pub fn new() -> anyhow::Result<Self> {
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

        let inbound_high_priority_message_drops = IntCounter::with_opts(Opts::new(
            "network_inbound_high_priority_message_drops_total",
            "Total inbound high priority consensus messages dropped due to a full queue",
        ))?;
        registry.register(Box::new(inbound_high_priority_message_drops.clone()))?;

        let inbound_low_priority_message_drops = IntCounter::with_opts(Opts::new(
            "network_inbound_low_priority_message_drops_total",
            "Total inbound low priority consensus messages dropped due to a full queue",
        ))?;
        registry.register(Box::new(inbound_low_priority_message_drops.clone()))?;

        let inbound_high_priority_messages = IntCounter::with_opts(Opts::new(
            "network_inbound_high_priority_messages_total",
            "Total inbound high priority consensus messages received",
        ))?;
        registry.register(Box::new(inbound_high_priority_messages.clone()))?;

        let inbound_low_priority_messages = IntCounter::with_opts(Opts::new(
            "network_inbound_low_priority_messages_total",
            "Total inbound low priority consensus messages received",
        ))?;
        registry.register(Box::new(inbound_low_priority_messages.clone()))?;

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
            "Timestamp for receiving the last finalized block",
        ))?;
        registry.register(Box::new(last_finalized_block_timestamp.clone()))?;

        let last_arrived_block_timestamp = IntGauge::with_opts(Opts::new(
            "consensus_last_arrived_block_timestamp",
            "Timestamp for receiving the last arrived block",
        ))?;
        registry.register(Box::new(last_arrived_block_timestamp.clone()))?;

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
            inbound_high_priority_message_drops,
            inbound_low_priority_message_drops,
            inbound_high_priority_messages,
            inbound_low_priority_messages,
            inbound_high_priority_message_queue_size,
            inbound_low_priority_message_queue_size,
            outbound_high_priority_message_queue_size,
            outbound_low_priority_message_queue_size,
            received_bytes,
            sent_bytes,
            last_finalized_block_height,
            last_finalized_block_timestamp,
            last_arrived_block_timestamp,
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

    /// Spawn tasks for receiving notifications to update relevant stats.
    pub fn start_handling_notification(&self, notification_handlers: NotificationHandlers) {
        use prost::Message;

        let NotificationHandlers {
            mut finalized_blocks,
            mut blocks,
        } = notification_handlers;

        let last_finalized_block_height = self.last_finalized_block_height.clone();
        let last_finalized_block_timestamp = self.last_finalized_block_timestamp.clone();
        tokio::spawn(async move {
            while let Ok(v) = finalized_blocks.recv().await {
                let timestamp = chrono::Utc::now().timestamp_millis();
                match crate::grpc2::types::FinalizedBlockInfo::decode(v.as_ref()) {
                    Ok(finalized_block_info) => {
                        let block_height = finalized_block_info.height.unwrap().value;
                        last_finalized_block_height.set(block_height);
                        last_finalized_block_timestamp.set(timestamp);
                    }
                    Err(err) => error!("Failed to decode finalized block info: {}", err),
                }
            }
        });

        let last_arrived_block_timestamp = self.last_arrived_block_timestamp.clone();
        tokio::spawn(async move {
            while let Ok(_) = blocks.recv().await {
                let timestamp = chrono::Utc::now().timestamp_millis();
                last_arrived_block_timestamp.set(timestamp);
            }
        });
    }
}

/// Starts the stats export engine.
pub fn instantiate_stats_export_engine() -> anyhow::Result<Arc<StatsExportService>> {
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
