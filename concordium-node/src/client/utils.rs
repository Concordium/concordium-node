use crate::configuration;
use concordium_common::stats_export_service::{StatsExportService, StatsServiceMode};
use failure::Fallible;
use std::sync::{Arc, RwLock};

cfg_if! {
    if #[cfg(feature = "instrumentation")] {
        use crate::common::P2PNodeId;
        use std::net::SocketAddr;
    }
}

#[cfg(feature = "instrumentation")]
pub fn instantiate_stats_export_engine(
    conf: &configuration::Config,
    mode: StatsServiceMode,
) -> Fallible<Option<Arc<RwLock<StatsExportService>>>> {
    let prom = if conf.prometheus.prometheus_server {
        info!("Enabling prometheus server");
        let mut srv = StatsExportService::new(mode)?;
        srv.start_server(SocketAddr::new(
            conf.prometheus.prometheus_listen_addr.parse()?,
            conf.prometheus.prometheus_listen_port,
        ));
        Some(Arc::new(RwLock::new(srv)))
    } else if let Some(ref push_gateway) = conf.prometheus.prometheus_push_gateway {
        info!("Enabling prometheus push gateway at {}", push_gateway);
        let srv = StatsExportService::new(mode)?;
        Some(Arc::new(RwLock::new(srv)))
    } else {
        None
    };
    Ok(prom)
}

#[cfg(not(feature = "instrumentation"))]
pub fn instantiate_stats_export_engine(
    _: &configuration::Config,
    mode: StatsServiceMode,
) -> Fallible<Option<Arc<RwLock<StatsExportService>>>> {
    Ok(Some(Arc::new(RwLock::new(StatsExportService::new(mode)?))))
}

#[cfg(feature = "instrumentation")]
pub fn start_push_gateway(
    conf: &configuration::PrometheusConfig,
    stats_export_service: &Option<Arc<RwLock<StatsExportService>>>,
    id: P2PNodeId,
) -> Fallible<()> {
    if let Some(ref service) = stats_export_service {
        if let Some(ref prom_push_addy) = conf.prometheus_push_gateway {
            let instance_name = if let Some(ref instance_id) = conf.prometheus_instance_name {
                instance_id.clone()
            } else {
                id.to_string()
            };
            safe_read!(service)?.start_push_to_gateway(
                prom_push_addy.clone(),
                conf.prometheus_push_interval,
                conf.prometheus_job_name.clone(),
                instance_name,
                conf.prometheus_push_username.clone(),
                conf.prometheus_push_password.clone(),
            )
        }
    }
    Ok(())
}

#[cfg(feature = "instrumentation")]
pub fn stop_stats_export_engine(
    conf: &configuration::Config,
    srv: &Option<Arc<RwLock<StatsExportService>>>,
) {
    if conf.prometheus.prometheus_server {
        if let Some(srv) = srv {
            info!("Stopping prometheus server");
            if let Ok(mut locked) = srv.write() {
                locked.stop_server();
            }
        }
    }
}

#[cfg(not(feature = "instrumentation"))]
pub fn stop_stats_export_engine(
    _: &configuration::Config,
    _: &Option<Arc<RwLock<StatsExportService>>>,
) {
}
