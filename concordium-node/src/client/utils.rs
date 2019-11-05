use crate::configuration;
use concordium_common::stats_export_service::{StatsExportService, StatsServiceMode};
use failure::Fallible;

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
) -> Fallible<Option<StatsExportService>> {
    let prom = if conf.prometheus.prometheus_server {
        info!("Enabling prometheus server");
        let srv = StatsExportService::new(mode)?;
        srv.start_server(SocketAddr::new(
            conf.prometheus.prometheus_listen_addr.parse()?,
            conf.prometheus.prometheus_listen_port,
        ));
        Some(srv)
    } else if let Some(ref push_gateway) = conf.prometheus.prometheus_push_gateway {
        info!("Enabling prometheus push gateway at {}", push_gateway);
        let srv = StatsExportService::new(mode)?;
        Some(srv)
    } else {
        None
    };
    Ok(prom)
}

#[cfg(not(feature = "instrumentation"))]
pub fn instantiate_stats_export_engine(
    _: &configuration::Config,
    mode: StatsServiceMode,
) -> Fallible<Option<StatsExportService>> {
    Ok(Some(StatsExportService::new(mode)?))
}

#[cfg(feature = "instrumentation")]
pub fn start_push_gateway(
    conf: &configuration::PrometheusConfig,
    stats_export_service: &Option<StatsExportService>,
    id: P2PNodeId,
) {
    stats_export_service.as_ref().and_then(|service| {
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
            })
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
