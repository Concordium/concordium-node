use std::ffi::OsString;
use windows_service::{
    service::*,
    service_manager::{ServiceManager, ServiceManagerAccess},
};

/// Friendly name for the service
pub const SERVICE_DISPLAY_NAME: &str = "Concordium Node Runner Service";
/// Description of the service
pub const SERVICE_DESCRIPTION: &str = "Runs one or more Concordium blockchain nodes.";

/// Install the service
pub fn install() -> anyhow::Result<()> {
    let exe_path = std::env::current_exe()?;
    let manager =
        ServiceManager::local_computer(None::<&str>, ServiceManagerAccess::CREATE_SERVICE)?;
    let service_info = ServiceInfo {
        name:             OsString::from(crate::SERVICE_NAME),
        display_name:     OsString::from(SERVICE_DISPLAY_NAME),
        service_type:     ServiceType::OWN_PROCESS,
        start_type:       ServiceStartType::OnDemand,
        error_control:    ServiceErrorControl::Normal,
        executable_path:  exe_path,
        launch_arguments: vec![],
        dependencies:     vec![],
        account_name:     None,
        account_password: None,
    };
    let service = manager.create_service(
        &service_info,
        ServiceAccess::QUERY_STATUS | ServiceAccess::CHANGE_CONFIG,
    )?;
    service.set_description(OsString::from(SERVICE_DESCRIPTION))?;
    Ok(())
}

/// Remove the service
pub fn remove() -> anyhow::Result<()> {
    let manager =
        ServiceManager::local_computer(None::<&str>, ServiceManagerAccess::ENUMERATE_SERVICE)?;
    let service = manager.open_service(crate::SERVICE_NAME, ServiceAccess::DELETE)?;
    service.delete()?;
    Ok(())
}

/// Start the service
pub fn start() -> anyhow::Result<()> {
    let manager =
        ServiceManager::local_computer(None::<&str>, ServiceManagerAccess::ENUMERATE_SERVICE)?;
    let service = manager.open_service(crate::SERVICE_NAME, ServiceAccess::START)?;
    service.start(&[] as &[OsString])?;
    Ok(())
}

/// Stop the service
pub fn stop() -> anyhow::Result<()> {
    let manager =
        ServiceManager::local_computer(None::<&str>, ServiceManagerAccess::ENUMERATE_SERVICE)?;
    let service = manager.open_service(crate::SERVICE_NAME, ServiceAccess::STOP)?;
    service.stop()?;
    Ok(())
}
