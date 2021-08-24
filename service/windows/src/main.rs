#[macro_use]
extern crate windows_service;

mod config;
mod manager;
mod node;
mod subprocess;

use crate::config::{get_config_file_path, load_config};
use crate::subprocess::create_console;
use anyhow::{bail, Context};
use log::*;
use retain_mut::RetainMut;
use std::ffi::OsString;
use std::string::String;
use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc,
};
use std::time::{Duration, Instant};
use std::{env, thread};
use winapi::shared::winerror::ERROR_FAILED_SERVICE_CONTROLLER_CONNECT;
use windows_service::{service::*, service_control_handler::*, service_dispatcher};

const SERVICE_NAME: &str = "ConcordiumNode";
const EVENT_LOG_NAME: &str = "Concordium Node Runner Service";

// Produce an FFI wrapper for the service main function.
define_windows_service!(ffi_service_main, runner_service_main);

/// Service main entrypoint. This is invoked by the service control dispatcher when the service
/// is started.
///
/// This delegates to run_service, and panics, logging an error, if an unrecoverable error occurs.
fn runner_service_main(arguments: Vec<OsString>) {
    winlog::init(EVENT_LOG_NAME).unwrap();

    if let Err(e) = run_service(arguments) {
        error!(
            "The node runner service failed for the following reason:\n{:#}",
            e
        );
        panic!()
    }
}

/// Macro for constructing a simple status message given the new state and enabled controls (if
/// any).

/// Construct a simple status message with no enabled controls.
fn simple_status(state: ServiceState) -> ServiceStatus {
    simple_status_with_controls(state, ServiceControlAccept::empty())
}

/// Construct a simple status message with the specified controls.
fn simple_status_with_controls(
    current_state: ServiceState,
    controls_accepted: ServiceControlAccept,
) -> ServiceStatus {
    ServiceStatus {
        service_type: ServiceType::OWN_PROCESS,
        current_state,
        controls_accepted,
        exit_code: ServiceExitCode::Win32(0),
        checkpoint: 0,
        wait_hint: Duration::from_secs(0),
        process_id: None,
    }
}

/// Constructing a pending status message given the new state, checkpoint, and wait hint.
fn pending_status(
    current_state: ServiceState,
    checkpoint: u32,
    wait_hint: Duration,
) -> ServiceStatus {
    ServiceStatus {
        service_type: ServiceType::OWN_PROCESS,
        current_state,
        controls_accepted: ServiceControlAccept::empty(),
        exit_code: ServiceExitCode::Win32(0),
        checkpoint,
        wait_hint,
        process_id: None,
    }
}

/// Service runner.
///
/// This function first registers the service event handler. The handler only responds to Stop
/// and Preshutdown events, for which it sets a shutdown flag and unparks the runner thread.
///
/// It then enters the StartPending state, reads the configuration file, and attempts to start
/// the nodes.
///
/// Next, it enters the Running state, and
fn run_service(_arguments: Vec<OsString>) -> anyhow::Result<()> {
    // We create a console in order to be able to sent CTRL+BREAK messages to subprocesses.
    // A service is created detatched (i.e. without a console). Creating a console before
    // starting the child processes will allow the children to share the console, and therefore
    // allow the service to send the appropriate messages.
    create_console()?;

    // Boolean flag for the event handler to signal that the service is being shut down.
    let is_shutdown = Arc::new(AtomicBool::new(false));
    let handler_is_shutdown = is_shutdown.clone();
    let service_thread = thread::current();

    // The event handler callback
    let event_handler = move |control_event| -> ServiceControlHandlerResult {
        match control_event {
            ServiceControl::Stop | ServiceControl::Preshutdown => {
                // Ideally, we should change the status to StopPending here. However, letting
                // the main thread handle the state change is currently much more convenient and
                // does not appear to cause any problems.
                handler_is_shutdown.store(true, Ordering::Release);
                service_thread.unpark();
                ServiceControlHandlerResult::NoError
            }
            ServiceControl::Interrogate => ServiceControlHandlerResult::NoError,
            _ => ServiceControlHandlerResult::NotImplemented,
        }
    };

    // Register the event handler callback.
    let status_handle = register(SERVICE_NAME, event_handler)?;

    // Set the status to StartPending while we load the configuration and start the nodes.
    status_handle.set_service_status(pending_status(
        ServiceState::StartPending,
        0,
        Duration::from_secs(1),
    ))?;

    let config = load_config()?;

    // Start the nodes
    let mut nodes = Vec::new();
    for node_conf in config.nodes {
        let node_name = node_conf.name.clone();
        match node_conf.start() {
            Ok(node) => {
                info!("Started node '{}'", &node.node_config.name);
                nodes.push(node);
            }
            Err(e) => error!("Could not start node '{}': {:#}", node_name, e),
        }
    }

    // Tell the system that the service is running now
    status_handle.set_service_status(simple_status_with_controls(
        ServiceState::Running,
        ServiceControlAccept::STOP | ServiceControlAccept::PRESHUTDOWN,
    ))?;

    // Loop until shutdown is triggered
    while !is_shutdown.load(Ordering::Acquire) {
        // Check for any stopped nodes. If a node is stopped, the fact is logged, but we do not
        // attempt to restart it.
        nodes.retain_mut(|node| match node.check_exit() {
            Ok(None) => true,
            Ok(Some(e)) => {
                let err_output = node.get_error_output().unwrap_or_default();
                error!(
                    "The node '{}' has stopped unexpectedly. ({})\n\nOutput:\n{}",
                    node.node_config.name, e, err_output
                );
                false
            }
            Err(e) => {
                error!(
                    "The state of node '{}' cannot be determined:\n{}",
                    node.node_config.name, e
                );
                false
            }
        });
        if nodes.is_empty() {
            warn!("All nodes have stopped. The node runner service will now shut down.");
            break;
        }
        // Wait for a second. This can be woken early by the event handler.
        thread::park_timeout(std::time::Duration::from_secs(1));
    }

    // Shut down the nodes.
    // Nodes are first sent a CTRL+BREAK signal to trigger shutdown gracefully, and are allowed 61
    // seconds to do so.  Any node that has not shut down in that time will be terminated.
    // The time duration is chosen to be shorter than the timeout for the anticipated shutdown
    // scenarios (180 seconds for preshutdown, and 125 seconds for shutdown from the Services
    // control panel).  It was chosen to be 61 seconds as that is a second longer than twice the
    // default housekeeping interval.
    let expected_shutdown_duration = Duration::from_secs(61);
    let mut checkpoint = 0;
    status_handle.set_service_status(pending_status(
        ServiceState::StopPending,
        checkpoint,
        expected_shutdown_duration,
    ))?;

    let shutdown_begin = std::time::Instant::now();
    // Signal all nodes to shutdown.
    for node in &mut nodes {
        node.shutdown()
            .with_context(|| format!("Failed shutting down node '{}'", node.node_config.name))?;
    }
    let mut t = Instant::now();
    // Loop, checking if all nodes have shutdown, until we run out of time.
    while t < shutdown_begin + expected_shutdown_duration {
        checkpoint += 1;
        status_handle.set_service_status(pending_status(
            ServiceState::StopPending,
            checkpoint,
            expected_shutdown_duration - (t - shutdown_begin),
        ))?;
        nodes.retain_mut(|node| match node.check_exit() {
            Ok(None) => true,
            Ok(Some(e)) => {
                info!(
                    "The node '{}' has been shut down successfully. ({})",
                    node.node_config.name, e
                );
                false
            }
            Err(e) => {
                error!(
                    "The state of node '{}' cannot be determined:\n{:#}",
                    node.node_config.name, e
                );
                false
            }
        });
        if nodes.is_empty() {
            break;
        }
        thread::park_timeout(Duration::from_millis(200));
        t = Instant::now();
    }

    for node in &mut nodes {
        error!(
            "Node '{}' did not shutdown gracefully; it will be killed.",
            node.node_config.name
        );
        node.force_shutdown().unwrap_or_else(|e| {
            error!(
                "Error while killing node '{}': {:#}",
                node.node_config.name, e
            )
        });
    }

    status_handle.set_service_status(simple_status(ServiceState::Stopped))?;

    Ok(())
}

/// Main entrypoint. This tries to start the service dispatcher.
/// If the program was not started as a service, this will fail, in which case we delegate
/// to nonservice_main.
fn main() -> anyhow::Result<()> {
    if let Err(e) = service_dispatcher::start(SERVICE_NAME, ffi_service_main) {
        match e {
            windows_service::Error::Winapi(os_err)
                if os_err.raw_os_error()
                    == Some(ERROR_FAILED_SERVICE_CONTROLLER_CONNECT as i32) =>
            {
                nonservice_main()?
            }
            _ => bail!(e),
        }
    }
    Ok(())
}

/// Parse the command line argument as one of the commands "install", "remove", "start" or "stop",
/// and perform the associated action for the service.
fn nonservice_main() -> anyhow::Result<()> {
    let arg = env::args()
        .nth(1)
        .map(|s| s.to_lowercase())
        .unwrap_or_else(|| String::from(""));
    match &arg as &str {
        "install" => {
            println!("Installing service...");
            manager::install()?;
            winlog::register(EVENT_LOG_NAME);
            println!("Done");
        }
        "remove" => {
            println!("Removing service...");
            manager::remove()?;
            winlog::deregister(EVENT_LOG_NAME);
            println!("Done");
        }
        "start" => {
            println!("Starting service...");
            manager::start()?;
            println!("Done");
        }
        "stop" => {
            println!("Stopping service...");
            manager::stop()?;
            println!("Done");
        }
        "configure" => {
            let config_path = get_config_file_path()?;
            std::process::Command::new("notepad")
                .arg(config_path)
                .spawn()?;
        }
        _ => {
            println!("Try running this program with one of the following arguments:");
            println!(" install - install the service");
            println!(" remove - remove the service");
            println!(" start - start the service");
            println!(" stop - stop the service");
            println!(" configure - open the configuration file in notepad");
        }
    }
    Ok(())
}
