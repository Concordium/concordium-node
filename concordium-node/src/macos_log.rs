use dashmap::DashMap;
use std::{
    ffi::{c_void, CString},
    os::raw::c_char,
};

/// The native representation of a logger. Used by [LogT].
#[repr(C)]
#[derive(Debug, Copy, Clone)]
struct MacOsLogS {
    _unused: [u8; 0],
}

/// Matches the native type `os_log_t`.
type LogT = *mut MacOsLogS;
/// Matches the native type `os_log_type_t`.
type LogLevelT = u8;

/// Log severity levels with `u8` values for macOS.
pub enum Level {
    _Default = 0,
    Info     = 1,
    Debug    = 2,
    Error    = 16,
    Fault    = 17,
}

/// Map log levels into appropriate macOS levels.
/// Note that we coalesce `Trace` and `Debug` into `Debug`, since the log viewer
/// on mac Console.app, only has The macOS log viewer, Console.app, shows Error
/// and Fault by default. Info and Debug has to be enabled manually.
impl From<log::Level> for Level {
    fn from(other: log::Level) -> Self {
        match other {
            log::Level::Trace => Self::Debug,
            log::Level::Debug => Self::Debug,
            log::Level::Info => Self::Info,
            log::Level::Warn => Self::Error,
            log::Level::Error => Self::Fault,
        }
    }
}

/// Provided by macOS.
extern "C" {
    fn os_log_create(subsystem: *const c_char, category: *const c_char) -> LogT;
    fn os_release(object: *mut c_void);
}

/// Wrapper for the macOS macro `os_log_with_type`. Defined in the file
/// macos_log_wrapper.c.
extern "C" {
    fn wrapped_os_log_with_type(log: LogT, log_level: LogLevelT, message: *const c_char);
}

/// Safely creates a new CString by replacing any `\0` chars with `(null)`.
#[inline]
fn to_cstr(message: &str) -> CString {
    let fixed = message.replace('\0', "(null)");
    CString::new(fixed).unwrap()
}

/// Logger that logs to the macOS syslog.
struct MacOsLog {
    inner: LogT,
}

unsafe impl Send for MacOsLog {}
unsafe impl Sync for MacOsLog {}

impl Drop for MacOsLog {
    fn drop(&mut self) {
        unsafe {
            os_release(self.inner as *mut c_void);
        }
    }
}

impl MacOsLog {
    /// Instantiate new logger with the macOS system call `os_log_create`.
    /// Asserts that the logger pointer returned is valid.
    fn new(subsystem: &str, category: &str) -> Self {
        let subsystem = to_cstr(subsystem);
        let category = to_cstr(category);

        let inner = unsafe { os_log_create(subsystem.as_ptr(), category.as_ptr()) };

        // According to the macOS documentation, `os_log_create` will always return a
        // valid logger.
        assert!(!inner.is_null(), "Unexpected null value from os_log_create");

        MacOsLog {
            inner,
        }
    }

    /// Send a log message to the macOS syslog with the provided [Level].
    fn log_with_level(&self, level: Level, msg: &str) {
        let msg = to_cstr(msg);
        unsafe { wrapped_os_log_with_type(self.inner, level as LogLevelT, msg.as_ptr()) }
    }
}

/// Logger that logs to the macOS syslog and implements [log::Log].
pub struct MacOsLogger {
    loggers:   DashMap<String, (Option<log::LevelFilter>, MacOsLog)>,
    subsystem: String,
}

impl MacOsLogger {
    /// Create a new logger with a given subsystem. The subsystem is an
    /// identifier string in reverse DNS notation, e.g. "com.example.
    /// Default and category/target filters can be set with [level_filter] and
    /// [category_level_filter], respectively. Once configured, **[init]
    /// must be called**, otherwise logging won't occur.
    pub fn new(subsystem: &str) -> Self {
        Self {
            loggers:   DashMap::new(),
            subsystem: subsystem.to_string(),
        }
    }

    /// Set a default level filter for the logger. If not set, `Trace` is used.
    /// If set, it can be overridden on individual categories/targets with
    /// [category_level_filter].
    pub fn level_filter(self, level: log::LevelFilter) -> Self {
        log::set_max_level(level);
        self
    }

    /// Set a level filter on a given category/target. Overrides the default
    /// level filter set by [level_filter].
    pub fn category_level_filter(self, category: &str, level: log::LevelFilter) -> Self {
        self.loggers
            .entry(category.into())
            .and_modify(|(existing_level, _)| *existing_level = Some(level))
            .or_insert((Some(level), MacOsLog::new(&self.subsystem, category)));
        self
    }

    /// Initialises the logger with `log::set_boxed_logger`.
    /// **Must be called**, otherwise logging is not enabled.
    pub fn init(self) -> Result<(), log::SetLoggerError> { log::set_boxed_logger(Box::new(self)) }
}

impl log::Log for MacOsLogger {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        let max_level =
            self.loggers.get(metadata.target()).and_then(|pair| pair.0).unwrap_or(log::max_level());
        metadata.level() <= max_level
    }

    fn log(&self, record: &log::Record) {
        if self.enabled(record.metadata()) {
            let pair = self
                .loggers
                .entry(record.target().to_string())
                .or_insert((None, MacOsLog::new(&self.subsystem, record.target())));
            let message = record.args().to_string();
            pair.1.log_with_level(record.level().into(), &message);
        }
    }

    fn flush(&self) {}
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn logs_correctly_to_syslog() {
        // To view the logs:
        //  1. Open the `Console` app (on macOS)
        //  2. Click 'Start' to start capturing logs
        //  3. Search for com.macoslog.test to filter the logs

        MacOsLogger::new("com.macoslog.test")
            .level_filter(log::LevelFilter::Trace)
            .category_level_filter("OnlyErrors", log::LevelFilter::Error)
            .category_level_filter("InfoWarnError", log::LevelFilter::Info)
            .category_level_filter("CompletelyOff", log::LevelFilter::Off)
            .init()
            .expect("Could not initialise MacOsLogger");

        let t = "testing";

        // Should be shown
        trace!("trace! {}", t);
        debug!("debug! {}", t);
        info!("info! {}", t);
        warn!("warn! {}", t);
        error!("error! {}", t);

        // Should not be shown due to category filter
        trace!(target: "OnlyErrors", "OnlyErrors: trace! {}", t);
        debug!(target: "OnlyErrors", "OnlyErrors: debug! {}", t);
        info!(target: "OnlyErrors", "OnlyErrors: info! {}", t);
        warn!(target: "OnlyErrors", "OnlyErrors: warn! {}", t);

        // Should be shown
        error!(target: "OnlyErrors", "OnlyErrors: error! {}", t);

        // Should not be shown due to category filter
        trace!(target: "InfoWarnError", "InfoWarnError: trace! {}", t);
        debug!(target: "InfoWarnError", "InfoWarnError: debug! {}", t);

        // Should be shown
        info!(target: "InfoWarnError", "InfoWarnError: info! {}", t);
        warn!(target: "InfoWarnError", "InfoWarnError: warn! {}", t);
        error!(target: "InfoWarnError", "InfoWarnError: error! {}", t);

        // Should not be shown due to category filter
        trace!(target: "CompletelyOff", "CompletelyOff: trace! {}", t);
        debug!(target: "CompletelyOff", "CompletelyOff: debug! {}", t);
        info!(target: "CompletelyOff", "CompletelyOff: info! {}", t);
        warn!(target: "CompletelyOff", "CompletelyOff: warn! {}", t);
        error!(target: "CompletelyOff", "CompletelyOff: error! {}", t);
    }
}
