/// Obtain a write lock or panic.
///
/// Equivalent to `safe_write` but panicking on error
#[macro_export]
macro_rules! write_or_die {
    ($e:expr) => {
        $e.write().expect("write_or_die failed!")
    };
}

/// Obtain a read lock or panic.
///
/// Equivalent to `safe_read` but panicking on error
#[macro_export]
macro_rules! read_or_die {
    ($e:expr) => {
        $e.read().expect("read_or_die failed!")
    };
}

/// Obtain a `Mutex` lock or panic.
///
/// Equivalent to `safe_lock` but panicking on error
#[macro_export]
macro_rules! lock_or_die {
    ($e:expr) => {
        $e.lock().expect("lock_or_die failed!")
    };
}

/// Spawns a new thread.
#[macro_export]
macro_rules! spawn_or_die {
    ($name:expr, $func:block) => {
        spawn_or_die!($name, move || {
            $func
            debug!("Stopping the {} thread", std::thread::current().name().unwrap());
        })
    };
    ($name:expr, $func:expr) => {
        std::thread::Builder::new()
            .name($name.to_owned())
            .spawn($func)
            .expect("The OS refused to create a new thread")
    }
}
