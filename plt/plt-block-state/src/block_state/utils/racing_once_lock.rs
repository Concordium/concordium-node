use std::cell::UnsafeCell;
use std::fmt::Debug;
use std::mem::MaybeUninit;
use std::sync::atomic::{AtomicU8, Ordering};
use std::{fmt, hint};

/// State of the value in the lock.
#[repr(u8)]
enum State {
    /// The value is not initialized.
    Uninitialized = 0,
    /// A thread is currently setting the value.
    Initializing = 1,
    /// The value is initialized.
    Initialized = 2,
}

/// Lightweight, thread-safe lock whose value can be initialized only once via interior mutability.
/// Once initialized, reading the value is essentially without overhead. Memory usage
/// and writing the value also carries minimal overhead.
///
/// The lock is **racing** in the sense that it does not support initialization in a critical region,
/// Hence, multiple threads may "race" to compute the value they intend to initialize the lock with,
/// but only one thread will successfully set the value. This has the benefit that the implementation
/// can be more lightweight than a lock like `OnceLock`, which allows computing the initialization
/// value in a critical region, such that only one thread computes the value.
pub struct RacingOnceLock<T> {
    state: AtomicU8,
    value: UnsafeCell<MaybeUninit<T>>,
}

impl<T> RacingOnceLock<T> {
    /// Create new lock which is not initialized with a value yet.
    pub fn new() -> Self {
        Self {
            state: AtomicU8::from(State::Uninitialized as u8),
            value: UnsafeCell::new(MaybeUninit::uninit()),
            // _marker: PhantomData,
        }
    }

    /// Return the initialized value, or `None` if the lock has not been
    /// initialized yet.
    pub fn get(&self) -> Option<&T> {
        if self.state.load(Ordering::Acquire) == State::Initialized as u8 {
            // Data-race safety: Reading the value happens-after writing it in Self::try_insert,
            // because the acquire load of state happens-after the release store of state in
            // Self::try_insert.
            // Initialization safety: By the same argument, the value has been
            // initialized in Self::try_insert.
            // Aliasing safety: The only unique reference created is in
            // Self::try_insert when the value is set.
            unsafe { Some((&*self.value.get()).assume_init_ref()) }
        } else {
            None
        }
    }

    /// Try to initialize the lock with the given value. The result may be setting
    /// the value, or doing nothing, because the lock has already
    /// been initialized. In any case, it is guaranteed that when this function returns,
    /// the lock has been initialized.
    ///
    /// # Returns
    ///
    /// If the lock was initialized with the given value, then `Ok` with a reference
    /// to the value is returned. If the lock was already initialized `Err`
    /// with a reference to the already initialized value, and the value given to
    /// `try_insert` is returned.
    pub fn try_insert(&self, value: T) -> Result<&T, (&T, T)> {
        loop {
            match self.state.compare_exchange(
                State::Uninitialized as u8,
                State::Initializing as u8,
                Ordering::Relaxed,
                Ordering::Acquire,
            ) {
                Ok(_) => {
                    // Aliasing safety: This is the only place a unique reference is created,
                    // and only on thread will ever see the state Uninitialized and set it to
                    // Initializing.
                    // Data-race safety: As argued, there is ever only one write. Each read has an
                    // argument for its data-race safety.
                    let value_ref = unsafe { (&mut *self.value.get()).write(value) as &T };
                    self.state
                        .store(State::Initialized as u8, Ordering::Release);
                    return Ok(value_ref);
                }
                Err(state) => {
                    if state == State::Initialized as u8 {
                        // Safety: Same argument as in Self::get. Notice that compare_exchange
                        // performs an acquire load in the failure case.
                        return Err((unsafe { (&*self.value.get()).assume_init_ref() }, value));
                    } else {
                        hint::spin_loop();
                    }
                }
            }
        }
    }
}

// todo ar sync, send, unwindsafe, drop

impl<T: Debug> Debug for RacingOnceLock<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut d = f.debug_tuple("RacingOnceLock");
        match self.get() {
            Some(v) => d.field(v),
            None => d.field(&format_args!("<uninit>")),
        };
        d.finish()
    }
}
