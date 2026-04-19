use std::cell::UnsafeCell;
use std::fmt;
use std::fmt::Debug;
use std::mem::MaybeUninit;
use std::sync::atomic::{AtomicU8, Ordering};

/// State of the value in the lock.
#[repr(u8)]
enum State {
    /// The value in
    Uninitialized = 0,
    Initializing = 1,
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
    set: AtomicU8,
    value: UnsafeCell<MaybeUninit<T>>,
}

/// Return value from [`RacingOnceLock::try_insert`].
pub enum InsertReturn<'a, T> {
    /// The value was initialized to the value given to [try_insert](RacingOnceLock::try_insert).
    /// The returned tuple is a reference to the value set in the lock.
    Initialized(&'a T),
    /// The value is being initialized by another thread. The returned tuple is
    /// the value given to [try_insert](RacingOnceLock::try_insert).
    Initializing(T),
    /// The value is already initialized in the lock. The returned tuple is
    /// a reference to the value already initialized in the lock and
    /// the value given to [try_insert](RacingOnceLock::try_insert).
    AlreadyInitialized(&'a T, T),
}

impl<T> RacingOnceLock<T> {
    /// Create new lock which is not initialized with a value yet.
    pub fn new() -> Self {
        Self {
            set: AtomicU8::from(State::Uninitialized as u8),
            value: UnsafeCell::new(MaybeUninit::uninit()),
            // _marker: PhantomData,
        }
    }

    /// Return the initialized value, or `None` if the lock has not been
    /// initialized yet.
    pub fn get(&self) -> Option<&T> {
        if self.set.load(Ordering::Acquire) == State::Initialized as u8 {
            unsafe { Some((&*self.value.get()).assume_init_ref()) }
        } else {
            None
        }
    }

    /// Try to initialize the lock with the given value. The result may be setting
    /// the value, or that the value is returned again, because the lock has already
    /// been initialized, or is being initialized by another thread.
    pub fn try_insert(&self, value: T) -> InsertReturn<'_, T> {
        match self.set.compare_exchange(
            State::Uninitialized as u8,
            State::Initializing as u8,
            Ordering::Relaxed,
            Ordering::Acquire,
        ) {
            Ok(_) => {
                let value_ref = unsafe { (&mut *self.value.get()).write(value) as &T };
                self.set.store(State::Initialized as u8, Ordering::Release);
                InsertReturn::Initialized(value_ref)
            }
            Err(state) => {
                if state == State::Initialized as u8 {
                    InsertReturn::AlreadyInitialized(
                        unsafe { (&*self.value.get()).assume_init_ref() },
                        value,
                    )
                } else {
                    InsertReturn::Initializing(value)
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
