#![recursion_limit = "1024"]

extern crate tempfile;

/// # Serialization packets
/// Benchmark of each serialization requires to enable it on features
#[cfg(feature = "s11n_serde")]
extern crate serde;

#[macro_use]
pub mod fails;

pub mod container_view;
pub mod functor;
pub mod ucursor;

pub use self::{container_view::ContainerView, ucursor::UCursor};
