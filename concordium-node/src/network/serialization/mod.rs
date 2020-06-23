//! Network object serialization.

#[cfg(all(not(feature = "s11n_capnp"), not(feature = "s11n_serde")))]
pub mod fbs;

#[cfg(feature = "s11n_serde")]
pub mod serde;

#[cfg(feature = "s11n_capnp")]
pub mod cap;

#[cfg(not(feature = "s11n_capnp"))]
#[cfg(test)]
mod tests;

#[macro_export]
macro_rules! only_fbs {
    ($t:expr) => {
        #[cfg(any(feature = "s11n_serde", feature = "s11n_capnp"))]
        unimplemented!("only fbs can be used in the actual program");
        #[cfg(all(not(feature = "s11n_serde"), not(feature = "s11n_capnp")))]
        $t;
    };
}
