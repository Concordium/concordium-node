#[cfg(feature = "s11n_fbs")]
pub mod fbs;

#[cfg(all(
    feature = "s11n_serde",
    not(feature = "s11n_fbs"),
    not(feature = "s11n_capnp")
))]
pub mod serde;

#[cfg(all(
    feature = "s11n_capnp",
    not(feature = "s11n_fbs"),
    not(feature = "s11n_serde")
))]
pub mod cap;

#[cfg(not(feature = "s11n_capnp"))]
#[cfg(test)]
mod tests;
