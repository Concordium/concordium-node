#[cfg(not(feature = "s11n_serde"))]
pub mod fbs;

#[cfg(feature = "s11n_serde")]
pub mod serde;

#[cfg(feature = "s11n_capnp")]
pub mod cap;

#[cfg(test)]
mod tests;
