//! Network object serialization.

#[cfg(not(feature = "s11n_serde"))]
pub mod fbs;

#[cfg(feature = "s11n_serde")]
pub mod serde;

#[cfg(test)]
mod tests;

#[macro_export]
/// We only support running the program with flatbuffers as it is the current
/// chosen mechanism but we want to retain the capability of running the
/// benchmarks with the other mechanism in order to test if they have
/// improvements. Because of that, if we compile with flatbuffers, this macro
/// will be a no-op, and in other case it will result in an unimplemented call
/// to make compilation success in order to run the benchmarks.
///
/// This macro must be used wherever we call `serialize` in the code of the main
/// program.
macro_rules! only_fbs {
    ($t:expr) => {
        #[cfg(feature = "s11n_serde")]
        unimplemented!("only fbs can be used in the actual program");
        #[cfg(not(feature = "s11n_serde"))]
        $t;
    };
}
