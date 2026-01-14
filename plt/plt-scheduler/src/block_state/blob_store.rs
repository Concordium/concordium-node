// Temporarily allow dead_code until we have block state implemented.
#![allow(dead_code)]

/// Reference to a storage location where an item may be retrieved.
#[derive(Default, Debug, Clone, Copy, Eq, PartialEq)]
#[repr(transparent)]
pub struct Reference(pub u64);

/// Trait implemented by types that can be used to store binary data, and return
/// a handle for loading data. Dual to [`BackingStoreLoad`].
pub trait BackingStoreStore {
    /// Store the provided value and return a reference that can be used
    /// to load it.
    fn store_raw(&mut self, data: &[u8]) -> Result<Reference, WriteError>;
}

/// Trait implemented by types that can load data from given locations.
/// Dual to [`BackingStoreStore`].
pub trait BackingStoreLoad {
    type R: AsRef<[u8]>;
    /// Load the provided value from the given location. The implementation of
    /// this should match [BackingStoreStore::store_raw].
    fn load_raw(&mut self, location: Reference) -> LoadResult<Self::R>;
}

/// A trait implemented by types that can be loaded from a [BackingStoreLoad]
/// storage.
pub trait Loadable: Sized {
    fn load<S: std::io::Read, F: BackingStoreLoad>(
        loader: &mut F,
        source: &mut S,
    ) -> LoadResult<Self>;

    fn load_from_location<F: BackingStoreLoad>(
        loader: &mut F,
        location: Reference,
    ) -> LoadResult<Self> {
        let mut source = std::io::Cursor::new(loader.load_raw(location)?);
        Self::load(loader, &mut source)
    }
}

/// An error that may occur when writing data to persistent storage.
#[derive(Debug, thiserror::Error)]
pub enum WriteError {
    #[error("{0}")]
    IOError(#[from] std::io::Error),
}

/// Result of storing data in persistent storage.
pub type StoreResult<A> = Result<A, WriteError>;

/// An error that may occur when loading data from persistent storage.
#[derive(Debug, thiserror::Error)]
pub enum LoadError {
    #[error("{0}")]
    IO(#[from] std::io::Error),
    #[error("Error decoding: {0}")]
    Decode(String),
    // #[error("Incorrect tag")]
    // IncorrectTag {
    //     // The tag that was provided.
    //     tag: u8,
    // },
    // #[error("Out of bounds read.")]
    // OutOfBoundsRead,
}

/// Result of loading data from persistent storage.
pub type LoadResult<A> = Result<A, LoadError>;

/// An error that may occur when storing or loadi ng data from persistent
/// storage.
#[derive(Debug, thiserror::Error)]
pub enum LoadWriteError {
    #[error("Write error: {0}")]
    Write(#[from] WriteError),
    #[error("Load error: {0}")]
    Load(#[from] LoadError),
}

/// In the context where the LoadWriteError is used we want to propagate
/// write errors out. So this implementation is the more useful one as opposed
/// to the implementation which maps io errors through the [`LoadError`].
impl From<std::io::Error> for LoadWriteError {
    fn from(err: std::io::Error) -> Self {
        Self::Write(err.into())
    }
}

/// Result of loading or storing data from or to persistent storage.
pub type LoadStoreResult<A> = Result<A, LoadWriteError>;
