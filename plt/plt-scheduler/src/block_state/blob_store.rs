/// Reference to a storage location where an item may be retrieved.
#[derive(Default, Debug, Clone, Copy, Eq, PartialEq)]
#[repr(transparent)]
pub struct Reference(pub u64);

/// Trait implemented by types that can be used to store binary data, and return
/// a handle for loading data. Dual to [`BackingStoreLoad`].
pub trait BackingStoreStore {
    /// Store the provided value and return a reference that can be used
    /// to load it.
    fn store_raw(&mut self, data: &[u8]) -> Reference;
}

/// Trait implemented by types that can load data from given locations.
/// Dual to [`BackingStoreStore`].
pub trait BackingStoreLoad {
    /// Load the provided value from the given location. The implementation of
    /// this should match [BackingStoreStore::store_raw].
    fn load_raw(&mut self, location: Reference) -> Vec<u8>;
}

/// A trait implemented by types that can be loaded from a [BackingStoreLoad]
/// storage.
pub trait Loadable: Sized {
    fn load(
        loader: &mut impl BackingStoreLoad,
        source: &mut impl std::io::Read,
    ) -> Result<Self, DecodeError>;

    fn load_from_location(
        loader: &mut impl BackingStoreLoad,
        location: Reference,
    ) -> Result<Self, DecodeError> {
        let mut source = std::io::Cursor::new(loader.load_raw(location));
        Self::load(loader, &mut source)
    }
}

/// An error that may occur when loading data from persistent storage.
#[derive(Debug, thiserror::Error)]
pub enum DecodeError {
    // add parsing errors here
}
