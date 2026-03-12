
use concordium_base::common::Serialize;
use std::sync::{Arc, RwLock};

pub mod hashed_buffered_reference;

/// Reference to a storage location where an item may be retrieved.
#[derive(Default, Debug, Clone, Copy, Eq, PartialEq, Serialize)]
#[repr(transparent)]
pub struct BlobReference(pub u64);

// impl Loadable for Reference {
//     fn load(loader: impl BackingStoreLoad, location: Reference) -> Result<Self, DecodeError> {
//         Ok(Self(LoadSerialized::<u64>::load(loader, location)?.0))
//     }
// }
//
// impl Storable for Reference {
//     fn store(&self, storer: impl BackingStoreStore) -> Reference {
//         StoreSerialized(self.0).store(storer)
//     }
// }

/// A link to a shared occurrence of a value V.
/// This is used in this module to construct trees, allowing for sharing of
/// values in trees and subtrees in case of the persistent tree.
///
/// This [Link] achieves the following properties
/// - it is cheap to clone
/// - it allows for inner mutability
/// - it is safe to use in a concurrent context.
///
/// The cheap cloning is necessary to have efficient persistent trees. The
/// [Link] is used to point to children, as well as values, in the persistent
/// tree. Modifying the value at a given point means we need to create copies of
/// the spine of the tree up to the root. This involves cloning pointers to any
/// parts that have not been modified. Thus these have to be cheap, so we use an
/// [Arc].
///
/// Inner mutability is needed so that the persistent tree may be loaded from
/// disk, as well as written to disk. This is achieved in combination with
/// [CachedRef]. Instead of using an [RwLock] we could instead use a
/// [Mutex](std::sync::Mutex) which would achieve the same API. However based on
/// benchmarks the RwLock has negligible overhead in the case of a single
/// reader, and thus the [RwLock] seems the more natural choice since it allows
/// concurrent reads of the tree.
///
/// Finally, the reference counting must be atomic since parts of the tree are
/// dropped concurrently. While all the operations of the smart contract
/// execution engine are sequential, multiple states may be derived from the
/// same state in different threads. Currently this happens when using
/// invokeContract, but in the future with parallel block execution it might
/// also happen during normal block processing.
/// Additionally, if the Haskell runtime is configured with the parallel garbage
/// collector then parts of the tree might be dropped concurrently. This also
/// requires atomic reference counting.
#[derive(Debug)]
pub struct Link<V> {
    link: Arc<RwLock<V>>,
}

impl<V> Clone for Link<V> {
    #[inline(always)]
    fn clone(&self) -> Self {
        Self {
            link: self.link.clone(),
        }
    }
}

impl<V> Link<V> {
    pub fn new(value: V) -> Self {
        Self {
            link: Arc::new(RwLock::new(value)),
        }
    }
}
