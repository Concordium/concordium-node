use crate::block_state::blob_reference::hashed_buffered_reference::HashedCacheableRef;
use crate::block_state::blob_store::{
    BackingStoreLoad, BackingStoreStore, DecodeError, Loadable, ParseResultExt, Storable,
};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash;
use crate::block_state::hash::Hashable;
use concordium_base::common::{Buffer, Get, Put};
use concordium_base::hashes::Hash;
use sha2::{Digest, Sha256};
use std::io::Read;
use std::marker::PhantomData;
use std::vec;

/// Representation of an immutable, left-full merkle binary (LFMB) tree with values of type `V`.
/// The represented tree is immutable in the sense that the tree itself does not change,
/// once it has been created. When values are inserted or updated, a new tree is created,
/// reusing the nodes that have not changed by the operation.
///
/// The operations supported for creating new trees are:
/// * Create empty tree with [`LFMBTree::empty`]: Returns an new empty tree.  
/// * Insert new value with [`LFMBTree::append`]: Inserts a new value, assigning the sequentially next unused key,
///   and returns new tree with the inserted value.
/// * Update value with [`LFMBTree::update`]: Updates an existing value, keeping the same key, and returns
///   the new tree with the updated value.
///
/// Keys are assigned as a sequential list of integers. Hence, the tree always fills up left branches
/// first.
///
/// // todo ar copy doc from Haskell
///
/// The internal representation in the tree may change during the lifetime via interior mutability.
/// This happens if values are cached, stored or hashes are lazily calculated.
#[derive(Debug, Clone)]
pub struct LFMBTree<K, V> {
    inner: LFMBTreeImpl<V>,
    _key_type: PhantomData<K>,
}

impl<K: LFMBTreeKey, V> Default for LFMBTree<K, V> {
    fn default() -> Self {
        Self::empty()
    }
}

pub trait LFMBTreeKey: Copy {
    fn to_u64(self) -> u64;

    fn from_u64(key: u64) -> Self;
}

impl<K: LFMBTreeKey, V> LFMBTree<K, V> {
    /// Create emtpy
    pub fn empty() -> Self {
        let inner = LFMBTreeImpl::Empty;

        Self {
            inner,
            _key_type: PhantomData,
        }
    }

    pub fn lookup<T>(
        &self,
        loader: &impl BackingStoreLoad,
        key: K,
        read: impl FnOnce(&V) -> T,
    ) -> Option<T>
    where
        V: Clone,
    {
        todo!()
    }

    pub fn values<T>(
        &self,
        loader: &impl BackingStoreLoad,
        read: impl FnMut(&V) -> T,
    ) -> impl Iterator<Item = T> {
        todo!() as vec::IntoIter<_>
    }

    pub fn append(&self, loader: &impl BackingStoreLoad, value: V) -> (K, Self) {
        todo!()
    }

    pub fn update_(
        &self,
        loader: &impl BackingStoreLoad,
        key: K,
        update: impl FnOnce(&V) -> V,
    ) -> Option<Self> {
        todo!()
    }
}

struct Key(u64);

#[derive(Debug, Clone)]
enum LFMBTreeImpl<V> {
    /// Emtpy Tree
    Empty,
    /// Non-empty tree
    NonEmpty(
        /// Size
        u64,
        /// Root
        Tree<V>,
    ),
}

/// Non-empty tree. The type is used recursively to represent subtrees of the tree also.
#[derive(Debug, Clone)]
enum Tree<V> {
    /// Leaf with value
    Leaf(V),
    /// Node with two subtrees
    Node(
        /// Height of tree
        u64,
        /// Left subtree
        HashedCacheableRef<Tree<V>>,
        /// Right subtree
        HashedCacheableRef<Tree<V>>,
    ),
}

impl<K, V: Loadable> Loadable for LFMBTree<K, V> {
    fn load_from_buffer(mut buffer: impl Read) -> Result<Self, DecodeError> {
        let size: u64 = buffer.get().into_decode_result()?;
        let inner = if size == 0 {
            LFMBTreeImpl::Empty
        } else {
            let tree = Tree::load_from_buffer(buffer)?;
            LFMBTreeImpl::NonEmpty(size, tree)
        };

        Ok(Self {
            inner,
            _key_type: PhantomData,
        })
    }
}

impl<K, V: Storable> Storable for LFMBTree<K, V> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BackingStoreStore) {
        match &self.inner {
            LFMBTreeImpl::Empty => {
                buffer.put(0u64);
            }
            LFMBTreeImpl::NonEmpty(size, tree) => {
                buffer.put(size);
                tree.store_to_buffer(buffer, storer);
            }
        }
    }
}

impl<V: Loadable> Loadable for Tree<V> {
    fn load_from_buffer(mut buffer: impl Read) -> Result<Self, DecodeError> {
        let disc: u8 = buffer.get().into_decode_result()?;
        Ok(match disc {
            0 => {
                let value = V::load_from_buffer(buffer)?;
                Tree::Leaf(value)
            }
            1 => {
                let size: u64 = buffer.get().into_decode_result()?;
                let left = Loadable::load_from_buffer(&mut buffer)?;
                let right = Loadable::load_from_buffer(&mut buffer)?;
                Tree::Node(size, left, right)
            }
            _ => return Err(format!("Invalid LFMB Tree discriminator: {}", disc).into()),
        })
    }
}

impl<V: Storable> Storable for Tree<V> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BackingStoreStore) {
        match self {
            Tree::Leaf(value) => {
                buffer.put(0u8);
                value.store_to_buffer(buffer, storer);
            }
            Tree::Node(size, left, right) => {
                buffer.put(1u8);
                buffer.put(size);
                left.store_to_buffer(&mut buffer, storer);
                right.store_to_buffer(&mut buffer, storer);
            }
        }
    }
}

impl<K, V: Hashable + Loadable> Hashable for LFMBTree<K, V> {
    fn hash(&self, loader: &impl BackingStoreLoad) -> Result<Hash, DecodeError> {
        let mut hasher = sha2::Sha256::new();

        match &self.inner {
            LFMBTreeImpl::Empty => {
                hasher.update(0u64.to_be_bytes());
                hasher.update(Sha256::digest("EmptyLFMBTree"));
            }
            LFMBTreeImpl::NonEmpty(size, tree) => {
                hasher.update(size.to_be_bytes());
                hasher.update(tree.hash(loader)?);
            }
        }
        Ok(Hash::new(hasher.finalize().into()))
    }
}

impl<V: Hashable + Loadable> Hashable for Tree<V> {
    fn hash(&self, loader: &impl BackingStoreLoad) -> Result<Hash, DecodeError> {
        Ok(match self {
            Tree::Node(_, left, right) => {
                hash::hash_of_hashes(left.hash(loader)?, right.hash(loader)?)
            }
            Tree::Leaf(v) => v.hash(loader)?,
        })
    }
}

impl<K, V: Cacheable + Loadable> Cacheable for LFMBTree<K, V> {
    fn cache_reference_values(&self, loader: &impl BackingStoreLoad) -> Result<(), DecodeError> {
        match &self.inner {
            LFMBTreeImpl::Empty => (),
            LFMBTreeImpl::NonEmpty(_, tree) => {
                tree.cache_reference_values(loader)?;
            }
        }
        Ok(())
    }
}

impl<V: Cacheable + Loadable> Cacheable for Tree<V> {
    fn cache_reference_values(&self, loader: &impl BackingStoreLoad) -> Result<(), DecodeError> {
        match self {
            Tree::Leaf(value) => {
                value.cache_reference_values(loader)?;
            }
            Tree::Node(_, left, right) => {
                left.cache_reference_values(loader)?;
                right.cache_reference_values(loader)?;
            }
        }
        Ok(())
    }
}
