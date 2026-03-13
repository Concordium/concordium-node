use crate::block_state::blob_reference::hashed_buffered_reference::HashedCacheableRef;
use crate::block_state::blob_store::{
    BackingStoreLoad, BackingStoreStore, DecodeError, Loadable, ParseResultExt, Storable,
};
use crate::block_state::hash::{Hashable, IntoPureHash};
use concordium_base::common::{Buffer, Get, Put};
use concordium_base::hashes::Hash;
use sha2::{Digest, Sha256};
use std::io::Read;

/// Representation of an immutable, left-full merkle binary (LFMB) tree with values of type `V`.
/// The represented tree is immutable in the sense that the tree itself does not change,
/// once it has been created. When values are inserted or updated, a new tree is created,
/// reusing the nodes that have not changed by the operation.
///
/// Keys are assigned as a sequential list of integers. Hence, the tree always fills up left branches
/// first.
///
/// // todo ar copy doc from Haskell
///
/// The operations supported on the tree are:
/// * inserting new values: inserts a new value, assigning the sequentially next unused key
/// * updating values: updates an existing value, keeping the same key
///
/// The internal representation in the tree may change during the lifetime via interior mutability.
/// This happens if values are cached, stored or hashes are lazily calculated.
#[derive(Debug)]
pub struct LFMBTree<V> {
    inner: LFMBTreeImpl<V>,
}

impl<V> LFMBTree<V> {
    pub fn empty() -> Self {
        let inner = LFMBTreeImpl::Empty;

        Self { inner }
    }
}

#[derive(Debug)]
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
#[derive(Debug)]
enum Tree<V> {
    /// Node with two subtrees
    Node(
        /// Height of tree
        u64,
        /// Left subtree
        HashedCacheableRef<Tree<V>>,
        /// Right subtree
        HashedCacheableRef<Tree<V>>,
    ),
    /// Leaf with value
    Leaf(V),
}

impl<V: Loadable> Loadable for LFMBTree<V> {
    fn load_from_buffer(mut buffer: impl Read) -> Result<Self, DecodeError> {
        let size: u64 = buffer.get().into_decode_result()?;
        let inner = if size == 0 {
            LFMBTreeImpl::Empty
        } else {
            let tree = Tree::load_from_buffer(buffer)?;
            LFMBTreeImpl::NonEmpty(size, tree)
        };

        Ok(Self { inner })
    }
}

impl<V: Storable> Storable for LFMBTree<V> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: impl BackingStoreStore) {
        match self.inner {
            LFMBTreeImpl::Empty => {
                buffer.put(0u64);
            }
            LFMBTreeImpl::NonEmpty(_, _) => {}
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
    fn store_to_buffer(&self, buffer: impl Buffer, storer: impl BackingStoreStore) {
        todo!()
    }
}

impl<V: Hashable + Loadable> Hashable for LFMBTree<V> {
    type Hash = Hash;

    fn hash(&self, loader: impl BackingStoreLoad) -> Result<Self::Hash, DecodeError> {
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
    type Hash = Hash;

    fn hash(&self, mut loader: impl BackingStoreLoad) -> Result<Hash, DecodeError> {
        Ok(match self {
            Tree::Node(_, left, right) => {
                let mut hasher = sha2::Sha256::new();
                hasher.update(left.hash(&mut loader)?);
                hasher.update(right.hash(&mut loader)?);
                Hash::new(hasher.finalize().into())
            }
            Tree::Leaf(v) => v.hash(loader)?.into_pure(),
        })
    }
}
