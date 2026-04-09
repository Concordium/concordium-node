//! Representation of an immutable, left-full merkle binary (LFMB) tree.
//!
//! See [`LFMBTree`].

use crate::block_state::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::block_state::blob_store::{
    BlobStoreLoad, BlobStoreStore, Loadable, ParseResultExt, Storable,
};
use crate::block_state::cacheable::Cacheable;
use crate::block_state::hash;
use crate::block_state::hash::Hashable;
use crate::block_state::utils::OwnedOrBorrowed;
use crate::block_state_interface::{BlockStateError, BlockStateResult};
use concordium_base::common::{Buffer, Get, Put};
use concordium_base::hashes::Hash;
use either::Either;
use sha2::{Digest, Sha256};
use std::fmt::Debug;
use std::io::Read;
use std::marker::PhantomData;
use std::{iter, vec};

/// Representation of an immutable, left-full merkle binary (LFMB) tree with values of type `V`.
/// The represented tree is immutable in the sense that the tree and its values does not change,
/// once it has been created. When values are inserted or updated, a new tree is created,
/// reusing the nodes that have not changed by the operation.
/// Keys are assigned as a sequential list of integers starting from 0. Hence, the tree always fills up left branches
/// first.
///
/// The operations supported for creating new trees are:
/// * Create empty tree with [`LFMBTree::empty`]: Returns an new empty tree.  
/// * Insert new value with [`LFMBTree::insert_value`]: Inserts a new value, assigning the sequentially next unused key,
///   and returns new tree with the inserted value.
/// * Update value with [`LFMBTree::update_value`]: Updates an existing value, keeping the same key, and returns
///   the new tree with the updated value.
///
/// ## Interior mutability
///
/// The internal representation in the tree may change during the lifetime via interior mutability.
/// This happens if values are cached, stored or hashes are lazily calculated.
///
/// ## Data structure
///
/// The data structure is a left-full binary tree where keys are assigned as a sequence of integers
/// starting with 0. There are no gaps in the sequence of keys, since entries are never removed.
/// That keys are assigned sequentially from 0 means that the tree can be maintained as left-full.
/// Each [node](Subtree::Node) has a height `h`, and the following invariant holds for the size of the two subtrees:
///
/// * *size of left subtree* `== 2^h`
/// * `0 <` *size of right subtree* `<= 2^h`
///
/// Notice that this gives a size invariant for node: `2^h <` *size of node* `<= 2^(h + 1)`.
///
/// This invariant for the nodes subtree sizes uniquely determines the structure of a tree of
/// a given size. See the examples below.
/// As the tree grows, new nodes are inserted in the tree on top off full subtrees or leafs.
///
/// Given the node subtree size invariant above, we can locate the leaf for a key by traversing the tree starting
/// at the root, and for each [node](Subtree::Node), use the height `h` to decide
/// on which branch to take based on the `h`'th bit in the key:
///
/// * `0`: follow left branch,
/// * `1`: follow right branch,
///
/// The algorithms for specific operations are described in more detail in the implementations:
///
/// * [`Subtree::insert_value`]
/// * [`Subtree::with_value_for_key`]
///
/// ### Enforcing invariants
///
/// Loading a tree from the blob store may result in broken invariants,
/// if the blob store is corrupted in some way. The implemented operations will return
/// [`BlockStateError::Invariant`] if broken invariants are encountered.
///
/// ### Example tree
///
/// This section contains diagrams of trees up to size eight. The tree are a result of
/// inserting letters as values alphabetically `A`, `B`, `C`, ... .
///
/// The symbols used in the diagrams are
///
/// * `[A]`: Leaf with value `A`.
/// * `(h)`: Node at height `h`.
///   and interpretations of the height.
/// * `#i`: Key label indicating that leaf corresponds to key `i`.
///   Notice that this is not part of the data structure, it is implicitly derived.
///
/// ```text
///
/// size 1:
///       [A]
///       #0
/// size 2:
///       (0)
///      /   \
///    [A]   [B]
///    #0    #1
///
/// size 3:
///          (1)
///         /   \
///       (0)   [C]
///       / \   #2
///     [A] [B]
///     #0  #1
///
/// size 4:
///          (1)
///         /   \
///      (0)     (0)
///      / \     / \
///    [A] [B] [C] [D]
///    #0  #1  #2  #3
///
/// size 5:
///              (2)
///             /   \
///          (1)    [E]
///         /   \   #4
///      (0)     (0)
///      / \     / \
///    [A] [B] [C] [D]
///    #0  #1  #2  #3
///
/// size 6:
///             ---(2)---
///            /         \
///          (1)         (0)
///         /   \        / \
///      (0)     (0)   [E] [F]
///      / \     / \   #4  #5
///    [A] [B] [C] [D]
///    #0  #1  #2  #3
///
/// size 7:
///             _----(2)----_
///            /             \
///          (1)             (1)
///         /   \           /   \
///      (0)     (0)     (0)     [G]
///      / \     / \     / \     #6
///    [A] [B] [C] [D] [E] [F]
///    #0  #1  #2  #3  #4  #5
///
/// size 8:
///             _----(2)----_
///            /             \
///          (1)             (1)
///         /   \           /   \
///      (0)     (0)     (0)     (0)
///      / \     / \     / \     / \
///    [A] [B] [C] [D] [E] [F] [G] [H]
///    #0  #1  #2  #3  #4  #5  #6  #7
/// ```
#[derive(Debug, Clone)]
pub struct LFMBTree<K, V> {
    inner: LFMBTreeInner<V>,
    _key_type: PhantomData<K>,
}

impl<K: LFMBTreeKey, V> Default for LFMBTree<K, V> {
    fn default() -> Self {
        Self::empty()
    }
}

/// Trait implemented by tree keys, which allows them to be bijectively mapped
/// to `u46` values.
pub trait LFMBTreeKey: Copy {
    /// Map key to `u64`
    fn to_u64(self) -> u64;

    /// Map `u64` to key
    fn from_u64(key: u64) -> Self;
}

impl<K: LFMBTreeKey, V> LFMBTree<K, V> {
    /// Create an empty tree.
    pub fn empty() -> Self {
        let inner = LFMBTreeInner::Empty;

        Self {
            inner,
            _key_type: PhantomData,
        }
    }

    /// Return the number of entries in the tree.
    #[allow(unused)]
    pub fn size(&self) -> u64 {
        match &self.inner {
            LFMBTreeInner::Empty => 0,
            LFMBTreeInner::NonEmpty(size, _) => *size,
        }
    }

    /// Access the value for the given `key` in the tree
    /// using the `read` closure. Returns the value returned by the
    /// closure if there is a value for the key in the tree, or `None` if there
    /// is no value for the key.
    ///
    /// # Arguments
    ///
    /// - `loader`: Loader for the blob store the tree is stored in.
    /// - `key`: The key to access the value for.
    /// - `read`: Closure that is given the value, either as owned or borrowed,
    ///   and returns the value of type `T` that will be returned by `with_value`.
    ///
    /// # Errors
    ///
    /// Returns [`BlockStateError`] if returned by `read`, or if decoding data from the
    /// blob store fails, or if the tree does not fulfill
    /// the expected invariants (this can happen if the blob store is corrupted in some way).
    pub fn with_value<T>(
        &self,
        loader: &impl BlobStoreLoad,
        key: K,
        read: impl FnOnce(OwnedOrBorrowed<'_, V>) -> BlockStateResult<T>,
    ) -> Option<BlockStateResult<T>>
    where
        V: Loadable,
    {
        match &self.inner {
            LFMBTreeInner::Empty => None,
            LFMBTreeInner::NonEmpty(size, subtree) => {
                let int_key = SubtreeKey(key.to_u64());
                if int_key.0 < *size {
                    Some(subtree.with_value(loader, int_key, read))
                } else {
                    None
                }
            }
        }
    }

    /// Iterates all values in the tree in insertion order (which is also the order
    /// of the keys) using the `read` closure. For each value in the tree, the value of type `T`
    /// returned by `read` will be the item in the iterator.
    ///
    /// # Arguments
    ///
    /// - `loader`: Loader for the blob store the tree is stored in.
    /// - `read`: Closure that is given each key-value pair, either as owned or borrowed,
    ///   and returns the value of type `T` that will be the item in the iterator.
    ///
    /// # Errors
    ///
    /// The iterator returns [`BlockStateError`] if returned by `read`, or if decoding data from the
    /// blob store fails, or if the tree does not fulfill the expected invariants (this can happen
    /// if the blob store is corrupted in some way).
    pub fn values<'a, L: BlobStoreLoad, F, T>(
        &self,
        loader: &'a L,
        mut read: F,
    ) -> impl ExactSizeIterator<Item = BlockStateResult<T>> + use<'a, K, V, L, F, T>
    where
        V: Loadable,
        F: FnMut(K, OwnedOrBorrowed<'_, V>) -> BlockStateResult<T>,
    {
        match &self.inner {
            LFMBTreeInner::Empty => Either::Left(iter::empty()),
            LFMBTreeInner::NonEmpty(_, subtree) => Either::Right(subtree.values(
                loader,
                self.size(),
                move |subtree_key, value| read(K::from_u64(subtree_key.0), value),
            )),
        }
    }

    /// Insert a value to the tree, and return the key for the inserted value and
    /// the tree with the inserted value. Keys are assigned sequentially,
    /// starting from `LFMBTreeKey::from_u64(0)`, then `LFMBTreeKey::from_u64(1)` and
    /// so on.
    ///
    /// Notice that trees are immutable data structures, see [`Self`].
    ///
    /// # Arguments
    ///
    /// - `loader`: loader for the blob store the tree is stored in
    /// - `value`: The value to insert
    ///
    /// # Errors
    ///
    /// Returns [`BlockStateError`] if decoding data from the
    /// blob store fails, or if the tree does not fulfill
    /// the expected invariants (this can happen if the blob store is corrupted in some way).
    pub fn insert_value(&self, loader: &impl BlobStoreLoad, value: V) -> BlockStateResult<(K, Self)>
    where
        V: Loadable,
    {
        Ok(match &self.inner {
            LFMBTreeInner::Empty => (
                LFMBTreeKey::from_u64(0),
                Self::from_inner(LFMBTreeInner::NonEmpty(
                    1,
                    Subtree::Leaf(HashedCacheableRef::new(value)),
                )),
            ),
            LFMBTreeInner::NonEmpty(size, subtree) => (
                LFMBTreeKey::from_u64(*size),
                Self::from_inner(LFMBTreeInner::NonEmpty(
                    *size + 1,
                    subtree.insert_value(loader, None, *size, value)?,
                )),
            ),
        })
    }

    /// Update the value with the given `key` in the tree
    /// using the `update` closure. Returns the tree with the updated
    /// value or `None` if there is no entry with the given key in the tree.
    ///
    /// Notice that trees are immutable data structures, see [`Self`].
    ///
    /// # Arguments
    ///
    /// - `loader`: Loader for the blob store the tree is stored in.
    /// - `key`: The key to update the value for.
    /// - `update`: Closure that is given the value, either as owned
    ///   or borrowed, and returns the new value for the key.
    ///
    /// # Errors
    ///
    /// Returns [`BlockStateError`] if returned by `update` or if decoding data from the
    /// blob store fails, or if the tree does not fulfill
    /// the expected invariants (this can happen if the blob store is corrupted in some way).
    pub fn update_value(
        &self,
        loader: &impl BlobStoreLoad,
        key: K,
        update: impl FnOnce(OwnedOrBorrowed<'_, V>) -> BlockStateResult<V>,
    ) -> Option<BlockStateResult<Self>>
    where
        V: Loadable,
    {
        match &self.inner {
            LFMBTreeInner::Empty => None,
            LFMBTreeInner::NonEmpty(size, subtree) => {
                let int_key = SubtreeKey(key.to_u64());
                if int_key.0 < *size {
                    let new_subtree = match subtree.update_value(loader, int_key, update) {
                        Ok(new_subtree) => new_subtree,
                        Err(err) => return Some(Err(err)),
                    };
                    Some(Ok(Self::from_inner(LFMBTreeInner::NonEmpty(
                        *size,
                        new_subtree,
                    ))))
                } else {
                    None
                }
            }
        }
    }

    fn from_inner(inner: LFMBTreeInner<V>) -> Self {
        Self {
            inner,
            _key_type: Default::default(),
        }
    }
}

/// Internal representation of tree key used in the subtree.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct SubtreeKey(u64);

/// Internal representation of the tree.
#[derive(Debug, Clone)]
enum LFMBTreeInner<V> {
    /// Emtpy Tree.
    Empty,
    /// Non-empty tree.
    ///
    /// Invariant: The number of entries in the subtree
    /// is equal to size.
    NonEmpty(
        /// Size
        u64,
        /// Root
        Subtree<V>,
    ),
}

/// Non-empty subtree. The type is used recursively to represent branches.
#[derive(Debug, Clone)]
enum Subtree<V> {
    /// Leaf with value
    Leaf(HashedCacheableRef<V>),
    /// Node with two subtrees/branches.
    ///
    /// Invariant relating height `h` and branches:
    /// * *size of left subtree* `== 2^h`
    /// * `0 <` *size of right subtree* `<= 2^h`
    Node(
        /// Height of tree
        u64,
        /// Left branch
        HashedCacheableRef<Subtree<V>>,
        /// Right branch
        HashedCacheableRef<Subtree<V>>,
    ),
}

/// Check if `nth` bit is set in `key`.
const fn is_nth_bit_set(nth: u64, key: SubtreeKey) -> bool {
    let bit = 1u64 << nth;
    (key.0 & bit) != 0
}

/// Flip the `nth` bit in `key`.
const fn flip_nth_bit(nth: u64, key: SubtreeKey) -> SubtreeKey {
    let bit = 1u64 << nth;
    SubtreeKey(key.0 ^ bit)
}

impl<V> Subtree<V> {
    /// Access the value for the given `key` in the subtree.
    ///
    /// # Arguments
    ///
    /// - `key`: The key to access the value for.
    /// - `read`: Closure that is given the value, either as owned or borrowed,
    ///   and returns the value of type `T` that will be returned by `with_value`.
    fn with_value<T>(
        &self,
        loader: &impl BlobStoreLoad,
        key: SubtreeKey,
        read: impl FnOnce(OwnedOrBorrowed<'_, V>) -> BlockStateResult<T>,
    ) -> BlockStateResult<T>
    where
        V: Loadable,
    {
        match self {
            Subtree::Leaf(val_ref) => {
                // When we reach the leaf for the key, the key must be 0.
                if key != SubtreeKey(0) {
                    return Err(BlockStateError::Invariant(format!(
                        "LFMB Subtree invariant broken: SubtreeKey must be zero at leaf, is {:?}",
                        key
                    )));
                }
                val_ref.with_value(loader, read)
            }
            Subtree::Node(height, left_ref, right_ref) => {
                // The height'th bit in key decides if we should follow left `0`
                // or right branch `1`. Additionally, when going right, we set the bit to 0.
                // This allows us to check the invariant that the key must be identical to 0
                // when we reach the leaf for the key.
                if is_nth_bit_set(*height, key) {
                    right_ref.with_value(loader, |right| {
                        right.with_value(loader, flip_nth_bit(*height, key), read)
                    })
                } else {
                    left_ref.with_value(loader, |left| left.with_value(loader, key, read))
                }
            }
        }
    }

    /// Iterates all values in the subtree in insertion order.
    ///
    /// # Arguments
    ///
    /// - `read`: Closure that is given each value, either as owned or borrowed,
    ///   and returns the value of type `T` that will be the item in the iterator.
    /// - `node_size`: The number of entries in the subtree.
    pub fn values<'a, L: BlobStoreLoad, F, T>(
        &self,
        loader: &'a L,
        node_size: u64,
        read: F,
    ) -> impl ExactSizeIterator<Item = BlockStateResult<T>> + use<'a, V, L, F, T>
    where
        V: Loadable,
        F: FnMut(SubtreeKey, OwnedOrBorrowed<'_, V>) -> BlockStateResult<T>,
    {
        ValuesIterator::new(self, loader, read, node_size)
    }

    /// Insert `new_value` into the subtree and return a new subtree with the inserted value.
    ///
    /// # Arguments
    ///
    /// - `subtree_ref_option`: Blob reference to the subtree. For the root tree, there is no such
    ///   reference, in which case the argument is `None`.
    /// - `node_size`: The number of entries in the subtree.
    /// - `new_value`: The value to insert in the subtree.
    fn insert_value(
        &self,
        loader: &impl BlobStoreLoad,
        subtree_ref_option: Option<&HashedCacheableRef<Subtree<V>>>,
        node_size: u64,
        new_value: V,
    ) -> BlockStateResult<Self>
    where
        V: Loadable,
    {
        Ok(match self {
            Subtree::Leaf(current_val_ref) => {
                if node_size != 1 {
                    return Err(BlockStateError::Invariant(format!(
                        "LFMB Subtree invariant broken: Expected size 1 for leaf, is {:?}",
                        node_size
                    )));
                }
                // Create node with height 0 with current value as the left side, and new value as the right side.
                Subtree::Node(
                    0,
                    HashedCacheableRef::new(Subtree::Leaf(current_val_ref.clone())),
                    HashedCacheableRef::new(Subtree::Leaf(HashedCacheableRef::new(new_value))),
                )
            }
            Subtree::Node(height, left_ref, right_ref) => {
                // Left branch is always filled with 2^height entries.
                let left_branch_size = 1u64 << height;
                // The max node size is the double of that.
                let node_max_size = left_branch_size << 1;

                if node_size > node_max_size || node_size <= left_branch_size {
                    return Err(BlockStateError::Invariant(format!(
                        "LFMB Subtree invariant broken: Node size {} for node with height {}",
                        node_size, height
                    )));
                }
                // Check if subtree is already full
                if node_size == node_max_size {
                    let subtree_ref = match subtree_ref_option {
                        None => HashedCacheableRef::new(Subtree::Node(
                            *height,
                            left_ref.clone(),
                            right_ref.clone(),
                        )),
                        Some(subtree_ref) => subtree_ref.clone(),
                    };
                    // There is no more room in the subtree, so we insert a new node
                    // with current node as left side, and the new value as the right side.
                    // The height of the new node is one higher than the existing.
                    Subtree::Node(
                        *height + 1,
                        subtree_ref,
                        HashedCacheableRef::new(Subtree::Leaf(HashedCacheableRef::new(new_value))),
                    )
                } else {
                    // There is still room in the right branch of the tree, so insert the new value in the right branch.
                    let new_right = right_ref.with_value(loader, |right| {
                        right.insert_value(
                            loader,
                            Some(right_ref),
                            node_size - left_branch_size,
                            new_value,
                        )
                    })?;
                    Subtree::Node(
                        *height,
                        left_ref.clone(),
                        HashedCacheableRef::new(new_right),
                    )
                }
            }
        })
    }

    /// Update the value with the given `key` in the tree
    /// using the `update` closure.
    ///
    /// # Arguments
    ///
    /// - `key`: The key to update the value for.
    /// - `update`: Closure that is given the value, either as owned
    ///   or borrowed, and returns the new value for the key.
    pub fn update_value(
        &self,
        loader: &impl BlobStoreLoad,
        key: SubtreeKey,
        update: impl FnOnce(OwnedOrBorrowed<'_, V>) -> BlockStateResult<V>,
    ) -> BlockStateResult<Self>
    where
        V: Loadable,
    {
        Ok(match self {
            Subtree::Leaf(val_ref) => {
                // When we reach the leaf for the key, the key must be 0.
                if key != SubtreeKey(0) {
                    return Err(BlockStateError::Invariant(format!(
                        "LFMB Subtree invariant broken: SubtreeKey must be zero at leaf, is {:?}",
                        key
                    )));
                }
                let new_value = val_ref.with_value(loader, update)?;
                Subtree::Leaf(HashedCacheableRef::new(new_value))
            }
            Subtree::Node(height, left_ref, right_ref) => {
                // The height'th bit in key decides if we should follow left `0`
                // or right branch `1`. Additionally, when going right, we set the bit to 0.
                // This allows us to check the invariant that the key must be identical to 0
                // when we reach the leaf for the key.
                if is_nth_bit_set(*height, key) {
                    let new_right = right_ref.with_value(loader, |right| {
                        right.update_value(loader, flip_nth_bit(*height, key), update)
                    })?;

                    Subtree::Node(
                        *height,
                        left_ref.clone(),
                        HashedCacheableRef::new(new_right),
                    )
                } else {
                    let new_left = left_ref
                        .with_value(loader, |left| left.update_value(loader, key, update))?;

                    Subtree::Node(
                        *height,
                        HashedCacheableRef::new(new_left),
                        right_ref.clone(),
                    )
                }
            }
        })
    }
}

struct ValuesIterator<'a, L, F, V> {
    loader: &'a L,
    peeked_value: Option<HashedCacheableRef<V>>,
    node_stack: Vec<HashedCacheableRef<Subtree<V>>>,
    read: F,
    tree_size: u64,
    next_key: SubtreeKey,
}

impl<'a, L: BlobStoreLoad, F, V> ValuesIterator<'a, L, F, V> {
    fn new(subtree: &Subtree<V>, loader: &'a L, read: F, tree_size: u64) -> Self {
        match subtree {
            Subtree::Leaf(value_ref) => Self {
                loader,
                peeked_value: Some(value_ref.clone()),
                node_stack: vec![],
                read,
                tree_size,
                next_key: SubtreeKey(0),
            },
            Subtree::Node(_, left_ref, right_ref) => Self {
                loader,
                peeked_value: None,
                node_stack: vec![right_ref.clone(), left_ref.clone()],
                read,
                tree_size,
                next_key: SubtreeKey(0),
            },
        }
    }
}

impl<'a, L: BlobStoreLoad, F, V: Loadable, T> ExactSizeIterator for ValuesIterator<'a, L, F, V> where
    F: FnMut(SubtreeKey, OwnedOrBorrowed<'_, V>) -> BlockStateResult<T>
{
}

impl<'a, L: BlobStoreLoad, F, V: Loadable, T> Iterator for ValuesIterator<'a, L, F, V>
where
    F: FnMut(SubtreeKey, OwnedOrBorrowed<'_, V>) -> BlockStateResult<T>,
{
    type Item = BlockStateResult<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next_value) = self.peeked_value.take() {
            if self.next_key.0 == self.tree_size {
                return Some(Err(BlockStateError::Invariant(
                    "LFMB Subtree invariant broken: ValuesIterator next_key equal to tree_size before end of iterator"
                        .to_string(),
                )));
            }
            let key = self.next_key;
            self.next_key.0 += 1;
            Some(next_value.with_value(self.loader, |value| (self.read)(key, value)))
        } else if let Some(next_node_ref) = self.node_stack.pop() {
            if self.next_key.0 == self.tree_size {
                return Some(Err(BlockStateError::Invariant(
                    "LFMB Subtree invariant broken: ValuesIterator next_key equal to tree_size before end of iterator"
                        .to_string(),
                )));
            }
            let key = self.next_key;
            self.next_key.0 += 1;
            Some(next_value_push_right_branches(
                key,
                self.loader,
                &next_node_ref,
                &mut self.node_stack,
                &mut self.read,
            ))
        } else {
            if self.next_key.0 != self.tree_size {
                return Some(Err(BlockStateError::Invariant(format!(
                    "LFMB Subtree invariant broken: ValuesIterator next_key not equal to tree_size at end of iterator, is {}",
                    self.next_key.0
                ))));
            }
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = (self.tree_size - self.next_key.0) as usize;
        (size, Some(size))
    }
}

/// Follow left branches to reach next value while pushing all right branches to the
/// node stack.
fn next_value_push_right_branches<L: BlobStoreLoad, F, V: Loadable, T>(
    key: SubtreeKey,
    loader: &L,
    node_ref: &HashedCacheableRef<Subtree<V>>,
    node_stack: &mut Vec<HashedCacheableRef<Subtree<V>>>,
    read: &mut F,
) -> BlockStateResult<T>
where
    F: FnMut(SubtreeKey, OwnedOrBorrowed<'_, V>) -> BlockStateResult<T>,
{
    node_ref.with_value(loader, |node| match &*node {
        Subtree::Leaf(value) => value.with_value(loader, |value| read(key, value)),
        Subtree::Node(_, left_ref, right_ref) => {
            node_stack.push(right_ref.clone());
            next_value_push_right_branches(key, loader, left_ref, node_stack, read)
        }
    })
}

impl<K, V: Loadable> Loadable for LFMBTree<K, V> {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> BlockStateResult<Self> {
        let size: u64 = buffer.get().map_parse_err_to_block_state_err()?;
        let inner = if size == 0 {
            LFMBTreeInner::Empty
        } else {
            let tree = Subtree::load_from_buffer(buffer, loader)?;
            LFMBTreeInner::NonEmpty(size, tree)
        };

        Ok(Self {
            inner,
            _key_type: PhantomData,
        })
    }
}

impl<K, V: Storable> Storable for LFMBTree<K, V> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        match &self.inner {
            LFMBTreeInner::Empty => {
                buffer.put(0u64);
            }
            LFMBTreeInner::NonEmpty(size, tree) => {
                buffer.put(size);
                tree.store_to_buffer(buffer, storer);
            }
        }
    }
}

impl<V: Loadable> Loadable for Subtree<V> {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> BlockStateResult<Self> {
        let disc: u8 = buffer.get().map_parse_err_to_block_state_err()?;
        Ok(match disc {
            0 => {
                let value_ref = Loadable::load_from_buffer(buffer, loader)?;
                Subtree::Leaf(value_ref)
            }
            1 => {
                let height: u64 = buffer.get().map_parse_err_to_block_state_err()?;
                let left_ref = Loadable::load_from_buffer(&mut buffer, loader)?;
                let right_ref = Loadable::load_from_buffer(&mut buffer, loader)?;
                Subtree::Node(height, left_ref, right_ref)
            }
            _ => {
                return Err(BlockStateError::BlobStoreDecode(format!(
                    "Invalid LFMB Tree discriminator: {}",
                    disc
                )));
            }
        })
    }
}

impl<V: Storable> Storable for Subtree<V> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        match self {
            Subtree::Leaf(value) => {
                buffer.put(0u8);
                value.store_to_buffer(buffer, storer);
            }
            Subtree::Node(height, left_ref, right_ref) => {
                buffer.put(1u8);
                buffer.put(height);
                left_ref.store_to_buffer(&mut buffer, storer);
                right_ref.store_to_buffer(&mut buffer, storer);
            }
        }
    }
}

impl<K, V: Hashable + Loadable> Hashable for LFMBTree<K, V> {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        let mut hasher = sha2::Sha256::new();

        match &self.inner {
            LFMBTreeInner::Empty => {
                hasher.update(0u64.to_be_bytes());
                hasher.update(Sha256::digest("EmptyLFMBTree"));
            }
            LFMBTreeInner::NonEmpty(size, subtree) => {
                hasher.update(size.to_be_bytes());
                hasher.update(subtree.hash(loader)?);
            }
        }
        Ok(Hash::new(hasher.finalize().into()))
    }
}

impl<V: Hashable + Loadable> Hashable for Subtree<V> {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        Ok(match self {
            Subtree::Node(_, left_ref, right_ref) => {
                hash::hash_of_hashes(left_ref.hash(loader)?, right_ref.hash(loader)?)
            }
            Subtree::Leaf(v) => v.hash(loader)?,
        })
    }
}

impl<K, V: Cacheable + Loadable> Cacheable for LFMBTree<K, V> {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        match &self.inner {
            LFMBTreeInner::Empty => (),
            LFMBTreeInner::NonEmpty(_, subtree) => {
                subtree.cache_reference_values(loader)?;
            }
        }
        Ok(())
    }
}

impl<V: Cacheable + Loadable> Cacheable for Subtree<V> {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        match self {
            Subtree::Leaf(value) => {
                value.cache_reference_values(loader)?;
            }
            Subtree::Node(_, left_ref, right_ref) => {
                left_ref.cache_reference_values(loader)?;
                right_ref.cache_reference_values(loader)?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_state::blob_store;
    use crate::block_state::blob_store::test_stub::{BlobStoreStub, UnreachableBlobStore};
    use crate::block_state::blob_store::{BlobStoreLocation, StoreSerialized};
    use assert_matches::assert_matches;
    use std::fmt::Debug;

    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    struct TestKey(u64);

    type TestTree = LFMBTree<TestKey, StoreSerialized<u64>>;

    impl LFMBTreeKey for TestKey {
        fn to_u64(self) -> u64 {
            self.0
        }

        fn from_u64(key: u64) -> Self {
            Self(key)
        }
    }

    fn create_tree(store: &mut impl BlobStoreLoad, size: u64) -> TestTree {
        let mut tree = TestTree::empty();
        for i in 0..size {
            let key;
            (key, tree) = tree.insert_value(store, StoreSerialized(i + 10)).unwrap();
            assert_eq!(key, TestKey(i));
        }
        tree
    }

    /// Test [`LFMBTree::size`]
    #[test]
    fn prop_test_size() {
        for i in 0..100 {
            let mut store = BlobStoreStub::default();

            // Append values to tree
            let tree = create_tree(&mut store, i);

            // Assert size
            assert_eq!(tree.size(), i, "get size for tree of size {}", i);
        }
    }

    /// Test [`LFMBTree::with_value`]
    #[test]
    fn prop_test_with_value() {
        for i in 0..100 {
            let mut store = BlobStoreStub::default();

            // Append values to tree
            let tree = create_tree(&mut store, i);

            // Lookup existing values
            for j in 0..i {
                assert_eq!(
                    tree.with_value(&store, TestKey(j), |val| Ok(*val))
                        .transpose()
                        .unwrap(),
                    Some(StoreSerialized(j + 10)),
                    "access value for key {:?} in tree of size {}",
                    TestKey(j),
                    i
                );
            }

            // Lookup non-existing values
            assert_eq!(
                tree.with_value(&store, TestKey(i), |val| Ok(*val))
                    .transpose()
                    .unwrap(),
                None,
                "access non-existing value for key {:?} in tree of size {}",
                TestKey(i),
                i
            );
            assert_eq!(
                tree.with_value(&store, TestKey(i + 1), |val| Ok(*val))
                    .transpose()
                    .unwrap(),
                None,
                "access non-existing value for key {:?} in tree of size {}",
                TestKey(i + 1),
                i
            );
        }
    }

    /// Test [`LFMBTree::values`]
    #[test]
    fn prop_test_values() {
        for i in 0..100 {
            let mut store = BlobStoreStub::default();

            // Append values to tree
            let tree = create_tree(&mut store, i);

            // Iterate values
            let mut values = tree.values(&store, |key, val| Ok((key, *val)));

            // Assert values as expected
            assert_eq!(
                values.len(),
                i as usize,
                "values length for tree of size {}",
                i
            );
            let mut j = 0;
            while let Some(entry_res) = values.next() {
                let (key, val) = entry_res.unwrap();
                assert_eq!(key, TestKey(j), "key {} in tree of size {}", j, i);
                assert_eq!(
                    val,
                    StoreSerialized(j + 10),
                    "value number {} in tree of size {}",
                    j,
                    i
                );
                j += 1;
                assert_eq!(values.len(), (i - j) as usize);
            }
            assert_eq!(values.len(), 0);
            assert_eq!(values.next().transpose().unwrap(), None);
            assert_eq!(values.len(), 0);
        }
    }

    /// Test [`LFMBTree::with_value`]
    #[test]
    fn prop_test_update_value() {
        for i in 0..100 {
            let mut store = BlobStoreStub::default();

            // Append values to tree
            let mut tree = create_tree(&mut store, i);

            // Update each of the values
            for j in 0..i {
                // Update the value
                tree = tree
                    .update_value(&store, TestKey(j), |val| Ok(StoreSerialized(val.0 + 10)))
                    .expect("update existing value")
                    .unwrap();

                // Lookup the value again
                assert_eq!(
                    tree.with_value(&store, TestKey(j), |val| Ok(*val))
                        .transpose()
                        .unwrap(),
                    Some(StoreSerialized(j + 20)),
                    "update value for key {:?} in tree of size {}",
                    TestKey(j),
                    i
                );
            }

            // Update non-existing values
            assert_matches!(
                tree.update_value(&store, TestKey(i), |val| Ok(*val)),
                None,
                "update non-existing value for key {:?} in tree of size {}",
                TestKey(i),
                i
            );
            assert_matches!(
                tree.update_value(&store, TestKey(i + 1), |val| Ok(*val)),
                None,
                "update non-existing value for key {:?} in tree of size {}",
                TestKey(i + 1),
                i
            );
        }
    }

    /// Tests storing the tree into the blob store and loading it again.
    #[test]
    fn prop_test_store_and_load() {
        for i in 0..100 {
            let mut store = BlobStoreStub::default();

            // Append values to tree
            let tree1 = create_tree(&mut store, i);

            // Store tree
            let blob_ref = blob_store::store_to_store(&mut store, &tree1);

            // Load tree
            let tree2: TestTree = blob_store::load_from_store(&store, blob_ref).unwrap();

            // Assert loaded tree is equal to the tree we started with
            assert_trees_eq(&store, &tree1, &tree2, format!("loaded tree of size {}", i));
        }
    }

    /// Tests caching tree.
    #[test]
    fn prop_test_cache() {
        for i in 0..100 {
            let mut store = BlobStoreStub::default();
            let tree1 = create_tree(&mut store, i);
            let blob_ref = blob_store::store_to_store(&mut store, &tree1);
            let tree2: TestTree = blob_store::load_from_store(&store, blob_ref).unwrap();

            // Cache tree
            tree2.cache_reference_values(&store).expect("cache");

            // Assert cached tree is identical to the tree with started with
            assert_trees_eq(&store, &tree1, &tree2, format!("cached tree of size {}", i));

            // Assert that when caching again or looking up entries, we don't need to read from the blob store again.
            // We assert that by using UnreachableBlobStore.
            tree2
                .cache_reference_values(&UnreachableBlobStore)
                .expect("cache");
            for j in 0..i {
                assert_eq!(
                    tree2
                        .with_value(&UnreachableBlobStore, TestKey(j), |val| Ok(*val))
                        .transpose()
                        .unwrap(),
                    Some(StoreSerialized(j + 10)),
                    "lookup value for key {:?} in cached tree of size {}",
                    TestKey(j),
                    i
                );
            }
            assert_eq!(
                tree2
                    .with_value(&UnreachableBlobStore, TestKey(i), |val| Ok(*val))
                    .transpose()
                    .unwrap(),
                None,
                "lookup non-existing value for key {:?} in cached tree of size {}",
                TestKey(i),
                i
            );
        }
    }

    /// Assert snapshot of hash of empty tree.
    /// Hash snapshot must not change and must be equal to Haskell LFMBTree implementation.
    #[test]
    fn snapshot_test_hash_empty_tree() {
        let store = BlobStoreStub::default();

        let tree = LFMBTree::<TestKey, StoreSerialized<String>>::empty();
        let hash = tree.hash(&store).unwrap();
        assert_eq!(
            hex::encode(hash.bytes),
            "c423f9e91ee218b2b5303485dd87a3093a653ddb9bdb839d30aa1924de1dbf05"
        );
    }

    /// Assert snapshot of hash of tree with 3 values A, B, C.
    /// Hash snapshot must not change and must be equal to Haskell LFMBTree implementation.
    #[test]
    fn snapshot_test_hash_simple_tree() {
        let store = BlobStoreStub::default();

        let tree = LFMBTree::<TestKey, StoreSerialized<String>>::empty();
        let tree1 = tree
            .insert_value(&store, StoreSerialized("A".to_string()))
            .unwrap()
            .1;
        let tree2 = tree1
            .insert_value(&store, StoreSerialized("B".to_string()))
            .unwrap()
            .1;
        let tree3 = tree2
            .insert_value(&store, StoreSerialized("C".to_string()))
            .unwrap()
            .1;
        let hash = tree3.hash(&store).unwrap();
        assert_eq!(
            hex::encode(hash.bytes),
            "b9cac19f6048ef301f586e7e0faa6c08b6012d4b100703eef5dc1fcb26c1ecd5"
        );
    }

    /// Load empty tree from storage bytes fixture.
    /// The fixture bytes must not change and must be compatible with Haskell LFMBTree implementation.
    #[test]
    fn fixture_test_storage_empty_tree() {
        let store = BlobStoreStub(hex::decode("00000000000000080000000000000000").unwrap());

        let tree: LFMBTree<TestKey, StoreSerialized<String>> =
            blob_store::load_from_store(&store, BlobStoreLocation(0)).expect("load tree");
        assert_eq!(tree.size(), 0);
    }

    /// Load tree with 3 values A, B, C from storage bytes fixture.
    /// The fixture bytes must not change and must be compatible with Haskell LFMBTree implementation.
    #[test]
    fn fixture_test_storage_simple_tree() {
        let store = BlobStoreStub(hex::decode("0000000000000009000000000000000141000000000000000900000000000000000000000000000000090000000000000001420000000000000009000000000000000022000000000000001901000000000000000000000000000000110000000000000033000000000000000900000000000000014300000000000000090000000000000000650000000000000021000000000000000301000000000000000100000000000000440000000000000076").unwrap());

        let tree: LFMBTree<TestKey, StoreSerialized<String>> =
            blob_store::load_from_store(&store, BlobStoreLocation(135)).expect("load tree");
        assert_eq!(tree.size(), 3);
        assert_eq!(
            tree.with_value(&store, TestKey(0), |val| Ok(val.into_owned()))
                .unwrap()
                .unwrap(),
            StoreSerialized("A".to_string())
        );
        assert_eq!(
            tree.with_value(&store, TestKey(1), |val| Ok(val.into_owned()))
                .unwrap()
                .unwrap(),
            StoreSerialized("B".to_string())
        );
        assert_eq!(
            tree.with_value(&store, TestKey(2), |val| Ok(val.into_owned()))
                .unwrap()
                .unwrap(),
            StoreSerialized("C".to_string())
        );
    }

    /// Assert node structure and values in tree are equal.
    fn assert_trees_eq<K: Debug, V: Loadable + Clone + PartialEq + Debug>(
        loader: &impl BlobStoreLoad,
        tree1: &LFMBTree<K, V>,
        tree2: &LFMBTree<K, V>,
        context: String,
    ) {
        match (&tree1.inner, &tree2.inner) {
            (LFMBTreeInner::Empty, LFMBTreeInner::Empty) => {
                // equal
            }
            (
                LFMBTreeInner::NonEmpty(size1, subtree1),
                LFMBTreeInner::NonEmpty(size2, subtree2),
            ) => {
                assert_eq!(size1, size2);
                assert_subtrees_eq(loader, subtree1, subtree2, context.clone());
            }
            (_, _) => {
                panic!("{}: trees not equal: {:?}, {:?}", context, tree1, tree2);
            }
        }
    }

    /// Assert node structure and values in subtree are equal.
    fn assert_subtrees_eq<V: Loadable + Clone + PartialEq + Debug>(
        loader: &impl BlobStoreLoad,
        subtree1: &Subtree<V>,
        subtree2: &Subtree<V>,
        context: String,
    ) {
        match (subtree1, subtree2) {
            (Subtree::Leaf(val_ref1), Subtree::Leaf(val_ref2)) => {
                let val1 = val_ref1.clone_or_load_value(loader).unwrap();
                let val2 = val_ref2.clone_or_load_value(loader).unwrap();
                assert_eq!(val1, val2, "{}: leaf value", context);
            }
            (
                Subtree::Node(height1, left_ref1, right_ref1),
                Subtree::Node(height2, left_ref2, right_ref2),
            ) => {
                assert_eq!(height1, height2);
                let left1 = left_ref1.clone_or_load_value(loader).unwrap();
                let right1 = right_ref1.clone_or_load_value(loader).unwrap();
                let left2 = left_ref2.clone_or_load_value(loader).unwrap();
                let right2 = right_ref2.clone_or_load_value(loader).unwrap();
                assert_subtrees_eq(loader, &left1, &left2, context.clone());
                assert_subtrees_eq(loader, &right1, &right2, context.clone());
            }
            (_, _) => {
                panic!("subtrees not equal: {:?}, {:?}", subtree1, subtree2);
            }
        }
    }
}
