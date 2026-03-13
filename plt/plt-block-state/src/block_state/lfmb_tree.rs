use crate::block_state::blob_reference::hashed_buffered_reference::HashedCacheableRef;

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
