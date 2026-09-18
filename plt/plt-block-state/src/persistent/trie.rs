//! Representation of an immutable trie.
//!
//! See [`Trie`].

mod path;

use crate::failure::{BlockStateFailure, BlockStateResult};
use crate::persistent::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::persistent::blob_store::{
    BlobStoreLoad, BlobStoreMovable, BlobStoreStore, Loadable, ParseResultExt, Storable,
    StoreSerialized,
};
use crate::persistent::cacheable::Cacheable;
use crate::persistent::hash::Hashable;
use crate::utils::Cow;
use concordium_base::common::{Buffer, Deserial, Get, ParseResult, Put, ReadBytesExt, Serial};
use concordium_base::hashes::Hash;
use sha2::Digest;
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt::Debug;
use std::io::Read;
use std::marker::PhantomData;
use tinyvec::{TinyVec, tiny_vec};

/// Representation of an immutable trie with values of type `V`.
/// The represented trie is immutable in the sense that the trie and its values does not change,
/// once it has been created. When entries are inserted, updated or deleted, a new trie is created,
/// reusing the nodes that have not changed by the operation.
/// Keys must allow converting to a type that allows borrowing a byte slice (`&[u8]`) that represents the
/// key, and convert back again from a byte slice. See the trait [`TrieKey`]. Keys of length up to
/// `INLINE_KEY_LENGTH` are stored "inline" and are not heap allocated.
///
/// The operations supported for creating new tries are:
///
/// * Create empty trie with [`Trie::empty`]: Returns a new empty trie.
/// * Insert or update a value with [`Trie::insert_or_update_entry`]: Returns the new trie with
///   the inserted or updated value.
/// * Delete an entry with [`Trie::delete_entry`]: Returns the new trie with the entry removed.
///
/// ## Interior mutability
///
/// The internal representation in the trie may change during the lifetime via interior mutability.
/// This happens if values are cached, stored or hashes are lazily calculated.
///
/// ## Data structure and invariants
///
/// The data structure is a compact trie. The stems are maximal, which means that each
/// node either has a value, or at least two children (except for the root node)
/// ```
#[derive(Debug)]
pub struct Trie<const INLINE_KEY_LENGTH: usize, K, V> {
    size: u64,
    root: Node<INLINE_KEY_LENGTH, V>,
    _key_type: PhantomData<K>,
}

impl<const INLINE_KEY_LENGTH: usize, K, V> Clone for Trie<INLINE_KEY_LENGTH, K, V>
where
    V: Clone,
{
    fn clone(&self) -> Self {
        Self {
            size: self.size,
            root: self.root.clone(),
            _key_type: self._key_type,
        }
    }
}

impl<const INLINE_KEY_LENGTH: usize, K, V> Default for Trie<INLINE_KEY_LENGTH, K, V> {
    fn default() -> Self {
        Self::empty()
    }
}

/// Trait implemented by trie keys, which allows them to be bijectively mapped
/// to byte arrays or slices.
pub trait TrieKey {
    /// Map key to bytes
    fn to_bytes(&self) -> impl Borrow<[u8]>;

    /// Map bytes to key
    fn try_from_bytes(key: &[u8]) -> BlockStateResult<Self>
    where
        Self: Sized;
}

impl TrieKey for Vec<u8> {
    fn to_bytes(&self) -> impl Borrow<[u8]> {
        self.as_slice()
    }

    fn try_from_bytes(key: &[u8]) -> BlockStateResult<Self>
    where
        Self: Sized,
    {
        Ok(key.to_vec())
    }
}

impl<const N: usize> TrieKey for TinyVec<[u8; N]> {
    fn to_bytes(&self) -> impl Borrow<[u8]> {
        self.as_slice()
    }

    fn try_from_bytes(key: &[u8]) -> BlockStateResult<Self>
    where
        Self: Sized,
    {
        Ok(key.into())
    }
}

impl<const N: usize> TrieKey for [u8; N] {
    fn to_bytes(&self) -> impl Borrow<[u8]> {
        *self
    }

    fn try_from_bytes(key: &[u8]) -> BlockStateResult<Self>
    where
        Self: Sized,
    {
        Self::try_from(key).map_err(|err| {
            BlockStateFailure::Invariant(format!("Byte array trie key invalid: {}", err))
        })
    }
}

impl<const INLINE_KEY_LENGTH: usize, K, V> Trie<INLINE_KEY_LENGTH, K, V> {
    /// Create an empty trie.
    pub fn empty() -> Self {
        let root = Node {
            children: ChildEdges::default(),
            stem: tiny_vec![],
            value: None,
        };

        Self {
            size: 0,
            root,
            _key_type: PhantomData,
        }
    }

    /// Return the number of entries in the trie.
    pub fn size(&self) -> u64 {
        self.size
    }

    /// Get the value for the given `key` in the trie or `None` if there
    /// is no value for the key.
    ///
    /// # Arguments
    ///
    /// - `loader`: Loader for the blob store the trie is stored in.
    /// - `key`: The key to access the value for.
    ///
    /// # Errors
    ///
    /// Returns [`BlockStateFailure`] if decoding data from the
    /// blob store fails, or if the trie does not fulfill
    /// the expected invariants (this can happen if the blob store is corrupted in some way).
    pub fn lookup_value(
        &self,
        loader: &impl BlobStoreLoad,
        key: &K,
    ) -> BlockStateResult<Option<Cow<'_, V>>>
    where
        K: TrieKey,
        V: Loadable,
    {
        let key_bytes = key.to_bytes();
        let scan_return = Cow::Borrowed(&self.root).scan_rec(loader, key_bytes.borrow())?;
        Ok(match scan_return.matched {
            ScanMatch::FullMatch { .. } if scan_return.path_split_from_matched_node.is_empty() => {
                scan_return
                    .prefix_matched_node
                    .map(|node| node.value, |node| &node.value)
                    .transpose()
            }
            _ => None,
        })
    }

    /// Returns whether there exist an entry with the given `key` in the trie.
    ///
    /// # Arguments
    ///
    /// - `loader`: Loader for the blob store the trie is stored in.
    /// - `key`: The key to access the value for.
    ///
    /// # Errors
    ///
    /// Returns [`BlockStateFailure`] if decoding data from the
    /// blob store fails, or if the trie does not fulfill
    /// the expected invariants (this can happen if the blob store is corrupted in some way).
    pub fn contains_key(&self, loader: &impl BlobStoreLoad, key: &K) -> BlockStateResult<bool>
    where
        K: TrieKey,
        V: Loadable,
    {
        let key_bytes = key.to_bytes();
        let scan_return = Cow::Borrowed(&self.root).scan_rec(loader, key_bytes.borrow())?;
        Ok(match scan_return.matched {
            ScanMatch::FullMatch { .. } if scan_return.path_split_from_matched_node.is_empty() => {
                scan_return.prefix_matched_node.value.is_some()
            }
            _ => false,
        })
    }

    /// Insert or update the `value` in the trie at the given `key`. Returns
    /// the updated trie.
    ///
    /// Notice that tries are immutable data structures, see [`Self`].
    ///
    /// # Arguments
    ///
    /// - `loader`: Loader for the blob store the trie is stored in.
    /// - `key`: The key to insert the value for.
    /// - `value`: The value to insert.
    ///
    /// # Errors
    ///
    /// Returns [`BlockStateFailure`] if decoding data from the
    /// blob store fails, or if the trie does not fulfill
    /// the expected invariants (this can happen if the blob store is corrupted in some way).
    pub fn insert_or_update_entry(
        &self,
        loader: &impl BlobStoreLoad,
        key: &K,
        value: V,
    ) -> BlockStateResult<Self>
    where
        K: TrieKey,
        V: Loadable + Clone,
    {
        let (new_root, replaced) = self
            .root
            .insert_rec(loader, key.to_bytes().borrow(), value)?;

        let new_size = if replaced { self.size } else { self.size + 1 };

        Ok(Self {
            size: new_size,
            root: new_root,
            _key_type: self._key_type,
        })
    }

    /// Delete the entry with the given key.
    /// Returns `Some` with the updated trie if the key existed, or `None` otherwise.
    ///
    /// Notice that tries are immutable data structures, see [`Self`].
    ///
    /// # Arguments
    ///
    /// - `loader`: Loader for the blob store the trie is stored in.
    /// - `key`: The key to delete the value for.
    ///
    /// # Errors
    ///
    /// Returns [`BlockStateFailure`] if decoding data from the
    /// blob store fails, or if the trie does not fulfill
    /// the expected invariants (this can happen if the blob store is corrupted in some way).
    pub fn delete_entry(
        &self,
        loader: &impl BlobStoreLoad,
        key: &K,
    ) -> BlockStateResult<Option<Self>>
    where
        K: Borrow<[u8]>,
        V: Loadable + Clone,
    {
        if self.size == 0 {
            return Ok(None);
        }
        let Some(new_root) = self.root.delete_rec(loader, key.borrow())? else {
            return Ok(None);
        };

        Ok(Some(Self {
            size: self.size - 1,
            root: new_root,
            _key_type: self._key_type,
        }))
    }

    /// Iterates all entries with keys that have the given `key` as prefix, including
    /// the entry for `key` itself, if it exists. The entries are iterated in
    /// lexicographical order.
    ///
    /// # Arguments
    ///
    /// - `loader`: Loader for the blob store the trie is stored in.
    /// - `key`: The key to iterate entries
    ///
    /// # Errors
    ///
    /// Returns [`BlockStateFailure`] if decoding data from the blob store fails, or if the trie
    /// does not fulfill the expected invariants (this can happen if the blob store is
    /// corrupted in some way).
    pub fn iter_prefix<'a, 'b, L: BlobStoreLoad>(
        &'b self,
        loader: &'a L,
        key: &K,
    ) -> BlockStateResult<
        impl Iterator<Item = BlockStateResult<(K, Cow<'b, V>)>>
        + use<'a, 'b, INLINE_KEY_LENGTH, L, K, V>,
    >
    where
        K: TrieKey,
        V: Loadable,
    {
        let key_bytes = key.to_bytes();
        let path = key_bytes.borrow();
        let scan_return = Cow::Borrowed(&self.root).scan_rec(loader, path)?;
        Ok(match scan_return.matched {
            ScanMatch::FullMatch { stem_matched_node } => {
                if let Some(stem_matched_node) = stem_matched_node {
                    let mut iter_root_path = path
                        .strip_suffix(scan_return.path_split_from_matched_node)
                        .expect("path suffix")
                        .to_vec();
                    iter_root_path.extend_from_slice(&stem_matched_node.stem);
                    PrefixIterator::with_root(iter_root_path, stem_matched_node, loader)
                } else {
                    PrefixIterator::with_root(
                        path.to_vec(),
                        scan_return.prefix_matched_node,
                        loader,
                    )
                }
            }
            ScanMatch::NotFullMatch => PrefixIterator::empty(loader),
        })
    }
}

/// Iterator of for a key prefix.
struct PrefixIterator<'a, 'b, const INLINE_KEY_LENGTH: usize, L, K: TrieKey, V> {
    /// Blob store loader reference
    loader: &'a L,
    /// Stack of next nodes to visit.
    node_stack: Vec<(Vec<u8>, Cow<'b, Node<INLINE_KEY_LENGTH, V>>)>,
    _trie_key: PhantomData<K>,
}

impl<'a, 'b, const INLINE_KEY_LENGTH: usize, L: BlobStoreLoad, K: TrieKey, V>
    PrefixIterator<'a, 'b, INLINE_KEY_LENGTH, L, K, V>
{
    fn empty(loader: &'a L) -> Self {
        Self {
            loader,
            node_stack: vec![],
            _trie_key: PhantomData,
        }
    }

    fn with_root(
        node_path: Vec<u8>,
        node: Cow<'b, Node<INLINE_KEY_LENGTH, V>>,
        loader: &'a L,
    ) -> Self {
        Self {
            loader,
            node_stack: vec![(node_path.to_vec(), node)],
            _trie_key: PhantomData,
        }
    }
}

impl<'a, 'b, const INLINE_KEY_LENGTH: usize, L: BlobStoreLoad, K: TrieKey, V: Loadable> Iterator
    for PrefixIterator<'a, 'b, INLINE_KEY_LENGTH, L, K, V>
{
    type Item = BlockStateResult<(K, Cow<'b, V>)>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((node_path, node)) = self.node_stack.pop() {
            for child_byte in node.children.0.iter().rev().map(|edge| edge.0) {
                let child_edge = match node.cow_project_child_edge(self.loader, child_byte) {
                    Ok(child_edge) => child_edge.expect("child byte exists"),
                    Err(err) => return Some(Err(err)),
                };

                let mut child_path = node_path.clone();
                child_path.extend_from_slice(&child_edge.child.stem);

                self.node_stack.push((child_path, child_edge.child));
            }

            if let Some(value) = node.map(|node| node.value, |node| &node.value).transpose() {
                let key = match K::try_from_bytes(&node_path) {
                    Ok(key) => key,
                    Err(err) => return Some(Err(err)),
                };
                return Some(Ok((key, value)));
            }
        }

        None
    }
}

/// Trie node
#[derive(Debug)]
struct Node<const INLINE_KEY_LENGTH: usize, V> {
    value: Option<V>,
    stem: TinyVec<[u8; INLINE_KEY_LENGTH]>,
    children: ChildEdges<INLINE_KEY_LENGTH, V>,
}

impl<const INLINE_KEY_LENGTH: usize, V> Clone for Node<INLINE_KEY_LENGTH, V>
where
    V: Clone,
{
    fn clone(&self) -> Self {
        Self {
            children: self.children.clone(),
            stem: self.stem.clone(),
            value: self.value.clone(),
        }
    }
}

/// Node children
#[derive(Debug)]
struct ChildEdges<const INLINE_KEY_LENGTH: usize, V>(
    /// Key-value vector, where they key is the first byte in the stem. Invariants:
    ///
    /// * No duplicate keys
    /// * Keys are sorted
    ///
    /// This also means there are at most 256 entries.
    Vec<(u8, Edge<INLINE_KEY_LENGTH, V>)>,
);

impl<const INLINE_KEY_LENGTH: usize, V> ChildEdges<INLINE_KEY_LENGTH, V> {
    fn size(&self) -> u16 {
        self.0.len() as u16
    }

    fn get(&self, byte: u8) -> Option<&Edge<INLINE_KEY_LENGTH, V>> {
        let index = self.0.binary_search_by_key(&byte, |(byte, _)| *byte).ok()?;
        Some(&self.0[index].1)
    }

    fn set(&mut self, byte: u8, edge: Edge<INLINE_KEY_LENGTH, V>) {
        match self.0.binary_search_by_key(&byte, |(byte, _)| *byte) {
            Ok(index) => {
                self.0[index].1 = edge;
            }
            Err(index) => {
                self.0.insert(index, (byte, edge));
            }
        }
    }

    fn delete(&mut self, byte: u8) -> bool {
        let Ok(index) = self.0.binary_search_by_key(&byte, |(byte, _)| *byte) else {
            return false;
        };
        self.0.remove(index);
        true
    }
}

impl<const INLINE_KEY_LENGTH: usize, V> Clone for ChildEdges<INLINE_KEY_LENGTH, V> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<const INLINE_KEY_LENGTH: usize, V> Default for ChildEdges<INLINE_KEY_LENGTH, V> {
    fn default() -> Self {
        Self(Vec::default())
    }
}

/// Trie child edge
#[derive(Debug)]
struct Edge<const INLINE_KEY_LENGTH: usize, V> {
    child_ref: HashedCacheableRef<Node<INLINE_KEY_LENGTH, V>>,
}

/// [`Edge`] with [`Cow`] structurally projected.
/// Used as return value for [`Cow<Node>::cow_project_child_edge`].
#[derive(Debug)]
struct EdgeCowProjection<'b, const INLINE_KEY_LENGTH: usize, V> {
    child: Cow<'b, Node<INLINE_KEY_LENGTH, V>>,
}

impl<const INLINE_KEY_LENGTH: usize, V> Clone for Edge<INLINE_KEY_LENGTH, V> {
    fn clone(&self) -> Self {
        Self {
            child_ref: self.child_ref.clone(),
        }
    }
}

/// Return value from scanning a path in the trie.
#[derive(Debug)]
struct ScanReturn<'a, 'b, const INLINE_KEY_LENGTH: usize, V> {
    /// Node fully or partially (maximally) matched by path.
    prefix_matched_node: Cow<'b, Node<INLINE_KEY_LENGTH, V>>,
    /// The path remaining when removing prefix matching `matched_node`.
    path_split_from_matched_node: &'a [u8],
    /// Whether full or partial match.
    matched: ScanMatch<'b, INLINE_KEY_LENGTH, V>,
}

#[derive(Debug)]
enum ScanMatch<'b, const INLINE_KEY_LENGTH: usize, V> {
    /// Entire path was found in trie (ending either in a node or in a stem).
    FullMatch {
        /// Node that sit on the stem from `matched_node` that the path follows.
        /// (or `None` if match ends at the node `prefix_matched_node` and `path_split_from_matched_node` is empty).
        stem_matched_node: Option<Cow<'b, Node<INLINE_KEY_LENGTH, V>>>,
    },
    /// Only part of path was found in trie,
    NotFullMatch,
}

fn common_prefix<'a>(a: &'a [u8], b: &[u8]) -> &'a [u8] {
    let mut i = 0;
    while i < a.len() && i < b.len() && a[i] == b[i] {
        i += 1;
    }
    &a[0..i]
}

impl<const INLINE_KEY_LENGTH: usize, V> Node<INLINE_KEY_LENGTH, V> {
    /// Delete the entry at `path` and restore path compression.
    /// Returns `Some` with the updated node if the entry existed, or `None` otherwise.
    fn delete_rec(
        &self,
        loader: &impl BlobStoreLoad,
        path: &[u8],
    ) -> BlockStateResult<Option<Node<INLINE_KEY_LENGTH, V>>>
    where
        V: Loadable + Clone,
    {
        let Some(&path_byte) = path.first() else {
            if self.value.is_none() {
                return Ok(None);
            }

            // The node to delete has been found. We propagate an update signal upwards.
            let new_node = Node {
                stem: self.stem.clone(),
                value: None,
                children: self.children.clone(),
            };
            return Ok(Some(new_node));
        };

        let Some(edge) = self.children.get(path_byte) else {
            // The node to delete was not found. We signal that no update should occur.
            return Ok(None);
        };

        let child_node = edge.child_ref.value(loader)?;
        // Find the common prefix of the remaining path and the stem of the selected child. We skip
        // the first byte here, as that has already been used to select the child node above.
        let common_prefix_len = common_prefix(&path[1..], &child_node.stem[1..]).len() + 1;

        match common_prefix_len.cmp(&child_node.stem.len()) {
            // The child node is either a step on the path or the end destination
            Ordering::Equal => {
                let deletion = child_node.delete_rec(loader, &path[child_node.stem.len()..])?;
                let Some(mut new_child) = deletion else {
                    return Ok(None);
                };

                let mut new_node = self.clone();
                if new_child.value.is_none() && new_child.children.0.is_empty() {
                    // The deleted entry left an empty node. Remove its edge.
                    new_node.children.delete(path_byte);
                    return Ok(Some(new_node));
                }

                if new_child.value.is_none() && new_child.children.size() == 1 {
                    // A non-value node with one child can be compressed into that child.
                    let only_edge = &new_child.children.0[0].1;
                    let grandchild = only_edge.child_ref.value(loader)?;
                    new_child = Node {
                        stem: new_child
                            .stem
                            .into_iter()
                            .chain(grandchild.stem.iter().copied())
                            .collect(),
                        value: grandchild.value.clone(),
                        children: grandchild.children.clone(),
                    };
                }

                let edge = Edge {
                    child_ref: HashedCacheableRef::new(new_child),
                };
                new_node.children.set(path_byte, edge);
                Ok(Some(new_node))
            }
            // The node does not exist in the trie: the key path ends inside the child stem
            Ordering::Less => Ok(None),
            // The common prefix cannot be longer than the stem of the node used in the original
            // comparison.
            Ordering::Greater => unreachable!(),
        }
    }

    /// Insert the given value at the given path. If a value already exists at the path,
    /// it is replaced. Returns the updated node and a boolean indicating if a replacement took place.
    fn insert_rec(
        &self,
        loader: &impl BlobStoreLoad,
        path: &[u8],
        value: V,
    ) -> BlockStateResult<(Node<INLINE_KEY_LENGTH, V>, bool)>
    where
        V: Loadable + Clone,
    {
        let Some(&path_byte) = path.first() else {
            // Replace existing value.
            let new_node = Node {
                stem: self.stem.clone(),
                children: self.children.clone(),
                value: Some(value),
            };

            return Ok((new_node, self.value.is_some()));
        };

        let Some(edge) = self.children.get(path_byte) else {
            // Insert new child in the node.
            let child_node = Node {
                stem: path.into(),
                children: ChildEdges::default(),
                value: Some(value),
            };

            let mut new_node = self.clone();

            new_node.children.set(
                path_byte,
                Edge {
                    child_ref: HashedCacheableRef::new(child_node),
                },
            );

            return Ok((new_node, false));
        };

        let child_node = edge.child_ref.value(loader)?;
        let common_prefix_len = common_prefix(&path[1..], &child_node.stem[1..]).len() + 1;

        Ok(
            match (
                common_prefix_len.cmp(&child_node.stem.len()),
                common_prefix_len.cmp(&path.len()),
            ) {
                (Ordering::Equal, _) => {
                    // Insert in child node.
                    let (new_child_node, replaced) =
                        child_node.insert_rec(loader, &path[child_node.stem.len()..], value)?;

                    let mut new_node = self.clone();

                    new_node.children.set(
                        path_byte,
                        Edge {
                            child_ref: HashedCacheableRef::new(new_child_node),
                        },
                    );

                    (new_node, replaced)
                }
                (Ordering::Less, Ordering::Equal) => {
                    // Insert in stem.
                    let mut stem_node = Node {
                        stem: child_node.stem[..common_prefix_len].into(),
                        children: ChildEdges::default(),
                        value: Some(value),
                    };

                    let new_child_node = Node {
                        stem: child_node.stem[common_prefix_len..].into(),
                        children: child_node.children.clone(),
                        value: child_node.value.clone(),
                    };

                    stem_node.children.set(
                        child_node.stem[common_prefix_len],
                        Edge {
                            child_ref: HashedCacheableRef::new(new_child_node),
                        },
                    );

                    let mut new_node = self.clone();

                    new_node.children.set(
                        path_byte,
                        Edge {
                            child_ref: HashedCacheableRef::new(stem_node),
                        },
                    );

                    (new_node, false)
                }
                (Ordering::Less, Ordering::Less) => {
                    // Insert as child branching out from the stem.
                    let mut stem_node = Node {
                        stem: child_node.stem[..common_prefix_len].into(),
                        children: ChildEdges::default(),
                        value: None,
                    };

                    let new_child_node = Node {
                        stem: child_node.stem[common_prefix_len..].into(),
                        children: child_node.children.clone(),
                        value: child_node.value.clone(),
                    };

                    stem_node.children.set(
                        child_node.stem[common_prefix_len],
                        Edge {
                            child_ref: HashedCacheableRef::new(new_child_node),
                        },
                    );
                    let branching_child_node = Node {
                        stem: path[common_prefix_len..].into(),
                        children: ChildEdges::default(),
                        value: Some(value),
                    };
                    stem_node.children.set(
                        path[common_prefix_len],
                        Edge {
                            child_ref: HashedCacheableRef::new(branching_child_node),
                        },
                    );

                    let mut new_node = self.clone();

                    new_node.children.set(
                        path_byte,
                        Edge {
                            child_ref: HashedCacheableRef::new(stem_node),
                        },
                    );

                    (new_node, false)
                }
                (_, _) => unreachable!(),
            },
        )
    }
}

impl<'b, const INLINE_KEY_LENGTH: usize, V> Cow<'b, Node<INLINE_KEY_LENGTH, V>> {
    /// Scan the trie for the given path and return information about where the path ends
    /// in the trie.
    fn scan_rec<'a>(
        self,
        loader: &impl BlobStoreLoad,
        path: &'a [u8],
    ) -> BlockStateResult<ScanReturn<'a, 'b, INLINE_KEY_LENGTH, V>>
    where
        V: Loadable,
    {
        let Some(&path_byte) = path.first() else {
            // Path matched fully
            return Ok(ScanReturn {
                prefix_matched_node: self,
                path_split_from_matched_node: &[],
                matched: ScanMatch::FullMatch {
                    stem_matched_node: None,
                },
            });
        };

        let Some(edge) = self.cow_project_child_edge(loader, path_byte)? else {
            // Path matched up until node, by does not match the start of any child stems.
            return Ok(ScanReturn {
                prefix_matched_node: self,
                path_split_from_matched_node: path,
                matched: ScanMatch::NotFullMatch,
            });
        };

        let common_prefix_len = common_prefix(&path[1..], &edge.child.stem[1..]).len() + 1;

        Ok(
            match (
                common_prefix_len.cmp(&edge.child.stem.len()),
                common_prefix_len.cmp(&path.len()),
            ) {
                (Ordering::Equal, _) => {
                    // Path matched node and the full stem.
                    let path_split = &path[edge.child.stem.len()..];
                    edge.child.scan_rec(loader, path_split)?
                }
                (Ordering::Less, Ordering::Equal) => {
                    // Path fully matched node and part of the child stem.
                    let stem_matched_node = Some(edge.child);
                    ScanReturn {
                        prefix_matched_node: self,
                        path_split_from_matched_node: path,
                        matched: ScanMatch::FullMatch { stem_matched_node },
                    }
                }
                (Ordering::Less, Ordering::Less) => {
                    // Path matched node and partly the child stem.
                    ScanReturn {
                        prefix_matched_node: self,
                        path_split_from_matched_node: path,
                        matched: ScanMatch::NotFullMatch,
                    }
                }
                _ => unreachable!(),
            },
        )
    }

    /// Return the child node with stem starting with given byte in a `Cow` (if the child exists).
    /// If the node reference is `Owned`, and
    /// `HashedCacheableRef::value(child_ref)` returns a borrowed value, `bind_child_edge` returns
    /// the error [`BlockStateFailure::CowJoin`]. See [`Cow<HashedCacheableRef>::bind_value`]
    /// for further details.
    pub fn cow_project_child_edge(
        &self,
        loader: &impl BlobStoreLoad,
        byte: u8,
    ) -> BlockStateResult<Option<EdgeCowProjection<'b, INLINE_KEY_LENGTH, V>>>
    where
        V: Loadable,
    {
        Ok(Some(match self {
            Cow::Owned(node) => {
                let edge = match node.children.get(byte) {
                    Some(edge) => edge,
                    None => return Ok(None),
                };

                match edge.child_ref.value(loader)? {
                    Cow::Owned(child) => EdgeCowProjection {
                        child: Cow::Owned(child),
                    },
                    Cow::Borrowed(_) => {
                        return Err(BlockStateFailure::CowJoin("child in trie::Node"));
                    }
                }
            }
            Cow::Borrowed(node) => {
                let edge = match node.children.get(byte) {
                    Some(edge) => edge,
                    None => return Ok(None),
                };

                EdgeCowProjection {
                    child: edge.child_ref.value(loader)?,
                }
            }
        }))
    }
}

struct TinyVecSerial<'a, const INLINE_KEY_LENGTH: usize>(&'a TinyVec<[u8; INLINE_KEY_LENGTH]>);

impl<'a, const INLINE_KEY_LENGTH: usize> Serial for TinyVecSerial<'a, INLINE_KEY_LENGTH> {
    fn serial<B: Buffer>(&self, out: &mut B) {
        out.put(self.0.len() as u64);
        out.write_all(self.0)
            .expect("Writing to a buffer should not fail.");
    }
}

struct TinyVecDeserial<const INLINE_KEY_LENGTH: usize>(TinyVec<[u8; INLINE_KEY_LENGTH]>);

impl<const INLINE_KEY_LENGTH: usize> Deserial for TinyVecDeserial<INLINE_KEY_LENGTH> {
    fn deserial<R: ReadBytesExt>(source: &mut R) -> ParseResult<Self> {
        let size: u64 = source.get()?;
        let mut vec = TinyVec::with_initial_len(size as usize);
        source.read_exact(&mut vec)?;
        Ok(TinyVecDeserial(vec))
    }
}

impl<const INLINE_KEY_LENGTH: usize, K, V: Loadable> Loadable for Trie<INLINE_KEY_LENGTH, K, V> {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> BlockStateResult<Self> {
        let size = buffer.get().map_parse_err_to_block_state_err()?;
        let root = Loadable::load_from_buffer(buffer, loader)?;

        Ok(Self {
            size,
            root,
            _key_type: PhantomData,
        })
    }
}

impl<const INLINE_KEY_LENGTH: usize, K, V: Storable> Storable for Trie<INLINE_KEY_LENGTH, K, V> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        buffer.put(self.size);
        self.root.store_to_buffer(buffer, storer);
    }
}

impl<const INLINE_KEY_LENGTH: usize, V: Loadable> Loadable for Node<INLINE_KEY_LENGTH, V> {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> BlockStateResult<Self> {
        let value = Loadable::load_from_buffer(&mut buffer, loader)?;
        let stem = <StoreSerialized<TinyVecDeserial<INLINE_KEY_LENGTH>>>::load_from_buffer(
            &mut buffer,
            loader,
        )?
        .0
        .0;
        let children = Loadable::load_from_buffer(&mut buffer, loader)?;

        Ok(Self {
            value,
            stem,
            children,
        })
    }
}

impl<const INLINE_KEY_LENGTH: usize, V: Storable> Storable for Node<INLINE_KEY_LENGTH, V> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.value.store_to_buffer(&mut buffer, storer);
        StoreSerialized(TinyVecSerial(&self.stem)).store_to_buffer(&mut buffer, storer);
        self.children.store_to_buffer(&mut buffer, storer);
    }
}

impl<const INLINE_KEY_LENGTH: usize, V> Loadable for ChildEdges<INLINE_KEY_LENGTH, V> {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> Result<Self, BlockStateFailure> {
        let size: u16 = buffer.get().map_parse_err_to_block_state_err()?;
        let mut children = Vec::with_capacity(size as usize);
        let mut prev_byte = None;
        for _ in 0..size {
            let byte: u8 = buffer.get().map_parse_err_to_block_state_err()?;
            let child_ref = Loadable::load_from_buffer(&mut buffer, loader)?;
            if let Some(prev_byte) = prev_byte
                && byte <= prev_byte
            {
                return Err(BlockStateFailure::Invariant(
                    "Trie node edges not sorted".to_string(),
                ));
            }
            children.push((byte, Edge { child_ref }));
            prev_byte = Some(byte);
        }

        Ok(Self(children))
    }
}

impl<const INLINE_KEY_LENGTH: usize, V: Storable> Storable for ChildEdges<INLINE_KEY_LENGTH, V> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        buffer.put(self.size());
        for (byte, edge) in self.0.iter() {
            buffer.put(*byte);
            edge.child_ref.store_to_buffer(&mut buffer, storer);
        }
    }
}

impl<const INLINE_KEY_LENGTH: usize, K, V: Hashable + Loadable> Hashable
    for Trie<INLINE_KEY_LENGTH, K, V>
{
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        let mut hasher = sha2::Sha256::new();
        hasher.update(self.size.to_be_bytes());
        hasher.update(self.root.hash(loader)?);
        Ok(Hash::new(hasher.finalize().into()))
    }
}

impl<const INLINE_KEY_LENGTH: usize, V: Hashable + Loadable> Hashable
    for Node<INLINE_KEY_LENGTH, V>
{
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        let mut hasher = sha2::Sha256::new();
        if let Some(value) = &self.value {
            hasher.update([1u8]);
            hasher.update(value.hash(loader)?);
        } else {
            hasher.update([0u8]);
        }
        TinyVecSerial(&self.stem).serial(&mut hasher);
        hasher.update(self.children.hash(loader)?);
        Ok(Hash::new(hasher.finalize().into()))
    }
}

impl<const INLINE_KEY_LENGTH: usize, V: Hashable + Loadable> Hashable
    for ChildEdges<INLINE_KEY_LENGTH, V>
{
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        let mut hasher = sha2::Sha256::new();
        hasher.update(self.size().to_be_bytes());
        for (byte, edge) in self.0.iter() {
            hasher.update([*byte]);
            hasher.update(edge.child_ref.hash(loader)?);
        }
        Ok(Hash::new(hasher.finalize().into()))
    }
}

impl<const INLINE_KEY_LENGTH: usize, K, V: Cacheable + Loadable> Cacheable
    for Trie<INLINE_KEY_LENGTH, K, V>
{
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.root.cache_reference_values(loader)
    }
}

impl<const INLINE_KEY_LENGTH: usize, V: Cacheable + Loadable> Cacheable
    for Node<INLINE_KEY_LENGTH, V>
{
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.value.cache_reference_values(loader)?;
        for (_, edge) in &self.children.0 {
            edge.child_ref.cache_reference_values(loader)?;
        }

        Ok(())
    }
}

impl<const INLINE_KEY_LENGTH: usize, K, V: BlobStoreMovable + Loadable + Storable> BlobStoreMovable
    for Trie<INLINE_KEY_LENGTH, K, V>
{
    fn move_blob_store(
        &self,
        from_store: &impl BlobStoreLoad,
        to_store: &mut impl BlobStoreStore,
    ) -> BlockStateResult<Self>
    where
        Self: Sized,
    {
        Ok(Self {
            size: self.size,
            root: self.root.move_blob_store(from_store, to_store)?,
            _key_type: self._key_type,
        })
    }
}

impl<const INLINE_KEY_LENGTH: usize, V: BlobStoreMovable + Loadable + Storable> BlobStoreMovable
    for Node<INLINE_KEY_LENGTH, V>
{
    fn move_blob_store(
        &self,
        from_store: &impl BlobStoreLoad,
        to_store: &mut impl BlobStoreStore,
    ) -> BlockStateResult<Self>
    where
        Self: Sized,
    {
        Ok(Self {
            value: self.value.move_blob_store(from_store, to_store)?,
            stem: self.stem.clone(),
            children: self.children.move_blob_store(from_store, to_store)?,
        })
    }
}

impl<const INLINE_KEY_LENGTH: usize, V: BlobStoreMovable + Loadable + Storable> BlobStoreMovable
    for ChildEdges<INLINE_KEY_LENGTH, V>
{
    fn move_blob_store(
        &self,
        from_store: &impl BlobStoreLoad,
        to_store: &mut impl BlobStoreStore,
    ) -> BlockStateResult<Self>
    where
        Self: Sized,
    {
        let mut children = Vec::with_capacity(self.0.len());
        for (byte, edge) in &self.0 {
            children.push((
                *byte,
                Edge {
                    child_ref: edge.child_ref.move_blob_store(from_store, to_store)?,
                },
            ));
        }

        Ok(Self(children))
    }
}



#[cfg(test)]
mod tests {
    use super::*;
    use crate::persistent::blob_store;
    use crate::persistent::blob_store::StoreSerialized;
    use crate::persistent::blob_store::test_stub::{BlobStoreStub, UnreachableBlobStore};
    use proptest::prelude::*;
    use proptest::sample::select;
    use std::collections::{BTreeMap, HashSet};
    use std::fmt::Debug;

    /// Trie type used by the property based tests. Keys are raw byte vectors
    /// (which borrow as `&[u8]`) and values are `u64`s.
    type TestTrie = Trie<4, Vec<u8>, StoreSerialized<u64>>;

    /// Trie with fixed length keys.
    type FixedKeyTestTrie = Trie<8, [u8; 8], StoreSerialized<u64>>;

    #[derive(Debug)]
    struct TestEntries {
        entries: Vec<(Vec<u8>, u64)>,
        non_existing_keys: HashSet<Vec<u8>>,
    }

    impl TestEntries {
        fn create_plain(&self) -> PlainTrie {
            PlainTrie {
                entries: self.entries.iter().cloned().collect(),
            }
        }

        fn create_trie(&self) -> Result<TestTrie, TestCaseError> {
            let mut trie = TestTrie::empty();
            for (key, value) in self.entries.iter() {
                trie = trie.insert_or_update_entry(
                    &UnreachableBlobStore,
                    key,
                    StoreSerialized(*value),
                )?;
            }
            Ok(trie)
        }
    }

    #[derive(Debug)]
    struct FixedKeyTestEntries {
        entries: Vec<([u8; 8], u64)>,
        non_existing_keys: HashSet<[u8; 8]>,
    }

    impl FixedKeyTestEntries {
        fn create_plain(&self) -> PlainTrie {
            PlainTrie {
                entries: self
                    .entries
                    .iter()
                    .map(|(key, value)| (key.to_vec(), *value))
                    .collect(),
            }
        }

        fn create_trie(&self) -> Result<FixedKeyTestTrie, TestCaseError> {
            let mut trie = FixedKeyTestTrie::empty();
            for (key, value) in self.entries.iter() {
                trie = trie.insert_or_update_entry(
                    &UnreachableBlobStore,
                    key,
                    StoreSerialized(*value),
                )?;
            }
            Ok(trie)
        }
    }

    /// Restrict the bytes we use for keys to this alphabet. This makes it more likely that
    /// keys "overlap". There should be no need to use all `u8` values.
    const ALPHABET: &[u8] = &[0u8, 1, 10, 100, 254, 255];

    prop_compose! {
        fn arb_entries()(
            entries in prop::collection::vec(
                (prop::collection::vec(select(ALPHABET), 0..8), any::<u64>()),
                0..32,
            ),
            non_existing_keys in prop::collection::vec(
                prop::collection::vec(select(ALPHABET), 0..8),
                32,
            ),
        ) -> TestEntries {
            let mut keys: HashSet<_> = entries.iter().map(|(key, _)| key.clone()).collect();

            TestEntries {
                // Keys that are not in entries
                non_existing_keys: non_existing_keys.into_iter().filter(
                    |key| !keys.contains(key)).collect(),
                // Deduplicate entries
                entries: entries.into_iter().filter(|(key, _)| keys.remove(key)).collect(),
            }
        }
    }

    prop_compose! {
        fn arb_fixed_key_entries()(
            entries in prop::collection::vec(
                (prop::array::uniform8(select(ALPHABET)), any::<u64>()),
                0..32,
            ),
            non_existing_keys in prop::collection::vec(
                prop::array::uniform8(select(ALPHABET)),
                32,
            ),
        ) -> FixedKeyTestEntries {
            let mut keys: HashSet<_> = entries.iter().map(|(key, _)| *key).collect();

            FixedKeyTestEntries {
                // Keys that are not in entries
                non_existing_keys: non_existing_keys.into_iter().filter(
                    |key| !keys.contains(key)).collect(),
                // Deduplicate entries
                entries: entries.into_iter().filter(|(key, _)| keys.remove(key)).collect(),
            }
        }
    }

    prop_compose! {
        fn arb_plain_trie()(
            entries in arb_entries()
        ) -> PlainTrie {
            entries.create_plain()
        }
    }

    prop_compose! {
        fn arb_fixed_key_plain_trie()(
            entries in arb_fixed_key_entries()
        ) -> PlainTrie {
            entries.create_plain()
        }
    }

    prop_compose! {
        fn arb_trie()(
            entries in arb_entries()
        ) -> TestTrie {
            entries.create_trie().unwrap()
        }
    }

    prop_compose! {
        fn arb_fixed_key_trie()(
            entries in arb_fixed_key_entries()
        ) -> FixedKeyTestTrie {
            entries.create_trie().unwrap()
        }
    }

    proptest! {
        #[test]
        fn prop_test_size(trie in arb_trie()) {
            prop_assert_eq!(trie.size(), trie.to_plain_validated(&UnreachableBlobStore)?.size());
        }

        #[test]
        fn prop_test_size_fixed_key(trie in arb_fixed_key_trie()) {
            prop_assert_eq!(trie.size(), trie.to_plain_validated(&UnreachableBlobStore)?.size());
        }

        #[test]
        fn prop_test_insert_values(entries in arb_entries()) {
            let mut trie = TestTrie::empty();
            let mut plain = PlainTrie::empty();

            for (key, value) in &entries.entries {
                trie = trie.insert_or_update_entry(&UnreachableBlobStore, key, StoreSerialized(*value))?;
                plain.insert(key, *value);

                prop_assert_eq!(&plain, &trie.to_plain_validated(&UnreachableBlobStore)?);
            }
        }

        #[test]
        fn prop_test_insert_values_fixed_key(entries in arb_fixed_key_entries()) {
            let mut trie = FixedKeyTestTrie::empty();
            let mut plain = PlainTrie::empty();

            for (key, value) in &entries.entries {
                trie = trie.insert_or_update_entry(&UnreachableBlobStore, key, StoreSerialized(*value))?;
                plain.insert(key, *value);

                prop_assert_eq!(&plain, &trie.to_plain_validated(&UnreachableBlobStore)?);
            }
        }

        #[test]
        fn prop_test_update_entry(entries in arb_entries()) {
            let mut trie = entries.create_trie()?;
            let mut plain = entries.create_plain();

            for (key, value) in &entries.entries {
                trie = trie.insert_or_update_entry(&UnreachableBlobStore, key, StoreSerialized(*value + 1))?;
                plain.insert(key, *value + 1);

                prop_assert_eq!(&plain, &trie.to_plain_validated(&UnreachableBlobStore)?);
            }
        }

        #[test]
        fn prop_test_update_entry_fixed_key(entries in arb_fixed_key_entries()) {
            let mut trie = entries.create_trie()?;
            let mut plain = entries.create_plain();

            for (key, value) in &entries.entries {
                trie = trie.insert_or_update_entry(&UnreachableBlobStore, key, StoreSerialized(*value + 1))?;
                plain.insert(key, *value + 1);

                prop_assert_eq!(&plain, &trie.to_plain_validated(&UnreachableBlobStore)?);
            }
        }

        #[test]
        fn prop_test_delete_entry(entries in arb_entries()) {
            let mut trie = entries.create_trie()?;
            let mut plain = entries.create_plain();

            for key in &entries.non_existing_keys {
                prop_assert!(trie.delete_entry(&UnreachableBlobStore, key)?.is_none());
                prop_assert_eq!(&plain, &trie.to_plain_validated(&UnreachableBlobStore)?);
            }

            for (key, _) in &entries.entries {
                let trie_option = trie
                    .delete_entry(&UnreachableBlobStore, key)?;
                prop_assert!(trie_option.is_some());
                trie = trie_option.unwrap();
                plain.delete(key);

                prop_assert_eq!(&plain, &trie.to_plain_validated(&UnreachableBlobStore)?);
            }
        }

        #[test]
        fn prop_test_delete_entry_fixed_key(entries in arb_fixed_key_entries()) {
            let mut trie = entries.create_trie()?;
            let mut plain = entries.create_plain();

            for key in &entries.non_existing_keys {
                prop_assert!(trie.delete_entry(&UnreachableBlobStore, key)?.is_none());
                prop_assert_eq!(&plain, &trie.to_plain_validated(&UnreachableBlobStore)?);
            }

            for (key, _) in &entries.entries {
                let trie_option = trie
                    .delete_entry(&UnreachableBlobStore, key)?;
                prop_assert!(trie_option.is_some());
                trie = trie_option.unwrap();
                plain.delete(key);

                prop_assert_eq!(&plain, &trie.to_plain_validated(&UnreachableBlobStore)?);
            }
        }

        #[test]
        fn prop_test_iter_prefix(entries in arb_entries()) {
            // Test in-memory trie
            let trie = entries.create_trie()?;
            let plain = entries.create_plain();

            for key in &entries.non_existing_keys {
                let entries: Vec<_> = trie.iter_prefix(&UnreachableBlobStore, key)?.map(
                    |res| {
                        let entry = res.unwrap();
                        (entry.0, entry.1.0)
                    }).collect();

                prop_assert_eq!(entries, plain.iter_prefix(key));
            }

            for (key, _) in &entries.entries {
                let entries: Vec<_> = trie.iter_prefix(&UnreachableBlobStore, key)?.map(
                    |res| {
                        let entry = res.unwrap();
                        (entry.0, entry.1.0)
                    }).collect();

                prop_assert_eq!(entries, plain.iter_prefix(key));
            }

            // Test on-disk trie
            let mut store = BlobStoreStub::default();
            let trie = trie.store_and_load(&mut store)?;

            for key in &entries.non_existing_keys {
                let entries: Vec<_> = trie.iter_prefix(&store, key)?.map(
                    |res| {
                        let entry = res.unwrap();
                        (entry.0, entry.1.0)
                    }).collect();

                prop_assert_eq!(entries, plain.iter_prefix(key));
            }

            for (key, _) in &entries.entries {
                let entries: Vec<_> = trie.iter_prefix(&store, key)?.map(
                    |res| {
                        let entry = res.unwrap();
                        (entry.0, entry.1.0)
                    }).collect();

                prop_assert_eq!(entries, plain.iter_prefix(key));
            }
        }

        #[test]
        fn prop_test_iter_prefix_fixed_key(entries in arb_fixed_key_entries()) {
            let trie = entries.create_trie()?;
            let plain = entries.create_plain();

            for key in &entries.non_existing_keys {
                let entries: Vec<_> = trie.iter_prefix(&UnreachableBlobStore, key)?.map(
                    |res| {
                        let entry = res.unwrap();
                        (entry.0.to_vec(), entry.1.into_owned().0)
                    }).collect();

                prop_assert_eq!(entries, plain.iter_prefix(key));
            }

            for (key, _) in &entries.entries {
                let entries: Vec<_> = trie.iter_prefix(&UnreachableBlobStore, key)?.map(
                    |res| {
                        let entry = res.unwrap();
                        (entry.0.to_vec(), entry.1.into_owned().0)
                    }).collect();

                prop_assert_eq!(entries, plain.iter_prefix(key));
            }
        }

        #[test]
        fn prop_test_lookup_value(entries in arb_entries()) {
            // Test in-memory trie
            let trie = entries.create_trie()?;

            for (key, value) in &entries.entries {
                prop_assert_eq!(trie.lookup_value(&UnreachableBlobStore, key)?, Some(Cow::Borrowed(&StoreSerialized(*value))));
            }

            for key in &entries.non_existing_keys {
                prop_assert_eq!(trie.lookup_value(&UnreachableBlobStore, key)?, None);
            }

            // Test on-disk trie
            let mut store = BlobStoreStub::default();
            let trie = trie.store_and_load(&mut store)?;

            for (key, value) in &entries.entries {
                if key.is_empty() {
                    // Root will be borrowed
                    prop_assert_eq!(trie.lookup_value(&store, key)?, Some(Cow::Borrowed(&StoreSerialized(*value))));
                } else {
                    prop_assert_eq!(trie.lookup_value(&store, key)?, Some(Cow::Owned(StoreSerialized(*value))));
                }
            }

            for key in &entries.non_existing_keys {
                prop_assert_eq!(trie.lookup_value(&store, key)?, None);
            }
        }

        #[test]
        fn prop_test_lookup_value_fixed_key(entries in arb_fixed_key_entries()) {
            let trie = entries.create_trie()?;

            for (key, value) in &entries.entries {
                prop_assert_eq!(trie.lookup_value(&UnreachableBlobStore, key)?, Some(Cow::Borrowed(&StoreSerialized(*value))));
            }

            for key in &entries.non_existing_keys {
                prop_assert_eq!(trie.lookup_value(&UnreachableBlobStore, key)?, None);
            }
        }

        #[test]
        fn prop_test_contains(entries in arb_entries()) {
            // Test in-memory trie
            let trie = entries.create_trie()?;

            for (key, _) in &entries.entries {
                prop_assert!(trie.contains_key(&UnreachableBlobStore, key)?);
            }

            for key in &entries.non_existing_keys {
                prop_assert!(!trie.contains_key(&UnreachableBlobStore, key)?);
            }

            // Test on-disk trie
            let mut store = BlobStoreStub::default();
            let trie = trie.store_and_load(&mut store)?;

            for (key, _) in &entries.entries {
                prop_assert!(trie.contains_key(&store, key)?);
            }

            for key in &entries.non_existing_keys {
                prop_assert!(!trie.contains_key(&store, key)?);
            }
        }

        #[test]
        fn prop_test_contains_key_fixed_key(entries in arb_fixed_key_entries()) {
            let trie = entries.create_trie()?;

            for (key, _) in &entries.entries {
                prop_assert!(trie.contains_key(&UnreachableBlobStore, key)?);
            }

            for key in &entries.non_existing_keys {
                prop_assert!(!trie.contains_key(&UnreachableBlobStore, key)?);
            }
        }

        #[test]
        fn prop_test_store_and_load(plain_trie in arb_plain_trie()) {
            let mut store = BlobStoreStub::default();

            // Store trie
            let blob_ref = blob_store::store_to_store(&mut store, &plain_trie.to_trie()?);

            // Load trie
            let trie: TestTrie = blob_store::load_from_store(&store, blob_ref)?;

            // Assert loaded trie is equal to the trie we started with
            prop_assert_eq!(plain_trie, trie.to_plain_validated(&store)?);
        }

        #[test]
        fn prop_test_store_and_load_fixed_key(plain_trie in arb_fixed_key_plain_trie()) {
            let mut store = BlobStoreStub::default();

            // Store trie
            let blob_ref = blob_store::store_to_store(&mut store, &plain_trie.to_fixed_key_trie()?);

            // Load trie
            let trie: FixedKeyTestTrie = blob_store::load_from_store(&store, blob_ref)?;

            // Assert loaded trie is equal to the trie we started with
            prop_assert_eq!(plain_trie, trie.to_plain_validated(&store)?);
        }

        #[test]
        fn prop_test_cache(entries in arb_entries()) {
            let trie = entries.create_trie()?;
            // Put trie in store
            let mut store = BlobStoreStub::default();
            let trie = trie.store_and_load(&mut store)?;

            // Cache trie
            trie.cache_reference_values(&store)?;

            // Lookup values using UnreachableBlobStore as store
            for (key, value) in &entries.entries {
                prop_assert_eq!(trie.lookup_value(&UnreachableBlobStore, key)?, Some(Cow::Borrowed(&StoreSerialized(*value))));
            }

            for key in &entries.non_existing_keys {
                prop_assert_eq!(trie.lookup_value(&UnreachableBlobStore, key)?, None);
            }

            // Iterate entries using UnreachableBlobStore as store
            let plain = trie.to_plain_validated(&store)?;
            for key in &entries.non_existing_keys {
                let entries: Vec<_> = trie.iter_prefix(&UnreachableBlobStore, key)?.map(
                    |res| {
                        let entry = res.unwrap();
                        (entry.0, entry.1.0)
                    }).collect();

                prop_assert_eq!(entries, plain.iter_prefix(key));
            }

            for (key, _) in &entries.entries {
                let entries: Vec<_> = trie.iter_prefix(&UnreachableBlobStore, key)?.map(
                    |res| {
                        let entry = res.unwrap();
                        (entry.0, entry.1.0)
                    }).collect();

                prop_assert_eq!(entries, plain.iter_prefix(key));
            }
        }

        #[test]
        fn prop_test_move_blob_store(entries in arb_entries()) {
            let mut from_store = BlobStoreStub::default();
            let mut to_store = BlobStoreStub::default();

            // Create trie and store it
            let trie = entries.create_trie()?;
            let trie = trie.store_and_load(&mut from_store)?;

            // Migrate the trie
            let moved_trie = trie.move_blob_store(&from_store, &mut to_store)?;
            prop_assert_eq!(moved_trie.to_plain_validated(&to_store)?, entries.create_plain());

            // Store migrated trie
            let moved_trie = moved_trie.store_and_load(&mut to_store)?;
            prop_assert_eq!(moved_trie.to_plain_validated(&to_store)?, entries.create_plain());
        }
    }

    /// Plain in-memory representation that supports semantically comparing if tries contains
    /// the same entries and has the correct representation.
    #[derive(Debug, Eq, PartialEq, Clone)]
    pub struct PlainTrie {
        entries: BTreeMap<Vec<u8>, u64>,
    }

    impl PlainTrie {
        fn empty() -> Self {
            Self {
                entries: Default::default(),
            }
        }

        fn size(&self) -> u64 {
            self.entries.len() as u64
        }

        fn insert(&mut self, key: &[u8], value: u64) {
            self.entries.insert(key.to_vec(), value);
        }

        fn delete(&mut self, key: &[u8]) {
            self.entries.remove(key);
        }

        fn iter_prefix(&self, prefix: &[u8]) -> Vec<(Vec<u8>, u64)> {
            self.entries
                .iter()
                .filter(|(key, _value)| key.starts_with(prefix))
                .map(|(key, value)| (key.clone(), *value))
                .collect()
        }
    }

    impl PlainTrie {
        fn to_trie(&self) -> Result<TestTrie, TestCaseError> {
            let mut trie = TestTrie::empty();
            for (key, value) in &self.entries {
                trie = trie.insert_or_update_entry(
                    &UnreachableBlobStore,
                    key,
                    StoreSerialized(*value),
                )?;
            }
            Ok(trie)
        }

        fn to_fixed_key_trie(&self) -> Result<FixedKeyTestTrie, TestCaseError> {
            let mut trie = FixedKeyTestTrie::empty();
            for (key, value) in &self.entries {
                trie = trie.insert_or_update_entry(
                    &UnreachableBlobStore,
                    &key.as_slice().try_into().expect("key of wrong length"),
                    StoreSerialized(*value),
                )?;
            }
            Ok(trie)
        }
    }

    impl<const INLINE_KEY_LENGTH: usize, K: TrieKey> Trie<INLINE_KEY_LENGTH, K, StoreSerialized<u64>> {
        /// Convert to plain representation and check representation invariants.
        fn to_plain_validated(
            &self,
            loader: &impl BlobStoreLoad,
        ) -> Result<PlainTrie, TestCaseError> {
            let mut entries = BTreeMap::new();

            self.root
                .validate_and_extract_entries(loader, &[], &mut entries, true)?;

            let plain = PlainTrie { entries };

            prop_assert_eq!(self.size, plain.size(), "trie size");

            Ok(plain)
        }

        /// Store in give store and return trie loaded from store.
        fn store_and_load(
            &self,
            store: &mut (impl BlobStoreLoad + BlobStoreStore),
        ) -> Result<Self, TestCaseError> {
            let location = blob_store::store_to_store(store, self);
            let trie = blob_store::load_from_store(store, location)?;
            Ok(trie)
        }
    }

    impl<const INLINE_KEY_LENGTH: usize> Node<INLINE_KEY_LENGTH, StoreSerialized<u64>> {
        /// Convert to plain representation and check representation invariants.
        fn validate_and_extract_entries(
            &self,
            loader: &impl BlobStoreLoad,
            path: &[u8],
            entries: &mut BTreeMap<Vec<u8>, u64>,
            root: bool,
        ) -> Result<(), TestCaseError> {
            prop_assert!(
                self.value.is_some() || self.children.size() > 1 || root,
                "node value or more than one child"
            );

            prop_assert!(!self.stem.is_empty() || root, "node stem not empty or root");

            prop_assert!(self.stem.is_empty() || !root, "node stem empty or not root");

            if let Some(value) = &self.value {
                let existing = entries.insert(path.to_vec(), value.0);
                prop_assert!(existing.is_none(), "existing entry with same key")
            };

            let mut prev_key = None;
            for (key, edge) in self.children.0.iter() {
                let child_node = edge.child_ref.value(loader)?;

                prop_assert!(!child_node.stem.is_empty(), "edge stem not empty");
                prop_assert_eq!(*key, child_node.stem[0], "key matches first byte in stem");

                if let Some(prev_key) = prev_key {
                    prop_assert!(prev_key < *key, "edge keys not ascending")
                }

                let mut child_path = path.to_vec();
                child_path.extend_from_slice(&child_node.stem);
                child_node.validate_and_extract_entries(loader, &child_path, entries, false)?;
                prev_key = Some(*key);
            }

            Ok(())
        }
    }

    /// Assert snapshot of hash of empty trie.
    #[test]
    fn snapshot_test_hash_empty_trie() {
        let trie = TestTrie::empty();
        let hash = trie.hash(&UnreachableBlobStore).unwrap();
        assert_eq!(
            hex::encode(hash.bytes),
            "19ba90b05fe2ffc32d375b67c65e99b30f0492f511e3975ffda16914ea5c0b8b"
        );
    }

    /// Assert snapshot of hash of simple trie.
    #[test]
    fn snapshot_test_hash_simple_trie() {
        let trie = TestTrie::empty()
            .insert_or_update_entry(&UnreachableBlobStore, &vec![0u8, 1u8], StoreSerialized(1))
            .unwrap()
            .insert_or_update_entry(
                &UnreachableBlobStore,
                &vec![0u8, 1u8, 2u8],
                StoreSerialized(2),
            )
            .unwrap()
            .insert_or_update_entry(
                &UnreachableBlobStore,
                &vec![0u8, 1u8, 3u8],
                StoreSerialized(3),
            )
            .unwrap()
            .insert_or_update_entry(
                &UnreachableBlobStore,
                &vec![0u8, 4u8, 4u8],
                StoreSerialized(4),
            )
            .unwrap();

        let hash = trie.hash(&UnreachableBlobStore).unwrap();
        assert_eq!(
            hex::encode(hash.bytes),
            "41739dc9ab8b91987954dcdbba5dccf9a83126d72fa0031660837a056d9694e1"
        );
    }

    /// Store empty trie.
    #[test]
    fn snapshot_test_storage_empty_trie() {
        let mut store = BlobStoreStub::default();

        let trie = TestTrie::empty();

        blob_store::store_to_store(&mut store, &trie);

        assert_eq!(
            hex::encode(store.0),
            "000000000000001300000000000000000000000000000000000000"
        );
    }

    /// Store simple trie.
    #[test]
    fn snapshot_test_storage_simple_trie() {
        let mut store = BlobStoreStub::default();

        let trie = TestTrie::empty()
            .insert_or_update_entry(&UnreachableBlobStore, &vec![0u8, 1u8], StoreSerialized(1))
            .unwrap()
            .insert_or_update_entry(
                &UnreachableBlobStore,
                &vec![0u8, 1u8, 2u8],
                StoreSerialized(2),
            )
            .unwrap()
            .insert_or_update_entry(
                &UnreachableBlobStore,
                &vec![0u8, 1u8, 3u8],
                StoreSerialized(3),
            )
            .unwrap()
            .insert_or_update_entry(
                &UnreachableBlobStore,
                &vec![0u8, 4u8, 4u8],
                StoreSerialized(4),
            )
            .unwrap();

        blob_store::store_to_store(&mut store, &trie);

        assert_eq!(
            hex::encode(store.0),
            "00000000000000140100000000000000020000000000000001020000000000000000001401000000000000000300000000000000010300000000000000000026010000000000000001000000000000000101000202000000000000000003000000000000001c0000000000000015010000000000000004000000000000000204040000000000000000001e000000000000000001000002010000000000000038040000000000000066000000000000001c00000000000000040000000000000000000001000000000000000083"
        );
    }
}
