//! Representation of an immutable trie.
//!
//! See [`Trie`].

use crate::failure::{BlockStateFailure, BlockStateResult};
use crate::persistent::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::persistent::blob_store::{
    BlobStoreLoad, BlobStoreMovable, BlobStoreStore, Loadable, ParseResultExt, Storable,
    StoreSerialized,
};
use crate::persistent::cacheable::Cacheable;
use crate::persistent::hash;
use crate::persistent::hash::Hashable;
use crate::utils::Cow;
use concordium_base::common::{Buffer, Get, Put};
use concordium_base::hashes::Hash;
use sha2::Digest;
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt::Debug;
use std::io::Read;
use std::marker::PhantomData;
// TODO: use TinyVec instead of Vec<u8> for keys?

/// Representation of an immutable trie with values of type `V`.
/// The represented trie is immutable in the sense that the trie and its values does not change,
/// once it has been created. When entries are inserted, updated or deleted, a new trie is created,
/// reusing the nodes that have not changed by the operation.
/// Keys must allow converting to a type that allows borrowing a byte slice (`&[u8]`) that represents the
/// key, and convert back again from a byte slice. See the trait [`TrieKey`].
///
/// The operations supported for creating new trees are:
///
/// * Create empty tree with [`Trie::empty`]: Returns a new empty tree.
/// * Insert or update a value with [`Trie::insert_or_update_entry`]: Returns the new tree with
///   the inserted or updated value.
/// * Delete a value with [`Trie::delete_value`]: Returns the new tree with
///   the inserted or updated value.
///
/// ## Interior mutability
///
/// The internal representation in the tree may change during the lifetime via interior mutability.
/// This happens if values are cached, stored or hashes are lazily calculated.
///
/// ## Data structure
///
/// TODO
///
/// ### Enforcing invariants
///
/// TODO
///
/// ### Example trie
///
/// TODO
/// ```
#[derive(Debug)]
pub struct Trie<K, V> {
    size: u64,
    root: HashedCacheableRef<Node<V>>,
    _key_type: PhantomData<K>,
}

impl<K, V> Clone for Trie<K, V> {
    fn clone(&self) -> Self {
        Self {
            size: self.size,
            root: self.root.clone(),
            _key_type: self._key_type,
        }
    }
}

impl<K, V> Default for Trie<K, V> {
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

impl<K, V> Trie<K, V> {
    /// Create an empty trie.
    pub fn empty() -> Self {
        let inner = Node::empty();

        Self {
            size: 0,
            root: HashedCacheableRef::new(inner),
            _key_type: PhantomData,
        }
    }

    /// Return the number of entries in the tree.
    pub fn size(&self) -> u64 {
        self.size
    }

    /// Get the value for the given `key` in the trie or `None` if there
    /// is no value for the key.
    ///
    /// # Arguments
    ///
    /// - `loader`: Loader for the blob store the tree is stored in.
    /// - `key`: The key to access the value for.
    ///
    /// # Errors
    ///
    /// Returns [`BlockStateFailure`] if decoding data from the
    /// blob store fails, or if the tree does not fulfill
    /// the expected invariants (this can happen if the blob store is corrupted in some way).
    pub fn lookup_value(&self, loader: &impl BlobStoreLoad, key: &K) -> BlockStateResult<Option<V>>
    where
        V: Loadable + Clone,
        K: TrieKey,
    {
        let key_bytes = key.to_bytes();
        let scan_return = Node::scan_rec(&self.root, loader, key_bytes.borrow())?;
        Ok(match scan_return.matched {
            ScanMatch::FullMatch { .. } if scan_return.path_split_from_matched_node.is_empty() => {
                scan_return
                    .prefix_matched_node
                    .value(loader)?
                    .terminal
                    .clone()
            }
            _ => None,
        })
    }

    /// Returns whether there exist an entry with the given `key` in the trie.
    ///
    /// # Arguments
    ///
    /// - `loader`: Loader for the blob store the tree is stored in.
    /// - `key`: The key to access the value for.
    ///
    /// # Errors
    ///
    /// Returns [`BlockStateFailure`] if decoding data from the
    /// blob store fails, or if the tree does not fulfill
    /// the expected invariants (this can happen if the blob store is corrupted in some way).
    pub fn contains_key(&self, loader: &impl BlobStoreLoad, key: &K) -> BlockStateResult<bool>
    where
        V: Loadable + Clone,
        K: TrieKey,
    {
        let key_bytes = key.to_bytes();
        let scan_return = Node::scan_rec(&self.root, loader, key_bytes.borrow())?;
        Ok(match scan_return.matched {
            ScanMatch::FullMatch { .. } if scan_return.path_split_from_matched_node.is_empty() => {
                scan_return
                    .prefix_matched_node
                    .value(loader)?
                    .terminal
                    .is_some()
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
    /// - `loader`: Loader for the blob store the tree is stored in.
    /// - `key`: The key to insert the value for.
    /// - `value`: The value to insert.
    ///
    /// # Errors
    ///
    /// Returns [`BlockStateFailure`] if decoding data from the
    /// blob store fails, or if the tree does not fulfill
    /// the expected invariants (this can happen if the blob store is corrupted in some way).
    pub fn insert_or_update_entry(
        &self,
        loader: &impl BlobStoreLoad,
        key: &K,
        value: V,
    ) -> BlockStateResult<Self>
    where
        V: Loadable + Clone,
        K: TrieKey,
    {
        let (new_root, replaced) =
            Node::insert_rec(&self.root, loader, key.to_bytes().borrow(), value)?;

        let new_size = if replaced { self.size } else { self.size + 1 };

        Ok(Self {
            size: new_size,
            root: new_root,
            _key_type: self._key_type,
        })
    }

    /// Deletes the entry with the given key if it exists. Returns
    /// the updated trie.
    ///
    /// Notice that tries are immutable data structures, see [`Self`].
    ///
    /// # Arguments
    ///
    /// - `loader`: Loader for the blob store the tree is stored in.
    /// - `key`: The key to delete the value for.
    ///
    /// # Errors
    ///
    /// Returns [`BlockStateFailure`] if decoding data from the
    /// blob store fails, or if the tree does not fulfill
    /// the expected invariants (this can happen if the blob store is corrupted in some way).
    pub fn delete_entry(&self, loader: &impl BlobStoreLoad, key: &K) -> BlockStateResult<Self>
    where
        K: Borrow<[u8]>,
    {
        // todo ar impl delete
        todo!()
    }

    /// Iterates all entries with keys that have the given `key` as prefix, including
    /// the entry for `key` itself, if it exists. The entries are iterated in
    /// lexicographical order.
    ///
    /// # Arguments
    ///
    /// - `loader`: Loader for the blob store the tree is stored in.
    /// - `key`: The key to iterate entries
    ///
    /// # Errors
    ///
    /// Returns [`BlockStateFailure`] if decoding data from the blob store fails, or if the tree
    /// does not fulfill the expected invariants (this can happen if the blob store is
    /// corrupted in some way).
    pub fn iter_prefix(
        &self,
        loader: &impl BlobStoreLoad,
        key: &K,
    ) -> BlockStateResult<impl Iterator<Item = BlockStateResult<(Vec<u8>, V)>>>
    where
        K: TrieKey,
        V: Loadable + Clone,
    {
        let key_bytes = key.to_bytes();
        let scan_return = Node::scan_rec(&self.root, loader, key_bytes.borrow())?;
        Ok(match scan_return.matched {
            ScanMatch::FullMatch { stem_matched_node } => {
                PrefixIterator::with_root(stem_matched_node, loader)
            }
            ScanMatch::NotFullMatch => PrefixIterator::empty(loader),
        })
    }
}

/// Iterator of for a key prefix.
struct PrefixIterator<'a, L, K: TrieKey, V> {
    /// Blob store loader reference
    loader: &'a L,
    /// Stack of next nodes to visit.
    node_ref_stack: Vec<HashedCacheableRef<Node<V>>>,
    _trie_key: PhantomData<K>,
}

impl<'a, L: BlobStoreLoad, K: TrieKey, V> PrefixIterator<'a, L, K, V> {
    fn empty(loader: &'a L) -> Self {
        Self {
            loader,
            node_ref_stack: vec![],
            _trie_key: PhantomData,
        }
    }

    fn with_root(node_ref: HashedCacheableRef<Node<V>>, loader: &'a L) -> Self {
        Self {
            loader,
            node_ref_stack: vec![node_ref],
            _trie_key: PhantomData,
        }
    }
}

impl<'a, L: BlobStoreLoad, K: TrieKey, V: Loadable + Clone> Iterator
    for PrefixIterator<'a, L, K, V>
{
    type Item = BlockStateResult<(K, V)>;

    fn next(&mut self) -> Option<Self::Item> {
        next_rec(self.loader, &mut self.node_ref_stack)
    }
}

fn next_rec<L: BlobStoreLoad, K: TrieKey, V: Loadable + Clone>(
    loader: &L,
    node_ref_stack: &mut Vec<HashedCacheableRef<Node<V>>>,
) -> Option<BlockStateResult<(K, V)>> {
    while let Some(next_node_ref) = node_ref_stack.pop() {
        let next_node = match next_node_ref.value(loader) {
            Ok(next_node) => next_node,
            Err(err) => return Some(Err(err)),
        };

        if let Some(terminal) = next_node.terminal.as_ref() {
            // todo ar key
            let key = match K::try_from_bytes(&[]) {
                Ok(key) => key,
                Err(err) => return Some(Err(err)),
            };
            return Some(Ok((key, terminal.clone())));
        }
    }

    None
}

/// Trie node
#[derive(Debug)]
struct Node<V> {
    children: ChildEdges<V>, // todo ar move stem into node?
    terminal: Option<V>,
}

impl<V> Clone for Node<V>
where
    V: Clone,
{
    fn clone(&self) -> Self {
        Self {
            children: self.children.clone(),
            terminal: self.terminal.clone(),
        }
    }
}

/// Node children
#[derive(Debug)]
struct ChildEdges<V>(Vec<(u8, Edge<V>)>);

impl<V> ChildEdges<V> {
    fn size(&self) -> u16 {
        self.0.len() as u16
    }

    fn get(&self, byte: u8) -> Option<&Edge<V>> {
        let index = self.0.binary_search_by_key(&byte, |(byte, _)| *byte).ok()?;
        Some(&self.0[index].1)
    }

    fn set(&mut self, byte: u8, edge: Edge<V>) {
        match self.0.binary_search_by_key(&byte, |(byte, _)| *byte) {
            Ok(index) => {
                self.0[index].1 = edge;
            }
            Err(index) => {
                self.0.insert(index, (byte, edge));
            }
        }
    }
}

impl<V> Clone for ChildEdges<V> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<V> Default for ChildEdges<V> {
    fn default() -> Self {
        Self(Vec::default())
    }
}

/// Trie edge
#[derive(Debug)]
struct Edge<V> {
    stem: Vec<u8>,
    target_ref: HashedCacheableRef<Node<V>>,
}

impl<V> Clone for Edge<V> {
    fn clone(&self) -> Self {
        Self {
            stem: self.stem.clone(),
            target_ref: self.target_ref.clone(),
        }
    }
}

/// Return value from scanning a path in the trie.
#[derive(Debug)]
struct ScanReturn<'a, V> {
    /// Node fully or partially (maximally) matched by path.
    prefix_matched_node: HashedCacheableRef<Node<V>>,
    /// The path remaining when removing prefix matching `matched_node`.
    path_split_from_matched_node: &'a [u8],
    /// Whether full or partial match.
    matched: ScanMatch<V>,
}

#[derive(Debug)]
enum ScanMatch<V> {
    /// Entire path was found in trie (ending either in a node or in a stem).
    FullMatch {
        /// Node that site on the stem from `matched_node` that path follows
        /// (or at the end of a stem which means it is equal to `prefix_matched_node`).
        stem_matched_node: HashedCacheableRef<Node<V>>,
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

impl<V> Node<V> {
    fn empty() -> Self {
        Self {
            children: ChildEdges::default(),
            terminal: None,
        }
    }

    /// Can the trie for the given path and return information about where the path ends
    /// in the trie.
    fn scan_rec<'a>(
        node_ref: &HashedCacheableRef<Node<V>>,
        loader: &impl BlobStoreLoad,
        path: &'a [u8],
    ) -> BlockStateResult<ScanReturn<'a, V>>
    where
        V: Loadable,
    {
        Ok(if let Some(&path_byte) = path.first() {
            if let Some(edge) = node_ref.value(loader)?.children.get(path_byte) {
                let common_prefix_len = common_prefix(&path[1..], &edge.stem[1..]).len() + 1;

                match common_prefix_len.cmp(&edge.stem.len()) {
                    Ordering::Equal => {
                        // Path matched node and the full stem.
                        Node::scan_rec(&edge.target_ref, loader, &path[edge.stem.len()..])?
                    }
                    Ordering::Less => match common_prefix_len.cmp(&path.len()) {
                        Ordering::Equal => ScanReturn {
                            // Path fully matched node and part of the child stem.
                            prefix_matched_node: node_ref.clone(),
                            path_split_from_matched_node: path,
                            matched: ScanMatch::FullMatch {
                                stem_matched_node: edge.target_ref.clone(),
                            },
                        },
                        Ordering::Less => ScanReturn {
                            // Path matched node and partly the child stem.
                            prefix_matched_node: node_ref.clone(),
                            path_split_from_matched_node: path,
                            matched: ScanMatch::NotFullMatch,
                        },
                        Ordering::Greater => {
                            unreachable!()
                        }
                    },
                    Ordering::Greater => {
                        unreachable!()
                    }
                }
            } else {
                // Path matched up until node, by does not match the start of any child stems.
                ScanReturn {
                    prefix_matched_node: node_ref.clone(),
                    path_split_from_matched_node: path,
                    matched: ScanMatch::NotFullMatch,
                }
            }
        } else {
            // Path matched fully
            ScanReturn {
                prefix_matched_node: node_ref.clone(),
                path_split_from_matched_node: &[],
                matched: ScanMatch::FullMatch {
                    stem_matched_node: node_ref.clone(),
                },
            }
        })
    }

    /// Insert the given value at the given path. If a value already exists at the path,
    /// it is replaced. Returns the updated node and a boolean indicating if a replacement took place.
    fn insert_rec(
        node_ref: &HashedCacheableRef<Node<V>>,
        loader: &impl BlobStoreLoad,
        path: &[u8],
        value: V,
    ) -> BlockStateResult<(HashedCacheableRef<Node<V>>, bool)>
    where
        V: Loadable + Clone,
    {
        let node = node_ref.value(loader)?;
        Ok(if let Some(&path_byte) = path.first() {
            if let Some(edge) = node.children.get(path_byte) {
                let common_prefix_len = common_prefix(&path[1..], &edge.stem[1..]).len() + 1;

                match common_prefix_len.cmp(&edge.stem.len()) {
                    Ordering::Equal => {
                        // Insert in child node.
                        let (new_child_node, replaced) = Self::insert_rec(
                            &edge.target_ref,
                            loader,
                            &path[edge.stem.len()..],
                            value,
                        )?;

                        let mut new_node = node.clone();

                        new_node.children.set(
                            path_byte,
                            Edge {
                                stem: edge.stem.clone(),
                                target_ref: new_child_node,
                            },
                        );

                        (HashedCacheableRef::new(new_node), replaced)
                    }
                    Ordering::Less => {
                        match common_prefix_len.cmp(&path.len()) {
                            Ordering::Equal => {
                                // Insert in stem.
                                let mut stem_node = Node {
                                    children: ChildEdges::default(),
                                    terminal: Some(value),
                                };
                                stem_node.children.set(
                                    edge.stem[common_prefix_len],
                                    Edge {
                                        stem: edge.stem[common_prefix_len..].to_vec(),
                                        target_ref: edge.target_ref.clone(),
                                    },
                                );

                                let mut new_node = node.clone();

                                new_node.children.set(
                                    path_byte,
                                    Edge {
                                        stem: edge.stem[..common_prefix_len].to_vec(),
                                        target_ref: HashedCacheableRef::new(stem_node),
                                    },
                                );

                                (HashedCacheableRef::new(new_node), false)
                            }
                            Ordering::Less => {
                                // Insert as child branching out from the stem.
                                let mut stem_node = Node {
                                    children: ChildEdges::default(),
                                    terminal: None,
                                };
                                stem_node.children.set(
                                    edge.stem[common_prefix_len],
                                    Edge {
                                        stem: edge.stem[common_prefix_len..].to_vec(),
                                        target_ref: edge.target_ref.clone(),
                                    },
                                );
                                let child_node = Node {
                                    children: ChildEdges::default(),
                                    terminal: Some(value),
                                };
                                stem_node.children.set(
                                    path[common_prefix_len],
                                    Edge {
                                        stem: path[common_prefix_len..].to_vec(),
                                        target_ref: HashedCacheableRef::new(child_node),
                                    },
                                );

                                let mut new_node = node.clone();

                                new_node.children.set(
                                    path_byte,
                                    Edge {
                                        stem: edge.stem[..common_prefix_len].to_vec(),
                                        target_ref: HashedCacheableRef::new(stem_node),
                                    },
                                );

                                (HashedCacheableRef::new(new_node), false)
                            }
                            Ordering::Greater => {
                                unreachable!()
                            }
                        }
                    }
                    Ordering::Greater => {
                        unreachable!()
                    }
                }
            } else {
                // Insert new child in the node.
                let child_node = Node {
                    children: ChildEdges::default(),
                    terminal: Some(value),
                };

                let mut new_node = node.clone();

                new_node.children.set(
                    path_byte,
                    Edge {
                        stem: path.to_vec(),
                        target_ref: HashedCacheableRef::new(child_node),
                    },
                );

                (HashedCacheableRef::new(new_node), false)
            }
        } else {
            // Replace exising value.
            let new_node = Node {
                children: node.children.clone(),
                terminal: Some(value),
            };

            (HashedCacheableRef::new(new_node), node.terminal.is_some())
        })
    }
}

impl<'b, V> Cow<'b, Node<V>> {
    /// Return the child with stem starting with given byte, if it exists.
    /// If the node reference is `Owned`, and
    /// `HashedCacheableRef::value(child_ref)` returns a borrowed value, `bind_child` returns
    /// the error [`BlockStateFailure::CowJoin`]. See [`Cow<HashedCacheableRef>::bind_value`]
    /// for further details.
    pub fn bind_child(
        self,
        loader: &impl BlobStoreLoad,
        byte: u8,
    ) -> Option<BlockStateResult<Cow<'b, Node<V>>>>
    where
        V: Loadable,
    {
        match self {
            Cow::Owned(node) => {
                node.children
                    .get(byte)
                    .map(|edge| match edge.target_ref.value(loader)? {
                        Cow::Owned(child) => Ok(Cow::Owned(child)),
                        Cow::Borrowed(_) => Err(BlockStateFailure::CowJoin("child in trie::Node")),
                    })
            }
            Cow::Borrowed(node) => node
                .children
                .get(byte)
                .map(|child| child.target_ref.value(loader)),
        }
    }
}

impl<K, V> Loadable for Trie<K, V> {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> BlockStateResult<Self> {
        let size = buffer.get().map_parse_err_to_block_state_err()?;

        Ok(Self {
            size,
            root: Loadable::load_from_buffer(buffer, loader)?,
            _key_type: PhantomData,
        })
    }
}

impl<K, V: Storable> Storable for Trie<K, V> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        buffer.put(self.size);
        self.root.store_to_buffer(buffer, storer);
    }
}

impl<V: Loadable> Loadable for Node<V> {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> BlockStateResult<Self> {
        Ok(Self {
            children: Loadable::load_from_buffer(&mut buffer, loader)?,
            terminal: Loadable::load_from_buffer(&mut buffer, loader)?,
        })
    }
}

impl<V: Storable> Storable for Node<V> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.children.store_to_buffer(&mut buffer, storer);
        self.terminal.store_to_buffer(&mut buffer, storer);
    }
}

impl<V> Loadable for ChildEdges<V> {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> Result<Self, BlockStateFailure> {
        let size: u16 = buffer.get().map_parse_err_to_block_state_err()?;
        let mut children = Vec::with_capacity(size as usize);
        let mut prev_byte = None;
        for _ in 0..size {
            let edge: Edge<_> = Loadable::load_from_buffer(&mut buffer, loader)?;
            let byte = *edge.stem.first().ok_or_else(|| {
                BlockStateFailure::Invariant("Trie stem of zero length".to_string())
            })?;
            if let Some(prev_byte) = prev_byte
                && byte <= prev_byte
            {
                return Err(BlockStateFailure::Invariant(
                    "Trie node edges not sorted".to_string(),
                ));
            }
            children.push((byte, edge));
            prev_byte = Some(byte);
        }

        Ok(Self(children))
    }
}

impl<V: Storable> Storable for ChildEdges<V> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        buffer.put(self.size());
        for (byte, edge) in self.0.iter() {
            edge.store_to_buffer(&mut buffer, storer);
        }
    }
}

impl<V> Loadable for Edge<V> {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> BlockStateResult<Self> {
        let stem: Vec<_> = StoreSerialized::load_from_buffer(&mut buffer, loader)?.0;
        if stem.is_empty() {
            return Err(BlockStateFailure::Invariant(
                "Trie node stem of zero length".to_string(),
            ));
        }
        let target_ref = Loadable::load_from_buffer(&mut buffer, loader)?;
        Ok(Self { stem, target_ref })
    }
}

impl<V: Storable> Storable for Edge<V> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        StoreSerialized(&self.stem).store_to_buffer(&mut buffer, storer);
        self.target_ref.store_to_buffer(&mut buffer, storer);
    }
}

// todo ar change hash implementation, make hash take a digest instead of returning hash

impl<K, V: Hashable + Loadable> Hashable for Trie<K, V> {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        Ok(hash::hash_of_hashes(
            StoreSerialized(self.size).hash(loader)?,
            self.root.hash(loader)?,
        ))
    }
}

impl<V: Hashable + Loadable> Hashable for Node<V> {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        Ok(hash::hash_of_hashes(
            self.terminal.hash(loader)?,
            self.children.hash(loader)?,
        ))
    }
}

impl<V: Hashable + Loadable> Hashable for ChildEdges<V> {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        let mut hasher = sha2::Sha256::new();
        hasher.update(self.size().to_be_bytes());
        for (_, edge) in self.0.iter() {
            hasher.update(edge.hash(loader)?);
        }

        Ok(Hash::new(hasher.finalize().into()))
    }
}

impl<V: Hashable + Loadable> Hashable for Edge<V> {
    fn hash(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<Hash> {
        Ok(hash::hash_of_hashes(
            self.target_ref.hash(loader)?,
            StoreSerialized(&self.stem).hash(loader)?,
        ))
    }
}

impl<K, V: Cacheable + Loadable> Cacheable for Trie<K, V> {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.root.cache_reference_values(loader)
    }
}

impl<V: Cacheable + Loadable> Cacheable for Node<V> {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.terminal.cache_reference_values(loader)?;
        for (_, edge) in &self.children.0 {
            edge.cache_reference_values(loader)?;
        }

        Ok(())
    }
}

impl<V: Cacheable + Loadable> Cacheable for Edge<V> {
    fn cache_reference_values(&self, loader: &impl BlobStoreLoad) -> BlockStateResult<()> {
        self.target_ref.cache_reference_values(loader)
    }
}

impl<K, V: BlobStoreMovable + Loadable + Storable> BlobStoreMovable for Trie<K, V> {
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

impl<V: BlobStoreMovable + Loadable + Storable> BlobStoreMovable for Node<V> {
    fn move_blob_store(
        &self,
        from_store: &impl BlobStoreLoad,
        to_store: &mut impl BlobStoreStore,
    ) -> BlockStateResult<Self>
    where
        Self: Sized,
    {
        Ok(Self {
            children: self.children.move_blob_store(from_store, to_store)?,
            terminal: self.terminal.move_blob_store(from_store, to_store)?,
        })
    }
}

impl<V: BlobStoreMovable + Loadable + Storable> BlobStoreMovable for ChildEdges<V> {
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
            children.push((*byte, edge.move_blob_store(from_store, to_store)?));
        }

        Ok(Self(children))
    }
}

impl<V: BlobStoreMovable + Loadable + Storable> BlobStoreMovable for Edge<V> {
    fn move_blob_store(
        &self,
        from_store: &impl BlobStoreLoad,
        to_store: &mut impl BlobStoreStore,
    ) -> BlockStateResult<Self>
    where
        Self: Sized,
    {
        Ok(Self {
            stem: self.stem.clone(),
            target_ref: self.target_ref.move_blob_store(from_store, to_store)?,
        })
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
    type TestTrie = Trie<Vec<u8>, StoreSerialized<u64>>;

    /// Trie with fixed length keys.
    type FixedKeyTestTrie = Trie<[u8; 8], StoreSerialized<u64>>;

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
            prop_assert_eq!(trie.size(), trie.to_plain(&UnreachableBlobStore)?.size());
        }

        #[test]
        fn prop_test_size_fixed_key(trie in arb_fixed_key_trie()) {
            prop_assert_eq!(trie.size(), trie.to_plain(&UnreachableBlobStore)?.size());
        }

        #[test]
        fn prop_test_insert_values(entries in arb_entries()) {
            let mut trie = TestTrie::empty();
            let mut plain = PlainTrie::empty();

            for (key, value) in &entries.entries {
                trie = trie.insert_or_update_entry(&UnreachableBlobStore, key, StoreSerialized(*value))?;
                plain.insert(key, *value);

                prop_assert_eq!(&plain, &trie.to_plain(&UnreachableBlobStore)?);
            }
        }

        #[test]
        fn prop_test_insert_values_fixed_key(entries in arb_fixed_key_entries()) {
            let mut trie = FixedKeyTestTrie::empty();
            let mut plain = PlainTrie::empty();

            for (key, value) in &entries.entries {
                trie = trie.insert_or_update_entry(&UnreachableBlobStore, key, StoreSerialized(*value))?;
                plain.insert(key, *value);

                prop_assert_eq!(&plain, &trie.to_plain(&UnreachableBlobStore)?);
            }
        }

        #[test]
        fn prop_test_update_entry(entries in arb_entries()) {
            let mut trie = entries.create_trie()?;
            let mut plain = entries.create_plain();

            for (key, value) in &entries.entries {
                trie = trie.insert_or_update_entry(&UnreachableBlobStore, key, StoreSerialized(*value + 1))?;
                plain.insert(key, *value + 1);

                prop_assert_eq!(&plain, &trie.to_plain(&UnreachableBlobStore)?);
            }
        }

        #[test]
        fn prop_test_update_entry_fixed_key(entries in arb_fixed_key_entries()) {
            let mut trie = entries.create_trie()?;
            let mut plain = entries.create_plain();

            for (key, value) in &entries.entries {
                trie = trie.insert_or_update_entry(&UnreachableBlobStore, key, StoreSerialized(*value + 1))?;
                plain.insert(key, *value + 1);

                prop_assert_eq!(&plain, &trie.to_plain(&UnreachableBlobStore)?);
            }
        }

        #[test]
        #[ignore]
        fn prop_test_delete_entry(entries in arb_entries()) {
            let mut trie = entries.create_trie()?;
            let mut plain = entries.create_plain();

            for key in &entries.non_existing_keys {
                trie = trie.delete_entry(&UnreachableBlobStore, key)?;
                plain.delete(key);

                prop_assert_eq!(&plain, &trie.to_plain(&UnreachableBlobStore)?);
            }

            for (key, _) in &entries.entries {
                trie = trie.delete_entry(&UnreachableBlobStore, key)?;
                plain.delete(key);

                prop_assert_eq!(&plain, &trie.to_plain(&UnreachableBlobStore)?);
            }
        }

        #[test]
        #[ignore]
        fn prop_test_delete_entry_fixed_key(entries in arb_fixed_key_entries()) {
            let mut trie = entries.create_trie()?;
            let mut plain = entries.create_plain();

            for key in &entries.non_existing_keys {
                trie = trie.delete_entry(&UnreachableBlobStore, key)?;
                plain.delete(key);

                prop_assert_eq!(&plain, &trie.to_plain(&UnreachableBlobStore)?);
            }

            for (key, _) in &entries.entries {
                trie = trie.delete_entry(&UnreachableBlobStore, key)?;
                plain.delete(key);

                prop_assert_eq!(&plain, &trie.to_plain(&UnreachableBlobStore)?);
            }
        }

        #[test]
        #[ignore]
        fn prop_test_iter_prefix(entries in arb_entries()) {
            let trie = entries.create_trie()?;
            let mut plain = entries.create_plain();

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
        #[ignore]
        fn prop_test_iter_prefix_fixed_key(entries in arb_fixed_key_entries()) {
            let trie = entries.create_trie()?;
            let mut plain = entries.create_plain();

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
        fn prop_test_lookup_value(entries in arb_entries()) {
            let trie = entries.create_trie()?;

            for (key, value) in &entries.entries {
                prop_assert_eq!(trie.lookup_value(&UnreachableBlobStore, key)?, Some(StoreSerialized(*value)));
            }

            for key in &entries.non_existing_keys {
                prop_assert_eq!(trie.lookup_value(&UnreachableBlobStore, key)?, None);
            }
        }

        #[test]
        fn prop_test_lookup_value_fixed_key(entries in arb_fixed_key_entries()) {
            let trie = entries.create_trie()?;

            for (key, value) in &entries.entries {
                prop_assert_eq!(trie.lookup_value(&UnreachableBlobStore, key)?, Some(StoreSerialized(*value)));
            }

            for key in &entries.non_existing_keys {
                prop_assert_eq!(trie.lookup_value(&UnreachableBlobStore, key)?, None);
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

            // Assert loaded tree is equal to the tree we started with
            prop_assert_eq!(plain_trie, trie.to_plain(&store)?);
        }

        #[test]
        fn prop_test_store_and_load_fixed_key(plain_trie in arb_fixed_key_plain_trie()) {
            let mut store = BlobStoreStub::default();

            // Store trie
            let blob_ref = blob_store::store_to_store(&mut store, &plain_trie.to_fixed_key_trie()?);

            // Load trie
            let trie: TestTrie = blob_store::load_from_store(&store, blob_ref)?;

            // Assert loaded tree is equal to the tree we started with
            prop_assert_eq!(plain_trie, trie.to_plain(&store)?);
        }
    }

    // todo ar test move blob store
    // todo ar test caching
    // todo ar test hashing via plain repr?
    // todo ar snapshots/fixtures

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

        fn iter_prefix(&mut self, prefix: &[u8]) -> Vec<(Vec<u8>, u64)> {
            self.entries
                .iter()
                .filter(|(key, value)| key.starts_with(prefix))
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

    impl<K: TrieKey> Trie<K, StoreSerialized<u64>> {
        /// Convert to plain representation and check representation invariants.
        fn to_plain(&self, loader: &impl BlobStoreLoad) -> Result<PlainTrie, TestCaseError> {
            let mut entries = BTreeMap::new();

            Node::extract_entries(&self.root, loader, &[], &mut entries, true)?;

            let plain = PlainTrie { entries };

            prop_assert_eq!(self.size, plain.size(), "trie size");

            Ok(plain)
        }
    }

    impl Node<u64> {
        /// Convert to plain representation and check representation invariants.
        fn extract_entries(
            node_ref: &HashedCacheableRef<Node<StoreSerialized<u64>>>,
            loader: &impl BlobStoreLoad,
            path: &[u8],
            entries: &mut BTreeMap<Vec<u8>, u64>,
            root: bool,
        ) -> Result<(), TestCaseError> {
            let node = node_ref.value(loader)?;

            prop_assert!(
                node.terminal.is_some() || node.children.size() > 1 || root,
                "node terminal or more than one child"
            );

            if let Some(terminal) = &node.terminal {
                let existing = entries.insert(path.to_vec(), terminal.0);
                prop_assert!(existing.is_none(), "existing entry with same key")
            };

            let mut prev_key = None;
            for (key, edge) in node.children.0.iter() {
                prop_assert!(!edge.stem.is_empty(), "edge stem not empty");
                prop_assert_eq!(*key, edge.stem[0], "key matches first byte in stem");

                if let Some(prev_key) = prev_key {
                    prop_assert!(prev_key < *key, "edge keys not ascending")
                }

                let mut child_path = path.to_vec();
                child_path.extend(edge.stem.iter().copied());
                Node::extract_entries(&edge.target_ref, loader, &child_path, entries, false)?;
                prev_key = Some(*key);
            }

            Ok(())
        }
    }
}
