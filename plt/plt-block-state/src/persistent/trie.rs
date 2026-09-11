//! Representation of an immutable trie.
//!
//! See [`Trie`].

use crate::failure::BlockStateResult;
use crate::persistent::blob_reference::hashed_cacheable_reference::HashedCacheableRef;
use crate::persistent::blob_store::{
    BlobStoreLoad, BlobStoreMovable, BlobStoreStore, Loadable, ParseResultExt, Storable,
    StoreSerialized,
};
use crate::persistent::cacheable::Cacheable;
use crate::persistent::hash;
use crate::persistent::hash::Hashable;
use concordium_base::common::{Buffer, Get, Put};
use concordium_base::hashes::Hash;
use sha2::Digest;
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt::Debug;
use std::io::Read;
use std::marker::PhantomData;

// TODO: use TinyVec instead of Vec for some values?

/// Representation of an immutable trie with values of type `V`.
/// The represented trie is immutable in the sense that the trie and its values does not change,
/// once it has been created. When entries are inserted, updated or deleted, a new trie is created,
/// reusing the nodes that have not changed by the operation.
/// Keys must allow borrowing a byte slice (`&[u8]`) representing it.
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
    root: HashedCacheableRef<Node<V>>, // todo ar remove and use Cow
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
    pub fn lookup_value(&self, loader: &impl BlobStoreLoad, key: K) -> BlockStateResult<Option<V>>
    where
        V: Loadable + Clone,
        K: Borrow<[u8]>,
    {
        Node::lookup_value(&self.root, loader, key.borrow())
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
        key: K,
        value: V,
    ) -> BlockStateResult<Self>
    where
        V: Loadable,
        K: Borrow<[u8]>,
    {
        let (new_root, replaced) = Node::insert_rec(&self.root, loader, key.borrow(), value)?;

        let new_size = if replaced { self.size } else { self.size + 1 };

        Ok(Self {
            size: new_size,
            root: new_root,
            _key_type: self._key_type,
        })
    }
}

/// Trie node
#[derive(Debug)]
struct Node<V> {
    children: Vec<Option<Edge<V>>>, // todo ar replace with filled vec
    terminal_ref: Option<HashedCacheableRef<V>>,
}

impl<V> Clone for Node<V> {
    fn clone(&self) -> Self {
        Self {
            children: self.children.clone(),
            terminal_ref: self.terminal_ref.clone(),
        }
    }
}

/// Trie edge
#[derive(Debug)]
pub struct Edge<V> {
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
    /// The path remaining when removing suffix matching `matched_node`.
    suffix_path_from_matched_node: &'a [u8],
    /// Whether full or partial match.
    matched: ScanMatch<'a>,
    /// Node that site on the stem from `matched_node` that is partially matched.
    partially_matched_node: Option<HashedCacheableRef<Node<V>>>,
}

#[derive(Debug)]
enum ScanMatch<'a> {
    /// Entire path was found in trie.
    FullMatch,
    /// Only part of path was found in trie,
    MaximalNonFullMatch {
        /// Path suffix that was not found in trie (not in any stem)
        suffix_unmatched: &'a [u8],
    },
}

pub fn common_prefix<'a>(a: &'a [u8], b: &[u8]) -> &'a [u8] {
    let mut i = 0;
    while i < a.len() && i < b.len() && a[i] == b[i] {
        i += 1;
    }
    &a[0..i]
}

impl<V> Node<V> {
    fn empty() -> Self {
        Self {
            children: Default::default(),
            terminal_ref: None,
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
            if let Some(edge) = &node_ref.value(loader)?.children[path_byte as usize] {
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
                            suffix_path_from_matched_node: path,
                            matched: ScanMatch::FullMatch,
                            partially_matched_node: Some(edge.target_ref.clone()),
                        },
                        Ordering::Less => ScanReturn {
                            // Path matched node and partly the child stem.
                            prefix_matched_node: node_ref.clone(),
                            suffix_path_from_matched_node: &path[..common_prefix_len],
                            matched: ScanMatch::MaximalNonFullMatch {
                                suffix_unmatched: &path[common_prefix_len..],
                            },
                            partially_matched_node: Some(edge.target_ref.clone()),
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
                    suffix_path_from_matched_node: path,
                    matched: ScanMatch::MaximalNonFullMatch {
                        suffix_unmatched: path,
                    },
                    partially_matched_node: None,
                }
            }
        } else {
            // Path matched fully
            ScanReturn {
                prefix_matched_node: node_ref.clone(),
                suffix_path_from_matched_node: &[],
                matched: ScanMatch::FullMatch,
                partially_matched_node: None,
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
    ) -> BlockStateResult<(HashedCacheableRef<Node<V>>, bool)> {
        let node = node_ref.value(loader)?;
        Ok(if let Some(&path_byte) = path.first() {
            if let Some(edge) = &node_ref.value(loader)?.children[path_byte as usize] {
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

                        new_node.children[path_byte as usize] = Some(Edge {
                            stem: edge.stem.clone(),
                            target_ref: new_child_node,
                        });

                        (HashedCacheableRef::new(new_node), replaced)
                    }
                    Ordering::Less => {
                        // Insert in the child stem.
                        let mut stem_node = Node {
                            children: Default::default(),
                            terminal_ref: Some(HashedCacheableRef::new(value)),
                        };
                        stem_node.children[edge.stem[common_prefix_len] as usize] = Some(Edge {
                            stem: edge.stem[common_prefix_len..].to_vec(),
                            target_ref: edge.target_ref.clone(),
                        });

                        let mut new_node = node.clone();

                        new_node.children[path_byte as usize] = Some(Edge {
                            stem: edge.stem[..common_prefix_len].to_vec(),
                            target_ref: HashedCacheableRef::new(stem_node),
                        });

                        (HashedCacheableRef::new(new_node), false)
                    }
                    Ordering::Greater => {
                        unreachable!()
                    }
                }
            } else {
                // Insert new child in the node.
                let child_node = Node {
                    children: Default::default(),
                    terminal_ref: Some(HashedCacheableRef::new(value)),
                };

                let mut new_node = node.clone();

                new_node.children[path_byte as usize] = Some(Edge {
                    stem: path.to_vec(),
                    target_ref: HashedCacheableRef::new(child_node),
                });

                (HashedCacheableRef::new(new_node), false)
            }
        } else {
            // Replace exising value.
            let new_node = Node {
                children: node.children.clone(),
                terminal_ref: Some(HashedCacheableRef::new(value)),
            };

            (HashedCacheableRef::new(new_node), true)
        })
    }

    /// Delete the value at the given path. Returns the updated node if it was updated.
    /// If no entry exists with the given path, the node is not updated.
    fn delete_rec(
        node_ref: &HashedCacheableRef<Node<V>>,
        loader: &impl BlobStoreLoad,
        path: &[u8],
    ) -> BlockStateResult<Option<HashedCacheableRef<Node<V>>>> {
        let node = node_ref.value(loader)?;
        Ok(if let Some(&path_byte) = path.first() {
            if let Some(edge) = &node_ref.value(loader)?.children[path_byte as usize] {
                let common_prefix_len = common_prefix(&path[1..], &edge.stem[1..]).len() + 1;

                match common_prefix_len.cmp(&edge.stem.len()) {
                    Ordering::Equal => {
                        // Delete in child node.
                        if let Some(new_child_node) =
                            Self::delete_rec(&edge.target_ref, loader, &path[edge.stem.len()..])?
                        {
                            let mut new_node = node.clone();

                            new_node.children[path_byte as usize] = Some(Edge {
                                stem: edge.stem.clone(),
                                target_ref: new_child_node,
                            });

                            Some(HashedCacheableRef::new(new_node))
                        } else {
                            None
                        }
                    }
                    Ordering::Less => {
                        // Entry does not exist.
                        None
                    }
                    Ordering::Greater => {
                        unreachable!()
                    }
                }
            } else {
                // Entry does not exist.
                None
            }
        } else {
            todo!()
            // // Replace exising value.
            // let new_node = Node {
            //     children: node.children.clone(),
            //     terminal: Some(HashedCacheableRef::new(value)),
            // };
            //
            // HashedCacheableRef::new(new_node)
        })
    }

    // todo ar impl delete

    fn lookup_value(
        node_ref: &HashedCacheableRef<Node<V>>,
        loader: &impl BlobStoreLoad,
        path: &[u8],
    ) -> BlockStateResult<Option<V>>
    where
        V: Loadable + Clone,
    {
        let scan_return = Node::scan_rec(node_ref, loader, path)?;
        Ok(match scan_return.matched {
            ScanMatch::FullMatch if scan_return.suffix_path_from_matched_node.is_empty() => {
                if let Some(terminal_ref) =
                    &scan_return.prefix_matched_node.value(loader)?.terminal_ref
                {
                    Some(terminal_ref.value(loader)?.into_owned())
                } else {
                    None
                }
            }
            _ => None,
        })
    }

    fn contains_key(
        node_ref: &HashedCacheableRef<Node<V>>,
        loader: &impl BlobStoreLoad,
        path: &[u8],
    ) -> BlockStateResult<bool>
    where
        V: Loadable + Clone,
    {
        let scan_return = Node::scan_rec(node_ref, loader, path)?;
        Ok(match scan_return.matched {
            ScanMatch::FullMatch if scan_return.suffix_path_from_matched_node.is_empty() => {
                scan_return
                    .prefix_matched_node
                    .value(loader)?
                    .terminal_ref
                    .is_some()
            }
            _ => false,
        })
    }

    // todo ar iterator
}

impl<K, V> Loadable for Trie<K, V> {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> BlockStateResult<Self> {
        let size: u64 = buffer.get().map_parse_err_to_block_state_err()?;

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

impl<V> Loadable for Node<V> {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> BlockStateResult<Self> {
        Ok(Self {
            children: Loadable::load_from_buffer(&mut buffer, loader)?,
            terminal_ref: Loadable::load_from_buffer(&mut buffer, loader)?,
        })
    }
}

impl<V: Storable> Storable for Node<V> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        self.terminal_ref.store_to_buffer(&mut buffer, storer);
        self.children.store_to_buffer(&mut buffer, storer);
    }
}

impl<V> Loadable for Edge<V> {
    fn load_from_buffer(
        mut buffer: impl Read,
        loader: &impl BlobStoreLoad,
    ) -> BlockStateResult<Self> {
        Ok(Self {
            stem: StoreSerialized::load_from_buffer(&mut buffer, loader)?.0,
            target_ref: Loadable::load_from_buffer(&mut buffer, loader)?,
        })
    }
}

impl<V: Storable> Storable for Edge<V> {
    fn store_to_buffer(&self, mut buffer: impl Buffer, storer: &mut impl BlobStoreStore) {
        StoreSerialized(&self.stem).store_to_buffer(&mut buffer, storer);
        self.target_ref.store_to_buffer(&mut buffer, storer);
    }
}

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
            self.terminal_ref.hash(loader)?,
            self.children.hash(loader)?,
        ))
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
        self.terminal_ref.cache_reference_values(loader)?;
        self.children.cache_reference_values(loader)?;

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
            terminal_ref: self.terminal_ref.move_blob_store(from_store, to_store)?,
        })
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
    use crate::persistent::blob_store::StoreSerialized;
    use std::fmt::Debug;

    #[derive(Debug, Clone, Eq, PartialEq)]
    struct TestKey(Vec<u8>);

    type TestTree = Trie<TestKey, StoreSerialized<u64>>;

    // fn create_tree_in_memory(store: &mut impl BlobStoreLoad, size: u64) -> TestTree {
    //     let mut tree = TestTree::empty();
    //     for i in 0..size {
    //         let key;
    //         (key, tree) = tree.insert_value(store, StoreSerialized(i + 10)).unwrap();
    //         assert_eq!(key, TestKey(i));
    //     }
    //     tree
    // }
    //
    // fn store_value<T: Storable + Loadable, S: BlobStoreLoad + BlobStoreStore>(
    //     store: &mut S,
    //     value: &T,
    // ) -> T {
    //     let blob_loc = blob_store::store_to_store(store, value);
    //     blob_store::load_from_store(store, blob_loc).unwrap()
    // }
    //
    // /// Test [`Trie::size`]
    // #[test]
    // fn prop_test_size() {
    //     for i in 0..100 {
    //         let mut store = BlobStoreStub::default();
    //
    //         // Append values to tree
    //         let tree = create_tree_in_memory(&mut store, i);
    //
    //         // Assert size
    //         assert_eq!(tree.size(), i, "get size for tree of size {}", i);
    //     }
    // }
    //
    // /// Test [`Trie::lookup_value`] for a tree that is in memory.
    // #[test]
    // fn prop_test_lookup_value_in_memory() {
    //     for i in 0..100 {
    //         let mut store = BlobStoreStub::default();
    //
    //         // Append values to tree
    //         let tree = create_tree_in_memory(&mut store, i);
    //
    //         // Lookup existing values
    //         for j in 0..i {
    //             assert_eq!(
    //                 tree.lookup_value(&store, TestKey(j)).unwrap().as_deref(),
    //                 Some(&StoreSerialized(j + 10)),
    //                 "access value for key {:?} in tree of size {}",
    //                 TestKey(j),
    //                 i
    //             );
    //         }
    //
    //         // Lookup non-existing values
    //         assert_eq!(
    //             tree.lookup_value(&store, TestKey(i)).unwrap(),
    //             None,
    //             "access non-existing value for key {:?} in tree of size {}",
    //             TestKey(i),
    //             i
    //         );
    //         assert_eq!(
    //             tree.lookup_value(&store, TestKey(i + 1)).unwrap(),
    //             None,
    //             "access non-existing value for key {:?} in tree of size {}",
    //             TestKey(i + 1),
    //             i
    //         );
    //     }
    // }
    //
    // /// Test [`Trie::lookup_value`] for a tree that is in blob store.
    // #[test]
    // fn prop_test_lookup_value_stored() {
    //     for i in 0..100 {
    //         let mut store = BlobStoreStub::default();
    //
    //         // Append values to tree and store it
    //         let tree = create_tree_in_memory(&mut store, i);
    //         let tree = store_value(&mut store, &tree);
    //
    //         // Lookup existing values
    //         for j in 0..i {
    //             assert_eq!(
    //                 tree.lookup_value(&store, TestKey(j)).unwrap().as_deref(),
    //                 Some(&StoreSerialized(j + 10)),
    //                 "access value for key {:?} in tree of size {}",
    //                 TestKey(j),
    //                 i
    //             );
    //         }
    //
    //         // Lookup non-existing values
    //         assert_eq!(
    //             tree.lookup_value(&store, TestKey(i)).unwrap(),
    //             None,
    //             "access non-existing value for key {:?} in tree of size {}",
    //             TestKey(i),
    //             i
    //         );
    //         assert_eq!(
    //             tree.lookup_value(&store, TestKey(i + 1)).unwrap(),
    //             None,
    //             "access non-existing value for key {:?} in tree of size {}",
    //             TestKey(i + 1),
    //             i
    //         );
    //     }
    // }
    //
    // /// Test [`Trie::values`] for a tree that is in memory.
    // #[test]
    // fn prop_test_values_in_memory() {
    //     for i in 0..100 {
    //         let mut store = BlobStoreStub::default();
    //
    //         // Append values to tree
    //         let tree = create_tree_in_memory(&mut store, i);
    //
    //         // Iterate values
    //         let mut values = tree.values(&store);
    //
    //         // Assert values as expected
    //         assert_eq!(
    //             values.len(),
    //             i as usize,
    //             "values length for tree of size {}",
    //             i
    //         );
    //         let mut j = 0;
    //         while let Some(entry_res) = values.next() {
    //             let (key, val) = entry_res.unwrap();
    //             assert_eq!(key, TestKey(j), "key {} in tree of size {}", j, i);
    //             assert_eq!(
    //                 *val,
    //                 StoreSerialized(j + 10),
    //                 "value number {} in tree of size {}",
    //                 j,
    //                 i
    //             );
    //             j += 1;
    //             assert_eq!(values.len(), (i - j) as usize);
    //         }
    //         assert_eq!(values.len(), 0);
    //         assert_eq!(values.next().transpose().unwrap(), None);
    //         assert_eq!(values.len(), 0);
    //     }
    // }
    //
    // /// Test [`Trie::values`] for a tree that is stored in blob store.
    // #[test]
    // fn prop_test_values_stored() {
    //     for i in 0..100 {
    //         let mut store = BlobStoreStub::default();
    //
    //         // Append values to tree
    //         let tree = create_tree_in_memory(&mut store, i);
    //         let tree = store_value(&mut store, &tree);
    //
    //         // Iterate values
    //         let mut values = tree.values(&store);
    //
    //         // Assert values as expected
    //         assert_eq!(
    //             values.len(),
    //             i as usize,
    //             "values length for tree of size {}",
    //             i
    //         );
    //         let mut j = 0;
    //         while let Some(entry_res) = values.next() {
    //             let (key, val) = entry_res.unwrap();
    //             assert_eq!(key, TestKey(j), "key {} in tree of size {}", j, i);
    //             assert_eq!(
    //                 *val,
    //                 StoreSerialized(j + 10),
    //                 "value number {} in tree of size {}",
    //                 j,
    //                 i
    //             );
    //             j += 1;
    //             assert_eq!(values.len(), (i - j) as usize);
    //         }
    //         assert_eq!(values.len(), 0);
    //         assert_eq!(values.next().transpose().unwrap(), None);
    //         assert_eq!(values.len(), 0);
    //     }
    // }
    //
    // /// Test [`Trie::update_value`]
    // #[test]
    // fn prop_test_update_value() {
    //     for i in 0..100 {
    //         let mut store = BlobStoreStub::default();
    //
    //         // Append values to tree
    //         let mut tree = create_tree_in_memory(&mut store, i);
    //
    //         // Update each of the values
    //         for j in 0..i {
    //             // Update the value
    //             tree = tree
    //                 .update_value(&store, TestKey(j), |val| Ok(StoreSerialized(val.0 + 10)))
    //                 .expect("update existing value")
    //                 .unwrap();
    //
    //             // Lookup the value again
    //             assert_eq!(
    //                 tree.lookup_value(&store, TestKey(j)).unwrap().as_deref(),
    //                 Some(&StoreSerialized(j + 20)),
    //                 "update value for key {:?} in tree of size {}",
    //                 TestKey(j),
    //                 i
    //             );
    //         }
    //
    //         // Update non-existing values
    //         assert_matches!(
    //             tree.update_value(&store, TestKey(i), |val| Ok(*val))
    //                 .unwrap(),
    //             None,
    //             "update non-existing value for key {:?} in tree of size {}",
    //             TestKey(i),
    //             i
    //         );
    //         assert_matches!(
    //             tree.update_value(&store, TestKey(i + 1), |val| Ok(*val))
    //                 .unwrap(),
    //             None,
    //             "update non-existing value for key {:?} in tree of size {}",
    //             TestKey(i + 1),
    //             i
    //         );
    //     }
    // }
    //
    // /// Tests storing the tree into the blob store and loading it again.
    // #[test]
    // fn prop_test_store_and_load() {
    //     for i in 0..100 {
    //         let mut store = BlobStoreStub::default();
    //
    //         // Append values to tree
    //         let tree1 = create_tree_in_memory(&mut store, i);
    //
    //         // Store tree
    //         let blob_ref = blob_store::store_to_store(&mut store, &tree1);
    //
    //         // Load tree
    //         let tree2: TestTree = blob_store::load_from_store(&store, blob_ref).unwrap();
    //
    //         // Assert loaded tree is equal to the tree we started with
    //         assert_trees_eq(
    //             &store,
    //             &store,
    //             &tree1,
    //             &tree2,
    //             format!("loaded tree of size {}", i),
    //         );
    //     }
    // }
    //
    // /// Tests moving tree into new blob store
    // #[test]
    // fn prop_test_move_blob_store() {
    //     for i in 0..100 {
    //         let mut from_store = BlobStoreStub::default();
    //         let mut to_store = BlobStoreStub::default();
    //
    //         // Create tree and store it
    //         let tree = create_tree_in_memory(&mut from_store, i);
    //         blob_store::store_to_store(&mut from_store, &tree);
    //
    //         // Migrate the tree and store it
    //         let new_tree = tree.move_blob_store(&from_store, &mut to_store).unwrap();
    //         let new_blob_loc = blob_store::store_to_store(&mut to_store, &new_tree);
    //
    //         // Assert migrated tree is equal to the tree we started with
    //         assert_trees_eq(
    //             &from_store,
    //             &to_store,
    //             &tree,
    //             &new_tree,
    //             format!("loaded tree of size {}", i),
    //         );
    //         drop(new_tree);
    //
    //         // Load migrated tree from destination store
    //         let new_tree2: TestTree = blob_store::load_from_store(&to_store, new_blob_loc).unwrap();
    //
    //         // Assert tree loaded from destination store is equal to the tree we started with
    //         assert_trees_eq(
    //             &from_store,
    //             &to_store,
    //             &tree,
    //             &new_tree2,
    //             format!("loaded tree of size {}", i),
    //         );
    //     }
    // }
    //
    // /// Tests caching tree.
    // #[test]
    // fn prop_test_cache() {
    //     for i in 0..100 {
    //         let mut store = BlobStoreStub::default();
    //         let tree1 = create_tree_in_memory(&mut store, i);
    //         let blob_ref = blob_store::store_to_store(&mut store, &tree1);
    //         let tree2: TestTree = blob_store::load_from_store(&store, blob_ref).unwrap();
    //
    //         // Cache tree
    //         tree2.cache_reference_values(&store).expect("cache");
    //
    //         // Assert cached tree is identical to the tree with started with
    //         assert_trees_eq(
    //             &store,
    //             &store,
    //             &tree1,
    //             &tree2,
    //             format!("cached tree of size {}", i),
    //         );
    //
    //         // Assert that when caching again or looking up entries, we don't need to read from the blob store again.
    //         // We assert that by using UnreachableBlobStore.
    //         tree2
    //             .cache_reference_values(&UnreachableBlobStore)
    //             .expect("cache");
    //         for j in 0..i {
    //             assert_eq!(
    //                 tree2
    //                     .lookup_value(&UnreachableBlobStore, TestKey(j))
    //                     .unwrap()
    //                     .as_deref(),
    //                 Some(&StoreSerialized(j + 10)),
    //                 "lookup value for key {:?} in cached tree of size {}",
    //                 TestKey(j),
    //                 i
    //             );
    //         }
    //         assert_eq!(
    //             tree2
    //                 .lookup_value(&UnreachableBlobStore, TestKey(i))
    //                 .unwrap(),
    //             None,
    //             "lookup non-existing value for key {:?} in cached tree of size {}",
    //             TestKey(i),
    //             i
    //         );
    //     }
    // }
    //
    // /// Assert snapshot of hash of empty tree.
    // /// Hash snapshot must not change and must be equal to Haskell LFMB tree implementation.
    // #[test]
    // fn snapshot_test_hash_empty_tree() {
    //     let store = BlobStoreStub::default();
    //
    //     let tree = Trie::<TestKey, StoreSerialized<String>>::empty();
    //     let hash = tree.hash(&store).unwrap();
    //     assert_eq!(
    //         hex::encode(hash.bytes),
    //         "c423f9e91ee218b2b5303485dd87a3093a653ddb9bdb839d30aa1924de1dbf05"
    //     );
    // }
    //
    // /// Assert snapshot of hash of tree with 3 values A, B, C.
    // /// Hash snapshot must not change and must be equal to Haskell LFMB tree implementation.
    // #[test]
    // fn snapshot_test_hash_simple_tree() {
    //     let store = BlobStoreStub::default();
    //
    //     let tree = Trie::<TestKey, StoreSerialized<String>>::empty();
    //     let tree1 = tree
    //         .insert_value(&store, StoreSerialized("A".to_string()))
    //         .unwrap()
    //         .1;
    //     let tree2 = tree1
    //         .insert_value(&store, StoreSerialized("B".to_string()))
    //         .unwrap()
    //         .1;
    //     let tree3 = tree2
    //         .insert_value(&store, StoreSerialized("C".to_string()))
    //         .unwrap()
    //         .1;
    //     let hash = tree3.hash(&store).unwrap();
    //     assert_eq!(
    //         hex::encode(hash.bytes),
    //         "b9cac19f6048ef301f586e7e0faa6c08b6012d4b100703eef5dc1fcb26c1ecd5"
    //     );
    // }
    //
    // /// Load empty tree from storage bytes fixture.
    // /// The fixture bytes must not change and must be compatible with Haskell LFMB tree implementation.
    // #[test]
    // fn fixture_test_storage_empty_tree() {
    //     let store = BlobStoreStub(hex::decode("00000000000000080000000000000000").unwrap());
    //
    //     let tree: Trie<TestKey, StoreSerialized<String>> =
    //         blob_store::load_from_store(&store, BlobStoreLocation(0)).expect("load tree");
    //     assert_eq!(tree.size(), 0);
    // }
    //
    // /// Load tree with 3 values A, B, C from storage bytes fixture.
    // /// The fixture bytes must not change and must be compatible with Haskell LFMB tree implementation.
    // #[test]
    // fn fixture_test_storage_simple_tree() {
    //     let store = BlobStoreStub(hex::decode("0000000000000009000000000000000141000000000000000900000000000000000000000000000000090000000000000001420000000000000009000000000000000022000000000000001901000000000000000000000000000000110000000000000033000000000000000900000000000000014300000000000000090000000000000000650000000000000021000000000000000301000000000000000100000000000000440000000000000076").unwrap());
    //
    //     let tree: Trie<TestKey, StoreSerialized<String>> =
    //         blob_store::load_from_store(&store, BlobStoreLocation(135)).expect("load tree");
    //     assert_eq!(tree.size(), 3);
    //     assert_eq!(
    //         *tree.lookup_value(&store, TestKey(0)).unwrap().unwrap(),
    //         StoreSerialized("A".to_string())
    //     );
    //     assert_eq!(
    //         *tree.lookup_value(&store, TestKey(1)).unwrap().unwrap(),
    //         StoreSerialized("B".to_string())
    //     );
    //     assert_eq!(
    //         *tree.lookup_value(&store, TestKey(2)).unwrap().unwrap(),
    //         StoreSerialized("C".to_string())
    //     );
    // }
    //
    // /// Assert node structure and values in tree are equal.
    // fn assert_trees_eq<K: Debug, V: Loadable + Clone + PartialEq + Debug>(
    //     loader1: &impl BlobStoreLoad,
    //     loader2: &impl BlobStoreLoad,
    //     tree1: &Trie<K, V>,
    //     tree2: &Trie<K, V>,
    //     context: String,
    // ) {
    //     match (&tree1.inner, &tree2.inner) {
    //         (TrieInner::Empty, TrieInner::Empty) => {
    //             // equal
    //         }
    //         (
    //             TrieInner::NonEmpty(size1, subtree1),
    //             TrieInner::NonEmpty(size2, subtree2),
    //         ) => {
    //             assert_eq!(size1, size2);
    //             assert_subtrees_eq(loader1, loader2, subtree1, subtree2, context.clone());
    //         }
    //         (_, _) => {
    //             panic!("{}: trees not equal: {:?}, {:?}", context, tree1, tree2);
    //         }
    //     }
    // }
    //
    // /// Assert node structure and values in subtree are equal.
    // fn assert_subtrees_eq<V: Loadable + Clone + PartialEq + Debug>(
    //     loader1: &impl BlobStoreLoad,
    //     loader2: &impl BlobStoreLoad,
    //     subtree1: &Node<V>,
    //     subtree2: &Node<V>,
    //     context: String,
    // ) {
    //     match (subtree1, subtree2) {
    //         (Node::Leaf(val_ref1), Node::Leaf(val_ref2)) => {
    //             let val1 = &*val_ref1.value(loader1).unwrap();
    //             let val2 = &*val_ref2.value(loader2).unwrap();
    //             assert_eq!(val1, val2, "{}: leaf value", context);
    //         }
    //         (
    //             Node::Node(height1, left_ref1, right_ref1),
    //             Node::Node(height2, left_ref2, right_ref2),
    //         ) => {
    //             assert_eq!(height1, height2);
    //             let left1 = &*left_ref1.value(loader1).unwrap();
    //             let right1 = &*right_ref1.value(loader1).unwrap();
    //             let left2 = &*left_ref2.value(loader2).unwrap();
    //             let right2 = &*right_ref2.value(loader2).unwrap();
    //             assert_subtrees_eq(loader1, loader2, left1, left2, context.clone());
    //             assert_subtrees_eq(loader1, loader2, right1, right2, context.clone());
    //         }
    //         (_, _) => {
    //             panic!("subtrees not equal: {:?}, {:?}", subtree1, subtree2);
    //         }
    //     }
    // }
}
