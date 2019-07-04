use std::{
    ops::{Deref, Index, IndexMut},
    slice,
};

/// A vector-like collection designed to hold contiguous elements with
/// specific indices.
pub struct IndexedVec<T> {
    inner: Vec<Option<T>>,
}

impl<T> IndexedVec<T> {
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            inner: Vec::with_capacity(cap),
        }
    }

    pub fn insert(&mut self, index: usize, value: T) {
        if self.inner.len() > index {
            self.inner[index] = Some(value);
        } else {
            self.inner.resize_with(index + 1, || None);
            self.inner[index] = Some(value);
        }
    }

    pub fn get(&self, index: usize) -> Option<&T> { self.inner[index].as_ref() }
}

impl<T> Index<usize> for IndexedVec<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.inner[index]
            .as_ref()
            .expect("The IndexedVec is not contiguous!")
    }
}

impl<T> IndexMut<usize> for IndexedVec<T> {
    fn index_mut(&mut self, index: usize) -> &mut T {
        self.inner[index]
            .as_mut()
            .expect("The IndexedVec is not contiguous!")
    }
}

impl<T> Deref for IndexedVec<T> {
    type Target = [Option<T>];

    fn deref(&self) -> &Self::Target { self.inner.as_slice() }
}

impl<'a, T> IntoIterator for &'a IndexedVec<T> {
    type IntoIter = slice::Iter<'a, Option<T>>;
    type Item = &'a Option<T>;

    fn into_iter(self) -> Self::IntoIter { self.inner.iter() }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_indexed_vec() {
        let mut iv: IndexedVec<&'static str> = IndexedVec::with_capacity(10);

        iv.insert(0, "0");
        assert_eq!(iv[0], "0");
        iv.insert(5, "5");
        assert_eq!(iv[5], "5");
        iv.insert(9, "9");
        assert_eq!(iv[9], "9");

        for idx in (1..5).chain(6..9) {
            assert!(iv.get(idx).is_none());
        }
    }
}
