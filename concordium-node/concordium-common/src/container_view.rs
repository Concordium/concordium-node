use std::{cmp::min, convert::From, ops::Index, sync::Arc, vec::Vec};

/// It is a shared view over
#[derive(Debug, Clone)]
pub struct ContainerView {
    data:   Arc<Vec<u8>>,
    offset: usize,
    len:    usize,
}

/// It is just a shared viewer over a vector.
/// As `Write` impl, it is not allowed to write more that current vector
/// capacity.
impl ContainerView {
    #[inline]
    pub fn len(&self) -> usize { self.len }

    #[inline]
    pub fn is_empty(&self) -> bool { self.len == 0 }

    #[inline]
    pub fn sub(&self, offset: usize) -> Self { self.sub_range(offset, self.data.len() - offset) }

    #[inline]
    pub fn sub_range(&self, offset: usize, len: usize) -> Self {
        let data_len = self.data.len();
        let valid_offset = min(self.offset + offset, data_len);
        let valid_len = min(len, data_len - valid_offset);

        ContainerView {
            data:   Arc::clone(&self.data),
            offset: valid_offset,
            len:    valid_len,
        }
    }

    #[inline]
    pub fn as_slice(&self) -> &[u8] { &self.data[self.offset..][..self.len] }
}

impl PartialEq for ContainerView {
    fn eq(&self, other: &ContainerView) -> bool { self.as_slice() == other.as_slice() }
}

impl Index<usize> for ContainerView {
    type Output = u8;

    #[inline]
    fn index(&self, index: usize) -> &Self::Output { &self.as_slice()[index] }
}

impl From<Vec<u8>> for ContainerView {
    #[inline]
    fn from(source: Vec<u8>) -> Self {
        let len = source.len();
        ContainerView {
            data: Arc::new(source),
            offset: 0,
            len,
        }
    }
}

impl AsRef<[u8]> for ContainerView {
    #[inline]
    fn as_ref(&self) -> &[u8] { self.as_slice() }
}

#[cfg(test)]
mod unit_test {
    use std::io::{Cursor, Read};

    use super::ContainerView;
    use failure::Fallible;

    #[test]
    fn sub_test() {
        let data = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
        let view_0 = ContainerView::from(data.clone());

        // Sub
        let view_1 = view_0.sub(5);
        assert_eq!(view_1.as_ref(), &data[5..]);

        let view_2 = view_1.sub(3);
        assert_eq!(view_2.as_ref(), &data[5 + 3..]);

        // Range
        let view_3 = view_0.sub_range(5, 3);
        assert_eq!(view_3.as_slice(), &data[5..(5 + 3)]);

        let view_4 = view_3.sub(2);
        assert_eq!(view_4.as_slice(), &data[(5 + 2)..(5 + 3 + 2)]);
    }

    #[test]
    fn index_access() {
        let view_0 = ContainerView::from(vec![1, 2, 3, 4, 5]);
        let view_1 = view_0.sub(2);
        assert_eq!(view_1.len(), 3);

        assert_eq!(view_1[0], view_0[2]);
        assert_eq!(view_1[1], view_0[3]);
    }

    #[test]
    fn read_from_cursor() -> Fallible<()> {
        let data = vec![1, 2, 3, 4, 5, 6, 7, 8, 9];
        let view_0 = ContainerView::from(data.clone());

        let mut buff = [0; 4];
        let cur_1 = Cursor::new(view_0.clone());
        let mut t1 = cur_1.take(3);
        t1.read(&mut buff)?;
        assert_eq!(buff, vec![1, 2, 3, 0].as_slice());

        let mut out: Vec<u8> = vec![];
        let mut cur_2 = Cursor::new(view_0);
        cur_2.read_to_end(&mut out)?;
        assert_eq!(out, data);
        Ok(())
    }
}
