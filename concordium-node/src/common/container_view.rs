use std::{
    cmp::min,
    convert::From,
    io::Result,
    ops::Index,
    slice,
    sync::{Arc, RwLock},
    vec::Vec,
};

/// It is a shared view over
#[derive(Debug, Clone)]
pub struct ContainerView {
    data:   Arc<RwLock<Vec<u8>>>,
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
    pub fn sub(&self, offset: usize) -> Self {
        self.sub_range(offset, self.data.read().unwrap().len() - offset)
    }

    #[inline]
    pub fn sub_range(&self, offset: usize, len: usize) -> Self {
        let data_len = self.data.read().unwrap().len();
        let valid_offset = min(self.offset + offset, data_len);
        let valid_len = min(len, data_len - valid_offset);

        ContainerView {
            data:   self.data.clone(),
            offset: valid_offset,
            len:    valid_len,
        }
    }

    #[inline]
    pub fn as_slice(&self) -> &[u8] {
        let ref_data: &Vec<u8> = &self.data.read().unwrap();
        let data_ptr = ref_data[self.offset..].as_ptr();

        unsafe { slice::from_raw_parts(data_ptr, self.len) }
    }

    #[inline]
    fn as_mut_slice(&mut self) -> &mut [u8] {
        let mut ref_data = self.data.write().unwrap();
        let data_ptr = ref_data[self.offset..].as_mut_ptr();
        unsafe { slice::from_raw_parts_mut(data_ptr, self.len) }
    }
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
            data: Arc::new(RwLock::new(source)),
            offset: 0,
            len,
        }
    }
}

impl AsRef<[u8]> for ContainerView {
    #[inline]
    fn as_ref(&self) -> &[u8] { self.as_slice() }
}

impl std::io::Write for ContainerView {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> Result<usize> { self.as_mut_slice().write(buf) }

    #[inline]
    fn write_all(&mut self, buf: &[u8]) -> Result<()> { self.as_mut_slice().write_all(buf) }

    #[inline]
    fn flush(&mut self) -> Result<()> { Ok(()) }
}

#[cfg(test)]
mod unit_test {
    use std::io::{Cursor, Read, Write};

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

    #[test]
    fn write_test() {
        let data = vec![1, 2, 3, 4, 5, 6, 7, 8, 9];
        let mut view_0 = ContainerView::from(data);

        assert_eq!(view_0.write(&[0; 3]).unwrap(), 3);
        assert_eq!(view_0.write_all(&[0; 4]).is_ok(), true);

        assert_eq!(view_0.write_all(&[0; 10]).is_err(), true);
        assert_eq!(view_0.as_slice(), &[0; 9]);

        assert_eq!(view_0.write_all(&[1; 9]).is_ok(), true);
        assert_eq!(view_0.as_slice(), &[1; 9]);
    }

}
