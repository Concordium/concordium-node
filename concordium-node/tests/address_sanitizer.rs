#[link(name = "asan")]
#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    #[test]
    pub fn asan_001_out_of_bounds() {
        let xs = [0, 1, 2, 3];
        let _y = unsafe { *xs.as_ptr().offset(4) };
    }

    #[derive(Clone)]
    struct Cycle {
        cell: RefCell<Option<Rc<Cycle>>>,
    }

    impl Drop for Cycle {
        fn drop(&mut self) {
            println!("Cycle freed");
        }
    }

    #[test]
    pub fn asan_002_rc_cycle() {
        let cycle = Rc::new(Cycle {
            cell: RefCell::new(None),
        });
        *cycle.cell.borrow_mut() = Some(cycle.clone());
    }
}
