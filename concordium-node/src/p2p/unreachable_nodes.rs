use crate::common;

use std::{
    net::SocketAddr,
    sync::{Arc, RwLock},
};

#[derive(Clone, Debug)]
pub struct UnreachableNodes {
    nodes: Arc<RwLock<Vec<(u64, SocketAddr)>>>,
}

impl Default for UnreachableNodes {
    fn default() -> Self { UnreachableNodes::new() }
}

impl UnreachableNodes {
    pub fn new() -> Self {
        UnreachableNodes {
            nodes: Arc::new(RwLock::new(vec![])),
        }
    }

    pub fn contains(&self, addr: SocketAddr) -> bool {
        if let Ok(ref nodes) = safe_read!(self.nodes) {
            return nodes.iter().any(|&(_, a)| a == addr);
        }
        true
    }

    pub fn insert(&mut self, addr: SocketAddr) -> bool {
        if let Ok(ref mut nodes) = safe_write!(self.nodes) {
            nodes.push((common::get_current_stamp(), addr));
            true
        } else {
            false
        }
    }

    pub fn cleanup(&mut self, since: u64) -> bool {
        if let Ok(ref mut nodes) = safe_write!(self.nodes) {
            nodes.retain(|&x| {
                let (time, ..) = x;
                time >= since
            });
            true
        } else {
            false
        }
    }
}
