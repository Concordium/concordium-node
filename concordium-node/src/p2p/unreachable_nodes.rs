use crate::common;

use std::sync::{Arc, RwLock};
use std::net::{ IpAddr };

#[derive(Clone, Debug)]
pub struct UnreachableNodes {
    nodes: Arc<RwLock<Vec<(u64, IpAddr, u16)>>>,
}

impl UnreachableNodes {
    pub fn new() -> Self {
        UnreachableNodes { nodes: Arc::new(RwLock::new(vec![])), }
    }

    pub fn contains(&self, ip: IpAddr, port: u16) -> bool {
        if let Ok(ref nodes) = safe_read!(self.nodes) {
            return nodes.iter()
                        .find(|&&x| {
                                  let (_, mip, mport) = x;
                                  ip == mip && port == mport
                              })
                        .is_some();
        }
        true
    }

    pub fn insert(&mut self, ip: IpAddr, port: u16) -> bool {
        if let Ok(ref mut nodes) = safe_write!(self.nodes) {
            nodes.push((common::get_current_stamp(), ip, port));
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


