use crate::common;

use std::sync::{Arc, Mutex};
use std::net::{ IpAddr };

#[derive(Clone, Debug)]
pub struct UnreachableNodes {
    nodes: Arc<Mutex<Vec<(u64, IpAddr, u16)>>>,
}

impl UnreachableNodes {
    pub fn new() -> Self {
        UnreachableNodes { nodes: Arc::new(Mutex::new(vec![])), }
    }

    pub fn contains(&self, ip: IpAddr, port: u16) -> bool {
        if let Ok(ref mut nodes) = self.nodes.lock() {
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
        if let Ok(ref mut nodes) = self.nodes.lock() {
            nodes.push((common::get_current_stamp(), ip.clone(), port));
            true
        } else {
            false
        }
    }

    pub fn cleanup(&mut self, since: u64) -> bool {
        if let Ok(ref mut nodes) = self.nodes.lock() {
            nodes.retain(|&x| {
                             let (time, _, _) = x;
                             time >= since
                         });
            true
        } else {
            false
        }
    }
}


