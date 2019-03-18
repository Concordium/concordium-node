#[cfg(not(target_os = "windows"))]
use vecio::{ Rawv };
#[cfg(not(target_os = "windows"))]
use std::io::{ Result };

#[cfg(not(target_os = "windows"))]
pub struct WriteVAdapter<'a> {
    rawv: &'a mut dyn Rawv,
}

#[cfg(not(target_os = "windows"))]
impl<'a> WriteVAdapter<'a> {
    pub fn new(rawv: &'a mut dyn Rawv) -> WriteVAdapter<'a> {
        WriteVAdapter { rawv }
    }
}

#[cfg(not(target_os = "windows"))]
impl<'a> rustls::WriteV for WriteVAdapter<'a> {
    fn writev(&mut self, bytes: &[&[u8]]) -> Result<usize> {
        self.rawv.writev(bytes)
    }
}

