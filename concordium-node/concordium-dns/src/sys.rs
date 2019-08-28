use crate::sys_c as sys;
use failure::{Fail, Fallible};
use libc::c_int;
use std::{
    borrow::Borrow,
    ffi::{CStr, CString, NulError},
    fmt,
    io::{self, Write},
    net, ptr,
};

const IP_CSTR_MAX: usize = 40;

#[derive(Debug, Fail)]
#[fail(display = "argument contains null byte")]
pub struct NullByteError;

#[derive(Debug, Fail)]
pub struct UBError {
    pub err_code: c_int,
}

impl fmt::Display for UBError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = unsafe {
            // At time of writing ub_strerror always returns a string.
            // Assume that won't change in the future.
            CStr::from_ptr(sys::ub_strerror(self.err_code))
                .to_str()
                .unwrap()
        };
        write!(f, "{}", s)
    }
}

impl std::convert::From<NulError> for NullByteError {
    fn from(_: NulError) -> NullByteError { NullByteError }
}

macro_rules! into_result {
    ($err:expr) => {
        into_result!($err, ())
    };
    ($err:expr, $ok:expr) => {
        match $err {
            0 => Ok($ok),
            err => Err(UBError { err_code: err })?,
        }
    };
}

/// Wraps `ub_result`. The result of DNS resolution and validation of a query.
pub struct Answer(*mut sys::ub_result);

impl Answer {
    /// Returns an iterator over answer record datas.
    pub fn data(&'_ self) -> DataIter<'_> {
        DataIter {
            index:  0,
            answer: self,
        }
    }

    /// True if result is secure.
    pub fn secure(&self) -> bool { unsafe { (*self.0).secure != 0 } }
}

impl Drop for Answer {
    fn drop(&mut self) { unsafe { sys::ub_resolve_free(self.0) } }
}

/// Wraps `ub_ctx`.
pub struct Context {
    ub_ctx: *mut sys::ub_ctx,
}

impl Context {
    /// Create a new `Context`.
    pub fn new() -> std::result::Result<Context, ()> {
        let ctx = unsafe { sys::ub_ctx_create() };
        if ctx.is_null() {
            Err(())
        } else {
            Ok(Context { ub_ctx: ctx })
        }
    }

    pub fn set_fwd<T: Borrow<net::IpAddr>>(&self, ip: T) -> Fallible<()> {
        match *ip.borrow() {
            net::IpAddr::V4(ref ip) => self.set_fwd4(ip),
            net::IpAddr::V6(ref ip) => self.set_fwd6(ip),
        }
    }

    /// Forward queries to an IPv4 host.
    pub fn set_fwd4<T: Borrow<net::Ipv4Addr>>(&self, ip: T) -> Fallible<()> {
        let mut buf = [0u8; IP_CSTR_MAX];
        let target = ipv4_to_cstr(ip.borrow(), &mut buf);
        unsafe { into_result!(sys::ub_ctx_set_fwd(self.ub_ctx, target.as_ptr())) }
    }

    /// Forward queries to an IPv6 host.
    pub fn set_fwd6<T: Borrow<net::Ipv6Addr>>(&self, ip: T) -> Fallible<()> {
        let mut buf = [0u8; IP_CSTR_MAX];
        let target = ipv6_to_cstr(ip.borrow(), &mut buf);
        unsafe { into_result!(sys::ub_ctx_set_fwd(self.ub_ctx, target.as_ptr())) }
    }

    /// Add a single line string containing a valid DNSKEY or DS RR as a trust
    /// anchor.
    pub fn add_ta(&self, ta: &str) -> Fallible<()> {
        let ta = CString::new(ta)?;
        unsafe { into_result!(sys::ub_ctx_add_ta(self.ub_ctx, ta.as_ptr())) }
    }

    /// Resolve and validate a query.
    pub fn resolve(&self, name: &str, rrtype: u16, class: u16) -> Fallible<Answer> {
        let mut result: *mut sys::ub_result = ptr::null_mut();
        let name = CString::new(name)?;
        unsafe {
            into_result!(
                sys::ub_resolve(
                    self.ub_ctx,
                    name.as_ptr(),
                    i32::from(rrtype),
                    i32::from(class),
                    &mut result
                ),
                Answer(result)
            )
        }
    }
}

/// An iterator over the datas of an [Answer](struct.Answer.html).
pub struct DataIter<'a> {
    index:  isize,
    answer: &'a Answer,
}

impl<'a> std::iter::Iterator for DataIter<'a> {
    type Item = &'a [u8];

    fn next(&mut self) -> Option<&'a [u8]> {
        let item = unsafe {
            let offset = (*self.answer.0).data.offset(self.index);
            if offset.is_null() {
                None
            } else {
                let len = *(*self.answer.0).len.offset(self.index) as usize;
                Some(std::slice::from_raw_parts(*offset as *const u8, len))
            }
        };
        if item.is_some() {
            self.index += 1
        }
        item
    }
}

fn ipv4_to_cstr<'a>(ip: &net::Ipv4Addr, buf: &'a mut [u8; IP_CSTR_MAX]) -> &'a CStr {
    let len = {
        let mut w = io::BufWriter::new(&mut buf[..]);
        w.write_fmt(format_args!("{}", &ip))
            .expect("write_fmt ipv4");
        IP_CSTR_MAX + 1 - w.into_inner().expect("into_inner ipv4").len()
    };
    CStr::from_bytes_with_nul(&buf[..len]).expect("valid ipv4 c str")
}

fn ipv6_to_cstr<'a>(ip: &net::Ipv6Addr, buf: &'a mut [u8; IP_CSTR_MAX]) -> &'a CStr {
    let len = {
        let mut w = io::BufWriter::new(&mut buf[..]);
        w.write_fmt(format_args!("{}", &ip))
            .expect("write_fmt ipv6");
        IP_CSTR_MAX + 1 - w.into_inner().expect("into_inner ipv6").len()
    };
    CStr::from_bytes_with_nul(&buf[..len]).expect("valid ipv6 c str")
}
