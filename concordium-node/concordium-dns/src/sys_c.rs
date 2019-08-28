#![allow(non_camel_case_types)]

pub enum ub_ctx {}
#[repr(C)]
#[derive(Debug)]
pub struct ub_result {
    pub qname:         *mut ::libc::c_char,
    pub qtype:         ::libc::c_int,
    pub qclass:        ::libc::c_int,
    pub data:          *mut *mut ::libc::c_char,
    pub len:           *mut ::libc::c_int,
    pub canonname:     *mut ::libc::c_char,
    pub rcode:         ::libc::c_int,
    pub answer_packet: *mut ::libc::c_void,
    pub answer_len:    ::libc::c_int,
    pub havedata:      ::libc::c_int,
    pub nxdomain:      ::libc::c_int,
    pub secure:        ::libc::c_int,
    pub bogus:         ::libc::c_int,
    pub why_bogus:     *mut ::libc::c_char,
    pub ttl:           ::libc::c_int,
}
extern "C" {
    pub fn ub_ctx_create() -> *mut ub_ctx;
}
extern "C" {
    pub fn ub_ctx_set_fwd(ctx: *mut ub_ctx, addr: *const ::libc::c_char) -> ::libc::c_int;
}
extern "C" {
    pub fn ub_ctx_add_ta(ctx: *mut ub_ctx, ta: *const ::libc::c_char) -> ::libc::c_int;
}
extern "C" {
    pub fn ub_resolve(
        ctx: *mut ub_ctx,
        name: *const ::libc::c_char,
        rrtype: ::libc::c_int,
        rrclass: ::libc::c_int,
        result: *mut *mut ub_result,
    ) -> ::libc::c_int;
}
extern "C" {
    pub fn ub_resolve_free(result: *mut ub_result);
}
extern "C" {
    pub fn ub_strerror(err: ::libc::c_int) -> *const ::libc::c_char;
}
