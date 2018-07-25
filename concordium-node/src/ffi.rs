//use libc::{c_int,c_char};
//use std::ffi::CStr;
//use std::str;

/*
#[link(name = "cpuid")]
extern {
    fn cpuid_lib_version() -> *const c_char;
    fn cpuid_present() -> c_int;
    pub fn cpu_clock() -> c_int;
}

pub fn is_present() -> bool {
    unsafe { cpuid_present() == 1 }
}

pub fn version() -> String {
    unsafe {
        let ptr = cpuid_lib_version();
        let bytes = CStr::from_ptr(ptr).to_bytes();
        return str::from_utf8(bytes).ok().expect("Invalid UTF8 string").to_string();
    }
}


pub fn clock_frequency() -> Option<i32> {
    let frequency = unsafe { cpu_clock() };
    if frequency != -1 {
        Some(frequency)
    } else {
        None
    }
}

#[test]
fn test_is_present() {
    assert!(is_present());
}

#[test]
fn test_freq() {
    assert_ne!(clock_frequency().unwrap_or(-1), -1);
}
*/