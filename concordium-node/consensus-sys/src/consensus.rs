use curryrs::types::I32;
use std::ptr;
use libc::c_char;
use libc::c_int;
use std::{thread,time};

#[repr(C)] pub struct baker_runner { private: [u8; 0] }

extern {
    pub fn hs_init(argc : *const c_int, argv: *const *const *const c_char);
    pub fn hs_exit();
    pub fn triple(x : I32) -> I32;
    pub fn callbackTwice(f : Option<extern "C" fn(I32) -> I32>) -> I32;
    pub fn printCString(string : *const u8);
    pub fn callbackWithCString(f : Option<extern "C" fn(*const u8)>);
    pub fn startBaker(genesis_time : u64,
                      number_of_bakers : u64, 
                      baker_index : u64,
                      bake_callback : extern "C" fn(*const u8, i64)) -> *mut baker_runner;
    pub fn printBlock(block_data : *const u8, data_length : i64);
    pub fn receiveBlock(baker : *mut baker_runner, block_data : *const u8, data_length : i64);
    pub fn receiveTransaction(baker : *mut baker_runner, n0 : u64, n1 : u64, n2 : u64, n3 : u64, transaction_data : *const u8, data_length : i64);
    pub fn stopBaker(baker : *mut baker_runner);
}

extern "C" fn add_one(x : I32) -> I32 {
    println!("add_one called with {}", x);
    x + 1
}

extern "C" fn wrap_print_cstring(string : *const u8) {
    unsafe {printCString(string)};
}

extern "C" fn my_print_cstring(string : *const u8) {
//    println!("{}", String::from(string));
}

extern "C" fn on_block_baked(block_data : *const u8, data_length : i64) {
    println!("Baked a block of length {}", data_length);
    unsafe {printBlock(block_data, data_length); }
}