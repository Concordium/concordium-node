use curryrs::types::I32;
use libc::c_char;
use libc::c_int;

#[repr(C)]
pub struct baker_runner {
    private: [u8; 0],
}

extern "C" {
    pub fn hs_init(argc: *const c_int, argv: *const *const *const c_char);
    pub fn hs_exit();
    pub fn triple(x: I32) -> I32;
    pub fn callbackTwice(f: Option<extern "C" fn(I32) -> I32>) -> I32;
    pub fn printCString(string: *const u8);
    pub fn callbackWithCString(f: Option<extern "C" fn(*const u8)>);
    pub fn startBaker(genesis_time: u64,
                      number_of_bakers: u64,
                      baker_index: u64,
                      bake_callback: extern "C" fn(*const u8, i64))
                      -> *mut baker_runner;
    pub fn printBlock(block_data: *const u8, data_length: i64);
    pub fn receiveBlock(baker: *mut baker_runner, block_data: *const u8, data_length: i64);
    pub fn receiveTransaction(baker: *mut baker_runner,
                              n0: u64,
                              n1: u64,
                              n2: u64,
                              n3: u64,
                              transaction_data: *const u8,
                              data_length: i64);
    pub fn stopBaker(baker: *mut baker_runner);
}
