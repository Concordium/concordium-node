(module

 ;; Imports
 (import "concordium" "write_output" (func $host_write_output (param $start i32) (param $length i32) (param $offset i32) (result i32)))

 ;; Contract

 ;; Initialize contract.
 (func $init (export "init_contract") (param i64) (result i32)
       (return (i32.const 0))) ;; Successful init

 ;; Receive function returning a constant value. Use to distinguish between modules.
 ;; The return value is always 4 bytes.
 (func $receive_name (export "contract.name") (param i64) (result i32)
       ;; Write 0 to memory.
       (i32.store
        (i32.const 0) ;; Write offset
        (i32.const 5678)) ;; Value to write
       ;; Output 4 bytes from linear memory
       (call $host_write_output (i32.const 0) (i32.const 4) (i32.const 0))
       (drop)
       (return (i32.const 0)))

 (memory 1))
