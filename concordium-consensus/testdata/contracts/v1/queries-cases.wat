(module

  ;; Imports
  (import "concordium" "get_parameter_section" (func $host_get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))

  (import "concordium" "invoke" (func $host_invoke (param $tag i32) (param $start i32) (param $length i32) (result i64)))
  (import "concordium" "state_create_entry" (func $state_create_entry (param $key_start i32) (param $key_length i32) (result i64)))
  (import "concordium" "state_entry_write" (func $state_entry_write (param $entry i64) (param $read_location i32) (param $length i32) (param $offset i32) (result i32)))


  ;; Initialize contract.
  (func $init (export "init_contract") (param i64) (result i32)
        (return (i32.const 0))) ;; Successful init

  (func (export "contract.query_account") (param $amount i64) (result i32)
       ;; Read account address (32 bytes), an amount (8 bytes) used for transfer and a flag (1 byte) from the parameter into memory. Setting the flag to: 0 means do not update the state, 1 means update the state before invoking transfer, 2 means update the state after invoking transfer.
       ;; 0 means do not update the state, 1 means do
       (call $host_get_parameter_section
             (i32.const 0) ;; index.
             (i32.const 0) ;; starting write offset in memory.
             (i32.const 33) ;; number of bytes to read.
             (i32.const 0)) ;; starting offset in parameter.
       (if (i32.eq (i32.const 1) (i32.load8_u (i32.const 32)))
          ;; update the state now
          (then (call $state_entry_write (call $state_create_entry (i32.const 0) (i32.const 32)) (i32.const 0) (i32.const 32)  (i32.const 0))
                 drop
                 )
       )
       ;; query
       (call $host_invoke (i32.const 2) (i32.const 0) (i32.const 32))

       (if (i32.eq (i32.const 2) (i32.load8_u (i32.const 32)))
           (then (call $state_entry_write (call $state_create_entry (i32.const 0) (i32.const 32)) (i32.const 0) (i32.const 32) (i32.const 0))
                 drop
                 )
       )
       ;; Return signal to succeed
       (return (i32.const 0)))
  (memory 1))
