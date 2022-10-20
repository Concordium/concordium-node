(module

 ;; Imports
 (import "concordium" "get_parameter_section" (func $host_get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))

 (import "concordium" "upgrade" (func $host_upgrade (param $start i32) (result i64)))

 ;; Helper functions

 (func $assert_eq_64 (param $actual i64) (param $expected i64)
       (if (i64.eq (local.get $actual) (local.get $expected))
           (then nop)
           (else unreachable)))

 ;; Contract

 ;; Initialize contract.
 (func $init (export "init_contract") (param i64) (result i32)
       (return (i32.const 0))) ;; Successful init

 ;; Receive function for triggering an upgrade and reject.
 ;; Takes the smart contract module to upgrade to as the parameter (32 bytes).
 (func $receive_upgrade (export "contract.upgrade") (param $amount i64) (result i32)
       ;; Read the module reference from the parameter into memory
       (call $host_get_parameter_section
             (i32.const 0) ;; index.
             (i32.const 0) ;; starting write offset in memory.
             (i32.const 32) ;; number of bytes to read (32 bytes for the module reference).
             (i32.const 0)) ;; starting offset in parameter.
       (drop)

       ;; Trigger the upgrade and check the result of upgrading was successful.
       (call $assert_eq_64
             (i64.const 0)
             (call $host_upgrade (i32.const 0)))

       ;; Return reject the update.
       (return (i32.const -1)))
 (memory 1))
