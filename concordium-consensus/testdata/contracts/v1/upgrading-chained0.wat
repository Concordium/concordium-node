;; This module contains a single contract with one receive function 'upgrade'. Initializing is always successful and a NOP.
;;
;; The 'upgrade' entrypoint takes a parameter containing 4 bytes for the counter and a module reference to upgrade to.
;; It will trigger an upgrade to the provided module, decrement the counter and if the counter is not 0 it will invoke 'upgrade' on itself with the decremented counter.
(module

 ;; Imports
 (import "concordium" "get_parameter_size" (func $host_get_parameter_size (param $index i32) (result i32)))
 (import "concordium" "get_parameter_section" (func $host_get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
 (import "concordium" "invoke" (func $host_invoke (param $tag i32) (param $start i32) (param $length i32) (result i64)))
 (import "concordium" "write_output" (func $host_write_output (param $start i32) (param $length i32) (param $offset i32) (result i32)))

 (import "concordium" "get_receive_self_address" (func $host_get_self_address (param $start i32)))
 (import "concordium" "upgrade" (func $host_upgrade (param $start i32) (result i64)))

 ;; Helper functions

 (func $assert_eq (param $actual i32) (param $expected i32)
       (if (i32.eq (local.get $actual) (local.get $expected))
           (then nop)
           (else unreachable)))

 (func $assert_eq_64 (param $actual i64) (param $expected i64)
       (if (i64.eq (local.get $actual) (local.get $expected))
           (then nop)
           (else unreachable)))

 ;; Contract

 ;; Initialize contract.
 (func $init (export "init_contract") (param i64) (result i32)
       (return (i32.const 0))) ;; Successful init

 ;; Invoke contract host function with `contract.upgrade` on itself.
 ;;
 ;; Note: this function writes into the memory starting from offset 0.
 ;;
 ;; Parameter for invoking contracts:
 ;; - 16 bytes for contract address.
 ;; - 2 bytes for parameter length (n).
 ;; - n bytes for the parameter.
 ;; - 2 bytes for the entrypoint name length (e).
 ;; - e bytes for the entrypoint name.
 ;; - 8 bytes for the amount.
 (func $invoke_contract_upgrade (param $counter i32)
       ;; Local variable to hold the return value when from invoking.
       (local $host_invoke_return i64)

       ;; (16 bytes) Puts the contract address in memory at offset 0.
       (call $host_get_self_address (i32.const 0))

       ;; (2 bytes) Store the parameter length.
       (i32.store16 (i32.const 16) ;; The offset after the contract address.
                    (i32.const 36)) ;; The length of the parameter. (counter + module reference)

       ;; (4 bytes) Store the counter in memory.
       (i32.store (i32.const 18) ;; Offset in memory.
                  (local.get $counter)) ;; Value to store.

       ;; (32 bytes) Store the module reference from the parameter in memory.
       (call $host_get_parameter_section
             (i32.const 0) ;; index.
             (i32.const 22) ;; starting write offset in memory.
             (i32.const 32) ;; number of bytes to read (4 bytes for the counter).
             (i32.const 4)) ;; starting offset in parameter.
       (drop)

       ;; (2 bytes) Store the entrypoint length.
       (i32.store16 (i32.const 54) ;; Current offset in memory.
                    (i32.const 7)) ;; The length of the entrypoint.

       ;; (7 bytes) Store the entrypoint bytes.
       (i64.store (i32.const 56) ;; Current offset in memory.
                  (i64.const 28539342341763189)) ;; Decimal for the ascii encoding of 'upgrade' (In hex: 75 70 67 72 61 64 65).
       ;; (8 bytes) Store the amount.
       (i64.store (i32.const 63) ;; Current offset in memory.
                  (i64.const 0)) ;; Amount to include in the invocation.

       ;; Trigger the contract invocation
       (local.set $host_invoke_return (call $host_invoke
                                            (i32.const 1) ;; Tag for contract invocation.
                                            (i32.const 0) ;; Offset in memory to start reading from.
                                            (i32.const 71))) ;; Length of the parameter.

       ;; Check that the last 5 bytes are 0, meaning success.
       (call $assert_eq_64
             (i64.shl (local.get $host_invoke_return) (i64.const 24))
             (i64.const 0)))


 ;; Receive function for triggering an upgrade.
 ;; Takes the smart contract module to upgrade to as the parameter (32 bytes).
 (func $receive_upgrade (export "contract.upgrade") (param $amount i64) (result i32)
       ;; Local variable for the counter.
       (local $counter i32)

       ;; Read the module reference from the parameter into memory
       (call $host_get_parameter_section
             (i32.const 0) ;; index.
             (i32.const 0) ;; starting write offset in memory.
             (i32.const 32) ;; number of bytes to read (32 bytes for the module reference).
             (i32.const 4)) ;; starting offset in parameter.
       (drop)

       ;; Trigger the upgrade and check the result of upgrading was successful.
       (call $assert_eq_64
             (i64.const 0)
             (call $host_upgrade (i32.const 0)))

       ;; Read the counter from the parameter into memory.
       (call $host_get_parameter_section
             (i32.const 0) ;; index.
             (i32.const 0) ;; starting write offset in memory.
             (i32.const 4) ;; number of bytes to read (4 bytes for the counter).
             (i32.const 0)) ;; starting offset in parameter.
       (drop)

       ;; Read the counter from the first 4 bytes in memory.
       (local.set $counter (i32.load (i32.const 0))) ;; Offset in memory to read from.

       ;; Check if the counter is 0 and if so exit with success.
       (if (i32.eq (local.get $counter) (i32.const 0))
           (then (return (i32.const 0))))

       ;; Decrement the counter
       (local.set $counter (i32.sub (local.get $counter)
                                    (i32.const 1)))

       ;; Invoke another upgrade
       (call $invoke_contract_upgrade (local.get $counter))

       ;; Return success
       (return (i32.const 0)))
 (memory 1))
