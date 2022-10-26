;; This module contains a single contract with two receive functions 'upgrade' and 'name'. Initializing is always successful and a NOP.
;;
;; - The receive function 'contract.name' returns a constant u32 value and is used to identify this module before upgrading.
;; - The receive function 'contract.upgrade' calls the name function ensuring the module matches some identifier.
;; Then triggers the upgrade of the module, checks whether it is successful and calls the name function checking that the identifier have changed.
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

 ;; Invoke contract host function with `contract.name` on the instance calling it.
 ;; Note: this function writes into the memory starting from offset 0.
 ;;
 ;; Parameter for invoking contracts:
 ;; - 16 bytes for contract address.
 ;; - 2 bytes for parameter length (n).
 ;; - n bytes for the parameter.
 ;; - 2 bytes for the entrypoint name length (e).
 ;; - e bytes for the entrypoint name.
 ;; - 8 bytes for the amount.
 (func $invoke_contact_name (result i32)
       ;; Declare local variable for storing the result of invoking 'coentract.name'.
       (local $invoke_name_return i64)
       ;; Declare local variable for storing the index of the return value, when invoking 'contract.name'.
       (local $invoke_name_return_index i32)

       (call $host_get_self_address (i32.const 0)) ;; Puts the contract address in memory at offset 0.
       (i32.store16 ;; Store the parameter length.
        (i32.const 16) ;; The offset after the contract address.
        (i32.const 0) ;; The length of the parameter.
        )
       (i32.store16 ;; Store the entrypoint length.
        (i32.const 18) ;; Current offset in memory.
        (i32.const 4) ;; The length of the entrypoint.
        )
       (i32.store ;; Store the entrypoint bytes.
        (i32.const 20) ;; Current offset in memory.
        (i32.const 1701667182) ;; Decimal for the ascii encoding of 'name' (In hex: 6E 61 6D 65).
        )
       (i64.store ;; Store the amount.
        (i32.const 24) ;; Current offset in memory.
        (i64.const 0) ;; Amount to include in the invocation.
        )
       (local.set $invoke_name_return (call $host_invoke
                                            (i32.const 1) ;; Tag for contract invocation.
                                            (i32.const 0) ;; Offset in memory to start reading from.
                                            (i32.const 32))) ;; The length of the parameter.

       ;; Get the index of the response
       ;; The numeric value 8388607 is the mask 0b0111_1111_1111_1111_1111_1111
       (local.set $invoke_name_return_index (i32.and (i32.const 8388607)
                                                     (i32.wrap_i64 (i64.shr_u (local.get $invoke_name_return) (i64.const 40)))))
       (call $host_get_parameter_section ;; Read the return value into memory.
             (local.get $invoke_name_return_index) ;; Index of the return value.
             (i32.const 0) ;; Write offset in memory.
             (i32.const 4) ;; Write length.
             (i32.const 0)) ;; Offset to read from
       (drop)
       (return (i32.load (i32.const 0)))) ;; Read the return value from memory.

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
        (i32.const 0)) ;; Value to write
       ;; Output 4 bytes from linear memory
       (call $host_write_output (i32.const 0) (i32.const 4) (i32.const 0))
       (drop)
       (return (i32.const 0)))

 ;; Receive function for triggering an upgrade.
 ;; Takes the smart contract module to upgrade to as the parameter (32 bytes).
 (func $receive_upgrade (export "contract.upgrade") (param $amount i64) (result i32)
       ;; Invoke its own 'contract.name' to check the module is the same.
       (call $assert_eq
             (i32.const 0)
             (call $invoke_contact_name))

       ;; Read the parameter into the first part of the memory.
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

       ;; Invoke its own 'contract.name' again to check the module have changed.
       (call $assert_eq
             (i32.const 1234)
             (call $invoke_contact_name))

       ;; Return success
       (return (i32.const 0)))
 (memory 1))
