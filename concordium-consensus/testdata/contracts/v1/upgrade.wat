(module

 ;; Imports
 (import "concordium" "get_parameter_size" (func $host_get_parameter_size (param $index i32) (result i32)))
 (import "concordium" "get_parameter_section" (func $host_get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
 (import "concordium" "invoke" (func $host_invoke (param $tag i32) (param $start i32) (param $length i32) (result i64)))
 (import "concordium" "write_output" (func $host_write_output (param $start i32) (param $length i32) (param $offset i32) (result i32)))

 (import "concordium" "get_receive_self_address" (func $host_get_self_address (param $start i32)))

 ;;(import "concordium" "upgrade" (func $host_upgrade (param $start i32) (result i64)))

 ;; Mock

 ;; Succeeding upgrade
 (func $host_upgrade (param $module_start i32) (result i64) (return (i64.const 0)))
 ;; ;; Failing upgrade: missing module.
 ;; (func $host_upgrade (param $module_start i32) (result i64) (return (i64.const 1)))
 ;; ;; Failing upgrade: missing contract with matching name.
 ;; (func $host_upgrade (param $module_start i32) (result i64) (return (i64.const 2)))
 ;; ;; Failing upgrade: unsupported module version.
 ;; (func $host_upgrade (param $module_start i32) (result i64) (return (i64.const 3)))

 ;; Helper functions

 (func $assert_eq (param $actual i32) (param $expected i32)
       (if (i32.eq (local.get $actual) (local.get $expected))
           (then nop)
           (else unreachable)))

 (func $assert_eq_64 (param $actual i64) (param $expected i64)
       (if (i64.eq (local.get $actual) (local.get $expected))
           (then nop)
           (else unreachable)))

 ;; Invoke contract host function.
 ;; Parameter:
 ;; - 16 bytes for contract address.
 ;; - 2 bytes for parameter length (n).
 ;; - n bytes for the parameter.
 ;; - 2 bytes for the entrypoint name length (e).
 ;; - e bytes for the entrypoint name.
 ;; - 8 bytes for the amount.
 (func $invoke_contract (param $start i32) (param $length i32)
       (call $host_invoke (i32.const 1) (local.get $start) (local.get $length)))

 ;; Contract A

 ;; Initialize contract A.
 (func $init (export "init_contract") (param i64) (result i32)
       (return (i32.const 0)) ;; Successful init
       )

 ;; Receive function returning a constant value. Use to distinguish between modules.
 (func $receive_name (export "contract.name") (param i64) (result i32)
       ;; Write 0 to linear memory
       (i32.const 0) (i32.const 0) (i32.store)
       ;; Output 4 bytes from linear memory
       (call $host_write_output (i32.const 0) (i32.const 4) (i32.const 0))
       (return (i32.const 0))
       )

 ;; Receive function for triggering an upgrade.
 ;; Takes the smart contract module to upgrade to as the parameter (32 bytes).
 (func $receive_upgrade (export "contract.upgrade") (param $amount i64) (result i32)
       ;; Declare local variable for storing the result ofinvoking the contract itself.
       (local $invoke_name_return i64)

       ;; Declare local variable for storing the result of calling $host_upgrade.
       (local $upgrade_result i64)

       ;; Invoke its own 'contract.name' to check the module is the same.

       (call $host_get_self_address (i32.const 0)) ;; Puts the contract address in memory at offset 0.
       (i32.const 16) ;; The offset after the contract address.
       (i32.const 0) ;; The length of the parameter.
       (i32.store16) ;; Store into memory.
       (i32.const 18) ;; Current offset in memory.
       (i32.const 7) ;; The length of the entrypoint.
       (i32.store16) ;; Store into memory.
       (i32.const 20) ;; Current offset in memory.
       (i64.const 8462377440787064064) ;; Decimal for the ascii encoding of 'upgrade' padded with a 0 byte.
       (i64.store)
       (i32.const 27) ;; Current offset in memory.
       (i64.const 0) ;; Amount to include in the invocation.
       (i64.store)
       (local.set $invoke_name_return (call $host_invoke
                                            (i32.const 1) ;; Tag for contract invocation.
                                            (i32.const 0) ;; Offset in memory to start reading from.
                                            (i32.const 35) ;; The length of the parameter.
                                            ))
       (local.get $invoke_name_return)
       (i64.const 141836999983103) ;; Mask with the last 23 bits as 0
       (i64.and)

       ;; Read the parameter into the first part of the memory.
       (call $host_get_parameter_section
             (i32.const 0) ;; index.
             (i32.const 0) ;; starting offset in parameter.
             (call $host_get_parameter_size (i32.const 0)) ;; number of bytes to read.
             (i32.const 0)) ;; starting offset in memory.

       ;; Trigger the upgrade.
       (local.set $upgrade_result (call $host_upgrade (i32.const 0)))


       ;; Check the result of upgrading was successful.
       (assert_eq (i32.const 0) (local.get $upgrade_result))

       ;; Invoke its own 'contract.name' again to check the module is the same.

 (memory 1)
 )
