;; A module that tests the upgrading functionality introduced as part of P5.
;;
;; This module contains a contract with a receive function which calls upgrade to a module reference provided as a parameter.
;; The module provided should be deployed but be a unsupported module version for upgrading (version 0).
;; It ensures the upgrade fails because of unsupported module version and then returns success.
(module
 ;; Imports

 (import "concordium" "get_parameter_section" (func $host_get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
 (import "concordium" "upgrade" (func $host_upgrade (param $module_ptr i32) (result i64)))

 ;; Helper functions
 (func $assert_eq_64 (param $actual i64) (param $expected i64)
       (if (i64.eq (local.get $actual) (local.get $expected))
           (then nop)
           (else unreachable)))

 ;; Init contract
 (func $init_contract (export "init_contract") (param i64) (result i32)
       (return (i32.const 0))) ;; Successful init

 ;; Upgrade
 (func $contract_upgrade (export "contract.upgrade")  (param i64) (result i32)
       ;; Read 32 bytes from the parameter corresponding to a module reference.
       (call $host_get_parameter_section
             (i32.const 0) ;; Index
             (i32.const 0) ;; Write offset in memory.
             (i32.const 32) ;; Number of bytes to read.
             (i32.const 0)) ;; Offset to read from.
       (drop)

       ;; Trigger the upgrade.
       (call $assert_eq_64
             (i64.const 38654705664) ;; expect the error code for unsupported module version 0x0000_0009_0000_0000.
             (call $host_upgrade (i32.const 0)))

       ;; And now we simply return 0 for success.
       (return (i32.const 0)))
 (memory 1))
