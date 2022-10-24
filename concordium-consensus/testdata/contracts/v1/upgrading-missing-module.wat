;; A module that tests the upgrading functionality introduced as part of P5.
;;
;; This module contains a contract with a receive function which calls upgrade to a module reference not deployed.
;; It ensures the upgrade fails because of missing module and then returns success.
(module
    ;; Imports
    (import "concordium" "upgrade" (func $host_upgrade (param $module_ptr i32) (result i64)))

    ;; Helper functions
    (func $assert_eq_64 (param $actual i64) (param $expected i64)
    (if (i64.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

    ;; Init contract
    (func $init_contract (export "init_contract") (param i64) (result i32)
        (return (i32.const 0)) ;; Successful init
    )

    ;; Upgrade
    (func $contract_upgrade (export "contract.upgrade")  (param i64) (result i32)
        ;; Store 32 bytes with a value of 0 in memory.
        (i64.store (i32.const 0) ;; Offset in memory.
                   (i64.const 0)) ;; Value to store.
        (i64.store (i32.const 8) ;; Offset in memory.
                   (i64.const 0)) ;; Value to store.
        (i64.store (i32.const 16) ;; Offset in memory.
                   (i64.const 0)) ;; Value to store.
        (i64.store (i32.const 24) ;; Offset in memory.
                   (i64.const 0)) ;; Value to store.

        ;; Trigger the upgrade using the 32 bytes for the module reference
        (call $assert_eq_64
              (i64.const 30064771072) ;; expect the error code for missing module 0x0000_0007_0000_0000.
              (call $host_upgrade (i32.const 0)))

        ;; And now we simply return 0 for success.
        (return (i32.const 0))
    )
    (memory 1)
)
