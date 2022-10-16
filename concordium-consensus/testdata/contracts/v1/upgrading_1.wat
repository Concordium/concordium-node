;; A module that tests the upgrading functionality introduced as part of P5.
;; It is intended that contract A of ./upgrading_0.wat upgrades to contract A on this module.
;;
;; This module contains of an init function for contract A and a receive function for inspecting that 
;; the state set before upgrading (in the old module) 'survived' the migration. Hence any state 
;; set prior to the upgrade must also be present after the upgrade.
(module 
    ;; Imports    
    (import "concordium" "upgrade" (func $upgrade (param $module_ptr i32) (result i64)))
    (import "concordium" "state_lookup_entry" (func $state_lookup_entry (param $key_start i32) (param $key_length i32) (result i64)))    
    (import "concordium" "state_entry_read" (func $state_entry_read (param $entry i64) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
    
    ;; Helper functions
    (func $assert_eq_64 (param $actual i64) (param $expected i64)
    (if (i64.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

    ;; Init a
    (func $init_a (export "init_a") (param i64) (result i32)
        (return (i32.const 0)) ;; Successful init
    )

    ;;View function for checking that the state from before the upgrade exists.
    (func $a.view (export "a.view") (result i32)
        ;; Local for the entry we wish to lookup
        (local $entry i64)
        ;; Lookup and read [0] of the state.
        ;; This should contain 8 zero bytes.
        (local.set $entry (call $state_lookup_entry (i32.const 0) (i32.const 1)))
        ;; Assert that the entry exists.
        (call $assert_eq_64 (i64.const 0) (local.get $entry))
        ;; Read from the entry
        (call $state_entry_read (local.get $entry) (i32.const 0) (i32.const 8) (i32.const 0))
        ;; Push the 8 bytes onto the stack.
        (i64.load (i32.const 0))
        ;; Pop and return the stored state.
        (return)
    )
)