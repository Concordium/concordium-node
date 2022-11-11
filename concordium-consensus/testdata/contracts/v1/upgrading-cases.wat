;; This module serves as a testbed for basic testing of smart contract upgrades.
;; It supports simple upgrades together with state and balance changes.
;; The initial state before the `upgrade` call is an empty contract, with an empty state.
;; The upgrade function is called with a parameter consisting of a module to upgrade to,
;; and u8 tag indicating what state changes should be done. 0 means no changes,
;; 1 means make state changes **before** the upgrade, 2 means make state changes after the call.
;; Other values are treated as 0.
(module

  ;; Imports
  (import "concordium" "get_parameter_section" (func $host_get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))

  (import "concordium" "upgrade" (func $host_upgrade (param $start i32) (result i64)))

  (import "concordium" "state_create_entry" (func $state_create_entry (param $key_start i32) (param $key_length i32) (result i64)))
  (import "concordium" "state_entry_write" (func $state_entry_write (param $entry i64) (param $read_location i32) (param $length i32) (param $offset i32) (result i32)))


  ;; Initialize contract.
  (func $init (export "init_contract") (param i64) (result i32)
        (return (i32.const 0))) ;; Successful init

  (func $receive_upgrade (export "contract.upgrade") (param $amount i64) (result i32)
       (call $host_get_parameter_section
             (i32.const 0) ;; index.
             (i32.const 0) ;; starting write offset in memory.
             (i32.const 33) ;; number of bytes to read (32 bytes for the module reference).
             (i32.const 0)) ;; starting offset in parameter.
       (if (i32.eq (i32.const 1) (i32.load8_u (i32.const 32)))
          ;; update the state now
          (then (call $state_entry_write (call $state_create_entry (i32.const 0) (i32.const 32)) (i32.const 0) (i32.const 32)  (i32.const 0))
                 drop
                 )
       )
       (if (i64.eqz (call $host_upgrade (i32.const 0))) (then nop)
          (else unreachable))
       (if (i32.eq (i32.const 2) (i32.load8_u (i32.const 32)))
           (then (call $state_entry_write (call $state_create_entry (i32.const 0) (i32.const 32)) (i32.const 0) (i32.const 32) (i32.const 0))
                 drop
                 )
       )
       ;; Return signal to succeed
       (return (i32.const 0)))
  (memory 1))
