;; Contains a smart contract with an 'upgrade' entrypoint, which triggers an upgrade to 'upgrading-persisting-state1.wasm' (provided as a parameter).
;; The contracts create and write to an entry in the state, before and after the triggering the upgrade.
;; The upgraded module contains an entrypoint for verifying the state persisted the upgrade.
(module

 ;; Imports
 (import "concordium" "get_parameter_section" (func $host_get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
 (import "concordium" "upgrade" (func $host_upgrade (param $start i32) (result i64)))
 (import "concordium" "state_create_entry" (func $host_state_create_entry (param $key_start i32) (param $key_length i32) (result i64)))
 (import "concordium" "state_entry_write" (func $host_state_entry_write (param $entry i64) (param $read_location i32) (param $length i32) (param $offset i32) (result i32)))

 ;; Helper functions

 (func $assert_eq_64 (param $actual i64) (param $expected i64)
       (if (i64.eq (local.get $actual) (local.get $expected))
           (then nop)
           (else unreachable)))

 ;; Create an entry at the provided key and write the value.
 ;; Note: This function writes to the memory.
 (func $create_and_write_entry (param $key i32) (param $value i32)
        ;; Variable for holding an entry.
       (local $entry i64)

       ;; Store the key in memory.
       (i32.store (i32.const 0) ;; Offset in memory.
                  (local.get $key))

       ;; Create an empty entry at key 1234 (i32).
       (local.set $entry
                  (call $host_state_create_entry
                        (i32.const 0) ;; Offset in memory for key start.
                        (i32.const 4))) ;; Key length in bytes.

       ;; Store a value in memory to write to the entry.
       (i32.store (i32.const 0) ;; Offset in memory.
                  (local.get $value)) ;; Value to store

       ;; Write the value in memory to the state.
       (call $host_state_entry_write
             (local.get $entry) ;; Entry to write to.
             (i32.const 0) ;; Offset in memory to read from.
             (i32.const 4) ;; Number of bytes to read.
             (i32.const 0)) ;; Offset in the entry to write to.
       (drop))

 ;; Contract

 ;; Initialize contract.
 (func $init (export "init_contract") (param i64) (result i32)
       (return (i32.const 0))) ;; Successful init

 ;; Receive function for triggering an upgrade and then manipulating some state.
 (func $receive_upgrade (export "contract.upgrade") (param $amount i64) (result i32)
       ;; Write something to the state.
       (call $create_and_write_entry
             (i32.const 12) ;; Key to store at
             (i32.const 34)) ;; Value to store

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

       ;; Write something to the state.
       (call $create_and_write_entry
             (i32.const 56) ;; Key to store at
             (i32.const 78)) ;; Value to store

       ;; Return success
       (return (i32.const 0)))
 (memory 1))
