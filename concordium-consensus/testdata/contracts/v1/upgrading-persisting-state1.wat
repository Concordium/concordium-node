(module

 ;; Imports
 (import "concordium" "state_lookup_entry" (func $host_state_lookup_entry (param $key_start i32) (param $key_length i32) (result i64)))
 (import "concordium" "state_entry_read" (func $host_state_entry_read (param $entry i64) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))

 ;; Helper functions

 (func $assert_eq (param $actual i32) (param $expected i32)
       (if (i32.eq (local.get $actual) (local.get $expected))
           (then nop)
           (else unreachable)))

 (func $assert_eq_64 (param $actual i64) (param $expected i64)
       (if (i64.eq (local.get $actual) (local.get $expected))
           (then nop)
           (else unreachable)))

 ;; Looks up an entry and reads the value.
 ;; Assumes the entry contains 4 bytes for a i32.
 (func $lookup_and_read_entry (param $key i32) (result i32)
       ;; Variable for holding an entry.
       (local $entry i64)

       ;; Store the key in memory.
       (i32.store (i32.const 0) ;; Offset in memory.
                  (local.get $key)) ;; Value to store.

       ;; Lookup an entry at key 1234 (i32).
       (local.set $entry (call $host_state_lookup_entry
                               (i32.const 0) ;; Offset in memory for key start.
                               (i32.const 4))) ;; Key length in bytes.

       ;; Write the value in memory to the state.
       (call $host_state_entry_read
             (local.get $entry) ;; The entry to read from.
             (i32.const 0) ;; Offset in memory for writing.
             (i32.const 4) ;; Length to read from the entry.
             (i32.const 0)) ;; Offset in the entry to start reading from.
       (drop)

       ;; Return the value read from the entry.
       (return (i32.load (i32.const 0)))) ;; Offset in memory.

 ;; Contract

 ;; Initialize contract.
 (func $init (export "init_contract") (param i64) (result i32)
       (return (i32.const 0))) ;; Successful init

 ;; Receive function for checking entries
 (func $receive_check (export "contract.check") (param $amount i64) (result i32)
       ;; Check entry at 12 (i32)
       (call $assert_eq
             (call $lookup_and_read_entry (i32.const 12))
             (i32.const 34))

       ;; Check entry at 56 (i32)
       (call $assert_eq
             (call $lookup_and_read_entry (i32.const 56))
             (i32.const 78))

       ;; Return success
       (return (i32.const 0)))
 (memory 1))
