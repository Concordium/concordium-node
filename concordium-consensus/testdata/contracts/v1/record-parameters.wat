;; Test with a contract that records all parameters it has ever received
;; and allows their queries.
(module

  ;; Imports

  (import "concordium" "get_parameter_size" (func $get_parameter_size (param $index i32) (result i32)))
  (import "concordium" "get_parameter_section" (func $get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "invoke" (func $invoke (param $tag i32) (param $start i32) (param $length i32) (result i64)))
  (import "concordium" "write_output" (func $write_output (param $start i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "get_receive_self_address" (func $get_receive_self_address (param $start i32)))

  ;; state functions
  (import "concordium" "state_lookup_entry" (func $state_lookup_entry (param $key_start i32) (param $key_length i32) (result i64)))
  (import "concordium" "state_create_entry" (func $state_create_entry (param $key_start i32) (param $key_length i32) (result i64)))
  (import "concordium" "state_entry_read" (func $state_entry_read (param $entry i64) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "state_entry_write" (func $state_entry_write (param $entry i64) (param $read_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "state_iterate_prefix" (func $state_iterator_prefix (param $key_start i32) (param $key_length i32) (result i64)))
  (import "concordium" "state_iterator_next" (func $state_iterator_next (param $iterator i64) (result i64)))

  ;; The counter contract

  ;; when initializing we only store the "allocator" at the empty key.
  ;; The allocator is used as the key to store further entries.
  (func $init_recorder (export "init_recorder") (param i64) (result i32)
    (local $entry i64)
    (local.set $entry (call $state_create_entry (i32.const 0) (i32.const 0)))
    ;; write 8 zeros at position 0
    (i64.store (i32.const 0) (i64.const 0))
    ;; store the state in the entry whose key is 8 zeroes.
    (call $state_entry_write (local.get $entry) (i32.const 0) (i32.const 8) (i32.const 0))
    (return (i32.const 0)) ;; Successful init
  )

  (func $recorder_record (export "recorder.record") (param i64) (result i32)
    (local $allocator_entry i64)
    (local $parameter_entry i64)
    ;; load the allocator
    (i64.store (i32.const 0) (i64.const 0))
    (local.set $allocator_entry (call $state_lookup_entry (i32.const 0) (i32.const 0)))
    (call $state_entry_read (local.get $allocator_entry) (i32.const 0) (i32.const 8) (i32.const 0))
    ;; use the allocator to create a new entry keyed by the current value of the allocator
    (local.set $parameter_entry (call $state_create_entry (i32.const 0) (i32.const 8)))
    ;; update the allocator
    (i64.store (i32.const 0) (i64.add (i64.const 1) (i64.load (i32.const 0))))
    (call $state_entry_write (local.get $allocator_entry) (i32.const 0) (i32.const 8) (i32.const 0))
    ;; now store the parameter at the newly allocated key
    ;; first read the parameter
    (call $get_parameter_section (i32.const 0) (i32.const 0) (call $get_parameter_size (i32.const 0)) (i32.const 0))
    (call $state_entry_write (local.get $parameter_entry) (i32.const 0) (call $get_parameter_size (i32.const 0)) (i32.const 0))
    ;; and return success
    (return (i32.const 0))
  )

  ;; an entrypoint that expects a numeric (u64) parameter and calls recorder.record
  ;; entrypoint that amount of times, with parameters parameter..0
  (func $recorder_record_u64 (export "recorder.record_u64") (param i64) (result i32)
    (local $counter i64)
    (call $get_parameter_section (i32.const 0) (i32.const 0) (i32.const 8) (i32.const 0))
    (local.set $counter (i64.load (i32.const 0)))
    (loop $countdown
       (if (i64.ge_u (local.get $counter) (i64.const 1))
         (then
           ;; write the parameter to invoke, starting at location 1
           (call $get_receive_self_address (i32.const 1))
           ;; parameter length
           (i32.store16 (i32.const 17) (i32.const 8))
           ;; parameter
           (i64.store (i32.const 19) (local.get $counter))
           ;; name length (of "record")
           (i32.store16 (i32.const 27) (i32.const 6))
           ;; the actual name "record" (ascii code points)
           (i32.store8 (i32.const 29) (i32.const 114))
           (i32.store8 (i32.const 30) (i32.const 101))
           (i32.store8 (i32.const 31) (i32.const 99 ))
           (i32.store8 (i32.const 32) (i32.const 111))
           (i32.store8 (i32.const 33) (i32.const 114))
           (i32.store8 (i32.const 34) (i32.const 100))
           ;; finally the amount
           (i64.store (i32.const 35) (i64.const 0))
           ;; length is 16 + 2 + 8 + 2 + 6 + 8
           (call $invoke (i32.const 1) (i32.const 1) (i32.const 51))
           ;; count down, and iterate 
           (local.set $counter (i64.sub (local.get $counter) (i64.const 1)))
           (br $countdown)
         )
       )
    )
    (return (i32.const 0)) ;; return success
  )
  (memory 1)
)