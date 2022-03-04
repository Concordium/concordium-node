;; This module contains two contracts namely Contract A and Contract B.
;; The purpose if this test is to make sure that checkpointing/fallback mechanisms
;; functions properly when carrying out inter-contract communication.

;; In particular the following cases are tested.
;;
;; 1) Contract A modifies its state a bit, then calls contract B, which calls contract A, which modifies the state a bit and accepts,
;; then contract B rejects/fails (e.g., runtime error). When returning to the original contract A the state it has should be unmodified.
;; All iterators/entries it had should exist.
;;
;; 2) Contract A modifies its state/looks up iterators/entries, calls contract B,
;; which calls contract A which looks up some of its state but does not modify it.
;; When control returns to the original contract A all iterators/entries it has looked up are still valid.
;;
;; 3) Contract A modifies its state a bit, then invokes a transfer.
;; When control goes back to contract A all its iterators and entries that it looked up before the invoke are still valid and can be used.     

(module

  ;; Imports
  (import "concordium" "get_parameter_size" (func $get_parameter_size (param $index i32) (result i32)))
  (import "concordium" "get_parameter_section" (func $get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "invoke" (func $invoke (param $tag i32) (param $start i32) (param $length i32) (result i64)))
  (import "concordium" "write_output" (func $write_output (param $start i32) (param $length i32) (param $offset i32) (result i32)))

  (import "concordium" "get_receive_entrypoint_size" (func $get_ep_size (result i32)))
  (import "concordium" "get_receive_entrypoint" (func $get_ep (param $start i32)))

  (import "concordium" "state_lookup_entry" (func $state_lookup_entry (param $key_start i32) (param $key_length i32) (result i64)))
  (import "concordium" "state_create_entry" (func $state_create_entry (param $key_start i32) (param $key_length i32) (result i64)))
  (import "concordium" "state_entry_read" (func $state_entry_read (param $entry i64) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "state_entry_write" (func $state_entry_write (param $entry i64) (param $read_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "state_iterate_prefix" (func $state_iterate_prefix (param $key_start i32) (param $key_length i32) (result i64)))
  (import "concordium" "state_iterator_next" (func $state_iterator_next (param $iter i64) (result i64)))
  (import "concordium" "state_iterator_delete" (func $state_iterator_delete (param $iter i64) (result i32)))
  (import "concordium" "state_iterator_key_size" (func $state_iterator_key_size (param $iter i64) (result i32)))
  (import "concordium" "state_iterator_key_read" (func $state_iterator_key_read (param $iter i64) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))

  ;; Helper functions

  (func $assert_eq (param $actual i32) (param $expected i32)
    (if (i32.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))
    
  (func $assert_eq_64 (param $actual i64) (param $expected i64)
    (if (i64.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

  ;; Contract A

  ;; Initialize contract A.
  (func $init_a (export "init_a") (param i64) (result i32)
    (return (i32.const 0)) ;; Successful init
  )

  ;; First store the address and entrypoint to invoke of contract B at [], then
  ;; modify state and invoke B where we forward the parameters for calling "a.a_test_one_modify"..
  (func $a_test_one (export "a.a_test_one") (param $amount i64) (result i32)
    ;; Declare a local for storing the entry we're about to write to.
    (local $entry i64)
    ;; Declare a local for storing the write result
    (local $entry_write i32)
    ;; Declare locals for invoking contract B together with the return value.
    (local $pos i32)
    (local $rv i64)

    ;; Set some initial state
    ;; Create an entry at [0] and write some bytes to it.
    (local.set $entry (call $state_create_entry (i32.const 0) (i32.const 1)))
    ;; Write 8 zero bytes to the entry.
    (local.set $entry_write (call $state_entry_write (local.get $entry) (i32.const 0) (i32.const 8) (i32.const 0)))
    ;; Check that all 8 zero bytes were written to the entry.
    (call $assert_eq (i32.const 8) (local.get $entry_write))
    
    ;; store the address and entrypoint "b.test_one" of contract B
    (call $get_parameter_section
          (i32.const 0)
          (i32.const 0)
          (i32.const 16)
          (i32.const 0))
    (call $state_entry_write
          (call $state_create_entry (i32.const 0) (i32.const 0))
          (i32.const 0)
          (i32.const 16)
          (i32.const 0))
    ;; Call contract B's "test_one" receive method which will lead to failure.
    ;; Read contract B's address. Assume 16 bytes are stored in the entry.
    (call $state_entry_read
          (call $state_lookup_entry (i32.const 0) (i32.const 0))
          (local.get $pos)
          (i32.const 16)
          (i32.const 0))
    (local.set $pos (i32.add (local.get $pos) (i32.const 16)))
    (i32.store16 (local.get $pos) (call $get_parameter_size (i32.const 0)))
    (local.set $pos (i32.add (local.get $pos) (i32.const 2)))
    ;; write the parameter
    (call $get_parameter_section
          (i32.const 0)
          (local.get $pos)
          (call $get_parameter_size (i32.const 0))
          (i32.const 0))
    (local.set $pos (i32.add (local.get $pos) (call $get_parameter_size (i32.const 0))))
    ;; write length of the entrypoint
    (i32.store16 (local.get $pos) (i32.const 10)) ;; 10 is the length of 'b_test_one' in bytes.
    (local.set $pos (i32.add (local.get $pos) (i32.const 2)))
    (call $get_parameter_section
          (i32.const 0)
          (local.get $pos)
          (i32.const 10)
          (i32.const 57)) ;; the offset in the parameters for the actual 'b_test_one'
    (local.set $pos (i32.add (local.get $pos) (i32.const 10)))
    ;; write amount
    (i64.store (local.get $pos) (local.get $amount))
    (local.set $pos (i32.add (local.get $pos) (i32.const 8)))
    ;; invoke with the amount we were given, and the parameter we were called with.
    (local.set $rv (call $invoke (i32.const 1) (i32.const 0) (local.get $pos)))
    ;; Check that the invocation resulted in TRAP i.e., '0x0006_0000_0000'
    (call $assert_eq_64 (local.get $rv) (i64.const 25769803776))
    ;; now check that all state above the invocation of B is still present and remains the same as before.
    ;; Check that entry at [00] does not exist i.e. it starts with a set bit.
    (call $assert_eq_64 (i64.const 0) (i64.clz (call $state_lookup_entry (i32.const 0) (i32.const 2))))
    ;; Check that no iterator exits. Note. we check here by the id 0 which is the id that the iterator
    ;; was created with.
    
    (return (i32.const 0)) ;; return success
  )

  ;; This function modifies the following state of contract A.
  ;; First it creates a new entry at [00] and writes 8 bytes to that entry.
  ;; Create an iterator at [0] and advance it by one.
  ;; Finally it returns 42.
  (func $a_test_one_modify (export "a.a_test_one_modify") (param i64) (result i32)
    ;; Declare a local for entry [00].
    (local $entry i64)
    ;; Declare local for storing the iter.
    (local $iter i64) 
    ;; Declaring locals for storing both write results.
    (local $entry_write i32)
    ;; Create new entry at [00].
    (local.set $entry (call $state_create_entry (i32.const 0) (i32.const 0)))
    ;; Write 8 zero bytes to entry.
    (local.set $entry_write (call $state_entry_write (local.get $entry) (i32.const 0) (i32.const 8) (i32.const 0)))
    ;; Create an iterator at [0].
    (local.set $iter (call $state_iterate_prefix (i32.const 0) (i32.const 1)))
    ;; Advance the iterator by one.
    (call $state_iterator_next (local.get $iter))
    (drop)
    (return (i32.const 42)) ;; return 42
  )    

  ;; Contract B
    
  (func $init_b (export "init_b") (param i64) (result i32)    
    (return (i32.const 0)) ;; Successful init
  )

  ;; Receive method of contract B.
  ;; This function calls Contract A's receive method "a.a_test_one_modify" and upon completion of
  ;; that call this function will produce an runtime error.
  (func $b_test_one (export "b.b_test_one") (param $amount i64) (result i32)
    (local $pos i32)
    (local $rv i64)
    ;; Read contract A's address. Assume 16 bytes are stored in the entry.
    (call $state_entry_read
          (call $state_lookup_entry (i32.const 0) (i32.const 0))
          (local.get $pos)
          (i32.const 16)
          (i32.const 0))
    (local.set $pos (i32.add (local.get $pos) (i32.const 16)))
    (i32.store16 (local.get $pos) (call $get_parameter_size (i32.const 0)))
    (local.set $pos (i32.add (local.get $pos) (i32.const 2)))
    ;; write the parameter
    (call $get_parameter_section
          (i32.const 0)
          (local.get $pos)
          (call $get_parameter_size (i32.const 0))
          (i32.const 0))
    (local.set $pos (i32.add (local.get $pos) (call $get_parameter_size (i32.const 0))))
    ;; write length of the entrypoint
    (i32.store16 (local.get $pos) (i32.const 17)) ;; length of 'a_test_one_modify'
    ;; Bump the pointer.
    (local.set $pos (i32.add (local.get $pos) (i32.const 2)))
    (call $get_parameter_section
          (i32.const 1)
          (local.get $pos)
          (i32.const 17)
          (i32.const 18)) ;; offset into the parameters receive method.
    ;; write amount
    (i64.store (local.get $pos) (local.get $amount))
    (local.set $pos (i32.add (local.get $pos) (i32.const 8)))
    ;; invoke with the amount we were given, and the parameter we were called with.
    (local.set $rv (call $invoke (i32.const 1) (i32.const 0) (local.get $pos)))
    ;; Check that contract A 'a.a_test_one_modify' returned '42'
    (call $assert_eq_64 (i64.const 42) (local.get $rv))
    ;; Finally we impose a runtime failure letting dropping the call stack back to the initial call
    ;; from Contract A's "a_test_one" function.
    unreachable
  )

  (memory 1)
)