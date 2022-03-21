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
  (import "concordium" "state_delete_entry" (func $state_delete_entry (param $key_start i32) (param $key_length i32) (result i32)))
  (import "concordium" "state_entry_read" (func $state_entry_read (param $entry i64) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "state_entry_write" (func $state_entry_write (param $entry i64) (param $read_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "state_delete_prefix" (func $state_delete_prefix (param $key_start i32) (param $key_length i32) (result i32)))
  (import "concordium" "state_iterate_prefix" (func $state_iterate_prefix (param $key_start i32) (param $key_length i32) (result i64)))
  (import "concordium" "state_iterator_next" (func $state_iterator_next (param $iter i64) (result i64)))
  (import "concordium" "state_iterator_delete" (func $state_iterator_delete (param $iter i64) (result i32)))
  (import "concordium" "state_iterator_key_size" (func $state_iterator_key_size (param $iter i64) (result i32)))
  (import "concordium" "state_iterator_key_read" (func $state_iterator_key_read (param $iter i64) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "state_entry_size" (func $state_entry_size (param $entry i64) (result i32)))
  (import "concordium" "state_entry_resize" (func $state_entry_resize (param $entry i64) (param $new_size i32) (result i32)))

  ;; Helper functions

  (func $assert_eq (param $actual i32) (param $expected i32)
    (if (i32.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))
    
  (func $assert_eq_64 (param $actual i64) (param $expected i64)
    (if (i64.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

  (func $assert_entry (param $entry i64)
    (if (i64.eq (i64.xor (i64.const 9223372036854775807) (i64.shl (i64.const 1) (i64.const 63))) (local.get $entry))
      (then unreachable)
      (else nop)))

  (func $assert_entry_absent (param $entry i64)
    (if (i64.ne (i64.xor (i64.const 9223372036854775807) (i64.shl (i64.const 1) (i64.const 63))) (local.get $entry))
      (then unreachable)
      (else nop)))
    
  ;; Contract A

  ;; Initialize contract A.
  (func $init_a (export "init_a") (param i64) (result i32)
    (return (i32.const 0)) ;; Successful init
  )

  ;; First store the address and entrypoint to invoke of contract B at [], then
  ;; modify state and invoke B where we forward the parameters for calling "a.a_test_one_modify"..
  (func $a_modify_proxy (export "a.a_modify_proxy") (param $amount i64) (result i32)
    ;; Declare a local for storing the entry we're about to write to.
    (local $entry i64)
    ;; Declare a local for storing the write result
    (local $entry_write i32)
    ;; the offset in memory
    (local $offset i32)
    ;; a local for storing the return value of an invocation.
    (local $rv i64)
    ;; a local for an iterator.
    (local $iter i64)

    ;; Set some initial state
    ;; Create an entry at [0] and write some bytes to it.
    (local.set $entry (call $state_create_entry (i32.const 0) (i32.const 1)))
    ;; Write 8 zero bytes to the entry.
    (local.set $entry_write (call $state_entry_write (local.get $entry) (i32.const 0) (i32.const 8) (i32.const 0)))
    ;; Check that all 8 zero bytes were written to the entry.
    (call $assert_eq (i32.const 8) (local.get $entry_write))
    
    (call $state_entry_write
          (call $state_create_entry (i32.const 0) (i32.const 0))
          (i32.const 0)
          (i32.const 16)
          (i32.const 0))
    (call $get_parameter_section
          (i32.const 0)
          (i32.const 0)
          (call $get_parameter_size (i32.const 0))
          (i32.const 0))
    (local.set $offset (call $get_parameter_size (i32.const 0)))
    ;; create an empty entry at [000]
    (call $assert_entry (call $state_create_entry (i32.const 0) (i32.const 3)))
    ;; create an entry at [0000]
    (call $assert_entry (call $state_create_entry (i32.const 0) (i32.const 4)))
    ;; create an entry at [00000]
    (call $assert_entry (call $state_create_entry (i32.const 0) (i32.const 5)))
    ;; create an iterator at [0000]
    (local.set $iter (call $state_iterate_prefix (i32.const 0) (i32.const 4)))
    (call $assert_eq_64 (i64.const 0) (local.get $iter)) ;; the first iter will have id 0.

    ;; We carry out a transfer if this contract is invoked with an amount of '3'.
    (if (i64.eq (i64.const 3) (local.get $amount))
        (then (local.set $rv (call $invoke (i32.const 0) (i32.const 0) (i32.const 40))))
        (else (local.set $rv (call $invoke (i32.const 1) (i32.const 0) (call $get_parameter_size (i32.const 0))))))

    ;; Otherwise we do a contract invocation.
    ;; If this contract was invoked with '1' then it is expected that the contract invocation is:
    ;; 1. Invoking a v1 contract and
    ;; 2. A subsequent invocation will trap.
    ;; If this contract was invoked with '2' then it is expected that the contract invocation is:
    ;; 1. Invoking a v0 contract and
    ;; 2. a subsequent invocation will trap.
    ;; Common for both cases above is that it is expected that this contract does not have an altered state after invocation.
    ;; Otherwise it is expected that the invocations are successfully and thus any changes related to the state
    ;; of this contract are reflected after the invocation.
    (if (i64.eq (i64.const 1) (local.get $amount))
      ;; Check that the invocation resulted in TRAP i.e., '0x0006_0000_0000'
      (then (call $assert_eq_64 (local.get $rv) (i64.const 25769803776)))
      (else (if (i64.eq (i64.const 2) (local.get $amount))
        ;; When a v0 fails execution then it returns '0x0005_0000_0000' back to the calling v1 contract.
        (then (call $assert_eq_64 (local.get $rv) (i64.const 21474836480)))
        ;; last 5 bytes are 0 if success
        (else (call $assert_eq_64 (i64.shl (local.get $rv) (i64.const 24)) (i64.const 0))))))


    ;; if we we're called with an amount of '4' then we
    ;; expect changes from subsequent calls to be reflected now,
    ;; otherwise we don't. 
    (if (i64.eq (i64.const 4) (local.get $amount))
      ;; state should be modified in this case.
      (then
        ;; a_modify resizes [0] to 1.
        (call $assert_eq (i32.const 1) (call $state_entry_size (call $state_lookup_entry (local.get $offset) (i32.const 1))))
        ;; a_modify writes 8 bytes to [000]
        (call $assert_eq (i32.const 8) (call $state_entry_size (call $state_lookup_entry (local.get $offset) (i32.const 2))))
        ;; a_modifies deletes [000]
        (call $assert_entry_absent (call $state_lookup_entry (local.get $offset) (i32.const 3)))
        ;; a_modifies deletes the iterator, so a double delete will return in u32::max.
        (call $assert_eq (i32.xor (i32.const 2147483647) (i32.shl (i32.const 1) (i32.const 31))) (call $state_iterator_delete (local.get $iter)))
      )
      ;; state should not be modified in this case.
      (else
        ;; check that the size of [0] has not changed.
        (call $assert_eq
          (i32.const 8)
          (call $state_entry_size (local.get $entry)))
        ;; Check that entry at [00] does not exist i.e. it starts with a set bit.
        (call $assert_eq_64 (i64.const 0) (i64.clz (call $state_lookup_entry (i32.const 0) (i32.const 2))))
        ;; check that we can lookup [000]
        (call $assert_entry (call $state_lookup_entry (i32.const 0) (i32.const 3)))
        ;; check that the key size is still 4 for the iter.
        (call $assert_eq
          (i32.const 4)
          (call $state_iterator_key_size (local.get $iter)))    
        ;; delete the state again
        ;; first we check that we cannot delete because of the iterator lingering at [0].
        (call $assert_eq (i32.const 0) (call $state_delete_prefix (i32.const 0) (i32.const 0)))
        ;; delete the iterator
        (call $state_iterator_delete (local.get $iter))
        ;; now delete the whole state.
        (call $assert_eq (i32.const 2) (call $state_delete_prefix (i32.const 0) (i32.const 0)))
        (drop)))
    (return (i32.const 0)) ;; return success
  )

  ;; This function modifies the following state of contract A.
  ;; precondition: 'a_modify_proxy' must be called before this one.
  (func $a_modify (export "a.a_modify") (param i64) (result i32)
    ;; Declare a local for entry [00].
    (local $entry i64)
    ;; Declaring locals for storing both write results.
    (local $entry_write i32)
    ;; Create new entry at [00].
    (local.set $entry (call $state_create_entry (i32.const 0) (i32.const 2)))
    ;; Write 8 zero bytes to entry [00].
    (local.set $entry_write (call $state_entry_write (local.get $entry) (i32.const 0) (i32.const 8) (i32.const 0)))
    ;; Resize the entry [0] to 1 byte.
    (call $state_entry_resize
        (call $state_lookup_entry (i32.const 0) (i32.const 1))
        (i32.const 1))
    ;; delete [000]
    (call $state_delete_entry (i32.const 0) (i32.const 3))
    ;; delete the iter
    (call $state_iterator_delete (i64.const 0)) ;; the iter created in the first receive function.
    (return (i32.const 0)) ;; return success.
  )

  ;; This function only looks up state but does not modify.
  (func $a_no_modify (export "a.a_no_modify") (param i64) (result i32)
    ;; lookup [0] and check it has a size of 8.
    (call $assert_eq
        (i32.const 8)
        (call $state_entry_size 
            (call $state_lookup_entry (i32.const 0) (i32.const 1))))
    (call $state_iterator_next (i64.const 0)) ;; only one iterator has been created and it has id 0.
    ;; the iterator should now be at [00000]
    (call $assert_eq (i32.const 5) (call $state_iterator_key_size (i64.const 0)))
    (return (i32.const 0)) ;; return success
  )
    
  ;; Contract B
  (func $init_b (export "init_b") (param i64) (result i32)    
    (return (i32.const 0)) ;; Successful init
  )

  ;; This function calls Contract A's receive method "a.a_test_one_modify" and upon completion of
  ;; that call this function will produce an runtime error.
  (func $b_forward_crash (export "b.b_forward_crash") (param $amount i64) (result i32)
    (local $rv i64)
    (call $get_parameter_section
        (i32.const 0)
        (i32.const 0)
        (call $get_parameter_size (i32.const 0))
        (i32.const 0))
    ;; invoke with the amount we were given, and the parameter we were called with.
    (local.set $rv (call $invoke (i32.const 1) (i32.const 0) (call $get_parameter_size (i32.const 0))))
    ;; Finally we impose a runtime failure here.
    unreachable
  )

  ;; This function calls Contract A's receive method "a.a_test_one_modify" and upon completion of
  ;; that call this function will produce an runtime error.
  (func $b_forward (export "b.b_forward") (param $amount i64) (result i32)
    (local $rv i64)
    (call $get_parameter_section
        (i32.const 0)
        (i32.const 0)
        (call $get_parameter_size (i32.const 0))
        (i32.const 0))
    ;; invoke with the amount we were given, and the parameter we were called with.
    (local.set $rv (call $invoke (i32.const 1) (i32.const 0) (call $get_parameter_size (i32.const 0))))
    (return (i32.const 0)) ;; return success.
  )

  (memory 1)
)