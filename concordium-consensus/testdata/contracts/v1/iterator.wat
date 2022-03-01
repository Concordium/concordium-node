;; A contract that creates a state and traverses it by using the iterator.
;; Furthermore it is checked that while a subtree is captured by an iterator,
;; then one cannot carry out structural changes to the ‘locked’ subtree.
;; The contract checks these properties by itself thus any failures will result in a
;; runtime failure when executing the contract.

(module
    
  ;; Imports
  (import "concordium" "get_parameter_size" (func $get_parameter_size (param $index i32) (result i32)))
  (import "concordium" "state_create_entry" (func $state_create_entry (param $key_start i32) (param $key_length i32) (result i64)))
  (import "concordium" "state_entry_read" (func $state_entry_read (param $entry i64) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "state_lookup_entry" (func $state_lookup_entry (param $key_start i32) (param $key_length i32) (result i64)))
  (import "concordium" "state_entry_write" (func $state_entry_write (param $entry i64) (param $read_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "state_delete_entry" (func $state_delete_entry (param $entry i64) (result i32)))
  (import "concordium" "state_delete_prefix" (func $state_delete_prefix (param $key_start i32) (param $key_length i32) (result i32)))
    
  ;; Iterator functions
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

  (func $assert_positive_64 (param $value i64)
    (if (i64.gt_s (local.get $value) (i64.const 0))
      (then nop)
      (else unreachable)))

     
  ;; Initialize the state.
  ;;
  ;;         []
  ;;     [000][1]
  (func $init_iterator (export "init_iterator") (param i64) (result i32)
    ;; Branch once
    (call $state_create_entry (i32.const 0) (i32.const 3))
    ;; Branch twice
    (call $state_create_entry (i32.const 1) (i32.const 1))
    ;; pop the stack
    (drop)
    (drop)
    ;; return 0 for success
    (return (i32.const 0))
  )
  ;; Create an iterator at [00].
  ;; Advance it and make sure the result is 'ok',
  ;; Read the key size at current position - should be len([000]).
  ;; Read the key - Should be [000]
  ;; Advance the iterator again. The result should be 'None'.
  ;; Read the key size and key and confirm they are the same as before as there
  ;; are no more nodes to visit.
  ;; Finally it is tested that an invalid iterator id results in all but the second bit is set.
  (func $iteratetest (export "iterator.iteratetest") (param i64) (result i32)
    ;; Declare a local for storing iter
    (local $iter i64)
    ;; Declare a local for storing the second iter
    (local $iter2 i64)
    ;; Declare local for storing the result of advancing the iterator.
    (local $iter_next_res i64)
    ;; Declare local for storing the key size
    (local $key_size i32)
    ;; Declare local for storing the key read result
    (local $key_read_res i32)
    ;; Declare local for storing call of next on invalid iterator.
    (local $iter_invalid_next_res i64)
    ;; Create the iterator at [00]
    (local.set $iter (call $state_iterate_prefix (i32.const 0) (i32.const 2)))
    ;; As this is the first iterator then it is sufficient to verify if the
    ;; create iter response is 0.
    (call $assert_eq_64 (i64.const 0) (local.get $iter))
    ;; Create another iterator at the same place and check the result is starting with a zero bit.
    (local.set $iter2 (call $state_iterate_prefix (i32.const 0) (i32.const 2)))
    (call $assert_eq_64 (i64.const 0) (i64.shr_u (local.get $iter2) (i64.const 1)))
    ;; Advance the iterator and make sure the result is ok (first bit is 0) (the last 7 bits is the entry)
    (local.set $iter_next_res (call $state_iterator_next (local.get $iter)))
    (call $assert_eq_64 (i64.const 0) (i64.shr_u (local.get $iter_next_res) (i64.const 7)))
    ;; Get the key size at the current node.
    (local.set $key_size (call $state_iterator_key_size (local.get $iter)))
    (call $assert_eq (local.get $key_size) (i32.const 3))
    ;; Read the key 
    (local.set $key_read_res (call $state_iterator_key_read (local.get $iter) (i32.const 0) (i32.const 3) (i32.const 0)))
    (call $assert_eq (local.get $key_read_res) (i32.const 3))
    ;; Advance the iterator again and check that the result is 'None'.
    (local.set $iter_next_res (call $state_iterator_next (local.get $iter)))
    (call $assert_eq_64 (i64.and (local.get $iter_next_res) (i64.const 1)) (i64.const 1))
    ;; Read the key size and key again as above.
    ;; These operations should yield the same results as the iterator is exausted at this point.
    (local.set $key_size (call $state_iterator_key_size (local.get $iter)))
    (call $assert_eq (local.get $key_size) (i32.const 3))
    ;; Read the key 
    (local.set $key_read_res (call $state_iterator_key_read (local.get $iter) (i32.const 0) (i32.const 3) (i32.const 0)))
    (call $assert_eq (local.get $key_read_res) (i32.const 3))
    ;; Check calling next on an invalid iterator returns correct result i.e. all but the second bit is set.
    (local.set $iter_invalid_next_res (call $state_iterator_next (i64.const 42)))
    (call $assert_eq_64 (local.get $iter_invalid_next_res) (i64.xor (i64.shl (i64.const 1) (i64.const 63)) (i64.xor (i64.const 9223372036854775807) (i64.shl (i64.const 1) (i64.const 62)))))
    ;; Delete the iter and check that the result is correct.
    (call $assert_eq (i32.const 1) (call $state_iterator_delete (local.get $iter)))
    ;; Delete the iter again and check that the return value is correct.
    (call $assert_eq (i32.const 0) (call $state_iterator_delete (local.get $iter)))
    ;; Delete a faulty iter check that the return value is correct.
    (call $assert_eq (i32.xor (i32.const 2147483647) (i32.shl (i32.const 1) (i32.const 31))) (call $state_iterator_delete (i64.const 42)))
    (return (i32.const 0)) ;; return ok.
  )

  ;; This test checks the 'locking' of an subtree
  (func $lockingtest (export "iterator.lockingtest") (param i64) (result i32)
    ;; Declare a local for storing iter which will lock the subtree [00].
    (local $iter i64)
    ;; Declare local for storing the root
    (local $entry_root i64)
    ;; Declare local for storing the locked entry
    (local $entry_locked i64)
    ;; Declare local for storing the entry which should not be locked.
    (local $entry i64)
    ;; Create the iterator at [00]
    (local.set $iter (call $state_iterate_prefix (i32.const 0) (i32.const 2)))
    ;; Try delete [000] which should be locked
    (local.set $entry_locked (call $state_lookup_entry (i32.const 0) (i32.const 3)))
    (call $assert_eq (call $state_delete_entry (local.get $entry_locked)) (i32.xor (i32.const 2147483647) (i32.shl (i32.const 1) (i32.const 31))))
    ;; Try add [0000] which should be locked.
    (call $assert_eq_64 (call $state_create_entry (i32.const 0) (i32.const 4)) (i64.xor (i64.const 9223372036854775807) (i64.shl (i64.const 1) (i64.const 63))))
    ;; Try delete [0] which should not be ok.
    (local.set $entry_locked (call $state_lookup_entry (i32.const 0) (i32.const 1)))
    (call $assert_eq (i32.const 0) (call $state_delete_prefix (i32.const 0) (i32.const 0)))
    ;; Try delete [1] which should be ok.
    (local.set $entry (call $state_lookup_entry (i32.const 1) (i32.const 1)))
    (call $assert_eq (call $state_delete_entry (local.get $entry)) (i32.const 1))
    ;; Try add [1] again which should be ok.
    ;; It suffices to check that the result is positive here since an 'ok' result is comprised by a 0 byte and then the entry id.
    (call $assert_positive_64 (call $state_create_entry (i32.const 1) (i32.const 1)))
    ;; Delete the iter and check we can insert into the locked subtree and afterwards we delete the whole state.
    (call $assert_eq (i32.const 1) (call $state_iterator_delete (local.get $iter)))
    ;; It suffices to check that the result is positive here since an 'ok' result is comprised by a 0 byte and then the entry id.
    (call $assert_positive_64 (call $state_create_entry (i32.const 0) (i32.const 4)))
    (call $assert_eq (i32.const 2) (call $state_delete_prefix (i32.const 0) (i32.const 0)))
    (return (i32.const 0)) ;; return ok.
  )
  
  (memory 1)
)