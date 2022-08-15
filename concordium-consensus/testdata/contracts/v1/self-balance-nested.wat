;; Test checking self-balance after nested calls.

(module

  ;; Imports

  (import "concordium" "get_parameter_size" (func $get_parameter_size (param $index i32) (result i32)))
  (import "concordium" "get_parameter_section" (func $get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "invoke" (func $invoke (param $tag i32) (param $start i32) (param $length i32) (result i64)))
  (import "concordium" "write_output" (func $write_output (param $start i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "get_receive_self_balance" (func $get_receive_self_balance (result i64)))
  (import "concordium" "state_create_entry" (func $state_create_entry (param $key_start i32) (param $key_length i32) (result i64)))
  (import "concordium" "state_lookup_entry" (func $state_lookup_entry (param $key_start i32) (param $key_length i32) (result i64)))
  (import "concordium" "state_entry_write" (func $state_entry_write (param $entry i64) (param $read_location i32) (param $length i32) (param $offset i32) (result i32)))

   ;; initialize the empty test contract
  (func $init_transfer(export "init_test") (param i64) (result i32)
    (return (i32.const 0)) ;; Successful init
  )

  ;; an endpoint that just accepts
  (func $accept (export "test.accept") (param i64) (result i32)
     (return (i32.const 0))
  )

  ;; An endpoint that invokes with the given parameter, and after the call
  ;; gets the self balance
  (func $invoke_nested (export "test.invoke_nested") (param i64) (result i32)
    (local $n i32)
    (local $size i32)
    (local.set $size (call $get_parameter_size (i32.const 0)))
    (call $state_create_entry (i32.const 0) (i32.const 0))
    (call $get_parameter_section (i32.const 0) (i32.const 0) (local.get $size) (i32.const 0))
    ;; invoke, interpret the first 4 bytes as the instruction, the remaining bytes as the parameter
    (call $invoke (i32.load (i32.const 0)) (i32.const 4) (i32.sub (local.get $size) (i32.const 4)))
    ;; get self balance and store it in the first 8 bytes of the return value.
    (i64.store (i32.const 0) (call $get_receive_self_balance))
    (call $write_output (i32.const 0) (i32.const 8) (i32.const 0))
    ;; append the result of the call to the return value of this method (this is assuming success of the call)
    (call $get_parameter_section (i32.const 1) (i32.const 0) (call $get_parameter_size (i32.const 1)) (i32.const 0))
    (call $write_output (i32.const 0) (call $get_parameter_size (i32.const 1)) (i32.const 8))
    ;; and accept
    (return (i32.const 0))
  )

  (memory 1)
)