;; Test checking self-balance when resuming. Both when sending to accounts and contracts.

(module

  ;; Imports

  (import "concordium" "get_parameter_size" (func $get_parameter_size (param $index i32) (result i32)))
  (import "concordium" "get_parameter_section" (func $get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "invoke" (func $invoke (param $tag i32) (param $start i32) (param $length i32) (result i64)))
  (import "concordium" "write_output" (func $write_output (param $start i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "get_receive_self_balance" (func $get_receive_self_balance (result i64)))

   ;; just a an empty contract that only holds its balance.
  (func $init_transfer(export "init_transfer") (param i64) (result i32)
    ;; no state
    (return (i32.const 0)) ;; Successful init
  )

  ;; an endpoint that just accepts.
  (func $accept (export "transfer.accept") (param i64) (result i32) 
     (return (i32.const 0))
  )

  (func $call (export "transfer.forward") (param i64) (result i32)
    (local $n i32)
    (local $size i32)
    (local $start_balance i64)
    (local.set $size (call $get_parameter_size (i32.const 0)))
    (call $get_parameter_section (i32.const 0) (i32.const 0) (local.get $size) (i32.const 0))
    ;; store the balance before the call
    (local.set $start_balance (call $get_receive_self_balance))
    ;; invoke, interpret the first 4 bytes as the instruction, the remaining bytes as the parameter
    (call $invoke (i32.load (i32.const 0)) (i32.const 4) (i32.sub (local.get $size) (i32.const 4)))
    ;; store the return value and balances before and after the call
    (i64.store (i32.const 0) (local.get $start_balance))
    (i64.store (i32.const 8) (call $get_receive_self_balance))
    (call $write_output (i32.const 0) (i32.const 16) (i32.const 0))
    ;; and return success
    (drop)
    (drop)
    (drop)
    (i32.const 0)
  )

  (memory 1)
)