;; Test for one contract calling another, both v1.
;; The first contract just maintains a counter as the state.
;; The second one calls it with a parameter which should be added to the counter.
;; The return value is the value of the counter after the call.
;;
;; Docs:
;;

(module

  ;; Imports

  (import "concordium" "get_parameter_size" (func $get_parameter_size (param $index i32) (result i32)))
  (import "concordium" "get_parameter_section" (func $get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "invoke" (func $invoke (param $tag i32) (param $start i32) (param $length i32) (result i64)))
  (import "concordium" "write_output" (func $write_output (param $start i32) (param $length i32) (param $offset i32) (result i32)))

  ;; state functions
  (import "concordium" "load_state" (func $load_state (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "write_state" (func $write_state (param $read_location i32) (param $length i32) (param $offset i32) (result i32)))

  ;; Helper Functions

  (func $assert_eq (param $actual i32) (param $expected i32)
    (if (i32.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

  (func $assert_ne (param $actual i32) (param $expected i32)
    (if (i32.ne (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

  ;; Gets an address from the parameters and asserts that the size is correct.
  ;; The address is saved in memory at location 0.
  (func $save_addr_from_param_to_mem_0
    (call $assert_eq
      (call $get_parameter_section (i32.const 0) (i32.const 0) (i32.const 32) (i32.const 0))
      (i32.const 32))
  )

  ;; The counter contract

  (func $init_counter (export "init_counter") (param i64) (result i32)
    (i64.store (i32.const 0) (i64.const 0))
    (call $write_state (i32.const 0) (i32.const 8) (i32.const 0))
    (return (i32.const 0)) ;; Successful init
  )

  (func $inc_counter (export "counter.inc") (param i64) (result i32)
    (call $load_state (i32.const 0) (i32.const 8) (i32.const 0))
    (drop)
    ;; read the integer from the contract state, add 1 to it and store it
    (i64.store (i32.const 0) (i64.add (i64.const 1) (i64.load (i32.const 0))))
    ;; update the contract state
    (call $write_state (i32.const 0) (i32.const 8) (i32.const 0))
    (drop)
    ;; and then write the return value
    (call $write_output (i32.const 0) (i32.const 8) (i32.const 0))
    (drop)
    ;; and return success
    (i32.const 0)
  )

   ;; call the counter.inc method 10 times
  (func $inc_counter_10 (export "counter.inc10") (param i64) (result i32)
    (local $n i32)
    (local $size i32)
    (local.set $size (call $get_parameter_size (i32.const 0)))
    (call $get_parameter_section (i32.const 0) (i32.const 0) (local.get $size) (i32.const 0))
    (loop $loop
      (call $invoke (i32.const 1) (i32.const 0) (local.get $size))
      (drop) ;; ignore the return value
      (local.set $n (i32.add (i32.const 1) (local.get $n)))
      (br_if $loop (i32.lt_u (local.get $n) (i32.const 10))))
    (drop)
    ;; and return success
    (i32.const 0)
  )

  (memory 1)
)