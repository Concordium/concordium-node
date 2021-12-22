;; Test for one contract calling itself.
;; There are two entrypoints, one which just increments the counter, and another
;; which repeatedly calls the former endpoint to increase the counter by 10.
;; This latter endpoint checks the return value.

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

  (func $assert_eq_64 (param $actual i64) (param $expected i64)
    (if (i64.eq (local.get $actual) (local.get $expected))
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

  ;; return the current value of the counter in the return value.
  (func $view_counter (export "counter.view") (param i64) (result i32)
    (call $load_state (i32.const 0) (i32.const 8) (i32.const 0))
    (drop)
    (call $write_output (i32.const 0) (i32.const 8) (i32.const 0))
    (drop)
    ;; and return success
    (i32.const 0)
  )

   ;; call the counter.inc method 10 times. Check returns each time.
  (func $inc_counter_10 (export "counter.inc10") (param i64) (result i32)
    (local $n i32)
    (local $size i32)
    (local $rv i64)
    (local $index i32)
    (local.set $size (call $get_parameter_size (i32.const 0)))
    (call $get_parameter_section (i32.const 0) (i32.const 0) (local.get $size) (i32.const 0))
    (loop $loop
      (local.set $rv (call $invoke (i32.const 1) (i32.const 0) (local.get $size)))
      ;; get the index of the response
      (local.set $index (i32.wrap_i64 (i64.shr_u (local.get $rv) (i64.const 40))))
      ;; and get the parameter size, check it that it is the value of the counter
      ;; first check that the size is correct
      (call $assert_eq (call $get_parameter_size (local.get $index)) (i32.const 8))
      ;; next check that the return value is the same as the current contract state (state after the call)
      ;; write the parameter just after the initial parameter
      (call $get_parameter_section (local.get $index) (local.get $size) (i32.const 8) (i32.const 0))
      (drop)
      ;; read the contract state as well
      (call $load_state (i32.add (local.get $size) (i32.const 8)) (i32.const 8) (i32.const 0))
      (drop)
      ;; and then check that the return value is the same as the current state of the contract
      (call $assert_eq_64 (i64.load (local.get $size)) (i64.load (i32.add (local.get $size) (i32.const 8))))
      (local.set $n (i32.add (i32.const 1) (local.get $n)))
      (br_if $loop (i32.lt_u (local.get $n) (i32.const 10))))
    (drop)
    ;; and return success
    (i32.const 0)
  )
  (memory 1)
)