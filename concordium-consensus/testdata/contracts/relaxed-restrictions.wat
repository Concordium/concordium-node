;; Testing the relaxed restrictions wrt. parameter size and number of logs introduced with protocol version 5.
;;
;; The two main functions are "relax.param" and "relax.logs". Each of which has comments
;; above it explaining what it does.

(module

  ;; Imports

  (import "concordium" "get_parameter_section" (func $get_parameter_section (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "accept" (func $accept (result i32)))
  (import "concordium" "send" (func $send (param $addr_index i64) (param $addr_subindex i64)
                                (param $receive_name i32) (param $receive_name_len i32)
                                (param $amount i64) (param $parameter i32) (param $parameter_len i32) (result i32)))
  (import "concordium" "log_event" (func $log_event (param $start i32) (param $length i32) (result i32)))

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

  ;; The counter contract

  (func $init (export "init_relax") (param i64) (result i32)
    (return (i32.const 0)) ;; Successful init
  )

  ;; Invokes "relax.param-aux" with a parameter of the size provided. The parameter data is all zeroes.
  ;;
  ;; Parameters:
  ;;  - Parameter length (u16)
  ;;  - Receive name length (u16)
  ;;  - Receive name ("relax.param-aux")
  ;;  - Additional bytes, not used by contract, but used to test the parameter size limit in the scheduler
  (func $param (export "relax.param") (param i64) (result i32)
    (local $param_len i32)
    (local $receive_name_len i32)

    ;; Load the length(u16) of the parameter to position 0 in the wasm memory. (from pos 0 in parameter)
    (call $get_parameter_section (i32.const 0) (i32.const 2) (i32.const 0))
    ;; Save the size of the parameter in $param_len.
    (local.set $param_len (i32.load16_u (i32.const 0)))

    ;; Load the receive name length (u16) to position 0 in the wasm memory. (from pos 2 in parameter)
    (call $get_parameter_section (i32.const 0) (i32.const 2) (i32.const 2))
    ;; Save the receive name length in $receive_name_len.
    (local.set $receive_name_len (i32.load16_u (i32.const 0)))

    ;; Load the receive name to position 0 in the wasm memory. (from pos 4 in the parameter).
    (call $get_parameter_section (i32.const 0) (local.get $receive_name_len) (i32.const 4))

    ;; Send message to "relax.param-aux" with a parameter of the provided size.
    ;; The parameter will just contain the receive name followed by zeroes, which is ok,
    ;; since we are just testing the size limit.

    ;; Call 'test.accept' through send
    (return (call $send
      (i64.const 0)  ;; index
      (i64.const 0)  ;; subindex
      (i32.const 0)  ;; receive_name
      (local.get $receive_name_len) ;; receive_name_len (length of 'relax.param-aux")
      (i64.const 0)  ;; amount
      (i32.const 0)  ;; parameter
      (local.get $param_len))) ;; parameter_len
  )

  ;; Helper function to be called by "relax.param". Simply returns with success.
  (func $param-aux (export "relax.param-aux") (param i64) (result i32)
        (call $accept)
  )

  ;; Loops $n (u32) times and creates a log message with the message $n each time.
  ;; When it logs, it asserts that logging was successful.
  ;;
  ;; Parameters
  ;;  - n: Number of logs to create (u32).
  (func $logs (export "relax.logs") (param i64) (result i32)
    (local $i i32)
    (local $n i32)
    (call $get_parameter_section (i32.const 0) (i32.const 4) (i32.const 0))
    (local.set $n (i32.load (i32.const 0))) ;; n is the number of logs to create
    (local.set $i (i32.const 0))

    ;; If n == 0, return early.
    (if
        (i32.eq (local.get $n) (i32.const 0))
        (return (i32.const 0)))

    (loop $loop
      ;; Log a message with $n. Assert that it worked and thus returned 1.
      (call $assert_eq
          (call $log_event (i32.const 0) (i32.const 4))
          (i32.const 1))

      ;; Increment $i.
      (local.set $i (i32.add (local.get $i) (i32.const 1)))

      ;; Go to top of loop if $i < $n.
      (if
          (i32.lt_u (local.get $i) (local.get $n))
          (br $loop)
      )
    )
    (return (call $accept))
  )

  (memory 2)
)
