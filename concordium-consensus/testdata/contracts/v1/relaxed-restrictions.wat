;; Test for the relaxation of restrictions on the parameter size, return value size, and number of logs.
;;
;; The three main functions are "relax.param", "relax.return-value", and "relax.logs". Each of which has comments
;; above it explaining what it does.

(module

  ;; Imports

  (import "concordium" "get_parameter_section" (func $get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "invoke" (func $invoke (param $tag i32) (param $start i32) (param $length i32) (result i64)))
  (import "concordium" "write_output" (func $write_output (param $start i32) (param $length i32) (param $offset i32) (result i32)))
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
  ;;  - Entrypoint name length (u16)
  ;;  - Entrypoint name ("relax.param-aux")
  ;;  - Additional bytes, not used by contract, but used to test the parameter size limit in the scheduler
  (func $param (export "relax.param") (param i64) (result i32)
    ;; Cursor used for writing data into wasm memory.
    (local $cursor i32)
    (local $param_len i32)
    (local $entrypoint_name_len i32)
    (local.set $cursor (i32.const 0))

    ;; Let the first 8 bytes be 0 for contract index.
    (local.set $cursor (i32.add (local.get $cursor) (i32.const 8)))
    ;; And the next 8 bytes be 0 for contract subindex.
    (local.set $cursor (i32.add (local.get $cursor) (i32.const 8)))
    ;; Load the length(u16) of the parameter at the cursor position. (at pos 0 in parameter)
    (call $get_parameter_section (i32.const 0) (local.get $cursor) (i32.const 2) (i32.const 0))
    ;; Save the size of the parameter in $param_len.
    (local.set $param_len (i32.load16_u (local.get $cursor)))
    ;; Move the cursor beyond the parameter length.
    (local.set $cursor (i32.add (local.get $cursor) (i32.const 2)))
    ;; Move the cursor beyond the parameter itself, which we just keep as 0s as we only care about its size.
    (local.set $cursor (i32.add (local.get $cursor) (local.get $param_len)))
    ;; Load the entrypoint name length (u16). (at pos 2 in parameter)
    (call $get_parameter_section (i32.const 0) (local.get $cursor) (i32.const 2) (i32.const 2))
    ;; Save the entrypoint name length in $entrypoint_name_len.
    (local.set $entrypoint_name_len (i32.load16_u (local.get $cursor)))
    ;; Move the cursor beyond the length.
    (local.set $cursor (i32.add (local.get $cursor) (i32.const 2)))
    ;; Load the entrypoint name (at pos 4 in the parameter).
    (call $get_parameter_section (i32.const 0) (local.get $cursor) (local.get $entrypoint_name_len) (i32.const 4))
    ;; Move the cursor beyond the entrypoint name.
    (local.set $cursor (i32.add (local.get $cursor) (local.get $entrypoint_name_len)))
    ;; Move the cursor 8 bytes forward to represent an amount (u64) of 0.
    (local.set $cursor (i32.add (local.get $cursor) (i32.const 8)))

    ;; Invoke "relax.param-aux" with the data we just created in the wasm memory.
    ;; The cursor is now the total length of the parameter to invoke.
    (call $invoke (i32.const 1) (i32.const 0) (local.get $cursor))
    (return (i32.const 0))
  )

  ;; Helper function to be called by "relax.param". Simply returns with success.
  (func $param-aux (export "relax.param-aux") (param i64) (result i32)
        (i32.const 0)
  )

  ;; Creates a return value of size $n (u32) and asserts that all of it was successfully written.
  ;;
  ;; Parameters:
  ;;  - n: Size of return value to be created (u32).
  (func $return-value (export "relax.return-value") (param i64) (result i32)
    (local $n i32)
    (call $get_parameter_section (i32.const 0) (i32.const 0) (i32.const 4) (i32.const 0))
    (local.set $n (i32.load (i32.const 0)))
    ;; Write the output (return value) with the length from the parameter, and check that the amount of data written equals n.
    (call $assert_eq
          (call $write_output (i32.const 0) (local.get $n) (i32.const 0))
          (local.get $n))

    (return (i32.const 0))
  )

  ;; Loops $n (u32) times and creates a log message with the message $n each time.
  ;; When it logs, it asserts that logging was successful.
  ;;
  ;; Parameters
  ;;  - n: Number of logs to create (u32).
  (func $logs (export "relax.logs") (param i64) (result i32)
    (local $i i32)
    (local $n i32)
    (call $get_parameter_section (i32.const 0) (i32.const 0) (i32.const 4) (i32.const 0))
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
    (return (i32.const 0))
  )

  (memory 2)
)
