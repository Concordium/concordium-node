;; A trivial contract that simply invokes with whatever information it was given.
;; The return value is written using write_output.

(module

  ;; Imports

  (import "concordium" "get_parameter_size" (func $get_parameter_size (param $index i32) (result i32)))
  (import "concordium" "get_parameter_section" (func $get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "invoke" (func $invoke (param $tag i32) (param $start i32) (param $length i32) (result i64)))
  (import "concordium" "write_output" (func $write_output (param $start i32) (param $length i32) (param $offset i32) (result i32)))

  ;; The caller contract

  (func $init_caller (export "init_caller") (param i64) (result i32)
    (return (i32.const 0)) ;; Successful init
  )

  (func $call (export "caller.call") (param i64) (result i32)
    (local $n i32)
    (local $size i32)
    (local $rv i64)
    (local.set $size (call $get_parameter_size (i32.const 0)))
    (call $get_parameter_section (i32.const 0) (i32.const 0) (local.get $size) (i32.const 0))
    ;; invoke, interpret the first 4 bytes as the instruction, the remaining bytes as the parameter
    (local.set $rv (call $invoke (i32.load (i32.const 0)) (i32.const 4) (i32.sub (local.get $size) (i32.const 4))))
    ;; store the return value
    (i64.store (local.get $size) (local.get $rv))
    (call $write_output (local.get $size) (i32.const 8) (i32.const 0))
    (drop)
    (drop)
    ;; and return success
    (i32.const 0)
  )
  ;; always fail with error code -17
  (func $fail (export "caller.fail") (param i64) (result i32)
      (i32.const -17)
  )

  ;; always unreachable
  (func $trap (export "caller.trap") (param i64) (result i32)
      unreachable
  )
  (memory 1)
)