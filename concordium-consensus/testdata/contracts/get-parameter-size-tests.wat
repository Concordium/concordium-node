;; Tests for get_parameter_size function.
;;
;; Docs:
;;   Get the byte size of the parameter.
;;

(module

  ;; Imports

  (import "concordium" "get_parameter_size" (func $get_parameter_size (result i32)))
  (import "concordium" "accept" (func $accept (result i32)))

  ;; Helper Functions

  (func $assert_eq (param $actual i32) (param $expected i32)
    (if (i32.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

  (func $assert_ne (param $actual i32) (param $expected i32)
    (if (i32.ne (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

  ;; Init

  (func $init (export "init_test") (param i64) (result i32)
    (i32.const 0) ;; Successful init
  )

  ;; Must stay in sync with MAX_PARAMETER_SIZE from 'wasm-chain-integration/src/constants.rs'
  (func $max_parameter_size (result i32)
    (i32.const 1024)
  )

  ;; Tests

  (func (export "test.size_is_0__return_0_and_succeed") (param i64) (result i32)

    (call $assert_eq
      (call $get_parameter_size)
      (i32.const 0))

    (call $accept)
  )

  (func (export "test.size_is_max__return_max_and_succeed") (param i64) (result i32)

    (call $assert_eq
      (call $get_parameter_size)
      (call $max_parameter_size))

    (call $accept)
  )

  (memory 1)
)
