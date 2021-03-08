;; Tests for resize_state function.
;;
;; Docs:
;;   Resize state to the new value (truncate if new size is smaller).
;;   The additional state is initialized to 0.
;;   Returns 0 on failure and 1 on success.
;;

(module

  ;; Imports

  (import "concordium" "accept" (func $accept (result i32)))
  (import "concordium" "resize_state" (func $resize_state (param $new_size i32) (result i32)))

  ;; Helper Functions

  (func $assert_eq (param $actual i32) (param $expected i32)
    (if (i32.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

  (func $assert_ne (param $actual i32) (param $expected i32)
    (if (i32.ne (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

   ;; Must stay in sync with MAX_CONTRACT_STATE in 'wasm-chain-integration/src/constants.rs'.
  (func $max_state_size (result i32)
    (i32.const 16384) ;; 16kB
  )

  ;; Init

  (func $init (export "init_test") (param i64) (result i32)
    (i32.const 0) ;; Successful init
  )

  ;; Tests

  (func (export "test.new_size_100__return_one_and_succeed") (param i64) (result i32)

    (call $assert_eq
      (call $resize_state (i32.const 100))
      (i32.const 1)) ;; Returns 0 on failure, 1 on success

    (return (call $accept))
  )

  (func (export "test.new_size_negative__return_zero_and_succeed") (param i64) (result i32)

    (call $assert_eq
      (call $resize_state (i32.const -1))
      (i32.const 0)) ;; Returns 0 on failure, 1 on success

    (return (call $accept))
  )

  (func (export "test.new_size_zero__return_one_and_succeed") (param i64) (result i32)

    (call $assert_eq
      (call $resize_state (i32.const 0))
      (i32.const 1)) ;; Returns 0 on failure, 1 on success

    (return (call $accept))
  )

  (func (export "test.new_size_max__return_one_and_succeed") (param i64) (result i32)

    (call $assert_eq
      (call $resize_state (call $max_state_size))
      (i32.const 1)) ;; Returns 0 on failure, 1 on success

    (return (call $accept))
  )

  (func (export "test.new_size_greater_than_max__return_zero_and_succeed") (param i64) (result i32)

    (call $assert_eq

      (call $resize_state
        (i32.add
          (call $max_state_size)
          (i32.const 1))

      (i32.const 0)) ;; Returns 0 on failure, 1 on success

    (return (call $accept)))
  )

  (memory 1)
)
