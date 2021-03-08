;; Tests for state_size function.
;;
;; Docs:
;;   Get the byte size of the contract state.
;;

(module

  ;; Imports

  (import "concordium" "accept" (func $accept (result i32)))
  (import "concordium" "state_size" (func $state_size (result i32)))
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

  ;; Must stay in sync with MAX_CONTRACT_STATE in 'wasm-chain-integration/src/constants.rs'.
  (func $max_state_size (result i32)
    (i32.const 16384) ;; 16kB
  )

  ;; Init

  (func $init (export "init_test") (param i64) (result i32)
    (i32.const 0) ;; Successful init
  )

  ;; Tests

  (func (export "test.size_is_0__return_0_and_succeed") (param i64) (result i32)

    (call $assert_eq
      (call $state_size)
      (i32.const 0))

    (call $accept)
  )

  (func (export "test.size_is_max__return_max_and_succeed") (param i64) (result i32)

    ;; Fill state with 0s from memory.
    (call $assert_eq
      (call $write_state
            (i32.const 0)
            (call $max_state_size)
            (i32.const 0))
      (call $max_state_size))

    ;; Assert that the state size is correct.
    (call $assert_eq
      (call $state_size)
      (call $max_state_size))

    (call $accept)
  )

  (memory 1)
)
