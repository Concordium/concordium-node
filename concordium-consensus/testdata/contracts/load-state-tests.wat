;; Tests for load_state function.
;;
;; Docs:
;;   Read a section of the state to the given location.
;;   Return the number of bytes written. The location is
;;   assumed to contain enough memory to write the requested
;;   length into. If not, the function will trap and abort
;;   execution of the contract.
;;

(module

  ;; Imports

  (import "concordium" "accept" (func $accept (result i32)))
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

  ;; Write 100 bytes to state.
  (func $setup_state_with_100_bytes
    (call $assert_eq
      (call $write_state (i32.const 0) (i32.const 100) (i32.const 0))
      (i32.const 100))
  )

  ;; Must stay in sync with MAX_CONTRACT_STATE in 'wasm-chain-integration/src/constants.rs'.
  (func $max_state_size (result i32)
    (i32.const 16384) ;; 16kB
  )

  ;; Init

  (func $init (export "init_test") (param i64) (result i32)
    (i32.const 0) ;; Successful init
  )

  ;; Tests

  (func (export "test.load_all_of_state__return_state_size_and_succeed") (param i64) (result i32)

    (call $setup_state_with_100_bytes)

    (call $assert_eq
      (call $load_state (i32.const 0) (i32.const 100) (i32.const 0))
      (i32.const 100))

    (return (call $accept))
  )

  (func (export "test.load_max_sized_state__return_state_size_and_succeed") (param i64) (result i32)

    (call $assert_eq
      (call $write_state (i32.const 0) (call $max_state_size) (i32.const 0))
      (call $max_state_size))

    (call $assert_eq
      (call $load_state (i32.const 0) (call $max_state_size) (i32.const 0))
      (call $max_state_size))

    (return (call $accept))
  )

  (func (export "test.length_greater_than_state_size__return_state_size_and_succeed") (param i64) (result i32)

    (call $setup_state_with_100_bytes)

    (call $assert_eq
      (call $load_state (i32.const 0) (i32.const 1000) (i32.const 0)) ;; Try to load 1000 bytes
      (i32.const 100))

    (return (call $accept))
  )

  ;; This should fail because negative offsets are not allowed
  (func (export "test.offset_negative__fail") (param i64) (result i32)

    (call $setup_state_with_100_bytes)

    (call $assert_eq
      (call $load_state
        (i32.const 0)
        (i32.const 1)
        (i32.const -10))
      (i32.const 0))

    (return (call $accept))
  )


  (func (export "test.offset_greater_than_state_size__fail") (param i64) (result i32)

    (call $setup_state_with_100_bytes)

    (call $assert_eq
      (call $load_state
        (i32.const 0)
        (i32.const 1)
        (i32.const 101))
      (i32.const 0))

    (return (call $accept))
  )

  (func (export "test.length_negative__fail") (param i64) (result i32)

    (call $setup_state_with_100_bytes)

    (call $load_state
      (i32.const 0)
      (i32.const -1)
      (i32.const 0))

    (return (call $accept))
  )

  (func (export "test.write_location_negative__fail") (param i64) (result i32)

    (call $setup_state_with_100_bytes)

    (call $load_state
      (i32.const -1)
      (i32.const 1)
      (i32.const 0))

    (return (call $accept))
  )

  (func (export "test.write_location_greater_than_mem__fail") (param i64) (result i32)

    (call $setup_state_with_100_bytes)

    (call $load_state
      (i32.const 65537)  ;; write_location is greater than allocated memory, i.e. 1 page of 2^16 (65536) bytes.
      (i32.const 1)
      (i32.const 0))

    (return (call $accept))
  )

  (memory 1)
)
