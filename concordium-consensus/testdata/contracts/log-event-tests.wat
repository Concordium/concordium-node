;; Tests for log_event function.
;;
;; Docs for log_event:
;;   Adds a log item from an array of bytes. If not enough data can be read
;;   then this function will trap and abort execution of the smart contract.
;;

(module

  ;; Imports

  (import "concordium" "log_event" (func $log_event (param $start i32) (param $length i32)))
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

  ;; Tests

  (func (export "test.start_negative__fail") (param i64) (result i32)
    (call $log_event
      (i32.const -1)
      (i32.const 1))

    (call $accept)
  )

  (func (export "test.length_negative__fail") (param i64) (result i32)
    (call $log_event
      (i32.const 0)
      (i32.const -1))

    (call $accept)
  )

  (func (export "test.start_and_length_negative__fail") (param i64) (result i32)
    (call $log_event
      (i32.const -1)
      (i32.const -1))

    (call $accept)
  )

  (func (export "test.start_greater_than_mem__fail") (param i64) (result i32)
    (call $log_event
      (i32.const 65537)  ;; start is greater than allocated memory, i.e. 1 page of 2^16 (65536) bytes.
      (i32.const 1))

    (call $accept)
  )

  (func (export "test.length_greater_than_mem__fail") (param i64) (result i32)
    (call $log_event
      (i32.const 1)
      (i32.const 65537))

    (call $accept)
  )

  (memory 1)
)
