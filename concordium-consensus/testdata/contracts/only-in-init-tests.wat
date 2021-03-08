;; Tests for functions only callable from init
;;
;; Docs:
;;   - get_init_origin: Get the address of the account that triggered the init function.
;;

(module

  ;; Imports

  (import "concordium" "accept" (func $accept (result i32)))
  (import "concordium" "get_init_origin" (func $get_init_origin (param $start i32)))

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

  ;; To be used with receive tests
  (func $init (export "init_test") (param i64) (result i32)
    (return (i32.const 0))
  )

  ;; Init Tests

  (func (export "init_get_init_origin__valid_param__succeed") (param i64) (result i32)

    (call $get_init_origin (i32.const 0))

    (return (i32.const 0))
  )

  (func (export "init_get_init_origin__write_location_negative__fail") (param i64) (result i32)

    (call $get_init_origin (i32.const -1))

    (return (i32.const 0))
  )

  (func (export "init_get_init_origin__write_location_greater_than_mem__fail") (param i64) (result i32)
    (call $get_init_origin (i32.const 65536)) ;; write_location is greater than allocated memory, i.e. 1 page of 2^16 (65536) bytes.

    (return (i32.const 0))
  )

  ;; Receive Tests

  (func (export "test.get_init_origin__valid_param_from_receive__fail") (param i64) (result i32)

    ;; Should fail in a receive function.
    (call $get_init_origin (i32.const 0))

    (return (call $accept))
  )

  (memory 1)
)
