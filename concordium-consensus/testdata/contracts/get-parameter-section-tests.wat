;; Tests for get_parameter_section function
;;
;; Docs:
;;    Read a section of the parameter to the given location.
;;    Return the number of bytes read. The location is assumed
;;    to contain enough memory to write the requested length into.
;;

(module

  ;; Imports

  (import "concordium" "get_parameter_section" (func $get_parameter_section
                                                 (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
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

  (func (export "test.offset_negative__fail") (param i64) (result i32)

    (call $get_parameter_section
      (i32.const 0)
      (i32.const 1)
      (i32.const -1))

    (return (call $accept))
  )

  (func (export "test.length_negative__fail") (param i64) (result i32)

    (call $get_parameter_section
      (i32.const 0)
      (i32.const -1)
      (i32.const 0))

    (return (call $accept))
  )

  (func (export "test.length_equal_to_param_size_100__return_100_and_succeed") (param i64) (result i32)

    (call $assert_eq
      (call $get_parameter_section
        (i32.const 0)
        (i32.const 100)
        (i32.const 0))
      (i32.const 100))

    (return (call $accept))
  )

  (func (export "test.length_greater_than_param_size_100__return_100_and_succeed") (param i64) (result i32)

    (call $assert_eq
      (call $get_parameter_section
        (i32.const 0)
        (i32.const 101)
        (i32.const 0))
      (i32.const 100))

    (return (call $accept))
  )

  (func (export "test.offset_greater_than_param_size_100__fail") (param i64) (result i32)

    (call $get_parameter_section
      (i32.const 0)
      (i32.const 1)
      (i32.const 101))

    (return (call $accept))
  )

  (func (export "test.write_location_negative__fail") (param i64) (result i32)

    (call $get_parameter_section
      (i32.const -1)
      (i32.const 1)
      (i32.const 0))

    (return (call $accept))
  )

  (func (export "test.write_location_greater_than_mem__fail") (param i64) (result i32)

    (call $get_parameter_section
      (i32.const 65537)  ;; write_location is greater than allocated memory, i.e. 1 page of 2^16 (65536) bytes.
      (i32.const 1)
      (i32.const 0))

    (return (call $accept))
  )

  (memory 1)
)
