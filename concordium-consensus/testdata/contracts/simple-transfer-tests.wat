;; Tests for simple_transfer.
;;
;; Docs:
;;   Constructs a simple transfer of GTU action.
;;

(module

  ;; Imports

  (import "concordium" "accept" (func $accept (result i32)))
  (import "concordium" "get_parameter_section" (func $get_parameter_section (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "simple_transfer" (func $simple_transfer (param $addr_bytes i32) (param $amount i64) (result i32)))

  ;; Helper Functions

  (func $assert_eq (param $actual i32) (param $expected i32)
    (if (i32.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

  (func $assert_ne (param $actual i32) (param $expected i32)
    (if (i32.ne (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

  ;; Gets an address from the parameters and asserts that the size is correct.
  ;; The address is saved in memory at location 0.
  (func $save_addr_from_param_to_mem_0
    (call $assert_eq
      (call $get_parameter_section (i32.const 0) (i32.const 32) (i32.const 0))
      (i32.const 32))
  )

  ;; Init

  (func $init (export "init_test") (param i64) (result i32)
    (i32.const 0) ;; Successful init
  )

  ;; Tests

  (func (export "test.multiple_transfers__transfer_4_and_succeed") (param i64) (result i32)
    (call $save_addr_from_param_to_mem_0)

    (call $simple_transfer (i32.const 0) (i64.const 1))
    (call $simple_transfer (i32.const 0) (i64.const 2))
    (call $simple_transfer (i32.const 0) (i64.const 3))
    (return (call $simple_transfer (i32.const 0) (i64.const 4))) ;; Only one that should and succeed
  )

  (func (export "test.addr_bytes_negative__fail") (param i64) (result i32)
    (call $save_addr_from_param_to_mem_0)

    (call $simple_transfer (i32.const -1) (i64.const 1))
  )

  (func (export "test.addr_bytes_greater_than_mem__fail") (param i64) (result i32)
    (call $save_addr_from_param_to_mem_0)

    (call $simple_transfer
      (i32.const 65537) ;; Greater than allocated memory, i.e. 1 page of 2^16 (65536) bytes.
      (i64.const 1))
  )

  (func (export "test.invalid_addr__fail_with_InvalidAccountReference") (param i64) (result i32)

    (call $simple_transfer
      (i32.const 0) ;; All 32 bytes are 0 at this point,
                    ;; which results in the address '2wkBET2rRgE8pahuaczxKbmv7ciehqsne57F9gtzf1PVdr2VP3'.
      (i64.const 1))
  )

  (func (export "test.amount_negative_one__fail_with_AmountTooLarge") (param i64) (result i32)

    (call $save_addr_from_param_to_mem_0)

    (call $simple_transfer
      (i32.const 0)
      (i64.const -1))
  )

  (func (export "test.amount_10001_greater_than_balance_of_10K__fail_with_AmountTooLarge") (param i64) (result i32)

    (call $save_addr_from_param_to_mem_0)

    (call $simple_transfer
      (i32.const 0)
      (i64.const 10001))
  )

  (memory 1)
)
