;; test transferring from a contract to an account.
;; We try to transfer twice, checking the first transfer succeeds
;; and the second one fails (since not enough tokens exist).

(module

  ;; Imports

  (import "concordium" "get_parameter_size" (func $get_parameter_size (param $index i32) (result i32)))
  (import "concordium" "get_parameter_section" (func $get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "invoke" (func $invoke (param $tag i32) (param $start i32) (param $length i32) (result i64)))
  (import "concordium" "write_output" (func $write_output (param $start i32) (param $length i32) (param $offset i32) (result i32)))

  ;; Helper Functions
  (func $assert_eq_64 (param $actual i64) (param $expected i64)
    (if (i64.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

  (func $assert_ne_64 (param $actual i64) (param $expected i64)
    (if (i64.ne (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

  ;; The transfer

  (func $init_transfer(export "init_transfer") (param i64) (result i32)
    ;; no state
    (return (i32.const 0)) ;; Successful init
  )

  ;; Just accept some tokens.
  (func $deposit(export "transfer.deposit") (param i64) (result i32)
    ;; no state
    (return (i32.const 0)) ;; Succeed.
  )

  (func $forward (export "transfer.forward") (param $amount i64) (result i32)
    ;; assume the parameter is the address to transfer to
    (call $get_parameter_section (i32.const 0) (i32.const 0) (i32.const 32) (i32.const 0))
    (drop)
    (i64.store (i32.const 32) (local.get $amount))
    (call $assert_eq_64 (call $invoke (i32.const 0) (i32.const 0) (i32.const 40)) (i64.const 0)) ;; ensure success without return value
    (call $assert_ne_64 (call $invoke (i32.const 0) (i32.const 0) (i32.const 40)) (i64.const 0)) ;; trying to transfer again fails since we have no tokens
    ;; and return success
    (i32.const 0)
  )

  ;; transfer the amount given in the parameter to the address given in the parameter
  (func $send (export "transfer.send") (param $amount i64) (result i32)
    ;; assume the parameter is the address to transfer to followed by the amount
    (call $get_parameter_section (i32.const 0) (i32.const 0) (i32.const 40) (i32.const 0))
    (drop)
    (call $assert_eq_64 (call $invoke (i32.const 0) (i32.const 0) (i32.const 40)) (i64.const 0)) ;; ensure success without return value
    ;; and return success
    (i32.const 0)
  )
  (memory 1)
)