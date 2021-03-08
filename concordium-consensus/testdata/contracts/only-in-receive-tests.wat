;; Tests for functions only callable from receive
;;
;; Docs:
;;   - get_receive_invoker:
;;       Get the address of the account that initiated the top-level transaction
;;       which lead to triggering the receive function.
;;   - get_receive_sender:
;;       Get the address of the account or contract, triggering the receive function.
;;   - get_receive_self_address:
;;       Get the address of the contract instance, running the receive function.
;;   - get_receive_owner
;;       Get the address of the account, which created the contract instance.
;;   - get_receive_self_balance
;;       Get the current balance of the contract instance.
;;

(module

  ;; Imports

  (import "concordium" "accept" (func $accept (result i32)))
  (import "concordium" "get_receive_invoker" (func $get_receive_invoker (param $start i32)))
  (import "concordium" "get_receive_sender" (func $get_receive_sender (param $start i32)))
  (import "concordium" "get_receive_self_address" (func $get_receive_self_address (param $start i32)))
  (import "concordium" "get_receive_owner" (func $get_receive_owner (param $start i32)))
  (import "concordium" "get_receive_self_balance" (func $get_receive_self_balance (result i64)))

  ;; Helper Functions

  (func $assert_eq (param $actual i32) (param $expected i32)
    (if (i32.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

  (func $assert_eq_i64 (param $actual i64) (param $expected i64)
    (if (i64.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

  (func $assert_ne (param $actual i32) (param $expected i32)
    (if (i32.ne (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

  ;; Init

  ;; The init used with the receive tests
  (func (export "init_test") (param i64) (result i32)
    (i32.const 0) ;; Successful init
  )

  ;; Init Tests

  (func (export "init_get_receive_invoker__valid_param_from_init__fail") (param i64) (result i32)
    (call $get_receive_invoker (i32.const 0))
    (i32.const 0)
  )

  (func (export "init_get_receive_sender__valid_param_from_init__fail") (param i64) (result i32)
    (call $get_receive_sender (i32.const 0))
    (i32.const 0)
  )

  (func (export "init_get_receive_self_address__valid_param_from_init__fail") (param i64) (result i32)
    (call $get_receive_self_address (i32.const 0))
    (i32.const 0)
  )

  (func (export "init_get_receive_owner__valid_param_from_init__fail") (param i64) (result i32)
    (call $get_receive_owner (i32.const 0))
    (i32.const 0)
  )

  (func (export "init_get_receive_self_balance__valid_param_from_init__fail") (param i64) (result i32)
    (call $get_receive_self_balance)
    (return (i32.const 0))
  )

  ;; Receive Tests


  ;;;;;; get_receive_invoker

  (func (export "test.get_receive_invoker__valid_param__succeed") (param i64) (result i32)
    (call $get_receive_invoker (i32.const 0))
    (call $accept)
  )

  (func (export "test.get_receive_invoker__start_negative__fail") (param i64) (result i32)
    (call $get_receive_invoker (i32.const -1))
    (call $accept)
  )

  (func (export "test.get_receive_invoker__start_greater_than_mem__fail") (param i64) (result i32)
    (call $get_receive_invoker (i32.const 65537)) ;; 1 page of 2^16 bytes + 1 byte
    (call $accept)
  )


  ;;;;;; get_receive_sender

  (func (export "test.get_receive_sender__valid_param__succeed") (param i64) (result i32)
    (call $get_receive_sender (i32.const 0))
    (call $accept)
  )

  (func (export "test.get_receive_sender__start_negative__fail") (param i64) (result i32)
    (call $get_receive_sender (i32.const -1))
    (call $accept)
  )

  (func (export "test.get_receive_sender__start_greater_than_mem__fail") (param i64) (result i32)
    (call $get_receive_sender (i32.const 65537)) ;; 1 page of 2^16 bytes + 1 byte
    (call $accept)
  )


  ;;;;;; get_receive_self_address

  (func (export "test.get_receive_self_address__valid_param__succeed") (param i64) (result i32)
    (call $get_receive_self_address (i32.const 0))
    (call $accept)
  )

  (func (export "test.get_receive_self_address__start_negative__fail") (param i64) (result i32)
    (call $get_receive_self_address (i32.const -1))
    (call $accept)
  )

  (func (export "test.get_receive_self_address__start_greater_than_mem__fail") (param i64) (result i32)
    (call $get_receive_self_address (i32.const 65537)) ;; 1 page of 2^16 bytes + 1 byte
    (call $accept)
  )


  ;;;;;; get_receive_owner

  (func (export "test.get_receive_owner__valid_param__succeed") (param i64) (result i32)
    (call $get_receive_owner (i32.const 0))
    (call $accept)
  )

  (func (export "test.get_receive_owner__start_negative__fail") (param i64) (result i32)
    (call $get_receive_owner (i32.const -1))
    (call $accept)
  )

  (func (export "test.get_receive_owner__start_greater_than_mem__fail") (param i64) (result i32)
    (call $get_receive_owner (i32.const 65537)) ;; 1 page of 2^16 bytes + 1 byte
    (call $accept)
  )


  ;;;;;; get_self_balance

  ;; get_receive_self_balance shows the balance prior to this specific update. So the balance asserted is the
  ;; amount provided in the init.
  (func (export "test.get_receive_self_balance__send_10K_microGTU__balance_is_10K_and_succeed") (param i64) (result i32)

    (call $assert_eq_i64
      (call $get_receive_self_balance)
      (i64.const 10000)) ;; 10K microGTU

    (call $accept)
  )

  (memory 1)
)
