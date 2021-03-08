;; Tests for the send function.
;;
;; Docs:
;;   Constructs an action for sending a message to another smart contract instance.
;;

(module

  ;; Imports

  (import "concordium" "send" (func $send (param $addr_index i64) (param $addr_subindex i64)
                                (param $receive_name i32) (param $receive_name_len i32)
                                (param $amount i64) (param $parameter i32) (param $parameter_len i32) (result i32)))

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

  (func $save_receive_name_to_mem_0

    ;; Get the receive name 'test.accept' and save it to memory.
    (call $assert_eq
      (call $get_parameter_section
        (i32.const 0)
        (i32.const 11) ;; The receive_name 'test.accept'
        (i32.const 0))
      (i32.const 11))

  )

  (func (export "test.accept") (param i64) (result i32)
    (call $accept)
  )

  ;; Init

  (func (export "init_test") (param i64) (result i32)
    (i32.const 0) ;; Successful init
  )

  ;; Tests

  (func (export "test.self_message_with_accept__succeed") (param i64) (result i32)

    ;; Save the text 'test.accept' to memory postion 0
    (call $save_receive_name_to_mem_0)

    ;; Call 'test.accept' through send
    (call $send
      (i64.const 0)  ;; index: The test contract always has address <0,0>
      (i64.const 0)  ;; subindex
      (i32.const 0)  ;; receive_name
      (i32.const 11) ;; receive_name_len (length of 'test.accept')
      (i64.const 1)  ;; amount
      (i32.const 0)  ;; parameter
      (i32.const 0)) ;; parameter_len
  )

  (func (export "test.invalid_address__fail_with_InvalidContractAddress_42_42") (param i64) (result i32)

    ;; Save the text 'test.accept' to memory postion 0
    (call $save_receive_name_to_mem_0)

    ;; Call 'test.accept' through send
    (call $send
      (i64.const 42)  ;; addr_index: An invalid address
      (i64.const 42)  ;; addr_subindex: An invalid address
      (i32.const 0)
      (i32.const 11)
      (i64.const 0)
      (i32.const 0)
      (i32.const 0))
  )

  (func (export "test.receive_name_negative__fail") (param i64) (result i32)

    ;; Save the text 'test.accept' to memory postion 0
    (call $save_receive_name_to_mem_0)

    (call $send
      (i64.const 0)
      (i64.const 0)
      (i32.const -1) ;; receive_name
      (i32.const 11)
      (i64.const 0)
      (i32.const 0)
      (i32.const 0))
  )

  (func (export "test.receive_name_greater_than_mem__fail") (param i64) (result i32)

    ;; Save the text 'test.accept' to memory postion 0
    (call $save_receive_name_to_mem_0)

    ;; Call 'test.accept' through send
    (call $send
      (i64.const 0)
      (i64.const 0)
      (i32.const 65537) ;; receive_name: 1 page (2^16) + 1 bytes
      (i32.const 11)
      (i64.const 0)
      (i32.const 0)
      (i32.const 0))
  )

  (func (export "test.receive_name_len_negative__fail") (param i64) (result i32)

    ;; Save the text 'test.accept' to memory postion 0
    (call $save_receive_name_to_mem_0)

    ;; Call 'test.accept' through send
    (call $send
      (i64.const 0)
      (i64.const 0)
      (i32.const 0)
      (i32.const -1) ;; receive_name_len
      (i64.const 0)
      (i32.const 0)
      (i32.const 0))
  )

  (func (export "test.receive_name_len_greater_than_mem__fail") (param i64) (result i32)

    ;; Save the text 'test.accept' to memory postion 0
    (call $save_receive_name_to_mem_0)

    ;; Call 'test.accept' through send
    (call $send
      (i64.const 0)
      (i64.const 0)
      (i32.const 0)
      (i32.const 65537) ;; receive_name_len: 1 page (2^16) + 1 bytes
      (i64.const 0)
      (i32.const 0)
      (i32.const 0))
  )

  (func (export "test.parameter_negative__fail") (param i64) (result i32)

    ;; Save the text 'test.accept' to memory postion 0
    (call $save_receive_name_to_mem_0)

    ;; Call 'test.accept' through send
    (call $send
      (i64.const 0)
      (i64.const 0)
      (i32.const 0)
      (i32.const 11)
      (i64.const 0)
      (i32.const -1) ;; parameter
      (i32.const 0))
  )

  (func (export "test.parameter_greater_than_mem__fail") (param i64) (result i32)

    ;; Save the text 'test.accept' to memory postion 0
    (call $save_receive_name_to_mem_0)

    ;; Call 'test.accept' through send
    (call $send
      (i64.const 0)
      (i64.const 0)
      (i32.const 0)
      (i32.const 11)
      (i64.const 0)
      (i32.const 65537) ;; parameter: 1 page (2^16) + 1 bytes
      (i32.const 0))
  )

  (func (export "test.parameter_len_negative__fail") (param i64) (result i32)

    ;; Save the text 'test.accept' to memory postion 0
    (call $save_receive_name_to_mem_0)

    ;; Call 'test.accept' through send
    (call $send
      (i64.const 0)
      (i64.const 0)
      (i32.const 0)
      (i32.const 11)
      (i64.const 0)
      (i32.const 0)
      (i32.const -1)) ;; parameter_len
  )

  (func (export "test.parameter_len_greater_than_mem__fail") (param i64) (result i32)

    ;; Save the text 'test.accept' to memory postion 0
    (call $save_receive_name_to_mem_0)

    ;; Call 'test.accept' through send
    (call $send
      (i64.const 0)
      (i64.const 0)
      (i32.const 0)
      (i32.const 11)
      (i64.const 0)
      (i32.const 0)
      (i32.const 65537)) ;; parameter_len: 1 page (2^16) + 1 bytes
  )

  (func (export "test.amount_negative_one__fail") (param i64) (result i32)

    ;; Save the text 'test.accept' to memory postion 0
    (call $save_receive_name_to_mem_0)

    ;; Call 'test.accept' through send
    (call $send
      (i64.const 0)
      (i64.const 0)
      (i32.const 0)
      (i32.const 11)
      (i64.const -1) ;; amount
      (i32.const 0)
      (i32.const 0))
  )

  (func (export "test.amount_10001_greater_than_balance_of_10K__fail_with_AmountTooLarge") (param i64) (result i32)

    ;; Save the text 'test.accept' to memory postion 0
    (call $save_receive_name_to_mem_0)

    (call $send
      (i64.const 0)
      (i64.const 0)
      (i32.const 0)
      (i32.const 11)
      (i64.const 10001)  ;; amount
      (i32.const 0)
      (i32.const 0))
  )

  (memory 1)
)
