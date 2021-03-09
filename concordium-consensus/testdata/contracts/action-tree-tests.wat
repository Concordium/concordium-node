;; Tests for action-trees (combine_and, combine_or).
;;
;; Docs:
;;  combine_and:
;;    Combine two actions using and. Only run the second if the first succeeds.
;;    If the given identifiers are not valid, i.e., returned by a previous call
;;    to one of the actions functions, this function will abort.
;;
;;  combine_or:
;;    Combine two actions using or. Only runs the second of the first fails.
;;    If the given identifiers are not valid, i.e., returned by a previous call
;;    to one of the actions functions, this function will abort.
;;

(module

  ;; Imports

  (import "concordium" "accept" (func $accept (result i32)))
  (import "concordium" "combine_and" (func $combine_and (param $first i32) (param $second i32) (result i32)))
  (import "concordium" "combine_or" (func $combine_or (param $first i32) (param $second i32) (result i32)))
  (import "concordium" "get_parameter_section" (func $get_parameter_section (param $location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "simple_transfer" (func $simple_transfer (param $addr_pointer i32) (param $amount i64) (result i32)))

  ;; Helper Functions

  (func $assert_eq (param $actual i32) (param $expected i32)
    (if (i32.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

  (func $assert_ne (param $actual i32) (param $expected i32)
    (if (i32.ne (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

  ;; Create a failing action by transferring to invalid account address.
  (func $fail (result i32)
    ;; Just points to some unused memory that definitely won't contain a valid address.
    (call $simple_transfer (i32.const 10000) (i64.const 1))
  )

  ;; Transfer 1 microGTU to address stored in memory index 0.
  (func $transfer_1 (result i32)
    (call $simple_transfer (i32.const 0) (i64.const 1))
  )

  ;; Transfer 2 microGTU to address stored in memory index 0.
  (func $transfer_2 (result i32)
    (call $simple_transfer (i32.const 0) (i64.const 2))
  )

  ;; Init

  (func $init (export "init_test") (param i64) (result i32)

    ;; Initialize memory
    (call $assert_ne
      (memory.grow (i32.const 1)) ;; returns -1 on failure
      (i32.const -1))

    (return (i32.const 0))
  )

  ;; Tests

  (func (export "test.complex_combine__transfer_2microGTU_and_succeed") (param i64) (result i32)

    ;; Get address and save it in linear memory. Also assert that 32 bytes were read.
    (call $assert_eq
      (call $get_parameter_section (i32.const 0) (i32.const 32) (i32.const 0))
      (i32.const 32))

    (call $combine_and
      (call $combine_or
        (call $fail)
        (call $transfer_1))  ;; Should transfer
      (call $combine_or
        (call $transfer_1)   ;; Should transfer
        (call $transfer_2))) ;; Should _not_ transfer
  )

  (func (export "test.complex_combine__transfer_3microGTU_and_succeed") (param i64) (result i32)

    ;; Get address and save it in linear memory. Also assert that 32 bytes were read.
    (call $assert_eq
      (call $get_parameter_section (i32.const 0) (i32.const 32) (i32.const 0))
      (i32.const 32))

    (call $combine_and
      (call $combine_or
        (call $transfer_2)   ;; Should transfer
        (call $transfer_1))  ;; Should _not_ transfer
      (call $combine_or
        (call $transfer_1)   ;; Should transfer
        (call $transfer_2))) ;; Should _not_ transfer
  )

  (func (export "test.complex_combine__transfer_6microGTU_and_succeed") (param i64) (result i32)

    ;; Get address and save it in linear memory. Also assert that 32 bytes were read.
    (call $assert_eq
      (call $get_parameter_section (i32.const 0) (i32.const 32) (i32.const 0))
      (i32.const 32))

    ;; All $transfer_2 should be executed, and none of $transfer_1.
    (call $combine_and
      (call $combine_or
        (call $combine_and
          (call $combine_or
            (call $transfer_2) ;; Should transfer
            (call $transfer_1))
          (call $combine_or
            (call $transfer_2) ;; Should transfer
            (call $transfer_1)))
        (call $combine_and
          (call $transfer_1)
          (call $transfer_1)))
      (call $transfer_2)) ;; Should transfer
  )

  (func $combine_and_transfer (param $n i32) (result i32)
    ;; Creates a nested action tree of combine_ands where
    ;; each left node is a simple_transfer, transferring 1 microGTU
    ;; and each right node is the previous iteration's combine_and.

    (local $i i32)
    (local $ref i32)
    (local.set $i (i32.const 0))
    (local.set $ref (call $accept)) ;; Set ref to point to the action identifier of accept

    (block
      (loop
        (br_if 1 (i32.eq (local.get $i) (local.get $n))) ;; stop loop if i == n

        ;; i += 1
        (local.set $i (i32.add (local.get $i) (i32.const 1)))

        (call $combine_and
          (call $simple_transfer (i32.const 0) (i64.const 1)) ;; Transfer 1 microGTU
          (local.get $ref)) ;; Use action identifier from previous iteration

        (local.set $ref) ;; Update ref to the action identfier of the newest $combine_and

        (br 0)))
    (return (local.get $ref)) ;; Return the final action identifier
  )

  (func (export "test.combine_100_transfers_with_and_then_accept__transfer_100_and_succeed") (param i64) (result i32)
    ;; Create an action tree of 100 nested combine_ands where
    ;; each left node is a simple_transfer, transferring 1 microGTU
    ;; and each right node is the previous iteration's combine_and (except last, which is accept).

    (local $i i32)
    (local $action_id i32)

    ;; Get address and save it in linear memory. Also assert that 32 bytes were read.
    (call $assert_eq
      (call $get_parameter_section (i32.const 0) (i32.const 32) (i32.const 0))
      (i32.const 32))

    (local.set $i (i32.const 0))
    (local.set $action_id (call $accept)) ;; Set action_id to the action identifier of accept

    (block
      (loop
        ;; Stop loop if i == 100
        (br_if 1 (i32.eq (local.get $i) (i32.const 100)))

        ;; i += 1
        (local.set $i (i32.add (local.get $i) (i32.const 1)))

        ;; Set action_id to the new combine_and
        (local.set $action_id
          (call $combine_and
            (call $simple_transfer (i32.const 0) (i64.const 1)) ;; Transfer 1 microGTU
            (local.get $action_id))) ;; Use action identifier from previous iteration

        (br 0)))
    (return (local.get $action_id)) ;; Return the final action identifier
  )

  (func (export "test.combine_and__first_invalid__fail") (param i64) (result i32)

    (call $combine_and
      (i32.const 1234) ;; Some invalid action identifier
      (call $accept))
  )

  (func (export "test.combine_and__second_invalid__fail") (param i64) (result i32)

    (call $combine_and
      (call $accept)
      (i32.const 1234)) ;; Some invalid action identifier
  )

  (func (export "test.combine_and__both_params_invalid__fail") (param i64) (result i32)

    (call $combine_and
      (i32.const 4321)  ;; Some invalid action identifier
      (i32.const 1234)) ;; Some invalid action identifier
  )

  (func (export "test.combine_and__own_action_id__fail") (param i64) (result i32)
    ;; Each action gets an action identifier starting from 0.
    ;; Test whether $combine_and can refer to its own acion id.

    (return (call $combine_and
      (i32.const 0)
      (i32.const 0)))
  )

  (func (export "test.combine_100_failing_transfers_with_or_then_accept__transfer_0_and_succeed") (param i64) (result i32)
    ;; Create an action tree of 100 nested combine_ors where
    ;; each left node is a failing simple_transfer
    ;; and each right node is the previous iteration's combine_or (except last, which is accept).

    (local $i i32)
    (local $action_id i32)

    ;; Get address and save it in linear memory. Also assert that 32 bytes were read.
    (call $assert_eq
      (call $get_parameter_section (i32.const 0) (i32.const 32) (i32.const 0))
      (i32.const 32))

    (local.set $i (i32.const 0))
    (local.set $action_id (call $accept)) ;; Set action_id to the action identifier of accept

    (block
      (loop
        ;; Stop loop if i == 100
        (br_if 1 (i32.eq (local.get $i) (i32.const 100)))

        ;; i += 1
        (local.set $i (i32.add (local.get $i) (i32.const 1)))

        ;; Set action_id to the new combine_and
        (local.set $action_id
          (call $combine_or
            (call $simple_transfer ;; Failing transfer
              (i32.const 1234) ;; With an invalid address
              (i64.const 1))
            (local.get $action_id))) ;; Use action identifier from previous iteration

        (br 0)))
    (return (local.get $action_id)) ;; Return the final action identifier
  )

  (func (export "test.combine_or__first_invalid__fail") (param i64) (result i32)

    (call $combine_or
      (i32.const 1234) ;; Some invalid action identifier
      (call $accept))
  )

  (func (export "test.combine_or__second_invalid__fail") (param i64) (result i32)

    (call $combine_or
      (call $accept)
      (i32.const 1234)) ;; Some invalid action identifier
  )

  (func (export "test.combine_or__both_params_invalid__fail") (param i64) (result i32)

    (call $combine_or
      (i32.const 4321)  ;; Some invalid action identifier
      (i32.const 1234)) ;; Some invalid action identifier
  )

  (func (export "test.combine_or__own_action_id__fail") (param i64) (result i32)
    ;; Each action gets an action identifier starting from 0.
    ;; Test whether $combine_or can refer to its own acion id.

    (return (call $combine_or
      (i32.const 0)
      (i32.const 0)))
  )

  (memory 1)
)
