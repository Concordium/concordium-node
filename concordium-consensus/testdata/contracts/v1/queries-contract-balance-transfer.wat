(module

 ;; Imports

 (import "concordium" "get_parameter_section" (func $host_get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
 (import "concordium" "invoke" (func $host_invoke (param $tag i32) (param $start i32) (param $length i32) (result i64)))

 ;; Helper functions

 ;; Cause a runtime error if the two provided numbers are not equal.
 (func $assert_eq (param $actual i32) (param $expected i32)
       (if (i32.eq (local.get $actual) (local.get $expected))
           (then nop)
           (else unreachable)))

 ;; Cause a runtime error if the two provided numbers are not equal.
 (func $assert_eq_64 (param $actual i64) (param $expected i64)
       (if (i64.eq (local.get $actual) (local.get $expected))
           (then nop)
           (else unreachable)))

 ;; Read the parameter into memory.
 (func $get_parameter (param $write_offset i32) (param $length i32) (param $read_offset i32) (result i32)
       (return
         (call $host_get_parameter_section
               (i32.const 0)
               (local.get $write_offset) ;; Write offset in memory.
               (local.get $length) ;; Write length.
               (local.get $read_offset)))) ;; Offset to read from.

 ;; Read the return value into memory, takes the response from invoke and assumes the response is successful.
 (func $get_invoke_return_value (param $invoke_result i64) (param $write_offset i32) (param $length i32) (param $read_offset i32) (result i32)
       ;; Declare local variable for storing the index of the return value.
       (local $return_index i32)

       ;; Get the index of the response
       ;; The numeric value 8388607 is the mask 0b0111_1111_1111_1111_1111_1111
       (local.set $return_index (i32.and (i32.const 8388607)
                                         (i32.wrap_i64 (i64.shr_u (local.get $invoke_result) (i64.const 40)))))

       ;; Read the return value into memory.
       (return (call $host_get_parameter_section
                     (local.get $return_index) ;; Index of the return value.
                     (local.get $write_offset) ;; Write offset in memory.
                     (local.get $length) ;; Write length.
                     (local.get $read_offset)))) ;; Offset to read from.

;; Invoke a transfer to an account
 ;;
 ;; Parameter:
 ;; - 32 bytes for the account address.
 ;; - 8 bytes for the amount.
 ;;
 ;; Errors:
 ;; - MissingAccount
 ;; - InsufficientAmount
 (func $invoke_transfer (param $offset i32) (result i64)
       (call $host_invoke
             (i32.const 0) ;; Tag for invoking a transfer.
             (local.get $offset) ;; Offset in memory to start reading from.
             (i32.const 40)))

 ;; Invoke a query for the balance of a contract.
 ;;
 ;; Parameter:
 ;; - 8 bytes for the index.
 ;; - 8 bytes for the subindex.
 ;;
 ;; Errors:
 ;; - MissingContract
 ;;
 ;; Return value:
 ;; - 8 bytes for the balance.
 (func $invoke_query_contract_balance (param $offset i32) (result i64)
       (call $host_invoke
             (i32.const 3) ;; Tag for the contract balance query
             (local.get $offset) ;; Offset in memory to start reading from.
             (i32.const 16))) ;; The length of the parameter.

 ;; Contract

 ;; Initialize contract.
 (func $init (export "init_contract") (param i64) (result i32)
       (return (i32.const 0))) ;; Successful init

 ;; Receive method
 (func $receive_method (export "contract.query") (param $amount i64) (result i32)

       ;; Read the account address and an amount into memoery.
       (call $get_parameter
             (i32.const 0) ;; Write offset in memory
             (i32.const 40) ;; Length.
             (i32.const 0)) ;; Read offset.
       (drop)

       ;; Transfer some amount to account address
       (call $invoke_transfer
             (i32.const 0))  ;; Offset in memory to start reading from.

       ;; Read the contract address into memoery.
       (call $get_parameter
             (i32.const 0) ;; Write offset in memory
             (i32.const 16) ;; Length.
             (i32.const 40)) ;; Read offset.
       (drop)

       ;; Query the contract balance and read the return value into memory.
       (call $get_invoke_return_value
             (call $invoke_query_contract_balance (i32.const 0))
             (i32.const 0) ;; Write offset in memory
             (i32.const 8) ;; Length.
             (i32.const 0)) ;; Read offset.

       ;; Read the expected amount from the parameter.
       (call $get_parameter
             (i32.const 8) ;; Write offset in memory
             (i32.const 8) ;; Length.
             (i32.const 56)) ;; Read offset.
       (drop)
       ;; Compare the balance with the expected value.
       (call $assert_eq_64
             (i64.load (i32.const 8)) ;; Expected
             (i64.load (i32.const 0))) ;; Actual

       ;; Return success
       (return (i32.const 0)))

 (memory 1))
