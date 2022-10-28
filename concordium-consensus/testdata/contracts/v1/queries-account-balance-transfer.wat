(module
 ;; Imports
 (import "concordium" "get_parameter_section" (func $host_get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
 (import "concordium" "invoke" (func $host_invoke (param $tag i32) (param $start i32) (param $length i32) (result i64)))

 ;; Helper functions
 (func $assert_eq (param $actual i32) (param $expected i32)
       (if (i32.eq (local.get $actual) (local.get $expected))
           (then nop)
           (else unreachable)))

 (func $assert_eq_64 (param $actual i64) (param $expected i64)
       (if (i64.eq (local.get $actual) (local.get $expected))
           (then nop)
           (else unreachable)))

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

 ;; Invoke a query for the balances of an account address.
 ;;
 ;; Parameter:
 ;; - 32 bytes for the account address.
 ;;
 ;; Errors:
 ;; - MissingAccount
 ;;
 ;; Return value:
 ;; - 8 bytes for the public balance.
 ;; - 8 bytes for the staked amount.
 ;; - 8 bytes for the locked amount.
 (func $invoke_query_account_balance (param $offset i32) (result i64)
       (call $host_invoke
             (i32.const 2) ;; Tag for the account balance query
             (local.get $offset) ;; Offset in memory to start reading from.
             (i32.const 32))) ;; The length of the parameter.

 ;; Contract

 ;; Initialize contract.
 (func $init (export "init_contract") (param i64) (result i32)
       (return (i32.const 0))) ;; Successful init

 ;; First transfers an amount to an account address, then query the account
 ;; balance and compares it to the expected amount provided by in the parameter.
 ;;
 ;; Parameter:
 ;; - 32 bytes for the account address.
 ;; - 8 bytes for the amount to transfer.
 ;; - 8 bytes for the expected balance.
 (func $receive_query (export "contract.query") (param $amount i64) (result i32)
       ;; Declare local variable for storing the result of querying.
       (local $query_return i64)

       ;; Read the account address and transfer amount from the parameter.
       (call $host_get_parameter_section
             (i32.const 0) ;; Index of the return value.
             (i32.const 0) ;; Write offset in memory.
             (i32.const 40) ;; Write length.
             (i32.const 0)) ;; Offset to read from.

       ;; Transfer some amount to account address
       (call $invoke_transfer
             (i32.const 0))  ;; Offset in memory to start reading from.

       ;; Query the account balance
       (local.set $query_return
                  (call $invoke_query_account_balance
                        (i32.const 0))) ;; Offset in memory to start reading from.

       ;; Read the return value into memory.
       (call $get_invoke_return_value (local.get $query_return)
             (i32.const 0) ;; Write offset in memory.
             (i32.const 8) ;; Write length.
             (i32.const 0)) ;; Offset to read from.)
       (drop)

       ;; Read the expected balances from the parameter.
       (call $host_get_parameter_section
             (i32.const 0) ;; Index of the return value.
             (i32.const 8) ;; Write offset in memory.
             (i32.const 8) ;; Write length.
             (i32.const 40)) ;; Offset to read from, skipping the 32 bytes for the account address and the 8 bytes for the amount to transfer.
       (drop)

       ;; Check the total balance.
       (call $assert_eq_64
             (i64.load (i32.const 8)) ;; Expected
             (i64.load (i32.const 0))) ;; Actual

       ;; Return success
       (return (i32.const 0)))
 (memory 1))
