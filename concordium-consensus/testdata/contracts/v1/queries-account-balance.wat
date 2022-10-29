;; Exposes a contract with a single entrypoint "contract.query" which takes
;; - an account address (32 bytes)
;; - 3 amounts (3 * 8 bytes), that is the expected public balance, stake amount
;;   and locked amount of the provided account address.
;; The entrypoint queries the balances of the provided account address and
;; compares it to the expected amounts provided.

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

 ;; Query an account balance (32 bytes for the account address) and compares it
 ;; to the expected amount provided by in the parameter (3 * 8 bytes).
 (func $receive_query (export "contract.query") (param $amount i64) (result i32)
       ;; Declare local variable for storing the result of querying.
       (local $query_return i64)
       ;; Declare local variable for storing the index of the return value.
       (local $query_return_index i32)

       ;; Read the account address from the parameter.
       (call $host_get_parameter_section
             (i32.const 0) ;; Index of the return value.
             (i32.const 0) ;; Write offset in memory.
             (i32.const 32) ;; Write length.
             (i32.const 0)) ;; Offset to read from.

       ;; Query the account balance.
       (local.set $query_return (call $invoke_query_account_balance
                                      (i32.const 0))) ;; Offset in memory to start reading from.

       ;; Read the return value into memory.
       (call $get_invoke_return_value (local.get $query_return)
             (i32.const 0) ;; Write offset in memory.
             (i32.const 24) ;; Write length.
             (i32.const 0)) ;; Offset to read from.
       (drop)

       ;; Read the expected balances into memory.
       (call $host_get_parameter_section
             (i32.const 0) ;; Index of the return value.
             (i32.const 24) ;; Write offset in memory.
             (i32.const 24) ;; Write length.
             (i32.const 32)) ;; Offset to read from, skipping the 32 bytes for the account address.
       (drop)

       ;; Check the total balance.
       (call $assert_eq_64
             (i64.load (i32.const 24)) ;; Expected
             (i64.load (i32.const 0))) ;; Actual

       ;; Staked amount.
       (call $assert_eq_64
             (i64.load (i32.const 32)) ;; Expected
             (i64.load (i32.const 8))) ;; Actual

       ;; Locked amount.
       (call $assert_eq_64
             (i64.load (i32.const 40)) ;; Expected
             (i64.load (i32.const 16))) ;; Actual

       ;; Return success
       (return (i32.const 0)))
 (memory 1))
