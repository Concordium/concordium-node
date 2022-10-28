(module
 ;; Imports
 (import "concordium" "get_parameter_section" (func $host_get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
 (import "concordium" "invoke" (func $host_invoke (param $tag i32) (param $start i32) (param $length i32) (result i64)))

 ;; Helper functions

 ;; Cause a runtime error if the two provided numbers are not equal.
 (func $assert_eq_64 (param $actual i64) (param $expected i64)
       (if (i64.eq (local.get $actual) (local.get $expected))
           (then nop)
           (else unreachable)))

 ;; Cause a runtime error if the response is not the error for missing contract (0x0000_0003_0000_0000).
 (func $assert_invoke_error_missing_contract (param $response i64)
       (call $assert_eq_64
             (local.get $response)
             (i64.const 12884901888))) ;; Error code encoded as decimal (big endian).

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

 ;; Query a contract balance and asserts that the query responds with missing contract.
 ;;
 ;; Parameter:
 ;; - 16 bytes for the contract address
 (func $receive_query (export "contract.query") (param $amount i64) (result i32)
       ;; Read the contract address from the parameter.
       (call $host_get_parameter_section
             (i32.const 0) ;; Index of the return value.
             (i32.const 0) ;; Write offset in memory.
             (i32.const 16) ;; Write length.
             (i32.const 0)) ;; Offset to read from.

       ;; Query and assert response is missing contract.
       (call $assert_invoke_error_missing_contract
             (call $invoke_query_contract_balance (i32.const 0)))

       ;; Return success
       (return (i32.const 0)))
 (memory 1))
