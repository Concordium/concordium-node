(module

 ;; Imports

 (import "concordium" "get_parameter_section" (func $host_get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))

 (import "concordium" "invoke" (func $host_invoke (param $tag i32) (param $start i32) (param $length i32) (result i64)))

 ;; Helper functions

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

 ;; Invoke a query for the exchange rates (Euro per NRG and CCD per Euro)
 ;;
 ;; Return value:
 ;; - 8 bytes for the numerator of Euro per NRG.
 ;; - 8 bytes for the denominator of Euro per NRG.
 ;; - 8 bytes for the numerator of CCD per Euro.
 ;; - 8 bytes for the denominator of CCD per Euro.
 (func $invoke_query_exchange_rates (result i64)
       (call $host_invoke
             (i32.const 4) ;; Tag for the exchange rates query
             (i32.const 0) ;; Offset in memory to start reading from.
             (i32.const 0))) ;; The length of the parameter.

 ;; Contract

 ;; Initialize contract.
 (func $init (export "init_contract") (param i64) (result i32)
       (return (i32.const 0))) ;; Successful init

 ;; Receive method
 (func $receive_account_balance (export "contract.account_balance") (param $amount i64) (result i32)
       ;; Call a query of the account balance
       (call $invoke_query_account_balance (i32.const 0))
       (drop)
       ;; Return success
       (return (i32.const 0)))

  ;; Receive method
 (func $receive_contract_balance (export "contract.contract_balance") (param $amount i64) (result i32)
       ;; Call a query of a contract balance
       (call $invoke_query_contract_balance (i32.const 0))
       (drop)
       ;; Return success
       (return (i32.const 0)))

   ;; Receive method
 (func $receive_exchange_rates (export "contract.exchange_rates") (param $amount i64) (result i32)
       ;; Call a query of the exchange rates
       (call $invoke_query_exchange_rates)
       (drop)
       ;; Return success
       (return (i32.const 0)))

 (memory 1))
