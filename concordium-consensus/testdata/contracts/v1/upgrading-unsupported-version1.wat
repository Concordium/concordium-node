(module
 ;; Init contract
 (func $init_contract (export "init_contract") (param i64) (result i32)
       (return (i32.const 0))) ;; Successful init

 ;; Receive function
 (func $contract_update (export "contract.update")  (param i64) (result i32)
       (return (i32.const 0)))) ;; Successful update
