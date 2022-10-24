(module
 ;; Imports

 ;; Init contract
 (func $init_contract (export "init_another-contract") (param i64) (result i32)
       (return (i32.const 0))) ;; Successful init

 ;; Upgrade
 (func $contract_upgrade (export "another-contract.upgrade")  (param i64) (result i32)
       ;; return 0 for success.
       (return (i32.const 0))))
