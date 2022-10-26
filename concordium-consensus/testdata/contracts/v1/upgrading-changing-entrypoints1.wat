;; See 'upgrading-changing-entrypoints0.wat' for the description.
(module
 ;; Contract

 ;; Initialize contract.
 (func $init (export "init_contract") (param i64) (result i32)
       (return (i32.const 0))) ;; Successful init

 ;; Receive function which always succeeds.
 ;; Notice this is not present in 'upgrading-changing-entrypoints0.wat'.
 (func $receive_name (export "contract.new_feature") (param i64) (result i32)
       (return (i32.const 0)))) ;; Successful init
