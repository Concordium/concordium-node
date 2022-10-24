;; See 'upgrading-reject0.wat' for the description.
(module
 ;; Initialize contract.
 (func $init (export "init_contract") (param i64) (result i32)
       (return (i32.const 0))) ;; Successful init

 ;; Receive function
 (func $receive_new_feature (export "contract.new-feature") (param i64) (result i32)
       (return (i32.const 0)))) ;; Successful init
