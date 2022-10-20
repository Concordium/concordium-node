(module
 ;; Contract

 ;; Initialize contract.
 (func $init (export "init_contract") (param i64) (result i32)
       (return (i32.const 0))) ;; Successful init

 (func $receive_name (export "contract.new_feature") (param i64) (result i32)
       (return (i32.const 0))) ;; Successful init
)
