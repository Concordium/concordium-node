(module
  ;; A dummy module that has extra exports that do not belong to any contracts.
  ;; valid init method export
  (func $init_caller (export "init_contract") (param i64) (result i32)
    (return (i32.const 0)) ;; Successful init
  )

  ;; valid receive method export
  (func $call (export "contract.call") (param i64) (result i32)
    (i32.const 0)
  )
  
  ;; neither init nor receive method, and some other type
  (func $somethingelse (export "something-else") (param i64)
  )

  (memory 1)
)