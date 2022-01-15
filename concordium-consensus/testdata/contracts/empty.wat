;; An empty contract. It can be initialized, and does nothing else.
(module

  (func $init_empty (export "init_empty") (param i64) (result i32)
    (return (i32.const 0)) ;; Successful init
  )

  (memory 1)
)
