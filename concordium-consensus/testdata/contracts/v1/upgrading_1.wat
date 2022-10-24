;; This is a module that contract a in 'upgrading_0.wat' can upgrade to.
;; This module defines a function 'new' for contract 'a' which is not
;; available in the module upgrade from (upgrading_0.wat).

(module
    ;; Helper functions
    (func $assert_eq_64 (param $actual i64) (param $expected i64)
    (if (i64.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

    ;; Init a
    (func $init_a (export "init_a") (param i64) (result i32)
        (return (i32.const 0)) ;; Successful init
    )

    ;; new simply returns 0.
    (func $newfun (export "a.newfun") (param i64) (result i32)
        ;; Simply return success.
        (return (i32.const 0))
    )
    (memory 1)
)
