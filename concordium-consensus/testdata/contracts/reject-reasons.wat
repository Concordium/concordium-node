(module
    (; This module provides init and receive functions that return different values  ;)
    (type (func (param i64) (result i32))) (;type of the init/receive functions;)
    (type (func (result i32))) (; type of the accept function ;)
    (import "concordium" "accept" (func $accept (type 1)))
    (func $init_success (type 0)
            (return (i32.const 0)))

    (func $init_fail1 (type 0)
            (return (i32.const 1)))

    (func $init_fail_minus2 (type 0)
            (return (i32.const -2)))

    (func $init_fail_minus1 (type 0)
            (return (i32.const -1)))

    (func $init_fail_overflow (type 0)
            (return (i32.const -256)))

    (func $init_fail_positive_overflow (type 0)
            (return (i32.const 256)))

    (func $receive_success (type 0)
            (return (i32.const 0)))

    (func $receive_success2 (type 0)
            (return (call $accept)))

    (func $receive_fail5 (type 0)
            (return (i32.const 5)))

    (func $receive_fail_minus2 (type 0)
            (return (i32.const -2)))

    (func $receive_fail_minus3 (type 0)
            (return (i32.const -3)))

    (func $receive_fail_overflow (type 0)
            (return (i32.const -256)))

    (func $receive_fail_positive_overflow (type 0)
            (return (i32.const 256)))

    (export "init_success" (func $init_success))
    (export "init_fail1" (func $init_fail1))
    (export "init_fail_minus2" (func $init_fail_minus2))
    (export "init_fail_minus1" (func $init_fail_minus1))
    (export "init_fail_overflow" (func $init_fail_overflow))
    (export "init_fail_positive_overflow" (func $init_fail_positive_overflow))
    (export "success.receive_success" (func $receive_success))
    (export "success.receive_success2" (func $receive_success2))
    (export "success.receive_fail5" (func $receive_fail5))
    (export "success.receive_fail_minus2" (func $receive_fail_minus2))
    (export "success.receive_fail_minus3" (func $receive_fail_minus3))
    (export "success.receive_fail_overflow" (func $receive_fail_overflow))
    (export "success.receive_fail_positive_overflow" (func $receive_fail_positive_overflow))
)