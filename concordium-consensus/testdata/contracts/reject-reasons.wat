(module
    (; This module provides init and receive functions that return different values  ;)
    (type (func (param i64) (result i32))) (;type of the init/receive functions;)
    (type (func (result i32))) (; type of the accept function ;)
    (import "concordium" "accept" (func $accept (type 1)))
    (func $init_success (type 0)
            (return (i32.const 0)))

    (func $init_error_pos (type 0)
            (return (i32.const 1)))

    (func $init_fail_minus2 (type 0)
            (return (i32.const -2)))

    (func $init_fail_big (type 0)
            (return (i32.const -2147483648)))

    (func $receive_error_no_action (type 0)
            (return (i32.const 0)))

    (func $receive_success (type 0)
            (return (call $accept)))

    (func $receive_error_pos (type 0)
            (return (i32.const 5)))

    (func $receive_fail_minus5 (type 0)
            (return (i32.const -5)))

    (func $receive_fail_big (type 0)
            (return (i32.const -2147483648)))

    (export "init_success" (func $init_success))
    (export "init_error_pos" (func $init_error_pos))
    (export "init_fail_minus2" (func $init_fail_minus2))
    (export "init_fail_big" (func $init_fail_big))
    (export "success.receive_error_no_action" (func $receive_error_no_action))
    (export "success.receive_success" (func $receive_success))
    (export "success.receive_error_pos" (func $receive_error_pos))
    (export "success.receive_fail_minus5" (func $receive_fail_minus5))
    (export "success.receive_fail_big" (func $receive_fail_big))
)