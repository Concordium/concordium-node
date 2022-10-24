;; A module that tests the upgrading functionality introduced as part of P5.
;; It is intended that contract A upgrades to contract A in ./upgrading_1.wat.
(module
    ;; Imports
    (import "concordium" "upgrade" (func $upgrade (param $module_ptr i32) (result i64)))
    (import "concordium" "get_parameter_section" (func $get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
    (import "concordium" "get_parameter_size" (func $get_parameter_size (param $index i32) (result i32)))

    ;; Helper functions
    (func $assert_eq_64 (param $actual i64) (param $expected i64)
    (if (i64.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

    (func $assert_eq_32 (param $actual i32) (param $expected i32)
    (if (i32.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

    ;; Init a
    (func $init_a (export "init_a") (param i64) (result i32)
        (return (i32.const 0)) ;; Successful init
    )

    ;; Upgrade
    (func $bump (export "a.bump")  (param i64) (result i32)
        ;; Here we carry out the upgrade with the parameters we're called with.
        ;; That is, we're called with a module reference to the module we want to upgrade to.
        ;; Module references are always 32 bytes long as they consist simply of a SHA256 hash.
        (call $assert_eq_32 (i32.const 32)(call $get_parameter_size (i32.const 0)))
        (call $assert_eq_32
                (call $get_parameter_section
                    (i32.const 0)
                    (i32.const 0)
                    (i32.const 32)
                    (i32.const 0))
                (i32.const 32))
        ;; Carry out the upgrade and ensure it was successful.
        (call $assert_eq_64 (i64.const 0) (call $upgrade (i32.const 0)))
        ;; And now we simply return 0 for success.
        (return (i32.const 0))
    )
    (memory 1)
)
