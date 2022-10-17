;; A module that tests the upgrading functionality introduced as part of P5.
;; It is intended that contract A upgrades to contract A in ./upgrading_1.wat.
(module 
    ;; Imports
    (import "concordium" "upgrade" (func $upgrade (param $module_ptr i32) (result i64)))
    (import "concordium" "get_parameter_section" (func $get_parameter_section (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))    

    ;; Helper functions
    (func $assert_eq_64 (param $actual i64) (param $expected i64)
    (if (i64.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

    ;; Init a
    (func $init_a (export "init_a") (param i64) (result i32)                        
        (return (i32.const 0)) ;; Successful init
    )

    ;; Upgrade
    (func $a_upgrade (export "a.upgrade")  (param i64) (result i32)
        (local $rv i64)
        ;; Here we carry out the upgrade with the parameters we're called with.
        ;; That is, we're called with a module reference to the module we want to upgrade to.
        ;; Module references are always 32 bytes long as they consist simply of a SHA256 hash.
        (call $get_parameter_section (i32.const 0) (i32.const 0) (i32.const 32) (i32.const 0))
        ;; We store the upgrade return value in $rv.
        (local.set $rv (call $upgrade (i32.const 0)))
        ;; We assert that the upgrade was successful.
        (call $assert_eq_64 (i64.const 0) (local.get $rv))
        ;; And now we simply return 0 for success.
        (return (i32.const 0))
    )
    (memory 1)
)