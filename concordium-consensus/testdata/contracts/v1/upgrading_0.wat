;; A module that tests the upgrading functionality introduced as part of P5.
;; It is intended that contract A upgrades to contract A in ./upgrading_1.wat.
;;
;; This contract only contains an 'init' for A and a receive function for upgrading. 
;; The init function initializes some state which should survive the upgrade to contract A of 
;; ./upgrading_1.wat.
(module 
    ;; Imports
    (import "concordium" "upgrade" (func $upgrade (param $module_ptr i32) (result i64)))
    (import "concordium" "state_create_entry" (func $state_create_entry (param $key_start i32) (param $key_length i32) (result i64)))
    (import "concordium" "state_entry_write" (func $state_entry_write (param $entry i64) (param $read_location i32) (param $length i32) (param $offset i32) (result i32)))

    ;; Helper functions
    (func $assert_eq_64 (param $actual i64) (param $expected i64)
    (if (i64.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

    ;; Init a
    (func $init_a (export "init_a") (param i64) (result i32)
        (i64.store (i32.const 0) (i64.const 0))        
        ;; Create an entry at [0] and write 8 zero bytes to it.
        (call $state_entry_write (call $state_create_entry (i32.const 0) (i32.const 1)) (i32.const 0) (i32.const 8) (i32.const 0))
        (return (i32.const 0)) ;; Successful init
    )

    ;; Upgrade
    (func $upgrade_a (export "upgrade_a")  (param i64) (result i32)
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
        (i32.const 0)
    )
)