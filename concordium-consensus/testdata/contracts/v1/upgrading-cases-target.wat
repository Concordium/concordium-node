;; 
(module

  ;; Imports
  (import "concordium" "get_parameter_section" (func $host_get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))

  (import "concordium" "upgrade" (func $host_upgrade (param $start i32) (result i64)))

  (import "concordium" "state_create_entry" (func $state_create_entry (param $key_start i32) (param $key_length i32) (result i64)))
  (import "concordium" "state_entry_write" (func $state_entry_write (param $entry i64) (param $read_location i32) (param $length i32) (param $offset i32) (result i32)))


  ;; Initialize contract.
  (func $init (export "init_contract") (param i64) (result i32)
        (return (i32.const 0))) ;; Successful init
  (memory 1))
