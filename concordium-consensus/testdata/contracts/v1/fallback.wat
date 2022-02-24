;; A couple of trivial contracts to test the fallback mechanism.

(module

  ;; Imports

  (import "concordium" "get_parameter_size" (func $get_parameter_size (param $index i32) (result i32)))
  (import "concordium" "get_parameter_section" (func $get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "invoke" (func $invoke (param $tag i32) (param $start i32) (param $length i32) (result i64)))
  (import "concordium" "write_output" (func $write_output (param $start i32) (param $length i32) (param $offset i32) (result i32)))

  (import "concordium" "get_receive_entrypoint_size" (func $get_ep_size (result i32)))
  (import "concordium" "get_receive_entrypoint" (func $get_ep (param $start i32)))

  (import "concordium" "state_lookup_entry" (func $state_lookup_entry (param $key_start i32) (param $key_length i32) (result i64)))
  (import "concordium" "state_create_entry" (func $state_create_entry (param $key_start i32) (param $key_length i32) (result i64)))
  (import "concordium" "state_entry_read" (func $state_entry_read (param $entry i64) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "state_entry_write" (func $state_entry_write (param $entry i64) (param $read_location i32) (param $length i32) (param $offset i32) (result i32)))

  ;; Contract 1 with a fallback

  (func $init_one (export "init_one") (param i64) (result i32)
    ;; store the address of the contract to redirect to
    (call $get_parameter_section
          (i32.const 0)
          (i32.const 0)
          (i32.const 16)
          (i32.const 0))
    (call $state_entry_write
          (call $state_create_entry (i32.const 0) (i32.const 0))
          (i32.const 0)
          (i32.const 16)
          (i32.const 0))
    (return (i32.const 0)) ;; Successful init
  )

  ;; a contract with a single entrypoint (and no defaults)
  (func $init_two (export "init_two") (param i64) (result i32)
    (return (i32.const 0)) ;; Successful init
  )

  ;; forward the call to another entrypoint. Copy the return value.
  (func $one_fallback (export "one.") (param $amount i64) (result i32)
    (local $pos i32)
    (local $rv i64)
    ;; write address to redirect to. Assume 16 bytes are stored in the entry.
    (call $state_entry_read
          (call $state_lookup_entry (i32.const 0) (i32.const 0))
          (local.get $pos)
          (i32.const 16)
          (i32.const 0))
    (local.set $pos (i32.add (local.get $pos) (i32.const 16)))
    ;; write length of parameter
    (i32.store16 (local.get $pos) (call $get_parameter_size (i32.const 0)))
    (local.set $pos (i32.add (local.get $pos) (i32.const 2)))
    ;; write the parameter
    (call $get_parameter_section
          (i32.const 0)
          (local.get $pos)
          (call $get_parameter_size (i32.const 0))
          (i32.const 0))
    (local.set $pos (i32.add (local.get $pos) (call $get_parameter_size (i32.const 0))))
    ;; write length of the entrypoint
    (i32.store16 (local.get $pos) (call $get_ep_size))
    (local.set $pos (i32.add (local.get $pos) (i32.const 2)))
    (call $get_ep (local.get $pos))
    (local.set $pos (i32.add (local.get $pos) (call $get_ep_size)))
    ;; write amount
    (i64.store (local.get $pos) (local.get $amount))
    (local.set $pos (i32.add (local.get $pos) (i32.const 8)))
    ;; invoke with the amount we were given, and the parameter we were called with.
    (local.set $rv (call $invoke (i32.const 1) (i32.const 0) (local.get $pos)))
    ;; forward the return value
    ;; store it in memory first
    ;; first check if there is a return value.
    (if (i32.eq (i32.const -1) (call $get_parameter_size (i32.const 1)))
        (then unreachable))
    (call $get_parameter_section
          (i32.const 1)
          (i32.const 0)
          (call $get_parameter_size (i32.const 1))
          (i32.const 0))
    (call $write_output (i32.const 0) (call $get_parameter_size (i32.const 1)) (i32.const 0))
    (drop)
    (drop)
    (drop)
    (drop)
    ;; and return success
    (i32.const 0)
  )

  ;; the single entrypoint of the "two" contract. It just writes the parameter to the
  ;; the return value.
  (func $call (export "two.do") (param i64) (result i32)
    ;; write the parameter as output
    (call $get_parameter_section
          (i32.const 0)
          (i32.const 0)
          (call $get_parameter_size (i32.const 0))
          (i32.const 0))
    (call $write_output (i32.const 0) (call $get_parameter_size (i32.const 0)) (i32.const 0))
    (drop)
    (drop)
    ;; return success
    (i32.const 0)
  )

  (memory 1)
)