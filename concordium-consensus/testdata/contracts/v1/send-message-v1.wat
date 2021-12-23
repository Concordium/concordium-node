;; A trivial contract that simply forwards a message.
;; Used to test messaging between v1 and v0 contracts.
;; This is a v0 contract.

(module

  ;; Imports

  (import "concordium" "get_parameter_size" (func $get_parameter_size (result i32)))
  (import "concordium" "get_parameter_section" (func $get_parameter_section (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "send" (func $send (param $addr_index i64) (param $addr_subindex i64)
                                (param $receive_name i32) (param $receive_name_len i32)
                                (param $amount i64) (param $parameter i32) (param $parameter_len i32) (result i32)))

  ;; The empty contract

  (func $init_proxy (export "init_proxy") (param i64) (result i32)
    (return (i32.const 0)) ;; Successful init
  )

  (func $forward_proxy (export "proxy.forward") (param $amount i64) (result i32)
    (local $size i32)
    (local.set $size (call $get_parameter_size))
    (call $get_parameter_section (i32.const 0) (local.get $size) (i32.const 0))
    (drop)
    (call $send (i64.load (i32.const 0)) (i64.load (i32.const 8))
                (i32.const 18) (i32.load16_u (i32.const 16)) ;; receive name (2 bytes for length + data)
                (local.get $amount) 
                (i32.add (i32.const 22) (i32.load16_u (i32.const 16))) ;; start of parameter
                (i32.load (i32.add (i32.const 18) (i32.load16_u (i32.const 16)))) ;; length of the parameter
                )
  )
  (memory 1)
)