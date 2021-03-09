;; Tests of memory.size and memory.grow

(module

  ;; Imports

  (import "concordium" "accept" (func $accept (result i32)))

  ;; Helper Functions

  (func $assert_eq (param $actual i32) (param $expected i32)
    (if (i32.eq (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

  (func $assert_ne (param $actual i32) (param $expected i32)
    (if (i32.ne (local.get $actual) (local.get $expected))
      (then nop)
      (else unreachable)))

  ;; Init

  (func $init (export "init_test") (param i64) (result i32)

    ;; Assert, initial size == 1 page (due to allocation in module)
    (call $assert_eq
      (memory.size) ;; Returns the number of pages currently allocated
      (i32.const 1))

    ;; Grow memory to 2 pages
    (call $assert_ne
      (memory.grow (i32.const 1)) ;; returns -1 on failure
      (i32.const -1))

    ;; Assert, memory size == 2 pages
    (call $assert_eq
      (memory.size) ;; Returns the number of pages currently allocated
      (i32.const 2))

    (return (i32.const 0))
  )

  ;; Receive Tests

  (func (export "test.memory_size_is_correct_and_growable__succeed") (param i64) (result i32)

    ;; `memory.grow` is transient, so the memory allocated in init should have been de-allocated.

    ;; Assert, memory size == 1 (due to allocation in module)
    (call $assert_eq
      (memory.size) ;; Returns the number of pages currently allocated
      (i32.const 1))

    ;; Grow memory to 2 pages
    (call $assert_ne
      (memory.grow (i32.const 1)) ;; returns -1 on failure
      (i32.const -1))

    ;; Assert, memory size == 2 pages
    (call $assert_eq
      (memory.size) ;; Returns the number of pages currently allocated
      (i32.const 2))

    (return (call $accept))
  )

  (memory 1)
)
