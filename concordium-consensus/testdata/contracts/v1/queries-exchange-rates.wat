(module

 ;; Imports

 (import "concordium" "get_parameter_section" (func $host_get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
 (import "concordium" "invoke" (func $host_invoke (param $tag i32) (param $start i32) (param $length i32) (result i64)))

 ;; Helper functions

 ;; Cause a runtime error if the two provided numbers are not equal.
 (func $assert_eq_64 (param $actual i64) (param $expected i64)
       (if (i64.eq (local.get $actual) (local.get $expected))
           (then nop)
           (else unreachable)))

 ;; Read the return value into memory, takes the response from invoke and assumes the response is successful.
 (func $get_invoke_return_value (param $invoke_result i64) (param $write_offset i32) (param $length i32) (param $read_offset i32) (result i32)
       ;; Declare local variable for storing the index of the return value.
       (local $return_index i32)

       ;; Get the index of the response
       ;; The numeric value 8388607 is the mask 0b0111_1111_1111_1111_1111_1111
       (local.set $return_index (i32.and (i32.const 8388607)
                                         (i32.wrap_i64 (i64.shr_u (local.get $invoke_result) (i64.const 40)))))

       ;; Read the return value into memory.
       (return (call $host_get_parameter_section
                     (local.get $return_index) ;; Index of the return value.
                     (local.get $write_offset) ;; Write offset in memory.
                     (local.get $length) ;; Write length.
                     (local.get $read_offset)))) ;; Offset to read from.

;; Read the parameter into memory.
 (func $get_parameter (param $write_offset i32) (param $length i32) (param $read_offset i32) (result i32)
       (return
         (call $host_get_parameter_section
               (i32.const 0)
               (local.get $write_offset) ;; Write offset in memory.
               (local.get $length) ;; Write length.
               (local.get $read_offset)))) ;; Offset to read from.

 ;; Invoke a query for the exchange rates (Euro per NRG and CCD per Euro)
 ;;
 ;; Return value:
 ;; - 8 bytes for the numerator of Euro per NRG.
 ;; - 8 bytes for the denominator of Euro per NRG.
 ;; - 8 bytes for the numerator of CCD per Euro.
 ;; - 8 bytes for the denominator of CCD per Euro.
 (func $invoke_query_exchange_rates (result i64)
       (call $host_invoke
             (i32.const 4) ;; Tag for the exchange rates query
             (i32.const 0) ;; Offset in memory to start reading from.
             (i32.const 0))) ;; The length of the parameter.

 ;; Contract

 ;; Initialize contract.
 (func $init (export "init_contract") (param i64) (result i32)
       (return (i32.const 0))) ;; Successful init

 ;; Receive method
 (func $receive_method (export "contract.query") (param $amount i64) (result i32)

       ;; Query the exchange rates.
       (call $get_invoke_return_value
             (call $invoke_query_exchange_rates)
             (i32.const 0) ;; Write offset
             (i32.const 32) ;; Length
             (i32.const 0)) ;; Read offset

       (call $get_parameter
             (i32.const 32) ;; Write offset
             (i32.const 32) ;; Length
             (i32.const 0)) ;; Read offset
       (drop)

       ;;
       (call $assert_eq_64
             (i64.load (i32.const 0))
             (i64.load (i32.const 32)))

       ;;
       (call $assert_eq_64
             (i64.load (i32.const 8))
             (i64.load (i32.const 40)))

       ;;
       (call $assert_eq_64
             (i64.load (i32.const 16))
             (i64.load (i32.const 48)))

       ;;
       (call $assert_eq_64
             (i64.load (i32.const 24))
             (i64.load (i32.const 56)))


       ;; Return success
       (return (i32.const 0)))

 (memory 1))
