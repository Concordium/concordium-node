(module
    (; This module provides a contract that tries to send an amount
       to a given address (given as parameter). If the sending fails the 
       contract call should still succeed. ;)
    (type (;0;) (func (param i32))) (;type of the get_init_ctx method;)
    (type (;1;) (func (param i64) (result i32))) (;type of the init/receive method;)
    (type (;2;) (func (result i32))) (; type of the accept method ;)
    (type (;3;) (func (param i32 i32 i32) (result i32))) (; type of the write_state/get_parameter function ;)
    (type (; 4 ;) (func (param i32 i64) (result i32))) (; type of the simple_tfansfer method ;)
    (type (; 5 ;) (func (param i32 i32) (result i32))) (; type of the combine_or/combine_and methods ;)
    (import "concordium" "accept" (func (;1;) $accept (type 2)))
    (import "concordium" "combine_or" (func (;1;) $or_else (type 5)))
    (import "concordium" "write_state" (func $write_state (type 3)))
    (import "concordium" "simple_transfer" (func $simple_transfer (type 4)))
    (import "concordium" "get_parameter_section" (func $get_parameter_section (type 3)))
     
    (; init method that does nothing but initialize the contract ;)
    (func (;6;) $init (type 1)
        (i32.const 0)
    )

    (; receive function that sends the tokens to whoever invoked it. 
       It sends the amount that it was invoked with ;)
    (func (;7;) $receive (type 1)
        (; allocate some memory  for the receive context ;)
        (if (i32.eq (i32.const -1) (memory.grow (i32.const 1)))
            (then unreachable) 
            (else nop))
        (; parameter is assumed to be an account address ;)
        (call $get_parameter_section (i32.const 0) (i32.const 32) (i32.const 0))
        (; the parameter is the whole address (or so we assume, so just pass it to simple-transfer) ;)
        (return (call $or_else (call $simple_transfer (i32.const 0) (local.get 0)) (call $accept)))
    )

    (memory (;0;) 1)
    (export "init_try" (func $init))
    (export "try.receive" (func $receive))
)