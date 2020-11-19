(module
    (; This provides a smart contract for testing whether sending from
       the smart contract to an account works correctly. The contract simply
       forwards the amount back to the receiver.
    ;)
    (type (;0;) (func (param i32))) (;type of the get_init_ctx method;)
    (type (;1;) (func (param i64) (result i32))) (;type of the init/receive method;)
    (type (;2;) (func (result i32))) (; type of the accept method ;)
    (type (;3;) (func (param i32 i32 i32) (result i32))) (; type of the write_state function ;)
    (type (; 4 ;) (func (param i32 i64) (result i32))) (; type of the simple_tfansfer method ;)
    (import "concordium" "get_receive_invoker" (func $get_receive_invoker (type 0)))
    (import "concordium" "accept" (func (;1;) $accept (type 2)))
    (import "concordium" "write_state" (func $write_state (type 3)))
    (import "concordium" "simple_transfer" (func $simple_transfer (type 4)))
     
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
        (call $get_receive_invoker (i32.const 0))
        (; the invoker is written at the beginning of state ;)
        (return (call $simple_transfer (i32.const 0) (local.get 0)))
    )

    (memory (;0;) 1)
    (export "init_send" (func $init))
    (export "send.receive" (func $receive))
)