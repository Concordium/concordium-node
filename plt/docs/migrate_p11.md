# Rust PLT Scheduler P11 Migration

The switch to use the Rust PLT Scheduler will happen at the protocol upgrade to P11. And not
at the node software update. This is to reduce risk of differences between the Rust and Haskell implementations.
The P9/10 and P11 block states will be compatible though. Long term (after the P11 migration on mainnet),
we way want to change to dispatch to the Rust PLT Scheduler also for P9 and P10 in order to clean 
up the code. We can do a catchup on mainnet using the Rust PLT scheduler, to ensure that it is compatible
with the Haskell implementation.

```mermaid
C4Container
      title PLT logic per protocol version
      Container_Boundary(node, "Node (Haskell)") {
        Component(scheduler, "Scheduler and Queries")
        
        Component(plt_scheduler_haskell, "PLT Scheduler Logic and Queries (Haskell)")

        Boundary(plt_scheduler_boundary_rust, "PLT Scheduler (Rust) (FFI)", "boundary") {
          Component(plt_scheduler, "PLT Scheduler and Queries (Rust)") 
        }     

        Boundary(block_state_boundary, "Block state", "boundary") {
          ComponentDb(block_state_9, "Block state P9")
          ComponentDb(block_state_10, "Block state P10")
          ComponentDb(block_state_11, "Block state P11")
        }
      }

Rel(scheduler, plt_scheduler_haskell, "Dispatch on P9 and P10")
Rel(scheduler, plt_scheduler, "Dispatch on >=P11")

Rel(plt_scheduler, block_state_11, "Updates and queries block state")
Rel(plt_scheduler_haskell, block_state_10, "Updates and queries block state")
Rel(plt_scheduler_haskell, block_state_9, "Updates and queries block state")

UpdateElementStyle(scheduler, $borderColor="black")
UpdateElementStyle(plt_scheduler, $borderColor="black")
UpdateElementStyle(plt_scheduler_haskell, $borderColor="black")

UpdateElementStyle(block_state_9, $borderColor="black")
UpdateElementStyle(block_state_10, $borderColor="black")
UpdateElementStyle(block_state_11, $borderColor="black")

UpdateLayoutConfig($c4ShapeInRow="5", $c4BoundaryInRow="3")
```