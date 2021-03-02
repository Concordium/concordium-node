(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (type (;1;) (func (param i32) (result i32)))
  (type (;2;) (func (param i32 i32 i32 i32)))
  (type (;3;) (func (param i32 i32)))
  (type (;4;) (func (param i32 i32 i32) (result i32)))
  (type (;5;) (func (result i32)))
  (type (;6;) (func (param i32)))
  (type (;7;) (func (result i64)))
  (type (;8;) (func (param i64) (result i32)))
  (type (;9;) (func (param i32 i32 i32)))
  (type (;10;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;11;) (func))
  (type (;12;) (func (param i32 i32 i32 i32 i32) (result i32)))
  (type (;13;) (func (param i32) (result i64)))
  (type (;14;) (func (param i32 i32 i32 i32 i32 i32) (result i32)))
  (type (;15;) (func (param i64 i32 i32) (result i32)))
  (import "concordium" "accept" (func (;0;) (type 5)))
  (import "concordium" "get_receive_self_address" (func (;1;) (type 6)))
  (import "concordium" "get_receive_self_balance" (func (;2;) (type 7)))
  (import "concordium" "get_receive_sender" (func (;3;) (type 6)))
  (import "concordium" "state_size" (func (;4;) (type 5)))
  (func (;5;) (type 2) (param i32 i32 i32 i32)
    (local i32)
    global.get 0
    i32.const 96
    i32.sub
    local.tee 4
    global.set 0
    local.get 4
    local.get 1
    i32.store offset=8
    local.get 4
    local.get 3
    i32.store offset=12
    block  ;; label = @1
      local.get 1
      local.get 3
      i32.ne
      br_if 0 (;@1;)
      local.get 0
      local.get 2
      local.get 1
      call 74
      drop
      local.get 4
      i32.const 96
      i32.add
      global.set 0
      return
    end
    local.get 4
    i32.const 40
    i32.add
    i32.const 20
    i32.add
    i32.const 1
    i32.store
    local.get 4
    i32.const 52
    i32.add
    i32.const 2
    i32.store
    local.get 4
    i32.const 16
    i32.add
    i32.const 20
    i32.add
    i32.const 3
    i32.store
    local.get 4
    local.get 4
    i32.const 8
    i32.add
    i32.store offset=64
    local.get 4
    local.get 4
    i32.const 12
    i32.add
    i32.store offset=68
    local.get 4
    i32.const 72
    i32.add
    i32.const 20
    i32.add
    i32.const 0
    i32.store
    local.get 4
    i64.const 3
    i64.store offset=20 align=4
    local.get 4
    i32.const 1048636
    i32.store offset=16
    local.get 4
    i32.const 2
    i32.store offset=44
    local.get 4
    i32.const 1048720
    i32.store offset=88
    local.get 4
    i64.const 1
    i64.store offset=76 align=4
    local.get 4
    i32.const 1048712
    i32.store offset=72
    local.get 4
    local.get 4
    i32.const 40
    i32.add
    i32.store offset=32
    local.get 4
    local.get 4
    i32.const 72
    i32.add
    i32.store offset=56
    local.get 4
    local.get 4
    i32.const 68
    i32.add
    i32.store offset=48
    local.get 4
    local.get 4
    i32.const 64
    i32.add
    i32.store offset=40
    local.get 4
    i32.const 16
    i32.add
    i32.const 1048836
    call 60
    unreachable)
  (func (;6;) (type 3) (param i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32 i64)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 2
    global.set 0
    local.get 1
    i32.const 8
    i32.add
    i32.load
    local.set 3
    local.get 1
    i32.load offset=4
    local.set 4
    local.get 1
    i32.load
    local.set 5
    i32.const 0
    local.set 6
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                loop  ;; label = @7
                  i32.const 8
                  local.get 6
                  i32.sub
                  local.tee 7
                  local.get 3
                  local.get 5
                  i32.sub
                  local.tee 8
                  local.get 8
                  local.get 7
                  i32.gt_u
                  select
                  local.tee 7
                  i32.eqz
                  br_if 1 (;@6;)
                  local.get 7
                  local.get 5
                  i32.add
                  local.tee 8
                  local.get 7
                  i32.lt_u
                  br_if 3 (;@4;)
                  local.get 3
                  local.get 8
                  i32.lt_u
                  br_if 4 (;@3;)
                  local.get 2
                  i32.const 8
                  i32.add
                  local.get 6
                  i32.add
                  local.get 4
                  local.get 5
                  i32.add
                  local.get 7
                  call 74
                  drop
                  local.get 1
                  local.get 8
                  i32.store
                  local.get 8
                  local.set 5
                  local.get 7
                  local.get 6
                  i32.add
                  local.tee 6
                  i32.const 8
                  i32.lt_u
                  br_if 0 (;@7;)
                end
                local.get 6
                i32.const 8
                i32.ne
                br_if 0 (;@6;)
                local.get 2
                i64.load offset=8
                local.set 9
                i32.const 0
                local.set 6
                loop  ;; label = @7
                  i32.const 8
                  local.get 6
                  i32.sub
                  local.tee 7
                  local.get 3
                  local.get 8
                  i32.sub
                  local.tee 5
                  local.get 5
                  local.get 7
                  i32.gt_u
                  select
                  local.tee 7
                  i32.eqz
                  br_if 1 (;@6;)
                  local.get 7
                  local.get 8
                  i32.add
                  local.tee 5
                  local.get 7
                  i32.lt_u
                  br_if 5 (;@2;)
                  local.get 3
                  local.get 5
                  i32.lt_u
                  br_if 6 (;@1;)
                  local.get 2
                  i32.const 8
                  i32.add
                  local.get 6
                  i32.add
                  local.get 4
                  local.get 8
                  i32.add
                  local.get 7
                  call 74
                  drop
                  local.get 1
                  local.get 5
                  i32.store
                  local.get 5
                  local.set 8
                  local.get 7
                  local.get 6
                  i32.add
                  local.tee 6
                  i32.const 8
                  i32.lt_u
                  br_if 0 (;@7;)
                end
                local.get 6
                i32.const 8
                i32.ne
                br_if 0 (;@6;)
                local.get 0
                i32.const 16
                i32.add
                local.get 2
                i64.load offset=8
                i64.store
                local.get 0
                local.get 9
                i64.store offset=8
                i64.const 0
                local.set 9
                br 1 (;@5;)
              end
              i64.const 1
              local.set 9
            end
            local.get 0
            local.get 9
            i64.store
            local.get 2
            i32.const 16
            i32.add
            global.set 0
            return
          end
          local.get 5
          local.get 8
          i32.const 1048988
          call 59
          unreachable
        end
        local.get 8
        local.get 3
        i32.const 1048988
        call 58
        unreachable
      end
      local.get 8
      local.get 5
      i32.const 1048988
      call 59
      unreachable
    end
    local.get 5
    local.get 3
    i32.const 1048988
    call 58
    unreachable)
  (func (;7;) (type 0) (param i32 i32) (result i32)
    local.get 0
    i32.load
    local.set 0
    block  ;; label = @1
      local.get 1
      call 69
      br_if 0 (;@1;)
      block  ;; label = @2
        local.get 1
        call 70
        br_if 0 (;@2;)
        local.get 0
        local.get 1
        call 61
        return
      end
      local.get 0
      local.get 1
      call 73
      return
    end
    local.get 0
    local.get 1
    call 71)
  (func (;8;) (type 8) (param i64) (result i32)
    (local i32)
    i32.const -1
    local.set 1
    block  ;; label = @1
      local.get 0
      i64.const 0
      i64.ne
      br_if 0 (;@1;)
      call 18
      i32.const 0
      local.set 1
    end
    local.get 1)
  (func (;9;) (type 8) (param i64) (result i32)
    (local i32 i32)
    global.get 0
    i32.const 48
    i32.sub
    local.tee 1
    global.set 0
    call 18
    local.get 1
    i32.const 0
    i32.store offset=24
    local.get 1
    call 0
    i32.store offset=28
    local.get 1
    i32.const 40
    i32.add
    i64.const 0
    i64.store
    local.get 1
    i64.const 0
    i64.store offset=32
    local.get 1
    i32.const 8
    i32.add
    local.get 1
    i32.const 24
    i32.add
    local.get 1
    i32.const 32
    i32.add
    call 17
    block  ;; label = @1
      local.get 1
      i32.load offset=8
      br_if 0 (;@1;)
      local.get 1
      i32.const 28
      i32.add
      call 19
      local.set 2
      local.get 1
      i32.const 48
      i32.add
      global.set 0
      local.get 2
      return
    end
    call 43
    unreachable)
  (func (;10;) (type 8) (param i64) (result i32)
    (local i32 i32 i32)
    global.get 0
    i32.const 112
    i32.sub
    local.tee 1
    global.set 0
    call 18
    local.get 1
    i32.const 0
    i32.store offset=20
    local.get 1
    i32.const 64
    i32.add
    call 1
    local.get 1
    i32.const 104
    i32.add
    i32.const 16
    i32.store
    local.get 1
    i32.const 0
    i32.store offset=96
    local.get 1
    local.get 1
    i32.const 64
    i32.add
    i32.store offset=100
    local.get 1
    i32.const 24
    i32.add
    local.get 1
    i32.const 96
    i32.add
    call 6
    block  ;; label = @1
      local.get 1
      i64.load offset=24
      i64.const 1
      i64.eq
      br_if 0 (;@1;)
      i32.const -1
      local.set 2
      block  ;; label = @2
        local.get 1
        i64.load offset=32
        i64.const 1
        i64.ne
        br_if 0 (;@2;)
        local.get 1
        i32.const 24
        i32.add
        i32.const 16
        i32.add
        i64.load
        i64.eqz
        i32.eqz
        br_if 0 (;@2;)
        call 2
        i64.const 42
        i64.ne
        br_if 0 (;@2;)
        local.get 1
        i32.const 24
        i32.add
        call 3
        local.get 1
        i32.load8_u offset=24
        local.tee 3
        i32.const 1
        i32.gt_u
        br_if 1 (;@1;)
        block  ;; label = @3
          block  ;; label = @4
            local.get 3
            br_table 1 (;@3;) 0 (;@4;) 1 (;@3;)
          end
          local.get 1
          i32.const 104
          i32.add
          i32.const 16
          i32.store
          local.get 1
          local.get 1
          i32.const 24
          i32.add
          i32.const 1
          i32.or
          i32.store offset=100
          local.get 1
          i32.const 0
          i32.store offset=96
          local.get 1
          i32.const 64
          i32.add
          local.get 1
          i32.const 96
          i32.add
          call 6
          local.get 1
          i64.load offset=64
          i64.const 1
          i64.eq
          br_if 2 (;@1;)
          local.get 1
          i64.load offset=72
          i64.const 72057594037927936
          i64.ne
          br_if 1 (;@2;)
          local.get 1
          i32.const 64
          i32.add
          i32.const 16
          i32.add
          i64.load
          i64.eqz
          i32.eqz
          br_if 1 (;@2;)
          local.get 1
          call 0
          i32.store offset=64
          local.get 1
          i32.const 32
          i32.add
          i64.const 0
          i64.store
          local.get 1
          i64.const 0
          i64.store offset=24
          local.get 1
          local.get 1
          i32.const 20
          i32.add
          local.get 1
          i32.const 24
          i32.add
          call 17
          local.get 1
          i32.load
          br_if 2 (;@1;)
          local.get 1
          i32.const 64
          i32.add
          call 19
          local.set 2
          br 1 (;@2;)
        end
        local.get 1
        i32.const 64
        i32.add
        i32.const 32
        local.get 1
        i32.const 24
        i32.add
        i32.const 1
        i32.or
        i32.const 32
        call 5
      end
      local.get 1
      i32.const 112
      i32.add
      global.set 0
      local.get 2
      return
    end
    call 43
    unreachable)
  (func (;11;) (type 0) (param i32 i32) (result i32)
    (local i32)
    local.get 0
    local.get 1
    call 14
    local.set 2
    local.get 2
    return)
  (func (;12;) (type 9) (param i32 i32 i32)
    local.get 0
    local.get 1
    local.get 2
    call 15
    return)
  (func (;13;) (type 10) (param i32 i32 i32 i32) (result i32)
    (local i32)
    local.get 0
    local.get 1
    local.get 2
    local.get 3
    call 16
    local.set 4
    local.get 4
    return)
  (func (;14;) (type 0) (param i32 i32) (result i32)
    i32.const 1049800
    local.get 0
    local.get 1
    call 30)
  (func (;15;) (type 9) (param i32 i32 i32)
    i32.const 1049800
    local.get 0
    local.get 1
    local.get 2
    call 31)
  (func (;16;) (type 10) (param i32 i32 i32 i32) (result i32)
    (local i32)
    block  ;; label = @1
      i32.const 1049800
      local.get 3
      local.get 2
      call 30
      local.tee 4
      i32.eqz
      br_if 0 (;@1;)
      local.get 4
      local.get 0
      local.get 3
      local.get 1
      local.get 1
      local.get 3
      i32.gt_u
      select
      call 74
      drop
      i32.const 1049800
      local.get 0
      local.get 1
      local.get 2
      call 31
    end
    local.get 4)
  (func (;17;) (type 9) (param i32 i32 i32)
    (local i64 i64 i64 i32)
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              local.get 2
              i32.load
              br_table 0 (;@5;) 1 (;@4;) 2 (;@3;) 0 (;@5;)
            end
            i64.const 1
            local.set 3
            local.get 2
            i64.load offset=8
            local.tee 4
            i64.const 4294967295
            i64.gt_u
            br_if 3 (;@1;)
            local.get 1
            local.get 4
            i64.store32
            br 2 (;@2;)
          end
          local.get 2
          i64.load offset=8
          local.set 5
          call 4
          local.set 2
          block  ;; label = @4
            block  ;; label = @5
              local.get 5
              i64.const -1
              i64.gt_s
              br_if 0 (;@5;)
              i64.const 1
              local.set 3
              block  ;; label = @6
                local.get 5
                i64.const -9223372036854775808
                i64.ne
                br_if 0 (;@6;)
                br 5 (;@1;)
              end
              i64.const 0
              local.get 5
              i64.sub
              local.tee 5
              i64.const 4294967295
              i64.le_u
              br_if 1 (;@4;)
              br 4 (;@1;)
            end
            i64.const 1
            local.set 3
            block  ;; label = @5
              local.get 5
              i64.const 4294967295
              i64.le_u
              br_if 0 (;@5;)
              br 4 (;@1;)
            end
            local.get 1
            i32.load
            local.tee 2
            local.get 5
            i32.wrap_i64
            i32.add
            local.tee 6
            local.get 2
            i32.lt_u
            br_if 3 (;@1;)
            local.get 1
            local.get 6
            i32.store
            local.get 6
            i64.extend_i32_u
            local.set 4
            br 2 (;@2;)
          end
          local.get 2
          local.get 5
          i32.wrap_i64
          local.tee 6
          i32.lt_u
          br_if 2 (;@1;)
          local.get 1
          local.get 2
          local.get 6
          i32.sub
          local.tee 2
          i32.store
          local.get 2
          i64.extend_i32_u
          local.set 4
          br 1 (;@2;)
        end
        block  ;; label = @3
          block  ;; label = @4
            local.get 2
            i64.load offset=8
            local.tee 5
            i64.const -1
            i64.le_s
            br_if 0 (;@4;)
            i64.const 1
            local.set 3
            block  ;; label = @5
              local.get 5
              i64.const 4294967295
              i64.le_u
              br_if 0 (;@5;)
              br 4 (;@1;)
            end
            local.get 1
            i32.load
            local.tee 6
            local.get 5
            i32.wrap_i64
            i32.add
            local.tee 2
            local.get 6
            i32.ge_u
            br_if 1 (;@3;)
            br 3 (;@1;)
          end
          i64.const 1
          local.set 3
          block  ;; label = @4
            local.get 5
            i64.const -9223372036854775808
            i64.ne
            br_if 0 (;@4;)
            br 3 (;@1;)
          end
          block  ;; label = @4
            i64.const 0
            local.get 5
            i64.sub
            local.tee 5
            i64.const 4294967295
            i64.le_u
            br_if 0 (;@4;)
            br 3 (;@1;)
          end
          local.get 1
          i32.load
          local.tee 6
          local.get 5
          i32.wrap_i64
          i32.sub
          local.tee 2
          local.get 6
          i32.gt_u
          br_if 2 (;@1;)
        end
        local.get 1
        local.get 2
        i32.store
        local.get 2
        i64.extend_i32_u
        local.set 4
      end
      i64.const 0
      local.set 3
    end
    local.get 0
    local.get 4
    i64.store offset=8
    local.get 0
    local.get 3
    i64.store)
  (func (;18;) (type 11))
  (func (;19;) (type 1) (param i32) (result i32)
    local.get 0
    i32.load)
  (func (;20;) (type 6) (param i32))
  (func (;21;) (type 2) (param i32 i32 i32 i32)
    (local i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 4
    global.set 0
    local.get 4
    local.get 1
    i32.load
    local.tee 5
    i32.load
    i32.store offset=12
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 2
          i32.const 2
          i32.add
          local.tee 2
          local.get 2
          i32.mul
          local.tee 2
          i32.const 2048
          local.get 2
          i32.const 2048
          i32.gt_u
          select
          local.tee 1
          i32.const 4
          local.get 4
          i32.const 12
          i32.add
          i32.const 1049004
          i32.const 1049004
          call 28
          local.tee 2
          i32.eqz
          br_if 0 (;@3;)
          local.get 5
          local.get 4
          i32.load offset=12
          i32.store
          br 1 (;@2;)
        end
        local.get 4
        i32.const 1049004
        local.get 1
        i32.const 4
        call 25
        block  ;; label = @3
          block  ;; label = @4
            local.get 4
            i32.load
            i32.eqz
            br_if 0 (;@4;)
            local.get 5
            local.get 4
            i32.load offset=12
            i32.store
            br 1 (;@3;)
          end
          local.get 4
          i32.load offset=4
          local.tee 2
          local.get 4
          i32.load offset=12
          i32.store offset=8
          local.get 4
          local.get 2
          i32.store offset=12
          local.get 1
          i32.const 4
          local.get 4
          i32.const 12
          i32.add
          i32.const 1049004
          i32.const 1049004
          call 28
          local.set 2
          local.get 5
          local.get 4
          i32.load offset=12
          i32.store
          local.get 2
          br_if 1 (;@2;)
        end
        i32.const 1
        local.set 1
        br 1 (;@1;)
      end
      local.get 2
      i64.const 0
      i64.store offset=4 align=4
      local.get 2
      local.get 2
      local.get 1
      i32.const 2
      i32.shl
      i32.add
      i32.const 2
      i32.or
      i32.store
      i32.const 0
      local.set 1
    end
    local.get 0
    local.get 2
    i32.store offset=4
    local.get 0
    local.get 1
    i32.store
    local.get 4
    i32.const 16
    i32.add
    global.set 0)
  (func (;22;) (type 0) (param i32 i32) (result i32)
    local.get 1)
  (func (;23;) (type 1) (param i32) (result i32)
    i32.const 0)
  (func (;24;) (type 6) (param i32))
  (func (;25;) (type 2) (param i32 i32 i32 i32)
    (local i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 2
        i32.const 2
        i32.shl
        local.tee 2
        local.get 3
        i32.const 3
        i32.shl
        i32.const 16384
        i32.add
        local.tee 3
        local.get 2
        local.get 3
        i32.gt_u
        select
        i32.const 65543
        i32.add
        local.tee 4
        i32.const 16
        i32.shr_u
        memory.grow
        local.tee 3
        i32.const -1
        i32.ne
        br_if 0 (;@2;)
        i32.const 1
        local.set 2
        i32.const 0
        local.set 3
        br 1 (;@1;)
      end
      local.get 3
      i32.const 16
      i32.shl
      local.tee 3
      i64.const 0
      i64.store
      i32.const 0
      local.set 2
      local.get 3
      i32.const 0
      i32.store offset=8
      local.get 3
      local.get 3
      local.get 4
      i32.const -65536
      i32.and
      i32.add
      i32.const 2
      i32.or
      i32.store
    end
    local.get 0
    local.get 3
    i32.store offset=4
    local.get 0
    local.get 2
    i32.store)
  (func (;26;) (type 0) (param i32 i32) (result i32)
    i32.const 512)
  (func (;27;) (type 1) (param i32) (result i32)
    i32.const 1)
  (func (;28;) (type 12) (param i32 i32 i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 2
        i32.load
        local.tee 5
        i32.eqz
        br_if 0 (;@2;)
        local.get 1
        i32.const -1
        i32.add
        local.set 6
        local.get 0
        i32.const 2
        i32.shl
        local.set 7
        i32.const 0
        local.get 1
        i32.sub
        local.set 8
        loop  ;; label = @3
          local.get 5
          i32.const 8
          i32.add
          local.set 9
          block  ;; label = @4
            local.get 5
            i32.load offset=8
            local.tee 10
            i32.const 1
            i32.and
            i32.eqz
            br_if 0 (;@4;)
            loop  ;; label = @5
              local.get 9
              local.get 10
              i32.const -2
              i32.and
              i32.store
              block  ;; label = @6
                block  ;; label = @7
                  local.get 5
                  i32.load offset=4
                  local.tee 10
                  i32.const -4
                  i32.and
                  local.tee 9
                  br_if 0 (;@7;)
                  i32.const 0
                  local.set 1
                  br 1 (;@6;)
                end
                i32.const 0
                local.get 9
                local.get 9
                i32.load8_u
                i32.const 1
                i32.and
                select
                local.set 1
              end
              block  ;; label = @6
                local.get 5
                i32.load
                local.tee 11
                i32.const -4
                i32.and
                local.tee 12
                i32.eqz
                br_if 0 (;@6;)
                i32.const 0
                local.get 12
                local.get 11
                i32.const 2
                i32.and
                select
                local.tee 11
                i32.eqz
                br_if 0 (;@6;)
                local.get 11
                local.get 11
                i32.load offset=4
                i32.const 3
                i32.and
                local.get 9
                i32.or
                i32.store offset=4
                local.get 5
                i32.load offset=4
                local.tee 10
                i32.const -4
                i32.and
                local.set 9
              end
              block  ;; label = @6
                local.get 9
                i32.eqz
                br_if 0 (;@6;)
                local.get 9
                local.get 9
                i32.load
                i32.const 3
                i32.and
                local.get 5
                i32.load
                i32.const -4
                i32.and
                i32.or
                i32.store
                local.get 5
                i32.load offset=4
                local.set 10
              end
              local.get 5
              local.get 10
              i32.const 3
              i32.and
              i32.store offset=4
              local.get 5
              local.get 5
              i32.load
              local.tee 9
              i32.const 3
              i32.and
              i32.store
              block  ;; label = @6
                local.get 9
                i32.const 2
                i32.and
                i32.eqz
                br_if 0 (;@6;)
                local.get 1
                local.get 1
                i32.load
                i32.const 2
                i32.or
                i32.store
              end
              local.get 2
              local.get 1
              i32.store
              local.get 1
              i32.const 8
              i32.add
              local.set 9
              local.get 1
              local.set 5
              local.get 1
              i32.load offset=8
              local.tee 10
              i32.const 1
              i32.and
              br_if 0 (;@5;)
            end
            local.get 1
            local.set 5
          end
          block  ;; label = @4
            local.get 5
            i32.load
            i32.const -4
            i32.and
            local.tee 1
            local.get 9
            i32.sub
            local.get 7
            i32.lt_u
            br_if 0 (;@4;)
            block  ;; label = @5
              local.get 9
              local.get 3
              local.get 0
              local.get 4
              i32.load offset=16
              call_indirect (type 0)
              i32.const 2
              i32.shl
              i32.add
              i32.const 8
              i32.add
              local.get 1
              local.get 7
              i32.sub
              local.get 8
              i32.and
              local.tee 1
              i32.le_u
              br_if 0 (;@5;)
              local.get 6
              local.get 9
              i32.and
              br_if 1 (;@4;)
              local.get 2
              local.get 9
              i32.load
              i32.const -4
              i32.and
              i32.store
              local.get 5
              local.set 1
              br 4 (;@1;)
            end
            local.get 1
            i32.const 0
            i32.store
            local.get 1
            i32.const -8
            i32.add
            local.tee 1
            i64.const 0
            i64.store align=4
            local.get 1
            local.get 5
            i32.load
            i32.const -4
            i32.and
            i32.store
            block  ;; label = @5
              local.get 5
              i32.load
              local.tee 10
              i32.const -4
              i32.and
              local.tee 11
              i32.eqz
              br_if 0 (;@5;)
              i32.const 0
              local.get 11
              local.get 10
              i32.const 2
              i32.and
              select
              local.tee 10
              i32.eqz
              br_if 0 (;@5;)
              local.get 10
              local.get 10
              i32.load offset=4
              i32.const 3
              i32.and
              local.get 1
              i32.or
              i32.store offset=4
            end
            local.get 1
            local.get 1
            i32.load offset=4
            i32.const 3
            i32.and
            local.get 5
            i32.or
            i32.store offset=4
            local.get 5
            local.get 5
            i32.load
            i32.const 3
            i32.and
            local.get 1
            i32.or
            i32.store
            local.get 9
            local.get 9
            i32.load
            i32.const -2
            i32.and
            i32.store
            local.get 5
            i32.load
            local.tee 9
            i32.const 2
            i32.and
            i32.eqz
            br_if 3 (;@1;)
            local.get 5
            local.get 9
            i32.const -3
            i32.and
            i32.store
            local.get 1
            local.get 1
            i32.load
            i32.const 2
            i32.or
            i32.store
            br 3 (;@1;)
          end
          local.get 2
          local.get 5
          i32.load offset=8
          local.tee 5
          i32.store
          local.get 5
          br_if 0 (;@3;)
        end
      end
      i32.const 0
      return
    end
    local.get 1
    local.get 1
    i32.load
    i32.const 1
    i32.or
    i32.store
    local.get 1
    i32.const 8
    i32.add)
  (func (;29;) (type 2) (param i32 i32 i32 i32)
    (local i32 i32 i32)
    local.get 0
    i32.load
    local.tee 4
    i32.const 0
    i32.store
    local.get 4
    i32.const -8
    i32.add
    local.tee 0
    local.get 0
    i32.load
    i32.const -2
    i32.and
    i32.store
    block  ;; label = @1
      local.get 2
      local.get 3
      i32.load offset=20
      call_indirect (type 1)
      i32.eqz
      br_if 0 (;@1;)
      block  ;; label = @2
        block  ;; label = @3
          local.get 4
          i32.const -4
          i32.add
          local.tee 3
          i32.load
          i32.const -4
          i32.and
          local.tee 2
          i32.eqz
          br_if 0 (;@3;)
          local.get 2
          i32.load
          local.tee 5
          i32.const 1
          i32.and
          i32.eqz
          br_if 1 (;@2;)
        end
        local.get 0
        i32.load
        local.tee 2
        i32.const -4
        i32.and
        local.tee 3
        i32.eqz
        br_if 1 (;@1;)
        i32.const 0
        local.get 3
        local.get 2
        i32.const 2
        i32.and
        select
        local.tee 2
        i32.eqz
        br_if 1 (;@1;)
        local.get 2
        i32.load8_u
        i32.const 1
        i32.and
        br_if 1 (;@1;)
        local.get 4
        local.get 2
        i32.load offset=8
        i32.const -4
        i32.and
        i32.store
        local.get 2
        local.get 0
        i32.const 1
        i32.or
        i32.store offset=8
        return
      end
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 0
            i32.load
            local.tee 6
            i32.const -4
            i32.and
            local.tee 4
            br_if 0 (;@4;)
            local.get 2
            local.set 1
            br 1 (;@3;)
          end
          local.get 2
          local.set 1
          i32.const 0
          local.get 4
          local.get 6
          i32.const 2
          i32.and
          select
          local.tee 6
          i32.eqz
          br_if 0 (;@3;)
          local.get 6
          local.get 6
          i32.load offset=4
          i32.const 3
          i32.and
          local.get 2
          i32.or
          i32.store offset=4
          local.get 3
          i32.load
          local.tee 4
          i32.const -4
          i32.and
          local.tee 1
          i32.eqz
          br_if 1 (;@2;)
          local.get 0
          i32.load
          i32.const -4
          i32.and
          local.set 4
          local.get 1
          i32.load
          local.set 5
        end
        local.get 1
        local.get 5
        i32.const 3
        i32.and
        local.get 4
        i32.or
        i32.store
        local.get 3
        i32.load
        local.set 4
      end
      local.get 3
      local.get 4
      i32.const 3
      i32.and
      i32.store
      local.get 0
      local.get 0
      i32.load
      local.tee 4
      i32.const 3
      i32.and
      i32.store
      block  ;; label = @2
        local.get 4
        i32.const 2
        i32.and
        i32.eqz
        br_if 0 (;@2;)
        local.get 2
        local.get 2
        i32.load
        i32.const 2
        i32.or
        i32.store
      end
      return
    end
    local.get 4
    local.get 1
    i32.load
    i32.store
    local.get 1
    local.get 0
    i32.store)
  (func (;30;) (type 4) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 3
    global.set 0
    local.get 2
    i32.const 1
    local.get 2
    select
    local.set 2
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.eqz
        br_if 0 (;@2;)
        local.get 1
        i32.const 3
        i32.add
        local.tee 4
        i32.const 2
        i32.shr_u
        local.set 5
        block  ;; label = @3
          local.get 2
          i32.const 4
          i32.gt_u
          br_if 0 (;@3;)
          local.get 5
          i32.const -1
          i32.add
          local.tee 1
          i32.const 255
          i32.gt_u
          br_if 0 (;@3;)
          local.get 3
          local.get 0
          i32.store offset=20
          local.get 3
          local.get 0
          local.get 1
          i32.const 2
          i32.shl
          i32.add
          i32.const 4
          i32.add
          local.tee 0
          i32.load
          i32.store offset=24
          block  ;; label = @4
            local.get 5
            local.get 2
            local.get 3
            i32.const 24
            i32.add
            local.get 3
            i32.const 20
            i32.add
            i32.const 1049052
            call 28
            local.tee 1
            br_if 0 (;@4;)
            local.get 3
            local.get 3
            i32.load offset=20
            local.tee 6
            i32.load
            i32.store offset=28
            block  ;; label = @5
              block  ;; label = @6
                local.get 5
                i32.const 2
                i32.add
                local.tee 1
                local.get 1
                i32.mul
                local.tee 1
                i32.const 2048
                local.get 1
                i32.const 2048
                i32.gt_u
                select
                local.tee 7
                i32.const 4
                local.get 3
                i32.const 28
                i32.add
                i32.const 1049004
                i32.const 1049004
                call 28
                local.tee 4
                i32.eqz
                br_if 0 (;@6;)
                local.get 6
                local.get 3
                i32.load offset=28
                i32.store
                br 1 (;@5;)
              end
              i32.const 0
              local.set 1
              local.get 3
              i32.const 8
              i32.add
              i32.const 1049004
              local.get 7
              i32.const 4
              i32.const 0
              i32.load offset=1049016
              call_indirect (type 2)
              block  ;; label = @6
                local.get 3
                i32.load offset=8
                i32.eqz
                br_if 0 (;@6;)
                local.get 6
                local.get 3
                i32.load offset=28
                i32.store
                br 2 (;@4;)
              end
              local.get 3
              i32.load offset=12
              local.tee 4
              local.get 3
              i32.load offset=28
              i32.store offset=8
              local.get 3
              local.get 4
              i32.store offset=28
              local.get 7
              i32.const 4
              local.get 3
              i32.const 28
              i32.add
              i32.const 1049004
              i32.const 1049004
              call 28
              local.set 4
              local.get 6
              local.get 3
              i32.load offset=28
              i32.store
              local.get 4
              i32.eqz
              br_if 1 (;@4;)
            end
            local.get 4
            i32.const 0
            i32.store offset=4
            local.get 4
            local.get 3
            i32.load offset=24
            i32.store offset=8
            local.get 4
            local.get 4
            local.get 7
            i32.const 2
            i32.shl
            i32.add
            i32.const 2
            i32.or
            i32.store
            local.get 3
            local.get 4
            i32.store offset=24
            local.get 5
            local.get 2
            local.get 3
            i32.const 24
            i32.add
            local.get 3
            i32.const 20
            i32.add
            i32.const 1049052
            call 28
            local.set 1
          end
          local.get 0
          local.get 3
          i32.load offset=24
          i32.store
          br 2 (;@1;)
        end
        local.get 3
        local.get 0
        i32.load
        i32.store offset=28
        block  ;; label = @3
          local.get 5
          local.get 2
          local.get 3
          i32.const 28
          i32.add
          i32.const 1049028
          i32.const 1049028
          call 28
          local.tee 1
          br_if 0 (;@3;)
          block  ;; label = @4
            local.get 4
            i32.const -4
            i32.and
            local.tee 1
            local.get 2
            i32.const 3
            i32.shl
            i32.const 16384
            i32.add
            local.tee 4
            local.get 1
            local.get 4
            i32.gt_u
            select
            i32.const 65543
            i32.add
            local.tee 4
            i32.const 16
            i32.shr_u
            memory.grow
            local.tee 1
            i32.const -1
            i32.ne
            br_if 0 (;@4;)
            i32.const 0
            local.set 1
            br 1 (;@3;)
          end
          local.get 1
          i32.const 16
          i32.shl
          local.tee 1
          local.get 1
          local.get 4
          i32.const -65536
          i32.and
          i32.add
          i32.const 2
          i32.or
          i32.store
          local.get 1
          i32.const 0
          i32.store offset=4
          local.get 1
          local.get 3
          i32.load offset=28
          i32.store offset=8
          local.get 3
          local.get 1
          i32.store offset=28
          local.get 5
          local.get 2
          local.get 3
          i32.const 28
          i32.add
          i32.const 1049028
          i32.const 1049028
          call 28
          local.set 1
        end
        local.get 0
        local.get 3
        i32.load offset=28
        i32.store
        br 1 (;@1;)
      end
      local.get 2
      local.set 1
    end
    local.get 3
    i32.const 32
    i32.add
    global.set 0
    local.get 1)
  (func (;31;) (type 2) (param i32 i32 i32 i32)
    (local i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 4
    global.set 0
    block  ;; label = @1
      local.get 1
      i32.eqz
      br_if 0 (;@1;)
      local.get 4
      local.get 1
      i32.store offset=4
      local.get 2
      i32.eqz
      br_if 0 (;@1;)
      block  ;; label = @2
        local.get 3
        i32.const 4
        i32.gt_u
        br_if 0 (;@2;)
        local.get 2
        i32.const 3
        i32.add
        i32.const 2
        i32.shr_u
        i32.const -1
        i32.add
        local.tee 1
        i32.const 255
        i32.gt_u
        br_if 0 (;@2;)
        local.get 4
        local.get 0
        i32.store offset=8
        local.get 4
        local.get 0
        local.get 1
        i32.const 2
        i32.shl
        i32.add
        i32.const 4
        i32.add
        local.tee 1
        i32.load
        i32.store offset=12
        local.get 4
        i32.const 4
        i32.add
        local.get 4
        i32.const 12
        i32.add
        local.get 4
        i32.const 8
        i32.add
        i32.const 1049052
        call 29
        local.get 1
        local.get 4
        i32.load offset=12
        i32.store
        br 1 (;@1;)
      end
      local.get 4
      local.get 0
      i32.load
      i32.store offset=12
      local.get 4
      i32.const 4
      i32.add
      local.get 4
      i32.const 12
      i32.add
      i32.const 1049028
      i32.const 1049028
      call 29
      local.get 0
      local.get 4
      i32.load offset=12
      i32.store
    end
    local.get 4
    i32.const 16
    i32.add
    global.set 0)
  (func (;32;) (type 13) (param i32) (result i64)
    i64.const -8947299319579089149)
  (func (;33;) (type 13) (param i32) (result i64)
    i64.const 6256179926448069205)
  (func (;34;) (type 6) (param i32))
  (func (;35;) (type 6) (param i32)
    (local i32)
    block  ;; label = @1
      local.get 0
      i32.load
      local.tee 1
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.const 4
      i32.add
      i32.load
      local.tee 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 1
      local.get 0
      i32.const 1
      call 12
    end)
  (func (;36;) (type 6) (param i32)
    (local i32)
    block  ;; label = @1
      local.get 0
      i32.load offset=4
      local.tee 1
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.const 8
      i32.add
      i32.load
      local.tee 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 1
      local.get 0
      i32.const 1
      call 12
    end)
  (func (;37;) (type 1) (param i32) (result i32)
    block  ;; label = @1
      local.get 0
      br_if 0 (;@1;)
      i32.const 1049116
      i32.const 43
      i32.const 1049200
      call 57
      unreachable
    end
    local.get 0)
  (func (;38;) (type 0) (param i32 i32) (result i32)
    block  ;; label = @1
      local.get 0
      br_if 0 (;@1;)
      i32.const 1049116
      i32.const 43
      local.get 1
      call 57
      unreachable
    end
    local.get 0)
  (func (;39;) (type 0) (param i32 i32) (result i32)
    (local i32 i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 2
    global.set 0
    local.get 0
    i32.load
    local.set 0
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 1
            i32.const 128
            i32.lt_u
            br_if 0 (;@4;)
            local.get 2
            i32.const 0
            i32.store offset=12
            local.get 1
            i32.const 2048
            i32.lt_u
            br_if 1 (;@3;)
            local.get 2
            i32.const 12
            i32.add
            local.set 3
            block  ;; label = @5
              local.get 1
              i32.const 65536
              i32.ge_u
              br_if 0 (;@5;)
              local.get 2
              local.get 1
              i32.const 63
              i32.and
              i32.const 128
              i32.or
              i32.store8 offset=14
              local.get 2
              local.get 1
              i32.const 12
              i32.shr_u
              i32.const 224
              i32.or
              i32.store8 offset=12
              local.get 2
              local.get 1
              i32.const 6
              i32.shr_u
              i32.const 63
              i32.and
              i32.const 128
              i32.or
              i32.store8 offset=13
              i32.const 3
              local.set 1
              br 3 (;@2;)
            end
            local.get 2
            local.get 1
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=15
            local.get 2
            local.get 1
            i32.const 18
            i32.shr_u
            i32.const 240
            i32.or
            i32.store8 offset=12
            local.get 2
            local.get 1
            i32.const 6
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=14
            local.get 2
            local.get 1
            i32.const 12
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=13
            i32.const 4
            local.set 1
            br 2 (;@2;)
          end
          block  ;; label = @4
            local.get 0
            i32.load offset=8
            local.tee 3
            local.get 0
            i32.const 4
            i32.add
            i32.load
            i32.ne
            br_if 0 (;@4;)
            local.get 0
            i32.const 1
            call 40
            local.get 0
            i32.load offset=8
            local.set 3
          end
          local.get 0
          i32.load
          local.get 3
          i32.add
          local.get 1
          i32.store8
          local.get 0
          local.get 0
          i32.load offset=8
          i32.const 1
          i32.add
          i32.store offset=8
          br 2 (;@1;)
        end
        local.get 2
        local.get 1
        i32.const 63
        i32.and
        i32.const 128
        i32.or
        i32.store8 offset=13
        local.get 2
        local.get 1
        i32.const 6
        i32.shr_u
        i32.const 192
        i32.or
        i32.store8 offset=12
        local.get 2
        i32.const 12
        i32.add
        local.set 3
        i32.const 2
        local.set 1
      end
      local.get 0
      local.get 1
      call 40
      local.get 0
      i32.load
      local.get 0
      i32.const 8
      i32.add
      local.tee 0
      i32.load
      local.tee 4
      i32.add
      local.get 3
      local.get 1
      call 74
      drop
      local.get 0
      local.get 4
      local.get 1
      i32.add
      i32.store
    end
    local.get 2
    i32.const 16
    i32.add
    global.set 0
    i32.const 0)
  (func (;40;) (type 3) (param i32 i32)
    (local i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 0
          i32.const 4
          i32.add
          i32.load
          local.tee 2
          local.get 0
          i32.load offset=8
          local.tee 3
          i32.sub
          local.get 1
          i32.ge_u
          br_if 0 (;@3;)
          local.get 3
          local.get 1
          i32.add
          local.tee 1
          local.get 3
          i32.lt_u
          br_if 1 (;@2;)
          local.get 2
          i32.const 1
          i32.shl
          local.tee 3
          local.get 1
          local.get 3
          local.get 1
          i32.gt_u
          select
          local.tee 1
          i32.const 8
          local.get 1
          i32.const 8
          i32.gt_u
          select
          local.tee 3
          i32.const 0
          i32.lt_s
          br_if 1 (;@2;)
          block  ;; label = @4
            block  ;; label = @5
              local.get 0
              i32.load
              i32.const 0
              local.get 2
              select
              local.tee 1
              i32.eqz
              br_if 0 (;@5;)
              local.get 2
              local.get 3
              i32.eq
              br_if 1 (;@4;)
              block  ;; label = @6
                local.get 2
                br_if 0 (;@6;)
                local.get 3
                i32.const 1
                call 11
                local.set 1
                br 2 (;@4;)
              end
              local.get 1
              local.get 2
              i32.const 1
              local.get 3
              call 13
              local.set 1
              br 1 (;@4;)
            end
            local.get 3
            i32.const 1
            call 11
            local.set 1
          end
          local.get 1
          i32.eqz
          br_if 2 (;@1;)
          local.get 0
          local.get 1
          i32.store
          local.get 0
          i32.const 4
          i32.add
          local.get 3
          i32.store
        end
        return
      end
      call 53
      unreachable
    end
    local.get 3
    i32.const 1
    call 52
    unreachable)
  (func (;41;) (type 0) (param i32 i32) (result i32)
    (local i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 2
    global.set 0
    local.get 2
    local.get 0
    i32.load
    i32.store offset=4
    local.get 2
    i32.const 8
    i32.add
    i32.const 16
    i32.add
    local.get 1
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    i32.const 8
    i32.add
    i32.const 8
    i32.add
    local.get 1
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 1
    i64.load align=4
    i64.store offset=8
    local.get 2
    i32.const 4
    i32.add
    i32.const 1049076
    local.get 2
    i32.const 8
    i32.add
    call 62
    local.set 1
    local.get 2
    i32.const 32
    i32.add
    global.set 0
    local.get 1)
  (func (;42;) (type 4) (param i32 i32 i32) (result i32)
    (local i32)
    local.get 0
    i32.load
    local.tee 0
    local.get 2
    call 40
    local.get 0
    i32.load
    local.get 0
    i32.const 8
    i32.add
    local.tee 0
    i32.load
    local.tee 3
    i32.add
    local.get 1
    local.get 2
    call 74
    drop
    local.get 0
    local.get 3
    local.get 2
    i32.add
    i32.store
    i32.const 0)
  (func (;43;) (type 11)
    unreachable
    unreachable)
  (func (;44;) (type 3) (param i32 i32))
  (func (;45;) (type 3) (param i32 i32)
    (local i32)
    local.get 0
    local.get 1
    i32.const 0
    i32.load offset=1050828
    local.tee 2
    i32.const 11
    local.get 2
    select
    call_indirect (type 3)
    unreachable
    unreachable)
  (func (;46;) (type 6) (param i32)
    (local i32 i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 1
    global.set 0
    local.get 0
    call 65
    i32.const 1049184
    call 38
    local.set 2
    local.get 0
    call 64
    call 37
    local.set 3
    local.get 1
    i32.const 0
    i32.store offset=4
    local.get 1
    local.get 3
    i32.store
    local.get 1
    i32.const 1049216
    local.get 0
    call 64
    local.get 2
    call 47
    unreachable)
  (func (;47;) (type 2) (param i32 i32 i32 i32)
    (local i32 i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 4
    global.set 0
    i32.const 1
    local.set 5
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            i32.const 0
            i32.load offset=1050848
            i32.const 1
            i32.eq
            br_if 0 (;@4;)
            i32.const 0
            i64.const 4294967297
            i64.store offset=1050848
            br 1 (;@3;)
          end
          i32.const 0
          i32.const 0
          i32.load offset=1050852
          i32.const 1
          i32.add
          local.tee 5
          i32.store offset=1050852
          local.get 5
          i32.const 2
          i32.gt_u
          br_if 1 (;@2;)
        end
        local.get 4
        local.get 3
        i32.store offset=28
        local.get 4
        local.get 2
        i32.store offset=24
        local.get 4
        i32.const 1049100
        i32.store offset=20
        local.get 4
        i32.const 1049100
        i32.store offset=16
        i32.const 0
        i32.load offset=1050832
        local.tee 2
        i32.const -1
        i32.le_s
        br_if 0 (;@2;)
        i32.const 0
        local.get 2
        i32.const 1
        i32.add
        local.tee 2
        i32.store offset=1050832
        block  ;; label = @3
          i32.const 0
          i32.load offset=1050840
          local.tee 3
          i32.eqz
          br_if 0 (;@3;)
          i32.const 0
          i32.load offset=1050836
          local.set 2
          local.get 4
          i32.const 8
          i32.add
          local.get 0
          local.get 1
          i32.load offset=16
          call_indirect (type 3)
          local.get 4
          local.get 4
          i64.load offset=8
          i64.store offset=16
          local.get 2
          local.get 4
          i32.const 16
          i32.add
          local.get 3
          i32.load offset=12
          call_indirect (type 3)
          i32.const 0
          i32.load offset=1050832
          local.set 2
        end
        i32.const 0
        local.get 2
        i32.const -1
        i32.add
        i32.store offset=1050832
        local.get 5
        i32.const 1
        i32.le_u
        br_if 1 (;@1;)
      end
      unreachable
      unreachable
    end
    local.get 0
    local.get 1
    call 50
    unreachable)
  (func (;48;) (type 3) (param i32 i32)
    (local i32 i32 i32 i32 i32)
    global.get 0
    i32.const 64
    i32.sub
    local.tee 2
    global.set 0
    block  ;; label = @1
      local.get 1
      i32.load offset=4
      local.tee 3
      br_if 0 (;@1;)
      local.get 1
      i32.const 4
      i32.add
      local.set 3
      local.get 1
      i32.load
      local.set 4
      local.get 2
      i32.const 0
      i32.store offset=32
      local.get 2
      i64.const 1
      i64.store offset=24
      local.get 2
      local.get 2
      i32.const 24
      i32.add
      i32.store offset=36
      local.get 2
      i32.const 40
      i32.add
      i32.const 16
      i32.add
      local.get 4
      i32.const 16
      i32.add
      i64.load align=4
      i64.store
      local.get 2
      i32.const 40
      i32.add
      i32.const 8
      i32.add
      local.get 4
      i32.const 8
      i32.add
      i64.load align=4
      i64.store
      local.get 2
      local.get 4
      i64.load align=4
      i64.store offset=40
      local.get 2
      i32.const 36
      i32.add
      i32.const 1049076
      local.get 2
      i32.const 40
      i32.add
      call 62
      drop
      local.get 2
      i32.const 8
      i32.add
      i32.const 8
      i32.add
      local.tee 4
      local.get 2
      i32.load offset=32
      i32.store
      local.get 2
      local.get 2
      i64.load offset=24
      i64.store offset=8
      block  ;; label = @2
        local.get 1
        i32.load offset=4
        local.tee 5
        i32.eqz
        br_if 0 (;@2;)
        local.get 1
        i32.const 8
        i32.add
        i32.load
        local.tee 6
        i32.eqz
        br_if 0 (;@2;)
        local.get 5
        local.get 6
        i32.const 1
        call 12
      end
      local.get 3
      local.get 2
      i64.load offset=8
      i64.store align=4
      local.get 3
      i32.const 8
      i32.add
      local.get 4
      i32.load
      i32.store
      local.get 3
      i32.load
      local.set 3
    end
    local.get 1
    i32.const 1
    i32.store offset=4
    local.get 1
    i32.const 12
    i32.add
    i32.load
    local.set 4
    local.get 1
    i32.const 8
    i32.add
    local.tee 1
    i32.load
    local.set 5
    local.get 1
    i64.const 0
    i64.store align=4
    block  ;; label = @1
      i32.const 12
      i32.const 4
      call 11
      local.tee 1
      br_if 0 (;@1;)
      i32.const 12
      i32.const 4
      call 52
      unreachable
    end
    local.get 1
    local.get 4
    i32.store offset=8
    local.get 1
    local.get 5
    i32.store offset=4
    local.get 1
    local.get 3
    i32.store
    local.get 0
    i32.const 1049236
    i32.store offset=4
    local.get 0
    local.get 1
    i32.store
    local.get 2
    i32.const 64
    i32.add
    global.set 0)
  (func (;49;) (type 3) (param i32 i32)
    (local i32 i32 i32 i32)
    global.get 0
    i32.const 64
    i32.sub
    local.tee 2
    global.set 0
    local.get 1
    i32.const 4
    i32.add
    local.set 3
    block  ;; label = @1
      local.get 1
      i32.load offset=4
      br_if 0 (;@1;)
      local.get 1
      i32.load
      local.set 4
      local.get 2
      i32.const 0
      i32.store offset=32
      local.get 2
      i64.const 1
      i64.store offset=24
      local.get 2
      local.get 2
      i32.const 24
      i32.add
      i32.store offset=36
      local.get 2
      i32.const 40
      i32.add
      i32.const 16
      i32.add
      local.get 4
      i32.const 16
      i32.add
      i64.load align=4
      i64.store
      local.get 2
      i32.const 40
      i32.add
      i32.const 8
      i32.add
      local.get 4
      i32.const 8
      i32.add
      i64.load align=4
      i64.store
      local.get 2
      local.get 4
      i64.load align=4
      i64.store offset=40
      local.get 2
      i32.const 36
      i32.add
      i32.const 1049076
      local.get 2
      i32.const 40
      i32.add
      call 62
      drop
      local.get 2
      i32.const 8
      i32.add
      i32.const 8
      i32.add
      local.tee 4
      local.get 2
      i32.load offset=32
      i32.store
      local.get 2
      local.get 2
      i64.load offset=24
      i64.store offset=8
      block  ;; label = @2
        local.get 1
        i32.load offset=4
        local.tee 5
        i32.eqz
        br_if 0 (;@2;)
        local.get 1
        i32.const 8
        i32.add
        i32.load
        local.tee 1
        i32.eqz
        br_if 0 (;@2;)
        local.get 5
        local.get 1
        i32.const 1
        call 12
      end
      local.get 3
      local.get 2
      i64.load offset=8
      i64.store align=4
      local.get 3
      i32.const 8
      i32.add
      local.get 4
      i32.load
      i32.store
    end
    local.get 0
    i32.const 1049236
    i32.store offset=4
    local.get 0
    local.get 3
    i32.store
    local.get 2
    i32.const 64
    i32.add
    global.set 0)
  (func (;50;) (type 3) (param i32 i32)
    (local i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 2
    global.set 0
    local.get 2
    local.get 1
    i32.store offset=12
    local.get 2
    local.get 0
    i32.store offset=8
    local.get 2
    i32.const 8
    i32.add
    call 51
    drop
    unreachable
    unreachable)
  (func (;51;) (type 1) (param i32) (result i32)
    unreachable
    unreachable)
  (func (;52;) (type 3) (param i32 i32)
    local.get 0
    local.get 1
    call 45
    unreachable)
  (func (;53;) (type 11)
    i32.const 1049275
    i32.const 17
    i32.const 1049292
    call 57
    unreachable)
  (func (;54;) (type 0) (param i32 i32) (result i32)
    local.get 0
    i32.load
    drop
    loop (result i32)  ;; label = @1
      br 0 (;@1;)
    end)
  (func (;55;) (type 6) (param i32))
  (func (;56;) (type 9) (param i32 i32 i32)
    (local i32)
    global.get 0
    i32.const 48
    i32.sub
    local.tee 3
    global.set 0
    local.get 3
    local.get 1
    i32.store offset=4
    local.get 3
    local.get 0
    i32.store
    local.get 3
    i32.const 28
    i32.add
    i32.const 2
    i32.store
    local.get 3
    i32.const 44
    i32.add
    i32.const 22
    i32.store
    local.get 3
    i64.const 2
    i64.store offset=12 align=4
    local.get 3
    i32.const 1049376
    i32.store offset=8
    local.get 3
    i32.const 22
    i32.store offset=36
    local.get 3
    local.get 3
    i32.const 32
    i32.add
    i32.store offset=24
    local.get 3
    local.get 3
    i32.store offset=40
    local.get 3
    local.get 3
    i32.const 4
    i32.add
    i32.store offset=32
    local.get 3
    i32.const 8
    i32.add
    local.get 2
    call 60
    unreachable)
  (func (;57;) (type 9) (param i32 i32 i32)
    (local i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 3
    global.set 0
    local.get 3
    i32.const 20
    i32.add
    i32.const 0
    i32.store
    local.get 3
    i32.const 1049308
    i32.store offset=16
    local.get 3
    i64.const 1
    i64.store offset=4 align=4
    local.get 3
    local.get 1
    i32.store offset=28
    local.get 3
    local.get 0
    i32.store offset=24
    local.get 3
    local.get 3
    i32.const 24
    i32.add
    i32.store
    local.get 3
    local.get 2
    call 60
    unreachable)
  (func (;58;) (type 9) (param i32 i32 i32)
    (local i32)
    global.get 0
    i32.const 48
    i32.sub
    local.tee 3
    global.set 0
    local.get 3
    local.get 1
    i32.store offset=4
    local.get 3
    local.get 0
    i32.store
    local.get 3
    i32.const 28
    i32.add
    i32.const 2
    i32.store
    local.get 3
    i32.const 44
    i32.add
    i32.const 22
    i32.store
    local.get 3
    i64.const 2
    i64.store offset=12 align=4
    local.get 3
    i32.const 1049728
    i32.store offset=8
    local.get 3
    i32.const 22
    i32.store offset=36
    local.get 3
    local.get 3
    i32.const 32
    i32.add
    i32.store offset=24
    local.get 3
    local.get 3
    i32.const 4
    i32.add
    i32.store offset=40
    local.get 3
    local.get 3
    i32.store offset=32
    local.get 3
    i32.const 8
    i32.add
    local.get 2
    call 60
    unreachable)
  (func (;59;) (type 9) (param i32 i32 i32)
    (local i32)
    global.get 0
    i32.const 48
    i32.sub
    local.tee 3
    global.set 0
    local.get 3
    local.get 1
    i32.store offset=4
    local.get 3
    local.get 0
    i32.store
    local.get 3
    i32.const 28
    i32.add
    i32.const 2
    i32.store
    local.get 3
    i32.const 44
    i32.add
    i32.const 22
    i32.store
    local.get 3
    i64.const 2
    i64.store offset=12 align=4
    local.get 3
    i32.const 1049780
    i32.store offset=8
    local.get 3
    i32.const 22
    i32.store offset=36
    local.get 3
    local.get 3
    i32.const 32
    i32.add
    i32.store offset=24
    local.get 3
    local.get 3
    i32.const 4
    i32.add
    i32.store offset=40
    local.get 3
    local.get 3
    i32.store offset=32
    local.get 3
    i32.const 8
    i32.add
    local.get 2
    call 60
    unreachable)
  (func (;60;) (type 3) (param i32 i32)
    (local i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 2
    global.set 0
    local.get 2
    local.get 1
    i32.store offset=12
    local.get 2
    local.get 0
    i32.store offset=8
    local.get 2
    i32.const 1049308
    i32.store offset=4
    local.get 2
    i32.const 1049308
    i32.store
    local.get 2
    call 46
    unreachable)
  (func (;61;) (type 0) (param i32 i32) (result i32)
    local.get 0
    i64.load32_u
    i32.const 1
    local.get 1
    call 72)
  (func (;62;) (type 4) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    i32.const 48
    i32.sub
    local.tee 3
    global.set 0
    local.get 3
    i32.const 36
    i32.add
    local.get 1
    i32.store
    local.get 3
    i32.const 3
    i32.store8 offset=40
    local.get 3
    i64.const 137438953472
    i64.store offset=8
    local.get 3
    local.get 0
    i32.store offset=32
    local.get 3
    i32.const 0
    i32.store offset=24
    local.get 3
    i32.const 0
    i32.store offset=16
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              local.get 2
              i32.load offset=8
              local.tee 4
              i32.eqz
              br_if 0 (;@5;)
              local.get 2
              i32.load
              local.set 5
              local.get 2
              i32.load offset=4
              local.tee 6
              local.get 2
              i32.const 12
              i32.add
              i32.load
              local.tee 7
              local.get 7
              local.get 6
              i32.gt_u
              select
              local.tee 7
              i32.eqz
              br_if 1 (;@4;)
              local.get 2
              i32.const 20
              i32.add
              i32.load
              local.set 8
              local.get 2
              i32.load offset=16
              local.set 9
              local.get 0
              local.get 5
              i32.load
              local.get 5
              i32.load offset=4
              local.get 1
              i32.load offset=12
              call_indirect (type 4)
              br_if 3 (;@2;)
              local.get 5
              i32.const 12
              i32.add
              local.set 2
              i32.const 0
              local.set 10
              block  ;; label = @6
                block  ;; label = @7
                  loop  ;; label = @8
                    local.get 3
                    local.get 4
                    i32.const 4
                    i32.add
                    i32.load
                    i32.store offset=12
                    local.get 3
                    local.get 4
                    i32.const 28
                    i32.add
                    i32.load8_u
                    i32.store8 offset=40
                    local.get 3
                    local.get 4
                    i32.const 8
                    i32.add
                    i32.load
                    i32.store offset=8
                    local.get 4
                    i32.const 24
                    i32.add
                    i32.load
                    local.set 0
                    i32.const 0
                    local.set 1
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          local.get 4
                          i32.const 20
                          i32.add
                          i32.load
                          br_table 1 (;@10;) 0 (;@11;) 2 (;@9;) 1 (;@10;)
                        end
                        local.get 0
                        local.get 8
                        i32.ge_u
                        br_if 3 (;@7;)
                        local.get 0
                        i32.const 3
                        i32.shl
                        local.set 11
                        i32.const 0
                        local.set 1
                        local.get 9
                        local.get 11
                        i32.add
                        local.tee 11
                        i32.load offset=4
                        i32.const 23
                        i32.ne
                        br_if 1 (;@9;)
                        local.get 11
                        i32.load
                        i32.load
                        local.set 0
                      end
                      i32.const 1
                      local.set 1
                    end
                    local.get 3
                    local.get 0
                    i32.store offset=20
                    local.get 3
                    local.get 1
                    i32.store offset=16
                    local.get 4
                    i32.const 16
                    i32.add
                    i32.load
                    local.set 0
                    i32.const 0
                    local.set 1
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          local.get 4
                          i32.const 12
                          i32.add
                          i32.load
                          br_table 1 (;@10;) 0 (;@11;) 2 (;@9;) 1 (;@10;)
                        end
                        local.get 0
                        local.get 8
                        i32.ge_u
                        br_if 4 (;@6;)
                        local.get 0
                        i32.const 3
                        i32.shl
                        local.set 11
                        local.get 9
                        local.get 11
                        i32.add
                        local.tee 11
                        i32.load offset=4
                        i32.const 23
                        i32.ne
                        br_if 1 (;@9;)
                        local.get 11
                        i32.load
                        i32.load
                        local.set 0
                      end
                      i32.const 1
                      local.set 1
                    end
                    local.get 3
                    local.get 0
                    i32.store offset=28
                    local.get 3
                    local.get 1
                    i32.store offset=24
                    block  ;; label = @9
                      local.get 4
                      i32.load
                      local.tee 0
                      local.get 8
                      i32.ge_u
                      br_if 0 (;@9;)
                      local.get 9
                      local.get 0
                      i32.const 3
                      i32.shl
                      i32.add
                      local.tee 0
                      i32.load
                      local.get 3
                      i32.const 8
                      i32.add
                      local.get 0
                      i32.load offset=4
                      call_indirect (type 0)
                      br_if 7 (;@2;)
                      local.get 10
                      i32.const 1
                      i32.add
                      local.tee 10
                      local.get 7
                      i32.ge_u
                      br_if 6 (;@3;)
                      local.get 4
                      i32.const 32
                      i32.add
                      local.set 4
                      local.get 2
                      i32.const -4
                      i32.add
                      local.set 0
                      local.get 2
                      i32.load
                      local.set 1
                      local.get 2
                      i32.const 8
                      i32.add
                      local.set 2
                      local.get 3
                      i32.load offset=32
                      local.get 0
                      i32.load
                      local.get 1
                      local.get 3
                      i32.load offset=36
                      i32.load offset=12
                      call_indirect (type 4)
                      i32.eqz
                      br_if 1 (;@8;)
                      br 7 (;@2;)
                    end
                  end
                  local.get 0
                  local.get 8
                  i32.const 1049656
                  call 56
                  unreachable
                end
                local.get 0
                local.get 8
                i32.const 1049672
                call 56
                unreachable
              end
              local.get 0
              local.get 8
              i32.const 1049672
              call 56
              unreachable
            end
            local.get 2
            i32.load
            local.set 5
            local.get 2
            i32.load offset=4
            local.tee 6
            local.get 2
            i32.const 20
            i32.add
            i32.load
            local.tee 4
            local.get 4
            local.get 6
            i32.gt_u
            select
            local.tee 7
            i32.eqz
            br_if 0 (;@4;)
            local.get 2
            i32.load offset=16
            local.set 4
            local.get 0
            local.get 5
            i32.load
            local.get 5
            i32.load offset=4
            local.get 1
            i32.load offset=12
            call_indirect (type 4)
            br_if 2 (;@2;)
            local.get 5
            i32.const 12
            i32.add
            local.set 2
            i32.const 0
            local.set 0
            loop  ;; label = @5
              local.get 4
              i32.load
              local.get 3
              i32.const 8
              i32.add
              local.get 4
              i32.const 4
              i32.add
              i32.load
              call_indirect (type 0)
              br_if 3 (;@2;)
              local.get 0
              i32.const 1
              i32.add
              local.tee 0
              local.get 7
              i32.ge_u
              br_if 2 (;@3;)
              local.get 4
              i32.const 8
              i32.add
              local.set 4
              local.get 2
              i32.const -4
              i32.add
              local.set 1
              local.get 2
              i32.load
              local.set 10
              local.get 2
              i32.const 8
              i32.add
              local.set 2
              local.get 3
              i32.load offset=32
              local.get 1
              i32.load
              local.get 10
              local.get 3
              i32.load offset=36
              i32.load offset=12
              call_indirect (type 4)
              i32.eqz
              br_if 0 (;@5;)
              br 3 (;@2;)
            end
          end
          i32.const 0
          local.set 7
        end
        block  ;; label = @3
          local.get 6
          local.get 7
          i32.le_u
          br_if 0 (;@3;)
          local.get 3
          i32.load offset=32
          local.get 5
          local.get 7
          i32.const 3
          i32.shl
          i32.add
          local.tee 4
          i32.load
          local.get 4
          i32.load offset=4
          local.get 3
          i32.load offset=36
          i32.load offset=12
          call_indirect (type 4)
          br_if 1 (;@2;)
        end
        i32.const 0
        local.set 4
        br 1 (;@1;)
      end
      i32.const 1
      local.set 4
    end
    local.get 3
    i32.const 48
    i32.add
    global.set 0
    local.get 4)
  (func (;63;) (type 13) (param i32) (result i64)
    i64.const -8947299319579089149)
  (func (;64;) (type 1) (param i32) (result i32)
    local.get 0
    i32.load offset=8)
  (func (;65;) (type 1) (param i32) (result i32)
    local.get 0
    i32.load offset=12)
  (func (;66;) (type 0) (param i32 i32) (result i32)
    (local i32 i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 2
    global.set 0
    local.get 1
    i32.const 28
    i32.add
    i32.load
    local.set 3
    local.get 1
    i32.load offset=24
    local.set 1
    local.get 2
    i32.const 8
    i32.add
    i32.const 16
    i32.add
    local.get 0
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    i32.const 8
    i32.add
    i32.const 8
    i32.add
    local.get 0
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 0
    i64.load align=4
    i64.store offset=8
    local.get 1
    local.get 3
    local.get 2
    i32.const 8
    i32.add
    call 62
    local.set 0
    local.get 2
    i32.const 32
    i32.add
    global.set 0
    local.get 0)
  (func (;67;) (type 14) (param i32 i32 i32 i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.eqz
        br_if 0 (;@2;)
        i32.const 43
        i32.const 1114112
        local.get 0
        i32.load
        local.tee 6
        i32.const 1
        i32.and
        local.tee 1
        select
        local.set 7
        local.get 1
        local.get 5
        i32.add
        local.set 8
        br 1 (;@1;)
      end
      local.get 5
      i32.const 1
      i32.add
      local.set 8
      local.get 0
      i32.load
      local.set 6
      i32.const 45
      local.set 7
    end
    block  ;; label = @1
      block  ;; label = @2
        local.get 6
        i32.const 4
        i32.and
        br_if 0 (;@2;)
        i32.const 0
        local.set 2
        br 1 (;@1;)
      end
      i32.const 0
      local.set 9
      block  ;; label = @2
        local.get 3
        i32.eqz
        br_if 0 (;@2;)
        local.get 3
        local.set 10
        local.get 2
        local.set 1
        loop  ;; label = @3
          local.get 9
          local.get 1
          i32.load8_u
          i32.const 192
          i32.and
          i32.const 128
          i32.eq
          i32.add
          local.set 9
          local.get 1
          i32.const 1
          i32.add
          local.set 1
          local.get 10
          i32.const -1
          i32.add
          local.tee 10
          br_if 0 (;@3;)
        end
      end
      local.get 8
      local.get 3
      i32.add
      local.get 9
      i32.sub
      local.set 8
    end
    i32.const 1
    local.set 1
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.load offset=8
        i32.const 1
        i32.eq
        br_if 0 (;@2;)
        local.get 0
        local.get 7
        local.get 2
        local.get 3
        call 68
        br_if 1 (;@1;)
        local.get 0
        i32.load offset=24
        local.get 4
        local.get 5
        local.get 0
        i32.const 28
        i32.add
        i32.load
        i32.load offset=12
        call_indirect (type 4)
        local.set 1
        br 1 (;@1;)
      end
      block  ;; label = @2
        local.get 0
        i32.const 12
        i32.add
        i32.load
        local.tee 9
        local.get 8
        i32.gt_u
        br_if 0 (;@2;)
        local.get 0
        local.get 7
        local.get 2
        local.get 3
        call 68
        br_if 1 (;@1;)
        local.get 0
        i32.load offset=24
        local.get 4
        local.get 5
        local.get 0
        i32.const 28
        i32.add
        i32.load
        i32.load offset=12
        call_indirect (type 4)
        return
      end
      block  ;; label = @2
        block  ;; label = @3
          local.get 6
          i32.const 8
          i32.and
          br_if 0 (;@3;)
          i32.const 0
          local.set 1
          local.get 9
          local.get 8
          i32.sub
          local.tee 9
          local.set 8
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                i32.const 1
                local.get 0
                i32.load8_u offset=32
                local.tee 10
                local.get 10
                i32.const 3
                i32.eq
                select
                br_table 2 (;@4;) 1 (;@5;) 0 (;@6;) 1 (;@5;) 2 (;@4;)
              end
              local.get 9
              i32.const 1
              i32.shr_u
              local.set 1
              local.get 9
              i32.const 1
              i32.add
              i32.const 1
              i32.shr_u
              local.set 8
              br 1 (;@4;)
            end
            i32.const 0
            local.set 8
            local.get 9
            local.set 1
          end
          local.get 1
          i32.const 1
          i32.add
          local.set 1
          loop  ;; label = @4
            local.get 1
            i32.const -1
            i32.add
            local.tee 1
            i32.eqz
            br_if 2 (;@2;)
            local.get 0
            i32.load offset=24
            local.get 0
            i32.load offset=4
            local.get 0
            i32.load offset=28
            i32.load offset=16
            call_indirect (type 0)
            i32.eqz
            br_if 0 (;@4;)
          end
          i32.const 1
          return
        end
        local.get 0
        i32.load offset=4
        local.set 6
        local.get 0
        i32.const 48
        i32.store offset=4
        local.get 0
        i32.load8_u offset=32
        local.set 11
        i32.const 1
        local.set 1
        local.get 0
        i32.const 1
        i32.store8 offset=32
        local.get 0
        local.get 7
        local.get 2
        local.get 3
        call 68
        br_if 1 (;@1;)
        i32.const 0
        local.set 1
        local.get 9
        local.get 8
        i32.sub
        local.tee 10
        local.set 3
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              i32.const 1
              local.get 0
              i32.load8_u offset=32
              local.tee 9
              local.get 9
              i32.const 3
              i32.eq
              select
              br_table 2 (;@3;) 1 (;@4;) 0 (;@5;) 1 (;@4;) 2 (;@3;)
            end
            local.get 10
            i32.const 1
            i32.shr_u
            local.set 1
            local.get 10
            i32.const 1
            i32.add
            i32.const 1
            i32.shr_u
            local.set 3
            br 1 (;@3;)
          end
          i32.const 0
          local.set 3
          local.get 10
          local.set 1
        end
        local.get 1
        i32.const 1
        i32.add
        local.set 1
        block  ;; label = @3
          loop  ;; label = @4
            local.get 1
            i32.const -1
            i32.add
            local.tee 1
            i32.eqz
            br_if 1 (;@3;)
            local.get 0
            i32.load offset=24
            local.get 0
            i32.load offset=4
            local.get 0
            i32.load offset=28
            i32.load offset=16
            call_indirect (type 0)
            i32.eqz
            br_if 0 (;@4;)
          end
          i32.const 1
          return
        end
        local.get 0
        i32.load offset=4
        local.set 10
        i32.const 1
        local.set 1
        local.get 0
        i32.load offset=24
        local.get 4
        local.get 5
        local.get 0
        i32.load offset=28
        i32.load offset=12
        call_indirect (type 4)
        br_if 1 (;@1;)
        local.get 3
        i32.const 1
        i32.add
        local.set 9
        local.get 0
        i32.load offset=28
        local.set 3
        local.get 0
        i32.load offset=24
        local.set 2
        block  ;; label = @3
          loop  ;; label = @4
            local.get 9
            i32.const -1
            i32.add
            local.tee 9
            i32.eqz
            br_if 1 (;@3;)
            i32.const 1
            local.set 1
            local.get 2
            local.get 10
            local.get 3
            i32.load offset=16
            call_indirect (type 0)
            br_if 3 (;@1;)
            br 0 (;@4;)
          end
        end
        local.get 0
        local.get 11
        i32.store8 offset=32
        local.get 0
        local.get 6
        i32.store offset=4
        i32.const 0
        return
      end
      local.get 0
      i32.load offset=4
      local.set 10
      i32.const 1
      local.set 1
      local.get 0
      local.get 7
      local.get 2
      local.get 3
      call 68
      br_if 0 (;@1;)
      local.get 0
      i32.load offset=24
      local.get 4
      local.get 5
      local.get 0
      i32.load offset=28
      i32.load offset=12
      call_indirect (type 4)
      br_if 0 (;@1;)
      local.get 8
      i32.const 1
      i32.add
      local.set 9
      local.get 0
      i32.load offset=28
      local.set 3
      local.get 0
      i32.load offset=24
      local.set 0
      loop  ;; label = @2
        block  ;; label = @3
          local.get 9
          i32.const -1
          i32.add
          local.tee 9
          br_if 0 (;@3;)
          i32.const 0
          return
        end
        i32.const 1
        local.set 1
        local.get 0
        local.get 10
        local.get 3
        i32.load offset=16
        call_indirect (type 0)
        i32.eqz
        br_if 0 (;@2;)
      end
    end
    local.get 1)
  (func (;68;) (type 10) (param i32 i32 i32 i32) (result i32)
    (local i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.const 1114112
        i32.eq
        br_if 0 (;@2;)
        i32.const 1
        local.set 4
        local.get 0
        i32.load offset=24
        local.get 1
        local.get 0
        i32.const 28
        i32.add
        i32.load
        i32.load offset=16
        call_indirect (type 0)
        br_if 1 (;@1;)
      end
      block  ;; label = @2
        local.get 2
        br_if 0 (;@2;)
        i32.const 0
        return
      end
      local.get 0
      i32.load offset=24
      local.get 2
      local.get 3
      local.get 0
      i32.const 28
      i32.add
      i32.load
      i32.load offset=12
      call_indirect (type 4)
      local.set 4
    end
    local.get 4)
  (func (;69;) (type 1) (param i32) (result i32)
    local.get 0
    i32.load8_u
    i32.const 16
    i32.and
    i32.const 4
    i32.shr_u)
  (func (;70;) (type 1) (param i32) (result i32)
    local.get 0
    i32.load8_u
    i32.const 32
    i32.and
    i32.const 5
    i32.shr_u)
  (func (;71;) (type 0) (param i32 i32) (result i32)
    (local i32 i32 i32)
    global.get 0
    i32.const 128
    i32.sub
    local.tee 2
    global.set 0
    local.get 0
    i32.load
    local.set 3
    i32.const 0
    local.set 0
    loop  ;; label = @1
      local.get 2
      local.get 0
      i32.add
      i32.const 127
      i32.add
      local.get 3
      i32.const 15
      i32.and
      local.tee 4
      i32.const 48
      i32.or
      local.get 4
      i32.const 87
      i32.add
      local.get 4
      i32.const 10
      i32.lt_u
      select
      i32.store8
      local.get 0
      i32.const -1
      i32.add
      local.set 0
      local.get 3
      i32.const 4
      i32.shr_u
      local.tee 3
      br_if 0 (;@1;)
    end
    block  ;; label = @1
      local.get 0
      i32.const 128
      i32.add
      local.tee 3
      i32.const 129
      i32.lt_u
      br_if 0 (;@1;)
      local.get 3
      i32.const 128
      i32.const 1049416
      call 59
      unreachable
    end
    local.get 1
    i32.const 1
    i32.const 1049432
    i32.const 2
    local.get 2
    local.get 0
    i32.add
    i32.const 128
    i32.add
    i32.const 0
    local.get 0
    i32.sub
    call 67
    local.set 0
    local.get 2
    i32.const 128
    i32.add
    global.set 0
    local.get 0)
  (func (;72;) (type 15) (param i64 i32 i32) (result i32)
    (local i32 i32 i64 i32 i32 i32)
    global.get 0
    i32.const 48
    i32.sub
    local.tee 3
    global.set 0
    i32.const 39
    local.set 4
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i64.const 10000
        i64.ge_u
        br_if 0 (;@2;)
        local.get 0
        local.set 5
        br 1 (;@1;)
      end
      i32.const 39
      local.set 4
      loop  ;; label = @2
        local.get 3
        i32.const 9
        i32.add
        local.get 4
        i32.add
        local.tee 6
        i32.const -4
        i32.add
        local.get 0
        local.get 0
        i64.const 10000
        i64.div_u
        local.tee 5
        i64.const 10000
        i64.mul
        i64.sub
        i32.wrap_i64
        local.tee 7
        i32.const 65535
        i32.and
        i32.const 100
        i32.div_u
        local.tee 8
        i32.const 1
        i32.shl
        i32.const 1049434
        i32.add
        i32.load16_u align=1
        i32.store16 align=1
        local.get 6
        i32.const -2
        i32.add
        local.get 7
        local.get 8
        i32.const 100
        i32.mul
        i32.sub
        i32.const 65535
        i32.and
        i32.const 1
        i32.shl
        i32.const 1049434
        i32.add
        i32.load16_u align=1
        i32.store16 align=1
        local.get 4
        i32.const -4
        i32.add
        local.set 4
        local.get 0
        i64.const 99999999
        i64.gt_u
        local.set 6
        local.get 5
        local.set 0
        local.get 6
        br_if 0 (;@2;)
      end
    end
    block  ;; label = @1
      local.get 5
      i32.wrap_i64
      local.tee 6
      i32.const 99
      i32.le_s
      br_if 0 (;@1;)
      local.get 3
      i32.const 9
      i32.add
      local.get 4
      i32.const -2
      i32.add
      local.tee 4
      i32.add
      local.get 5
      i32.wrap_i64
      local.tee 6
      local.get 6
      i32.const 65535
      i32.and
      i32.const 100
      i32.div_u
      local.tee 6
      i32.const 100
      i32.mul
      i32.sub
      i32.const 65535
      i32.and
      i32.const 1
      i32.shl
      i32.const 1049434
      i32.add
      i32.load16_u align=1
      i32.store16 align=1
    end
    block  ;; label = @1
      block  ;; label = @2
        local.get 6
        i32.const 10
        i32.lt_s
        br_if 0 (;@2;)
        local.get 3
        i32.const 9
        i32.add
        local.get 4
        i32.const -2
        i32.add
        local.tee 4
        i32.add
        local.get 6
        i32.const 1
        i32.shl
        i32.const 1049434
        i32.add
        i32.load16_u align=1
        i32.store16 align=1
        br 1 (;@1;)
      end
      local.get 3
      i32.const 9
      i32.add
      local.get 4
      i32.const -1
      i32.add
      local.tee 4
      i32.add
      local.get 6
      i32.const 48
      i32.add
      i32.store8
    end
    local.get 2
    local.get 1
    i32.const 1049308
    i32.const 0
    local.get 3
    i32.const 9
    i32.add
    local.get 4
    i32.add
    i32.const 39
    local.get 4
    i32.sub
    call 67
    local.set 4
    local.get 3
    i32.const 48
    i32.add
    global.set 0
    local.get 4)
  (func (;73;) (type 0) (param i32 i32) (result i32)
    (local i32 i32 i32)
    global.get 0
    i32.const 128
    i32.sub
    local.tee 2
    global.set 0
    local.get 0
    i32.load
    local.set 3
    i32.const 0
    local.set 0
    loop  ;; label = @1
      local.get 2
      local.get 0
      i32.add
      i32.const 127
      i32.add
      local.get 3
      i32.const 15
      i32.and
      local.tee 4
      i32.const 48
      i32.or
      local.get 4
      i32.const 55
      i32.add
      local.get 4
      i32.const 10
      i32.lt_u
      select
      i32.store8
      local.get 0
      i32.const -1
      i32.add
      local.set 0
      local.get 3
      i32.const 4
      i32.shr_u
      local.tee 3
      br_if 0 (;@1;)
    end
    block  ;; label = @1
      local.get 0
      i32.const 128
      i32.add
      local.tee 3
      i32.const 129
      i32.lt_u
      br_if 0 (;@1;)
      local.get 3
      i32.const 128
      i32.const 1049416
      call 59
      unreachable
    end
    local.get 1
    i32.const 1
    i32.const 1049432
    i32.const 2
    local.get 2
    local.get 0
    i32.add
    i32.const 128
    i32.add
    i32.const 0
    local.get 0
    i32.sub
    call 67
    local.set 0
    local.get 2
    i32.const 128
    i32.add
    global.set 0
    local.get 0)
  (func (;74;) (type 4) (param i32 i32 i32) (result i32)
    (local i32)
    block  ;; label = @1
      local.get 2
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      local.set 3
      loop  ;; label = @2
        local.get 3
        local.get 1
        i32.load8_u
        i32.store8
        local.get 1
        i32.const 1
        i32.add
        local.set 1
        local.get 3
        i32.const 1
        i32.add
        local.set 3
        local.get 2
        i32.const -1
        i32.add
        local.tee 2
        br_if 0 (;@2;)
      end
    end
    local.get 0)
  (table (;0;) 26 26 funcref)
  (memory (;0;) 17)
  (global (;0;) (mut i32) (i32.const 1048576))
  (global (;1;) i32 (i32.const 1050856))
  (global (;2;) i32 (i32.const 1050856))
  (export "memory" (memory 0))
  (export "init_c1" (func 8))
  (export "c1.call_c2" (func 9))
  (export "c2.check_receive_context" (func 10))
  (export "init_c2" (func 8))
  (export "__data_end" (global 1))
  (export "__heap_base" (global 2))
  (elem (;0;) (i32.const 1) func 66 7 20 25 26 27 24 21 22 23 44 34 42 39 41 32 36 48 49 35 33 61 54 55 63)
  (data (;0;) (i32.const 1048576) "assertion failed: `(left == right)`\0a  left: ``,\0a right: ``: \00\00\10\00-\00\00\00-\00\10\00\0c\00\00\009\00\10\00\03\00\00\00destination and source slices have different lengthsT\00\10\004\00\00\00/home/mrapoport/.rustup/toolchains/1.45.2-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/libcore/macros/mod.rs\00\00\00\90\00\10\00q\00\00\00\10\00\00\00\09\00\00\00/home/mrapoport/.cargo/git/checkouts/concordium-rust-smart-contracts-05bbbb354c171ad2/c2cc3b8/concordium-contracts-common/src/impls.rs\00\00\14\01\10\00\86\00\00\00\9d\02\00\00*\00\00\00\03\00\00\00\00\00\00\00\01\00\00\00\04\00\00\00\05\00\00\00\06\00\00\00\07\00\00\00\00\00\00\00\01\00\00\00\04\00\00\00\05\00\00\00\06\00\00\00\07\00\00\00\04\00\00\00\04\00\00\00\08\00\00\00\09\00\00\00\0a\00\00\00\0c\00\00\00\04\00\00\00\04\00\00\00\0d\00\00\00\0e\00\00\00\0f\00\00\00\0c\00\00\00\00\00\00\00\01\00\00\00\10\00\00\00called `Option::unwrap()` on a `None` valuesrc/libstd/panicking.rs\00\00G\02\10\00\17\00\00\00\82\01\00\00\0f\00\00\00G\02\10\00\17\00\00\00\83\01\00\00\0f\00\00\00\11\00\00\00\10\00\00\00\04\00\00\00\12\00\00\00\13\00\00\00\14\00\00\00\0c\00\00\00\04\00\00\00\15\00\00\00src/liballoc/raw_vec.rscapacity overflow\a4\02\10\00\17\00\00\00D\02\00\00\05\00\00\00\18\00\00\00\00\00\00\00\01\00\00\00\19\00\00\00index out of bounds: the len is  but the index is \00\00\ec\02\10\00 \00\00\00\0c\03\10\00\12\00\00\00src/libcore/fmt/num.rs\00\000\03\10\00\16\00\00\00T\00\00\00\14\00\00\000x00010203040506070809101112131415161718192021222324252627282930313233343536373839404142434445464748495051525354555657585960616263646566676869707172737475767778798081828384858687888990919293949596979899src/libcore/fmt/mod.rs\22\04\10\00\16\00\00\00S\04\00\00\11\00\00\00\22\04\10\00\16\00\00\00]\04\00\00$\00\00\00index  out of range for slice of length X\04\10\00\06\00\00\00^\04\10\00\22\00\00\00slice index starts at  but ends at \00\90\04\10\00\16\00\00\00\a6\04\10\00\0d\00\00\00"))
