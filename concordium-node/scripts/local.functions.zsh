#####
# Define an export for CONCORDIUM_P2P_DIR pointing to
# the p2p-client checked out dir on your disk, and
# include this file in your .zshrc or the like.
# 
# For overrides such as feature gates et all, use the
# environment variables CONCORDIUM_P2P_EXTRA_ARGS
#
# Then use the functions below to easily startup a
# local environment of nodes.
#
# Example (one bootstrapper, and two nodes)
# testnet_bootstrap 1
# testnet_node 1 1
#
# For further examples simply use the functions without
# any arguments.
#
# When included by zsh this file also exports local
# LD_LIBRARY_PATH overrides.
#
# Notes NixOS : Everything is wrapped in a nix-shell
# when the functions are used on a NixOS distro.
#
#####

if [[ "$OSTYPE" == "darwin"* ]]; then
  CONCORDIUM_GHC_PLATFORM="osx"
else
  CONCORDIUM_GHC_PLATFORM="linux"
fi

if (( ${+NIX_PATH} )); then
  NIXOS=1
  if (( ${+CONCORDIUM_P2P_EXTRA_ARGS} )); then
    CONCORDIUM_P2P_EXTRA_ARGS="$CONCORDIUM_P2P_EXTRA_ARGS --features=static"
  else
    CONCORDIUM_P2P_EXTRA_ARGS="--features=static"
  fi
else
  NIXOS=0
fi

export CONCORDIUM_GHC_VERSION=$(stack ghc -- --version --short | awk '{ print $NF }')

if (( ${+LD_LIBRARY_PATH} )); then
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib:~/.stack/programs/x86_64-$CONCORDIUM_GHC_PLATFORM/ghc-tinfo6-$CONCORDIUM_GHC_VERSION/lib/ghc-$CONCORDIUM_GHC_VERSION/rts
else
  export LD_LIBRARY_PATH=/usr/local/lib:~/.stack/programs/x86_64-$CONCORDIUM_GHC_PLATFORM/ghc-tinfo6-$CONCORDIUM_GHC_VERSION/lib/ghc-$CONCORDIUM_GHC_VERSION/rts
fi

NIGHTLY_FMT_VERSION="nightly-2019-07-25"

#####
# testnet_bootstrap and testnet_node can be run with
# valgrind profiling by passing the name of the desired
# tool (callgrind / massif / memcheck etc.) or "none"
# for no profiling
#####

#####
# Start up a testnet bootstrapper of instance id 1 and no profiling.
#
# testnet_bootstrap 1 none
#
# the build determines the run mode (debug/release)
# (if running with profiling the built binary is used, but otherwise it's
# rebuilt with cargo every run)
#
#####
function testnet_bootstrap() {
  if (( $# < 2 ))
    then
    echo "Usage: testnet_bootstrap instanceid profiling"
    return 1
  fi
  if [ "$2" != 'none' ];
    then
      profiling="valgrind --tool="$2" "
      binary="./target/debug/p2p_bootstrapper-cli"
    else
      profiling=""
      binary="cargo run $CONCORDIUM_P2P_EXTRA_ARGS --bin p2p_bootstrapper-cli --"
  fi
  bootstrap_id=$1; shift
  shift
  (
    cmd="${profiling}\
       $binary \
      --listen-port $((10900+$bootstrap_id)) \
      --id $((9900000000000000+$bootstrap_id))"
    if [ $# > 0 ] ; then
        cmd="$cmd $@"
    fi
    if (( $NIXOS == 1 )); then 
      cmd="nix-shell --run '$cmd'"
    fi
    cd $CONCORDIUM_P2P_DIR && eval "$cmd"
  )
}

#####
# Start up a testnet node of instance id 1 connection
# to 1 bootstrappers in the network and with heap profiling.
#
# testnet_node 1 1 massif
#
# the build determines the run mode (debug/release)
# (if running with profiling the built binary is used, but otherwise it's
# rebuilt with cargo every run)
#
#####
function testnet_node() {
  if (( $# < 3 ))
  then
    echo "Usage: testnet_node instanceid bootstrapper_count profiling"
    return 1
  elif (( $2 <1 ))
  then
    echo "bootstrappercount can't be less than 1"
    return 2
  fi
  if [ "$3" != "none" ];
    then
      profiling="valgrind --tool="$3" "
      binary="./target/debug/p2p_client-cli"
    else
      profiling=""
      binary="cargo run $CONCORDIUM_P2P_EXTRA_ARGS --bin p2p_client-cli --"
  fi
  instanceid=$1; shift
  bootstrappercount=$1; shift
  shift
  (
    cmd="$profiling \
       $binary \
      --listen-port $((10800+$instanceid)) \
      --id $((9800000000000000+$instanceid))\
      --rpc-server-port $((10000+$instanceid))"
    for n ({1..$bootstrappercount})
      do
        cmd="${cmd} --bootstrap-node 127.0.0.1:$(($n+10900))"
      done
    if [ $# > 0 ] ; then
        cmd="$cmd $@"
    fi
    
    if (( $NIXOS == 1 )); then 
      cmd="nix-shell --run '$cmd'"
    fi
    ( cd $CONCORDIUM_P2P_DIR && eval "$cmd"  )
  )
}

#####
# Start up a testnet using docker-compose with 5 bakers.
# (baker count must be 5, 20, 30, 50, 75, 100, 150, or 200)
#
# testnet_docker_compose 5
#
# or for a debug edition
#
# testnet_docker_compose 5 --debug
#
#####
function testnet_docker_compose() {
  if (( $# < 1 ))
  then
    echo "Usage: testnet_docker_compose amount_of_bakers extra_args"
    return 1
  fi
  baker_count=$1; shift
  (
    cd $CONCORDIUM_P2P_DIR/scripts/local && \
    ./pre_build.sh && \
    docker-compose build && \
    NUM_BAKERS=$baker_count DESIRED_PEERS=$(($baker_count)) EXTRA_ARGS="$@" docker-compose up --scale baker=$baker_count
  )
}

#####
# Start a tps receiver node, expecting 1000 packets
#
# c_tps_recv 1000
#
#####
function c_tps_recv() {
  if (( $# < 1 ))
    then
    echo "Usage: c_tps_recv packet_count"
    return 1
  fi
  (
    cmd="cargo run $CONCORDIUM_P2P_EXTRA_ARGS --bin p2p_client-cli -- \
      --bootstrap-node=127.0.0.1:9999 \
      --no-dnssec \
      --listen-port 9990 \
      --id 05c2198f706ebede \
      --tps-message-count $1 \
      --enable-tps-test-recv"
    if (( $NIXOS == 1 )); then 
      cmd="nix-shell --run '$cmd'"
    fi
    cd $CONCORDIUM_P2P_DIR && eval "$cmd $@"
  )
}

#####
# Start a tps sender node, expecting 1000 packets
#
# c_tps_recv 1000
#
#####
function c_tps_send() {
  if (( $# < 1 ))
    then
    echo "Usage: c_tps_send packet_count"
    return 1
  fi
  (
    cmd="cargo run $CONCORDIUM_P2P_EXTRA_ARGS --bin p2p_client-cli -- \
      --bootstrap-node=127.0.0.1:9999 \
      --no-dnssec \
      --listen-port 9991 \
      --id 05c2198f706ebedf \
      --tps-message-count $1 \
      --tps-test-data-dir /tmp/datatest \
      --tps-test-recv-id 05c2198f706ebede \
      --connect-to 127.0.0.1:9990"
    if (( $NIXOS == 1 )); then 
      cmd="nix-shell --run '$cmd'"
    fi
    cd $CONCORDIUM_P2P_DIR && eval "$cmd $@"
  )
}

#####
# Generate 1000 dummy test paylods of 1MB for c_tps_send()
#
# c_make_test_pkts 1000 1m
#
#####
c_make_test_pkts() {
  setopt localoptions rmstarsilent
  if (( $# < 2 ))
  then
    echo "Usage: make_test_pkts packet_count packet_size"
    return 1
  elif (( $2 < 1 ))
  then
    echo "packet size can't be less than one!"
    return 1
  fi
  if [ ! -d /tmp/datatest ]
  then
    mkdir -p /tmp/datatest
  elif
    then
    rm -f /tmp/datatest/*
  fi
  for n ({0..$1})
  do
    echo "Generating test packet $n"
    dd if=/dev/urandom of=/tmp/datatest/test-$n bs=1 count=$2 > /dev/null 2>&1
  done
}

#####
# Clear the console screen and clear buffer
#
#####
alias clear_screen='printf "\033c"'

#####
# Run rustfmt on all modules in project
#
#####
lint_fmt() {
  echo "Formatting code with $NIGHTLY_FMT_VERSION"
  ( cd $CONCORDIUM_P2P_DIR && cargo +$NIGHTLY_FMT_VERSION fmt)
  ( cd $CONCORDIUM_P2P_DIR/concordium-common && cargo +$NIGHTLY_FMT_VERSION fmt)
  ( cd $CONCORDIUM_P2P_DIR/concordium-consensus && cargo +$NIGHTLY_FMT_VERSION fmt)
  ( cd $CONCORDIUM_P2P_DIR/concordium-global-state && cargo +$NIGHTLY_FMT_VERSION fmt)
  ( cd $CONCORDIUM_P2P_DIR/concordium-global-state-sys && cargo +$NIGHTLY_FMT_VERSION fmt)
  ( cd $CONCORDIUM_P2P_DIR/concordium-dns && cargo +$NIGHTLY_FMT_VERSION fmt)
}

#####
# Run nix-shell for p2p-client
#
#####
concordium_p2p_nix_shell() {
   if [[ -f /etc/NIXOS ]]; then
       (
        cd $CONCORDIUM_P2P_DIR &&
            printf "Entering nix-shell environment for p2p-client\n"
            nix-shell $@
       )
    else
       printf "Not a NixOS environment!\n"
   fi
}
