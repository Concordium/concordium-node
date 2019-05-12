#####
# Define an export for CONCORDIUM_P2P_DIR pointing to
# the p2p-client checked out dir on your disk, and 
# include this file in your .zshrc or the like.
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
#####

if [[ "$OSTYPE" == "darwin"* ]]; then
  CONCORDIUM_GHC_PLATFORM="osx"
else
  CONCORDIUM_GHC_PLATFORM="linux"
fi

export CONCORDIUM_GHC_VERSION=$(stack ghc -- --version --short | awk '{ print $NF }')
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib:~/.stack/programs/x86_64-$CONCORDIUM_GHC_PLATFORM/ghc-tinfo6-$CONCORDIUM_GHC_VERSION/lib/ghc-$CONCORDIUM_GHC_VERSION/rts

#####
# Start up a testnet bootstrapper of instance id 1.
#
# testnet_bootstrap 1
#
# or for a debug edition
#
# testnet_bootstrap 1 --debug
#
#####
function testnet_bootstrap() {
  if (( $# < 1 ))
    then
    echo "Usage: testnet_bootstrap instanceid"
    return 1
  fi
  bootstrap_id=$1; shift
  (
    cd $CONCORDIUM_P2P_DIR && \
    cargo run --bin p2p_bootstrapper-cli -- \
      --listen-port $((10900+$bootstrap_id)) \
      --id $((9900000000000000+$bootstrap_id)) \
      $@
  )
}

#####
# Start up a testnet node of instance id 1 connection
# to 1 bootstrappers in the network.
#
# testnet_node 1 1
#
# or for a debug edition
#
# testnet_node 1 1 --debug
#
#####
function testnet_node() {
  if (( $# < 2 ))
  then
    echo "Usage: testnet_node instanceid bootstrapper_count"
    return 1
  elif (( $2 <1 ))
  then
    echo "bootstrappercount can't be less than 1"
    return 2
  fi
  instanceid=$1; shift
  bootstrappercount=$1; shift
  (
    cmd="cargo run --bin p2p_client-cli -- --listen-port $((10800+$instanceid)) --id $((9800000000000000+$instanceid))"
    for n ({1..$bootstrappercount})
    do
      cmd="${cmd} --bootstrap-node 127.0.0.1:$(($n+10900))"
    done
    cd $CONCORDIUM_P2P_DIR && eval "$cmd $@"
  )
}

#####
# Start up a testnet using docker-compose with 4 bakers.
#
# testnet_docker_compose 4
#
# or for a debug edition
#
# testnet_docker_compose 4 --debug
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
    cd $CONCORDIUM_P2P_DIR && \
    cargo run --bin p2p_client-cli -- \
      --bootstrap-node=127.0.0.1:9999 \
      --no-dnssec \
      --listen-port 9990 \
      --id 05c2198f706ebede \
      --tps-message-count $1 \
      --enable-tps-test-recv
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
    cd $CONCORDIUM_P2P_DIR && \
    cargo run --bin p2p_client-cli -- \
      --bootstrap-node=127.0.0.1:9999 \
      --no-dnssec \
      --listen-port 9991 \
      --id 05c2198f706ebedf \
      --tps-message-count $1 \
      --tps-test-data-dir /tmp/datatest \
      --tps-test-recv-id 05c2198f706ebede \
      --connect-to 127.0.0.1:9990
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
