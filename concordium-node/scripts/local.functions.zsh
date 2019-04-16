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
#####
function testnet_docker_compose() {
  if (( $# < 1 ))
  then
    echo "Usage: testnet_docker_compose amount_of_bakers"
    return 1
  fi
  baker_count=$1; shift
  (
    cd $CONCORDIUM_P2P_DIR/scripts/local && \
    ./pre_build.sh && \
    docker-compose build --no-cache && \
    NUM_BAKERS=$baker_count docker-compose up --scale baker=$baker_count
  )
}