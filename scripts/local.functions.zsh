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
# The default setting for where the application data
# directory is (can be overriden by setting 
# CONCORDIUM_P2P_DATA_DIR):
# $HOME/.local/share/concordium
#
#####

if [[ -n "$CONCORDIUM_P2P_DATA_DIR" ]]; then
  CONCORDIUM_P2P_APPDATA_DIR="$CONCORDIUM_P2P_DATA_DIR/local"
else
  CONCORDIUM_P2P_APPDATA_DIR="$HOME/.local/share/concordium/local"
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
  CONCORDIUM_GHC_PLATFORM="osx"
else
  CONCORDIUM_GHC_PLATFORM="linux"
fi

if [[ -f "/etc/NIXOS" ]]; then
  NIXOS=1
  if (( ${+CONCORDIUM_P2P_EXTRA_ARGS} )); then
    CONCORDIUM_P2P_EXTRA_ARGS="$CONCORDIUM_P2P_EXTRA_ARGS --features=static"
  else
    CONCORDIUM_P2P_EXTRA_ARGS="--features=static"
  fi
else
  NIXOS=0
fi

if [ -d "$CONCORDIUM_P2P_DIR/deps/internal/consensus" ];
then
  export CONCORDIUM_GHC_VERSION="$(stack ghc -- --version --short | awk '{ print $NF }')"
else
  export CONCORDIUM_GHC_VERSION="8.6.5"
fi

if (( ${+LD_LIBRARY_PATH} )); then
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib:~/.stack/programs/x86_64-$CONCORDIUM_GHC_PLATFORM/ghc-tinfo6-$CONCORDIUM_GHC_VERSION/lib/ghc-$CONCORDIUM_GHC_VERSION/rts
else
  export LD_LIBRARY_PATH=/usr/local/lib:~/.stack/programs/x86_64-$CONCORDIUM_GHC_PLATFORM/ghc-tinfo6-$CONCORDIUM_GHC_VERSION/lib/ghc-$CONCORDIUM_GHC_VERSION/rts
fi

NIGHTLY_FMT_VERSION="nightly-2019-11-13"
NIGHTLY_FMT_INSTALL_VERSION="nightly-2019-11-13"

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
      --id $(printf '%016d\n' $(($bootstrap_id+1000000)))"
    
    # Allocate a temporary directory for data storage for the instance
    RUNNER_BASE_PATH=$(mktemp -d /tmp/p2p-client.XXXXXXXXXXXXXXXX)
    RUNNER_DATA_PATH="$RUNNER_BASE_PATH/data"
    RUNNER_CONFIG_PATH="$RUNNER_BASE_PATH/config"
    mkdir -p $RUNNER_DATA_PATH
    mkdir -p $RUNNER_CONFIG_PATH

    cmd="${cmd} --override-data-dir $RUNNER_DATA_PATH --override-config-dir $RUNNER_CONFIG_PATH"

    if [ $# > 0 ] ; then
        cmd="$cmd $@"
    fi
    if (( $NIXOS == 1 ))  && [[ "$IN_NIX_SHELL" == "" ]]  ; then
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
      binary="./target/debug/concordium-node"
    else
      profiling=""
      binary="cargo run $CONCORDIUM_P2P_EXTRA_ARGS --bin concordium-node --"
  fi
  instanceid=$1; shift
  bootstrappercount=$1; shift
  shift
  (
    cmd="$profiling \
       $binary \
      --listen-port $((10800+$instanceid)) \
      --id $(printf '%016d\n' $(($instanceid))) \
      --rpc-server-port $((10000+$instanceid))"
    for n ({1..$bootstrappercount})
    do
        cmd="${cmd} --bootstrap-node 127.0.0.1:$(($n+10900))"
    done

    # Allocate a temporary directory for data storage for the instance
    RUNNER_BASE_PATH=$(mktemp -d /tmp/p2p-client.XXXXXXXXXXXXXXXX)
    RUNNER_DATA_PATH="$RUNNER_BASE_PATH/data"
    RUNNER_CONFIG_PATH="$RUNNER_BASE_PATH/config"
    mkdir -p $RUNNER_DATA_PATH
    mkdir -p $RUNNER_CONFIG_PATH

    cp -R $CONCORDIUM_P2P_APPDATA_DIR/* $RUNNER_DATA_PATH/

    cmd="${cmd} --override-data-dir $RUNNER_DATA_PATH --override-config-dir $RUNNER_CONFIG_PATH"
    
    if [ $# > 0 ] ; then
        cmd="$cmd $@"
    fi

    
    if (( $NIXOS == 1 ))  && [[ "$IN_NIX_SHELL" == "" ]]  ; then
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
    echo "Usage: testnet_docker_compose number_of_bakers extra_args"
    return 1
  fi
  baker_count=$1; shift
  (
    cd $CONCORDIUM_P2P_DIR &&
    NUM_BAKERS=$baker_count DESIRED_PEERS=$(($baker_count)) EXTRA_ARGS="$@" docker-compose up --scale baker=$baker_count
  )
}

#####
# Clear the console screen and clear buffer
#
#####
alias clear_screen='printf "\033c"'

#####
# Run rustfmt on all modules in the current directory
#
#####
lint_fmt() {
  echo "Formatting code with $NIGHTLY_FMT_VERSION"
  cargo +$NIGHTLY_FMT_VERSION fmt
}

#####
# Install the currently used nightly for formatting rust code
#
#####
install_lint_fmt() {
  rustup install $NIGHTLY_FMT_INSTALL_VERSION &&
  rustup component add rustfmt --toolchain $NIGHTLY_FMT_INSTALL_VERSION
}

#####
# Run nix-shell for p2p-client
#
#####
concordium_p2p_nix_shell() {
  if [[ "$IN_NIX_SHELL" != "" ]]  ; then
    echo "Already dropped into a nix-shell"
  elif (( $NIXOS == 1 )) ; then
    (
      cd $CONCORDIUM_P2P_DIR &&
      printf "Entering nix-shell environment for p2p-client\n"
      nix-shell $@
    )
  else
    printf "Not a NixOS environment!\n"
  fi
}

#####
# Copy local baker data for local testing
#
#####
testnet_copy_baker_data() {
  if (( $# < 1 ))
  then
    echo "Usage: testnet_copy_baker_data number_of_bakers"
    return 1
  fi
  baker_count=$1; shift
  (
    GENESIS_DATA_VERSION=$(cat ${CONCORDIUM_P2P_DIR}/scripts/GENESIS_DATA_VERSION)
    GENESIS_FILE_NAME="${baker_count}-bakers-${GENESIS_DATA_VERSION}.tar.gz" 
    GENESIS_FILE_URL="https://s3-eu-west-1.amazonaws.com/genesis-data.concordium.com/${GENESIS_FILE_NAME}"
    rm -rf $CONCORDIUM_P2P_APPDATA_DIR &&
    mkdir -p $CONCORDIUM_P2P_APPDATA_DIR &&
    cd  $CONCORDIUM_P2P_APPDATA_DIR &&
    curl -s $GENESIS_FILE_URL > $GENESIS_FILE_NAME &&
    tar xzf $GENESIS_FILE_NAME &&
    mv genesis_data/* . &&
    rmdir genesis_data &&
    rm $GENESIS_FILE_NAME
  )
}
