#!/usr/bin/env bash
set -euo pipefail


# Parameters
readonly version=${1:?"Please provide a version number (e.g. '1.0.2')"}
readonly developerIdApplication="Developer ID Application: Concordium Software Aps (K762RM4LQ3)"
readonly developerIdInstaller="Developer ID Installer: Concordium Software Aps (K762RM4LQ3)"

readonly GREEN='\033[0;32m'
readonly NC='\033[0m' # No Color

step () {
    printf "\n${GREEN}$@${NC}\n"
}

readonly ghcVariant="x86_64-osx-ghc-8.10.4"

macPackageDir=$(pwd)
readonly macPackageDir
readonly nodeDir="$macPackageDir/../../../concordium-node"
readonly consensusDir="$macPackageDir/../../../concordium-consensus"
readonly distDir="$macPackageDir/dist"
readonly tmpDir="$macPackageDir/tmp"
readonly macdylibbundlerDir="$tmpDir/macdylibbundler-1.0.0"


# Cleanup
[ -d "$distDir" ] && step "Cleaning '$distDir' folder" && rm -r "$distDir"


# Compile consensus
cd "$consensusDir"
step "Building Consensus..."
stack build
step "Done"


# Compile node
cd "$nodeDir"
step "Building Node and Collector..."
cargo build --bin concordium-node --bin node-collector --features collector --release
step "Done"


# Get concordium-node binary
step "Copy concordium-node and node-collector binaries to '$distDir'.."
mkdir "$distDir"
cp "$nodeDir/target/release/concordium-node" "$distDir"
cp "$nodeDir/target/release/node-collector" "$distDir"
step "Done"

step "Downloading genesis.dat"
curl -sSL "https://distribution.mainnet.concordium.software/data/genesis.dat" > "$distDir/genesis.dat"
step "Done"


# Fetch dylibbundler and build it
step "Getting dylibbundler..."

if test -f "$macdylibbundlerDir/dylibbundler"
then
    step "Skipped: already exists"
else
    step " -- Downloading..."
    mkdir "$tmpDir"
    cd "$macPackageDir"
    curl -sSL "https://github.com/auriamg/macdylibbundler/archive/refs/tags/1.0.0.zip" > "$tmpDir/dylibbundler.zip" \
                && step " -- Unzipping..." \
                && cd "$tmpDir" \
                && unzip "dylibbundler.zip" \
                && step " -- Building..." \
                && cd "$macdylibbundlerDir" \
                && make
    step "Done"
fi


# Collect dylibs
step "Collecting dylibs with dylibbundler"

concordiumDylibDir=$(stack --stack-yaml "$consensusDir/stack.yaml" path --local-install-root)"/lib/$ghcVariant"
stackSnapshotDir=$(stack --stack-yaml "$consensusDir/stack.yaml" path --snapshot-install-root)"/lib/$ghcVariant"
stackLibDirs=$(find "$(stack --stack-yaml "$consensusDir/stack.yaml" ghc -- --print-libdir)" -maxdepth 1 -type d | awk '{print "-s "$0}')
readonly concordiumDylibDir
readonly stackSnapshotDir
readonly stackLibDirs

function collectDylibs() {
    local fileToFix=${1:?"Missing file to fix with dylibbundler"};
    cd "$distDir"
    "$macdylibbundlerDir/dylibbundler" --fix-file "$fileToFix" --bundle-deps --dest-dir "./libs" --install-path "@executable_path/libs/" --overwrite-dir \
        -s "$concordiumDylibDir" \
        -s "$stackSnapshotDir" \
        $stackLibDirs # Unquoted on purpose to use as arguments correctly
}

step " -- Processing concordium-node"
collectDylibs "$distDir/concordium-node"
step " -- Processing node-collector"
collectDylibs "$distDir/node-collector"

step "Done"
function buildPackage {
    logInfo "Building package"
    cd "$macPackageDir"
    pkgbuild --identifier software.concordium.node \
        --version "$version" \
        --install-location "$installDir" \
        --root "$distDir" \
        concordium-node.pkg
}

function buildProduct {
    logInfo "Building product"
    cd "$macPackageDir"
    productbuild \
        --distribution template/distribution.xml \
        --scripts template/scripts \
        --package-path concordium-node.pkg \
        --resources template/resources \
        --sign "$developerIdInstaller" \
        concordium-node-signed.pkg
}

