#!/usr/bin/env bash
set -euo pipefail


# Parameters
readonly version=${1:?"Please provide a version number (e.g. '1.0.2')"}
readonly developerIdApplication="Developer ID Application: Concordium Software Aps (K762RM4LQ3)"
readonly developerIdInstaller="Developer ID Installer: Concordium Software Aps (K762RM4LQ3)"

readonly GREEN='\033[0;32m'
readonly NC='\033[0m' # No Color

logInfo () {
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
readonly installDir="/Library/concordium-node/$version"


function cleanOldDist() {
    [ -d "$distDir" ] && logInfo "Cleaning '$distDir' folder" && rm -r "$distDir"
}


function compileConsensus() {
    cd "$consensusDir"
    logInfo "Building Consensus..."
    stack build
    logInfo "Done"
}

function compileNodeAndCollector() {
    cd "$nodeDir"
    logInfo "Building Node and Collector..."
    cargo build --bin concordium-node --bin node-collector --features collector --release
    logInfo "Done"
}

function compile() {
    compileConsensus
    compileNodeAndCollector
}

function copyBinaries() {
    logInfo "Copy concordium-node and node-collector binaries to '$distDir'.."
    mkdir "$distDir"
    cp "$nodeDir/target/release/concordium-node" "$distDir"
    cp "$nodeDir/target/release/node-collector" "$distDir"
    logInfo "Done"
}

function downloadGenesis() {
    logInfo "Downloading genesis.dat"
    curl -sSL "https://distribution.mainnet.concordium.software/data/genesis.dat" > "$distDir/genesis.dat"
    logInfo "Done"
}


function getDylibbundler() {
    logInfo "Getting dylibbundler..."

    if test -f "$macdylibbundlerDir/dylibbundler"
    then
        logInfo "Skipped: already exists"
    else
        logInfo " -- Downloading..."
        mkdir "$tmpDir"
        cd "$macPackageDir"
        curl -sSL "https://github.com/auriamg/macdylibbundler/archive/refs/tags/1.0.0.zip" > "$tmpDir/dylibbundler.zip" \
                    && logInfo " -- Unzipping..." \
                    && cd "$tmpDir" \
                    && unzip "dylibbundler.zip" \
                    && logInfo " -- Building..." \
                    && cd "$macdylibbundlerDir" \
                    && make
        logInfo "Done"
    fi
}

function collectDylibsFor() {
    local fileToFix=${1:?"Missing file to fix with dylibbundler"};
    cd "$distDir"
    "$macdylibbundlerDir/dylibbundler" --fix-file "$fileToFix" --bundle-deps --dest-dir "./libs" --install-path "@executable_path/libs/" --overwrite-dir \
        -s "$concordiumDylibDir" \
        -s "$stackSnapshotDir" \
        $stackLibDirs # Unquoted on purpose to use as arguments correctly
}

function collectDylibs() {
    logInfo "Collecting dylibs with dylibbundler"

    concordiumDylibDir=$(stack --stack-yaml "$consensusDir/stack.yaml" path --local-install-root)"/lib/$ghcVariant"
    stackSnapshotDir=$(stack --stack-yaml "$consensusDir/stack.yaml" path --snapshot-install-root)"/lib/$ghcVariant"
    stackLibDirs=$(find "$(stack --stack-yaml "$consensusDir/stack.yaml" ghc -- --print-libdir)" -maxdepth 1 -type d | awk '{print "-s "$0}')
    readonly concordiumDylibDir
    readonly stackSnapshotDir
    readonly stackLibDirs

    logInfo " -- Processing concordium-node"
    collectDylibsFor "$distDir/concordium-node"
    logInfo " -- Processing node-collector"
    collectDylibsFor "$distDir/node-collector"

    logInfo "Done"
}

function signBinaries() {
    logInfo "Signing binaries"
    # perm +111 finds the executable files
    find "$distDir" \
        -type f \
        -perm +111 \
        -execdir sudo codesign -f -s "$developerIdApplication" {} \;
    logInfo "Done"
}

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

function main() {

    cleanOldDist
    compile
    copyBinaries
    downloadGenesis
    getDylibbundler
    collectDylibs
    signBinaries
    buildPackage
    buildProduct
}

main
