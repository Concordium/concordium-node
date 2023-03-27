#!/usr/bin/env bash
set -euo pipefail

readonly version=${1:?"Please provide a version number (e.g. '1.0.2')"}
year="$(date +"%Y")" # Used for copyright notices.
readonly year

if ! git diff --quiet --exit-code; then
    echo "Uncommitted changes in the repository.";
    git --no-pager diff
    exit 1
fi

readonly teamId="K762RM4LQ3"
readonly developerIdApplication="Developer ID Application: Concordium Software Aps ($teamId)"
readonly developerIdInstaller="Developer ID Installer: Concordium Software Aps ($teamId)"

# Get the location of this script.
macPackageDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
readonly macPackageDir

readonly nodeDir="$macPackageDir/../../../concordium-node"
readonly collectorDir="$macPackageDir/../../../collector"
readonly consensusDir="$macPackageDir/../../../concordium-consensus"

readonly toolsDir="$macPackageDir/tools"
readonly macdylibbundlerDir="$toolsDir/macdylibbundler-1.0.0"

readonly buildDir="$macPackageDir/build"
readonly payloadDir="$buildDir/payload"

readonly pkgFile="$buildDir/packages/concordium-node.pkg"
readonly productFile="$buildDir/packages/concordium-node-$version-unsigned.pkg"
readonly signedProductFile="$buildDir/packages/concordium-node-$version.pkg"

ghcVersion="$(stack --stack-yaml "$consensusDir/stack.yaml" ghc -- --version | cut -d' ' -f8)" # Get the GHC version used in Consensus.
readonly ghcVersion

readonly ghcVariant="x86_64-osx-ghc-$ghcVersion"

# Log info in green color.
logInfo () {
    local GREEN='\033[0;32m'
    local NOCOLOR='\033[0m'
    printf "\n${GREEN}$@${NOCOLOR}\n"
}

function printVersions() {
    logInfo "Printing versions:"
    echo "stack version: $(stack --version)"
    echo "stack GHC version: $ghcVersion"
    echo "cargo version: $(cargo --version)"
    echo "flatc version: $(flatc --version)"
    echo "protoc version: $(protoc --version)"
    logInfo "Done"
}

function cleanBuildDir() {
    if [ -d "$buildDir" ]; then
        logInfo "Cleaning '$buildDir' folder"
        rm -r "$buildDir"
        logInfo "Done"
    fi
}

# Replace placeholder with replacement in a given file using sed.
# Parameters: file, placeholder, replacement
function replacePlaceholderInFile() {
    local theFile=${1:?"replacePlaceholderInFile expects 3 parameters: file, placeholder, replacement"}
    local placeholder=${2:?"replacePlaceholderInFile expects 3 parameters: file, placeholder, replacement"}
    local replacement=${3:?"replacePlaceholderInFile expects 3 parameters: file, placeholder, replacement"}
    sed -i '' -e 's/'"$placeholder"'/'"$replacement"'/g' "$theFile"
}

# Replace __VERSION__ in given file with $version.
# Parameters: file
function replaceVersionPlaceholder() {
    local theFile=${1:?"replaceVersionPlaceholder expects 1 parameter: file"}
    replacePlaceholderInFile "$theFile" "__VERSION__" "$version"
}

# Create stop/start testnet/mainnet helper apps from the template versions.
# Replaces a number of variables in the files.
# NB: Should be called after build folder has been created.
function createHelperAppsFromTemplate() {
    local startNodeMainnet="$payloadDir/Applications/Concordium Node/Concordium Node Start Mainnet.app"
    local startNodeTestnet="$payloadDir/Applications/Concordium Node/Concordium Node Start Testnet.app"
    local stopNodeMainnet="$payloadDir/Applications/Concordium Node/Concordium Node Stop Mainnet.app"
    local stopNodeTestnet="$payloadDir/Applications/Concordium Node/Concordium Node Stop Testnet.app"
    local nodeUninstaller="$payloadDir/Applications/Concordium Node/Concordium Node Uninstaller.app"

    # Use 'mv' to replace the __NET__ version template folder.
    cp -r "$payloadDir/Applications/Concordium Node/Concordium Node Start __NET__.app" "$startNodeMainnet"
    mv "$payloadDir/Applications/Concordium Node/Concordium Node Start __NET__.app" "$startNodeTestnet"
    cp -r "$payloadDir/Applications/Concordium Node/Concordium Node Stop __NET__.app" "$stopNodeMainnet"
    mv "$payloadDir/Applications/Concordium Node/Concordium Node Stop __NET__.app" "$stopNodeTestnet"

    replacePlaceholderInFile  "$startNodeMainnet/Contents/MacOS/run.applescript" "__NET__" "mainnet"
    replacePlaceholderInFile  "$startNodeMainnet/Contents/MacOS/run.applescript" "__NET_UPPERCASE__" "MAINNET"
    replacePlaceholderInFile  "$startNodeMainnet/Contents/Info.plist" "__NET_CAPITALISED__" "Mainnet"
    replacePlaceholderInFile  "$startNodeMainnet/Contents/Info.plist" "__NET__" "mainnet"
    replacePlaceholderInFile  "$startNodeMainnet/Contents/Info.plist" "__YEAR__" "$year"
    replaceVersionPlaceholder "$startNodeMainnet/Contents/Info.plist"

    replacePlaceholderInFile  "$startNodeTestnet/Contents/MacOS/run.applescript" "__NET__" "testnet"
    replacePlaceholderInFile  "$startNodeTestnet/Contents/MacOS/run.applescript" "__NET_UPPERCASE__" "TESTNET"
    replacePlaceholderInFile  "$startNodeTestnet/Contents/Info.plist" "__NET_CAPITALISED__" "Testnet"
    replacePlaceholderInFile  "$startNodeTestnet/Contents/Info.plist" "__NET__" "testnet"
    replacePlaceholderInFile  "$startNodeTestnet/Contents/Info.plist" "__YEAR__" "$year"
    replaceVersionPlaceholder "$startNodeTestnet/Contents/Info.plist"

    replacePlaceholderInFile  "$stopNodeMainnet/Contents/MacOS/run.applescript"  "__NET__" "mainnet"
    replacePlaceholderInFile  "$stopNodeMainnet/Contents/Info.plist" "__NET_CAPITALISED__" "Mainnet"
    replacePlaceholderInFile  "$stopNodeMainnet/Contents/Info.plist" "__NET__" "mainnet"
    replacePlaceholderInFile  "$stopNodeMainnet/Contents/Info.plist" "__YEAR__" "$year"
    replaceVersionPlaceholder "$stopNodeMainnet/Contents/Info.plist"

    replacePlaceholderInFile  "$stopNodeTestnet/Contents/MacOS/run.applescript"  "__NET__" "testnet"
    replacePlaceholderInFile  "$stopNodeTestnet/Contents/Info.plist" "__NET_CAPITALISED__" "Testnet"
    replacePlaceholderInFile  "$stopNodeTestnet/Contents/Info.plist" "__NET__" "testnet"
    replacePlaceholderInFile  "$stopNodeTestnet/Contents/Info.plist" "__YEAR__" "$year"
    replaceVersionPlaceholder "$stopNodeTestnet/Contents/Info.plist"

    replaceVersionPlaceholder "$nodeUninstaller/Contents/Info.plist"
}

# Fetch the genesis.dat files for mainnet and testnet.
function fetchGenesisFiles() {
    logInfo "Fetching genesis.dat files"
    logInfo "-- Mainnet"
    wget -P "$payloadDir/Library/Application Support/Concordium Node/Mainnet/Data" "https://distribution.mainnet.concordium.software/data/genesis.dat"
    logInfo "-- Testnet"
    # To lower the maintenance burden, we should, ideally, also download this file instead of storing it in this repo.
    cp "$nodeDir/../service/windows/installer/resources/testnet-genesis.dat" "$payloadDir/Library/Application Support/Concordium Node/Testnet/Data/genesis.dat"
    logInfo "Done"
}

# Create the 'build' folder from the 'template' folder.
# It copies the 'template' folder to 'build', creates a few new folders
# and replaces a number of variables in the files.
function createBuildDirFromTemplate() {
    logInfo "Creating build folder from template..."

    cp -r "$macPackageDir/template" "$buildDir"

    mkdir "$buildDir/plugins"
    mkdir -p "$payloadDir/Library/Application Support/Concordium Node/Mainnet/Config"
    mkdir "$payloadDir/Library/Application Support/Concordium Node/Mainnet/Data"
    mkdir -p "$payloadDir/Library/Application Support/Concordium Node/Testnet/Config"
    mkdir "$payloadDir/Library/Application Support/Concordium Node/Testnet/Data"
    mkdir "$payloadDir/Library/Concordium Node"

    createHelperAppsFromTemplate

    replaceVersionPlaceholder "$buildDir/resources/welcome.html"
    replacePlaceholderInFile "$buildDir/resources/conclusion.html" "__YEAR__" "$year"

    logInfo "Done"
}

# Compile Consensus using stack.
function compileConsensus() {
    cd "$consensusDir"
    logInfo "Building Consensus..."
    stack build
    logInfo "Done"
}

# Compile the node and collector using dynamic linking.
function compileNodeAndCollector() {
    cd "$nodeDir"
    logInfo "Building Node and Collector..."
    cargo build --bin concordium-node --release
    cd "$collectorDir"
    cargo build --release
    logInfo "Done"
}

# Compile the installer plugin using xcodebuild.
# The plugin is located in 'NodeConfigurationInstallerPlugin'.
function compileInstallerPlugin() {
    logInfo "Building installer plugin..."
    xcodebuild -project "$macPackageDir/NodeConfigurationInstallerPlugin/NodeConfigurationInstallerPlugin.xcodeproj" > /dev/null
    logInfo "Done"
}

# Compile consensus, node, collector, and the installer plugin.
function compile() {
    compileConsensus
    compileNodeAndCollector
    compileInstallerPlugin
}

# Copy the needed compiled files from the installer plugin to the build folder.
function copyInstallerPluginData() {
    logInfo "Copying installer plugin data to build folder"
    cp -r "$macPackageDir/NodeConfigurationInstallerPlugin/build/Release/NodeConfigurationInstallerPlugin.bundle" "$buildDir/plugins"
    cp "$macPackageDir/NodeConfigurationInstallerPlugin/NodeConfigurationInstallerPlugin/InstallerSections.plist" "$buildDir/plugins"
    logInfo "Done"
}

# Copy the node and collector binaries to the build folder.
function copyNodeBinaries() {
    logInfo "Copy concordium-node and node-collector binaries to '$payloadDir/Library/Concordium Node/'.."
    cp "$nodeDir/target/release/concordium-node" "$payloadDir/Library/Concordium Node"
    cp "$collectorDir/target/release/node-collector" "$payloadDir/Library/Concordium Node"
    logInfo "Done"
}

# Copy the compiled items (binaries and supporting data) to the build folder.
function copyCompiledItemsToBuildDir() {
    copyNodeBinaries
    copyInstallerPluginData
}

# Get the tool dylibbundler, which is used to recursively find and
# bundle the needed dylibs for a binary.
# Checks whether the tool is already installed and then potentially skips the build step.
function getDylibbundler() {
    logInfo "Getting dylibbundler..."

    if test -f "$macdylibbundlerDir/dylibbundler"
    then
        logInfo "Done (skipped: already exists)"
    else
        logInfo " -- Downloading..."
        mkdir "$toolsDir"
        cd "$macPackageDir"
        curl -sSL "https://github.com/auriamg/macdylibbundler/archive/refs/tags/1.0.0.zip" > "$toolsDir/dylibbundler.zip" \
                    && logInfo " -- Unzipping..." \
                    && cd "$toolsDir" \
                    && unzip "dylibbundler.zip" \
                    && logInfo " -- Building..." \
                    && cd "$macdylibbundlerDir" \
                    && make
        logInfo "Done"
    fi
}

# Use dylibbundler to recursively find and bundle the dylibs for node and collector.
# It moves all the dylibs into relative folder /libs and rewrites the binaries
# to look for the dylibs there.
# NB: This should be mostly reproducible across machines as we use a fixed resolver
# for Stack, and most of the dependencies are Haskell dylibs.
function collectDylibs() {

    function collectDylibsFor() {
        local fileToFix=${1:?"Missing file to fix with dylibbundler"};
        cd "$payloadDir/Library/Concordium Node"
        # Paths to search for dylibs are added with the '-s' flag.
        "$macdylibbundlerDir/dylibbundler" --fix-file "$fileToFix" --bundle-deps --dest-dir "./libs" --install-path "@executable_path/libs/" --overwrite-dir \
            -s "$concordiumDylibDir" \
            -s "$stackSnapshotDir" \
            $stackLibDirs # Unquoted on purpose to use as arguments correctly
    }

    logInfo "Collecting dylibs with dylibbundler (this will take a few minutes)..."

    concordiumDylibDir=$(stack --stack-yaml "$consensusDir/stack.yaml" path --local-install-root)"/lib/$ghcVariant"
    stackSnapshotDir=$(stack --stack-yaml "$consensusDir/stack.yaml" path --snapshot-install-root)"/lib/$ghcVariant"
    # Use awk to preprend '-s ' to each dylib, to be used as argument for dylibbundler directly.
    stackLibDirs=$(find "$(stack --stack-yaml "$consensusDir/stack.yaml" ghc -- --print-libdir)" -maxdepth 1 -type d | awk '{print "-s "$0}')
    readonly concordiumDylibDir
    readonly stackSnapshotDir
    readonly stackLibDirs

    logInfo " -- Processing concordium-node"
    collectDylibsFor "$payloadDir/Library/Concordium Node/concordium-node" &> /dev/null
    logInfo " -- Processing node-collector"
    collectDylibsFor "$payloadDir/Library/Concordium Node/node-collector" &> /dev/null

    logInfo "Done"
}

function signBinaries() {
    logInfo "Signing binaries..."

    # Find and sign all the binaries and dylibs.
    find "$payloadDir/Library" \
        -type f \
        -execdir sudo codesign -f --entitlement "$buildDir/entitlements.plist" --options runtime -s "$developerIdApplication" {} \;

    # Sign the installer plugin.
    sudo codesign -f --options runtime -s "$developerIdApplication" \
        "$buildDir/plugins/NodeConfigurationInstallerPlugin.bundle"

    logInfo "Done"
}

# Build the package.
# Look in the README.md for descriptions of the different files.
# The install-location is where to put the contents of the build/payload folder.
function buildPackage() {
    logInfo "Building package..."
    mkdir -p "$buildDir/packages"
    pkgbuild --identifier software.concordium.node \
        --version "$version" \
        --scripts "$buildDir/scripts" \
        --component-plist "$buildDir/components.plist" \
        --install-location "/" \
        --root "$payloadDir" \
        "$pkgFile"
    logInfo "Done"
}

# Build the product, which contains the package from the buildPackage step.
function buildProduct() {
    logInfo "Building product..."
    productbuild \
        --distribution "$buildDir/distribution.xml" \
        --package-path "$buildDir/packages" \
        --resources "$buildDir/resources" \
        --plugins "$buildDir/plugins" \
        "$productFile"

    # Remove the .pkg file now included in $productFile
    rm "$pkgFile"

    logInfo "Done"
}

# Sign the product.
function signProduct() {
    logInfo "Signing product..."
    productSign --sign "$developerIdInstaller" "$productFile" "$signedProductFile"
    logInfo "Done"
}

# Notarize the product and wait for it to finish.
# If successful, a notarization 'ticket' will be created on Apple's servers for the product.
# To enable offline installation without warnings, the ticket should be stapled onto the installer.
function notarize() {
    logInfo "Notarizing..."
    xcrun notarytool submit \
        "$signedProductFile" \
        --apple-id "$APPLEID" \
        --password "$APPLEIDPASS" \
        --team-id "$teamId" \
        --wait
    logInfo "Done"
}

# Staple the notarization ticket onto the installer.
function staple() {
    logInfo "Stapling..."
    xcrun stapler staple "$signedProductFile"
    logInfo "Done"
}

function signBuildAndNotarizeInstaller() {
    signBinaries
    buildPackage
    buildProduct
    signProduct
    notarize
    logInfo "Build complete"
    logInfo "Installer located at:\n$signedProductFile"
}

function buildInstaller() {
    buildPackage
    buildProduct
    logInfo "Build complete"
    logInfo "Installer located at:\n$productFile"
}

# Ask whether the installer should be signed or not.
# Then performs the selected action.
function promptToSignOrJustBuild() {
    while true; do
    read -rp "Do you wish to sign and notarize the installer [y/n]? " yn
    case $yn in
        [Yy]* ) signBuildAndNotarizeInstaller; break;;
        [Nn]* ) buildInstaller; break;;
        * ) echo "Please answer yes or no.";;
    esac
done
}

function main() {
    printVersions
    cleanBuildDir
    createBuildDirFromTemplate
    fetchGenesisFiles
    compile
    copyCompiledItemsToBuildDir
    getDylibbundler
    collectDylibs
    promptToSignOrJustBuild
}

main
