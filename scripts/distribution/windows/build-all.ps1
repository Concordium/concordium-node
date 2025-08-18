param ([string] $rustVersion = "1.73", [string] $nodeVersion)

Write-Output "stack version: $(stack --version)"
Write-Output "cargo version: $(cargo --version)"
Write-Output "flatc version: $(flatc --version)"
Write-Output "protoc version: $(protoc --version)"

# Set the default rust toolchain so that consensus rust dependencies use it.
rustup default $rustVersion-x86_64-pc-windows-gnu

Write-Output "Building consensus..."
stack build
if ($LASTEXITCODE -ne 0) { throw "Failed building consensus" }

Write-Output "Building node..."
stack exec -- cargo build --manifest-path concordium-node\Cargo.toml --release --locked
if ($LASTEXITCODE -ne 0) { throw "Failed building node" }

Write-Output "Building the collector..."
cargo +$rustVersion-x86_64-pc-windows-msvc build --manifest-path collector\Cargo.toml --release --locked
if ($LASTEXITCODE -ne 0) { throw "Failed building the collector" }

Write-Output "Building node runner service..."

# We navigate to the folder so that the .cargo\Config.toml is used.
# This ensures that the MSVC runtime is linked statically, and the output is produced
# in the right target folder.
Push-Location service\windows
cargo +$rustVersion-x86_64-pc-windows-msvc build --release --locked
Pop-Location
if ($LASTEXITCODE -ne 0) { throw "Failed building node runner service" }

if (-not $env:WINDOWS_SM_KEYPAIR_ALIAS) {
    Write-Output "WINDOWS_SM_KEYPAIR_ALIAS is not set. Not signing."
}
if (-not $env:WINDOWS_PKCS11_CONFIG) {
    Write-Output "WINDOWS_PKCS11_CONFIG is not set. Not signing."
}
if (-not $env:SM_ARGS) {
    Write-Output "SM_ARGS is not set. Not signing."
}

if (-not (Get-Command "smctl" -ErrorAction SilentlyContinue)){
    Write-Output "smctl not available. Not signing"
}
# Sign files if smctl is available and necessary environment variables are set.
if ($WINDOWS_SM_KEYPAIR_ALIAS -and $WINDOWS_PKCS11_CONFIG -and $SM_ARGS -and (Get-Command "smctl" -ErrorAction SilentlyContinue)) {

# Move to the location of the script so that relative paths make sense
Push-Location $PSScriptRoot

$StackInstallRoot = stack path --local-install-root

try {
smctl sign --keypair-alias $env:WINDOWS_SM_KEYPAIR_ALIAS --input .\$StackInstallRoot\lib\ConcordiumConsensusDLL --config-file $env:WINDOWS_PKCS11_CONFIG $env:SM_ARGS
smctl sign --keypair-alias $env:WINDOWS_SM_KEYPAIR_ALIAS --input ..\..\..\concordium-base\lib\ConcordiumBaseDLL --config-file $env:WINDOWS_PKCS11_CONFIG $env:SM_ARGS
smctl sign --keypair-alias $env:WINDOWS_SM_KEYPAIR_ALIAS --input ..\..\..\concordium-base\smart-contracts\lib\ConcordiumSmartContractEngineDLL --config-file $env:WINDOWS_PKCS11_CONFIG $env:SM_ARGS
smctl sign --keypair-alias $env:WINDOWS_SM_KEYPAIR_ALIAS --input ..\..\..\concordium-base\lib\Sha2DLL.dll --config-file $env:WINDOWS_PKCS11_CONFIG  $env:SM_ARGS
smctl sign --keypair-alias $env:WINDOWS_SM_KEYPAIR_ALIAS --input ..\target\x86_64-pc-windows-msvc\release\NodeRunnerService --config-file $env:WINDOWS_PKCS11_CONFIG $env:SM_ARGS
smctl sign --keypair-alias $env:WINDOWS_SM_KEYPAIR_ALIAS --input ..\..\..\collector\target\release\NodeCollector  --config-file $env:WINDOWS_PKCS11_CONFIG $env:SM_ARGS
smctl sign --keypair-alias $env:WINDOWS_SM_KEYPAIR_ALIAS --input ..\..\..\concordium-node\target\release\ConcordiumNode  --config-file $env:WINDOWS_PKCS11_CONFIG $env:SM_ARGS
}
finally{
    Pop-Location
}

} else {
    Write-Output "Not signing: Missing required environment variables or smctl utility missing."
}

# Build the installer
service\windows\installer\build.ps1 -toolchain $rustVersion-x86_64-pc-windows-msvc -nodeVersion $nodeVersion
