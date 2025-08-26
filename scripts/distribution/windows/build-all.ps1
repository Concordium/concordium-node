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
if ($env:WINDOWS_SM_KEYPAIR_ALIAS -and $env:WINDOWS_PKCS11_CONFIG -and $env:SM_ARGS -and (Get-Command "smctl" -ErrorAction SilentlyContinue)) {


try {
    # Move to the location of the script so that relative paths make sense
    Push-Location $PSScriptRoot
    $StackInstallRoot = stack path --local-install-root
    
    # Split the SM_ARGS into an array and remove quotes
    $smArgs = $env:SM_ARGS.Trim('"') -split '\s+'
    
    # Define array of files to sign
    $filesToSign = @(
        "$StackInstallRoot\lib\concordium-consensus.dll",
        "..\..\..\concordium-base\lib\concordium_base.dll",
        "..\..\..\concordium-base\smart-contracts\lib\concordium_smart_contract_engine.dll",
        "..\..\..\concordium-base\lib\sha_2.dll",
        "..\..\..\service\windows\target\x86_64-pc-windows-msvc\release\node-runner-service.exe",
        "..\..\..\collector\target\release\node-collector.exe",
        "..\..\..\concordium-node\target\release\concordium-node.exe"
    )
    
    # Sign all files
    foreach ($filePath in $filesToSign) {
        if (Test-Path $filePath) {
            Write-Host "Signing $filePath"
            & smctl sign --keypair-alias $env:WINDOWS_SM_KEYPAIR_ALIAS --input $filePath --config-file $env:WINDOWS_PKCS11_CONFIG @smArgs
        } else {
            Write-Warning "File not found for signing: $filePath"
        }
    }
}
finally {
    Pop-Location
}
} else {
    Write-Output "Not signing: Missing required environment variables or smctl utility missing."
}

# Build the installer
service\windows\installer\build.ps1 -toolchain $rustVersion-x86_64-pc-windows-msvc -nodeVersion $nodeVersion
