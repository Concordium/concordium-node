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

service\windows\installer\build.ps1 -toolchain $rustVersion-x86_64-pc-windows-msvc -nodeVersion $nodeVersion