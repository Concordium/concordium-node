param ([string] $nodeVersion)

Write-Output "Building Windows node installer..."

# Move to the location of the script so that relative paths make sense
Push-Location $PSScriptRoot

try {

    # Build the custom actions DLL. This provides custom functionality used by the installer.
    Write-Output "Building custom-actions.dll..."
    Push-Location custom-actions
    cargo build --release --locked
    Pop-Location
    if ($LASTEXITCODE -ne 0) { throw "Failed building custom-actions.dll" }

    $StackInstallRoot = stack path --local-install-root

    # Create path bindings for the various sources.
    $Binds = @(
        "-b", "consensus=$StackInstallRoot\lib",
        "-b", "node=..\..\..\concordium-node\target\release",
        "-b", "baselib=..\..\..\concordium-base\lib",
        "-b", "contractlib=..\..\..\concordium-base\smart-contracts\lib",
        "-b", "collector=..\..\..\collector\target\release",
        "-b", "service=..\target\x86_64-pc-windows-msvc\release"
        "-b", "ca=.\custom-actions\target\x86_64-pc-windows-msvc\release",
        "-b", "res=.\resources")

    # We look up the location of the dependency DLLs in stack's path, and add them to
    # the search path for the linker.
    $ExtDLLs = @("liblmdb.dll", "libunwind.dll")
    Foreach ($ExtDLL in $ExtDLLs) {
        # Get the first instance on the path.
        $DLLLoc = ((stack exec -- where $ExtDLL) -split '\n')[0]
        $Binds += "-b", ("lib=" + (Get-Item $DLLLoc).Directory)
    }

    $env:_NodeProductId = [guid]::NewGuid().ToString();
    Write-Output "Generated fresh GUID for the build: $env:_NodeProductId"

    # We replace the - separator with a . since the convention at Concordium is
    # to use `-` as the separator between the binary version and build number,
    # but Wix only supports versions in the format X.Y.Z.B with all X,Y,Z,B
    # being integers.
    $env:_NodeVersion = $nodeVersion.Replace('-', '.')
    Write-Output "Building installer for node version $nodeVersion"

    Write-Output "Compiling installer..."
    candle -arch x64 -ext WixUtilExtension Node.wxs CustomDlgs.wxs
    if ($LASTEXITCODE -ne 0) { throw "Failed compiling installer" }
    Write-Output "Linking installer..."
    light $Binds -ext WixUIExtension -ext WixUtilExtension -out Node.msi Node.wixobj CustomDlgs.wixobj
    if ($LASTEXITCODE -ne 0) { throw "Failed linking installer" }

}
finally {
    Pop-Location
}