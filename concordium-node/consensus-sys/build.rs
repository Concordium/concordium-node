#[cfg(windows)]
fn main() {
    // Copy HSdll.dll.a to HSdll.dll in build directory for now
    println!("cargo:rustc-link-search=native=.");
    println!("cargo:rustc-link-lib=dylib=HSdll");
}

#[cfg(unix)]
fn main() {
    // Copy ouput .so file to library path
    println!("cargo:rustc-link-search=native={}/../.stack/programs/x86_64-linux/ghc-tinfo6-8.4.3/lib/ghc-8.4.3/rts", env!("CARGO_HOME"));
    //println!("cargo:rustc-link-search=native={}/../.stack/programs/x86_64-linux/ghc-tinfo6-8.4.3/lib/ghc-8.4.3/integer-gmp-1.0.2.0", env!("CARGO_HOME"));
    //println!("cargo:rustc-link-search=native={}/../.stack/programs/x86_64-linux/ghc-tinfo6-8.4.4/lib/ghc-8.4.4/rts", env!("CARGO_HOME"));
    println!("cargo:rustc-link-search=native=/usr/local/lib");
    println!("cargo:rustc-link-lib=dylib=HSConcordium-0.1.0.0");
}
