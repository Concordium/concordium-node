use std::env;

fn main() {
    let target_os = env::var("CARGO_CFG_TARGET_OS");
    match target_os.as_ref().map(|x| &**x) {
        Ok("windows") => {
            // Copy HSdll.dll.a to HSdll.dll in build directory for now
            println!("cargo:rustc-link-search=native=.");
            println!("cargo:rustc-link-lib=dylib=HSdll");
        }
        _ => {
            // Copy ouput .so file to library path
            println!("cargo:rustc-link-search=native={}/../.stack/programs/x86_64-linux/ghc-tinfo6-8.4.4/lib/ghc-8.4.4/rts", env!("CARGO_HOME"));
            println!("cargo:rustc-link-search=native=/usr/local/lib");
            println!("cargo:rustc-link-lib=dylib=HSConcordium-0.1.0.0");
        }
    }
}