use std::{borrow, env, process::Command};

fn main() {
    match env::var("CARGO_CFG_TARGET_OS").as_ref().map(|x| &**x) {
        Ok("windows") => {
            // Copy HSdll.dll.a to HSdll.dll in build directory for now
            println!("cargo:rustc-link-search=native=.");
            println!("cargo:rustc-link-lib=dylib=HSdll");
        }
        _ => {
            let version = String::from_utf8(
                Command::new("stack")
                    .args(&["ghc", "--", "--version"])
                    .output()
                    .expect("running stack failed")
                    .stdout,
            )
            .expect("non-UTF8 string returned by stack")
            .split_ascii_whitespace()
            .last()
            .map(borrow::ToOwned::to_owned)
            .expect("No version returned from ghc");

            println!(
                "cargo:rustc-link-search=native={}/../.stack/programs/x86_64-linux/ghc-tinfo6-{}/\
                 lib/ghc-{}/rts",
                env!("CARGO_HOME"),
                &version,
                &version,
            );
            println!("cargo:rustc-link-search=native=/usr/local/lib");
            println!("cargo:rustc-link-lib=dylib=HSConcordium-0.1.0.0");
            println!("cargo:rustc-link-lib=dylib=HSacorn-0.1.0.0");
            println!("cargo:rustc-link-lib=dylib=HSconcordium-crypto-0.1");
            println!("cargo:rustc-link-lib=dylib=HSglobalstate-0.1");
            println!("cargo:rustc-link-lib=dylib=HSglobalstate-types-0.1.0.0");
        }
    }
}
