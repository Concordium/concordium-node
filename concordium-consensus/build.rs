use std::{borrow, env, path::Path, process::Command, str};
use walkdir;

fn main() {
    // Traverse the directory to link all of the libs in ghc
    // then tell cargo where to get htest for linking
    match env::var("CARGO_CFG_TARGET_OS").as_ref().map(|x| &**x) {
        Ok("windows") => {
            // Copy HSdll.dll.a to HSdll.dll in build directory for now
            println!("cargo:rustc-link-search=native=.");
            println!("cargo:rustc-link-lib=dylib=HSdll");
        }
        Ok(_) => {
            println!("cargo:rustc-link-search=native=/usr/local/lib");
            println!("cargo:rustc-link-lib=dylib=HSConcordium-0.1.0.0");
            println!("cargo:rustc-link-lib=dylib=HSacorn-0.1.0.0");
            println!("cargo:rustc-link-lib=dylib=HSconcordium-crypto-0.1");
            println!("cargo:rustc-link-lib=dylib=HSglobalstate-0.1");
            println!("cargo:rustc-link-lib=dylib=HSglobalstate-types-0.1.0.0");
            println!("cargo:rustc-link-lib=dylib=HSscheduler-0.1.0.0");

            match link_ghc_libs() {
                Err(e) => panic!("Unable to link ghc_libs: {}", e),
                Ok(_) => println!("cargo:rustc-link-search=native=htest"),
            };
        }
        _ => panic!("Unknown architecture / OS"),
    }
}

fn command_output(cmd: &mut Command) -> String {
    str::from_utf8(&cmd.output().unwrap().stdout)
        .unwrap()
        .trim_end()
        .to_string()
}

fn exec_ghc(builder: &str, arg: &str) -> String {
    command_output(Command::new(builder).args(&["exec", "--", "ghc", arg]))
}

// Each os has a diferent extesion for the Dynamic Libraries. This compiles for
// the correct ones.
#[cfg(not(any(target_os = "macos", target_os = "windows")))]
const DYLIB_EXTENSION: &'static str = ".so";

#[cfg(target_os = "macos")]
const DYLIB_EXTENSION: &'static str = ".dylib";

#[cfg(target_os = "windows")]
const DYLIB_EXTENSION: &'static str = ".dll";

const RTS: &'static str = "libHSrts_thr-";

fn link_ghc_libs() -> std::io::Result<()> {
    // Go to the libdir for ghc then traverse all the entries
    let walker = walkdir::WalkDir::new(Path::new(&exec_ghc("stack", "--print-libdir")))
        .into_iter()
        .filter_map(Result::ok);

    for entry in walker {
        if entry.file_type().is_dir() {
            let walker2 = walkdir::WalkDir::new(entry.path())
                .into_iter()
                .filter_map(Result::ok);
            for item in walker2 {
                match (entry.path().to_str(), item.file_name().to_str()) {
                    // This directory has lib files link them
                    (Some(lib_path), Some(lib_file)) => {
                        if lib_file.starts_with("lib") && lib_file.ends_with(DYLIB_EXTENSION) {
                            // This filtering of items gets us the bare minimum of libraries
                            // we need in order to get the Haskell Runtime linked into the
                            // library. By default it's the non-threaded version that is
                            // chosen
                            if lib_file.starts_with(RTS)
                                || lib_file.starts_with("libHSghc-")
                                    && !lib_file.starts_with("libHSghc-boot-")
                                || lib_file.starts_with("libHSbase")
                                || lib_file.starts_with("libHSinteger-gmp")
                            {
                                println!("cargo:rustc-link-search=native={}", lib_path);
                                println!(
                                    "cargo:rustc-link-lib=dylib={}",
                                    &lib_file[3..lib_file.len() - DYLIB_EXTENSION.len()]
                                );
                            }
                        }
                    }
                    _ => panic!("Unable to link ghc libs"),
                }
            }
        }
    }

    Ok(())
}
