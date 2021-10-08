use std::{env, path::Path};
#[cfg(any(all(unix, not(feature = "static")), windows))]
use std::{process::Command, str};

#[cfg(all(not(feature = "static"), target_os = "linux"))]
const GHC_VARIANT: &str = "x86_64-linux-ghc-8.10.4";
#[cfg(all(not(feature = "static"), target_os = "macos"))]
const GHC_VARIANT: &str = "x86_64-osx-ghc-8.10.4";

#[cfg(not(feature = "static"))]
fn command_output(cmd: &mut Command) -> String {
    str::from_utf8(&cmd.output().unwrap().stdout).unwrap().trim_end().to_string()
}

fn main() -> std::io::Result<()> {
    let cargo_dir = env!("CARGO_MANIFEST_DIR");
    // Compile the flatbuffers schema
    println!("cargo:rerun-if-changed={}/src/network/serialization/schema.fbs", cargo_dir);
    flatc_rust::run(flatc_rust::Args {
        inputs: &[Path::new("src/network/serialization/schema.fbs")],
        out_dir: Path::new("target/"),
        ..Default::default()
    })
    .expect("Can't compile the flatbuffers schema");

    // MacOS logger
    #[cfg(target_os = "macos")]
    cc::Build::new().file("macos_log_wrapper.c").compile("macos_log_wrapper");

    // Build GRPC

    let proto_root_input = format!("{}/../concordium-grpc-api", cargo_dir);
    let proto = format!("{}/concordium_p2p_rpc.proto", proto_root_input);

    println!("cargo:rerun-if-changed={}", proto);

    #[cfg(not(feature = "static"))]
    {
        // Traverse the directory to link all of the libs in ghc.
        #[cfg(windows)]
        {
            let extra_library_dirs = command_output(Command::new("stack").args(&[
                "--stack-yaml",
                "../concordium-consensus/stack.yaml",
                "path",
                "--extra-library-dirs",
            ]));
            for extra_library_dir in extra_library_dirs.split(", ") {
                println!("cargo:rustc-link-search=native={}", extra_library_dir);
            }
            println!("cargo:rustc-link-lib=dylib=libpq");

            println!("cargo:rustc-link-search=native=../concordium-consensus");
            println!("cargo:rustc-link-lib=dylib=HSdll");
        }
        #[cfg(not(windows))]
        {
            // If the haskell root is provided we use it. In this case we assume.
            // that the libraries are put in the appropriate location and named as listed
            // below.
            if let Ok(root) = env::var("CONCORDIUM_HASKELL_ROOT") {
                if let Err(e) = std::fs::read_dir(&root) {
                    println!("Cannot read CONCORDIUM_HASKELL_ROOT: {}", e);
                    return Err(e);
                }
                println!("cargo:rustc-link-search=native={}", root);
                println!("cargo:rustc-link-lib=dylib=HSconcordium-consensus-0.1.0.0");
                println!("cargo:rustc-link-lib=dylib=HSconcordium-base-0.1.0.0");
                println!("cargo:rustc-link-lib=dylib=HSlmdb-0.2.5");
            } else {
                // otherwise auto-discover the directories via stack
                let stack_install_root_command = command_output(Command::new("stack").args(&[
                    "--stack-yaml",
                    "../concordium-consensus/stack.yaml",
                    "path",
                    "--local-install-root",
                ]));
                let stack_install_root = Path::new(&stack_install_root_command);

                let local_package = stack_install_root.join("lib").join(GHC_VARIANT);
                let dir = std::fs::read_dir(&local_package)?;

                println!("cargo:rustc-link-search={}", local_package.to_string_lossy());
                // Traverse all the files in the lib directory, and add all that end with
                // `.DYLIB_EXTENSION` to the linked libraries list.
                for dir_entry in dir {
                    let path = dir_entry?.path();
                    if let Some(true) =
                        path.extension().map(|ext| ext.to_string_lossy() == DYLIB_EXTENSION)
                    // FIXME: On a mac we should ignore case, but not on linux
                    {
                        let name = path
                            .file_stem()
                            .unwrap()
                            .to_str()
                            .unwrap()
                            .strip_prefix("lib")
                            .unwrap();
                        println!("cargo:rustc-link-lib=dylib={}", name);
                    }
                }
                println!("cargo:rustc-link-lib=dylib=pq");
                if let Ok(ref extra_libs_path) = env::var("EXTRA_LIBS_PATH").as_ref() {
                    println!("cargo:rustc-link-search=native={}", extra_libs_path);
                }
                let ghc_lib_dir = link_ghc_libs()?;
                println!(
                    "cargo:rustc-env=LD_LIBRARY_PATH={}:{}",
                    ghc_lib_dir.as_path().to_string_lossy(),
                    local_package.as_path().to_string_lossy()
                );
            }
        }
    }

    #[cfg(feature = "static")]
    link_static_libs()?;

    tonic_build::configure()
        .build_server(true)
        .build_client(true)
        .compile(&[&proto], &[&proto_root_input])
        .expect("Failed to compile gRPC definitions!");
    Ok(())
}

// Each os has a diferent extesion for the Dynamic Libraries. This compiles for
// the correct ones.
#[cfg(not(any(target_os = "macos", target_os = "windows")))]
#[cfg(not(feature = "static"))]
const DYLIB_EXTENSION: &str = "so";

#[cfg(target_os = "macos")]
#[cfg(not(feature = "static"))]
const DYLIB_EXTENSION: &str = "dylib";

#[cfg(all(not(feature = "static"), not(windows)))]
/// Link with Haskell runtime libraries.
/// The RTS version defaults to the threaded one, but can be overridded by the
/// HASKELL_RTS_VARIANT environment variable
fn link_ghc_libs() -> std::io::Result<std::path::PathBuf> {
    let rts_variant =
        env::var("HASKELL_RTS_VARIANT").unwrap_or_else(|_| "libHSrts_thr-".to_owned());
    let ghc_lib_dir = env::var("HASKELL_GHC_LIBDIR").unwrap_or_else(|_| {
        command_output(Command::new("stack").args(&[
            "--stack-yaml",
            "../concordium-consensus/stack.yaml",
            "ghc",
            "--",
            "--print-libdir",
        ]))
    });
    let rts_dir = Path::new(&ghc_lib_dir).join("rts");
    println!("cargo:rustc-link-search=native={}", rts_dir.to_string_lossy());
    for item in std::fs::read_dir(&rts_dir)?.filter_map(Result::ok) {
        let path = item.path();
        let file_stem = path.file_stem().unwrap().to_string_lossy();
        if file_stem.starts_with(&rts_variant) {
            if let Some(true) = item.path().extension().map(|ext| ext == DYLIB_EXTENSION) {
                let lib_name = file_stem.strip_prefix("lib").unwrap();
                println!("cargo:rustc-link-lib=dylib={}", lib_name);
            }
        }
    }
    Ok(rts_dir)
}

#[cfg(feature = "static")]
fn link_static_libs() -> std::io::Result<()> {
    let out_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    #[cfg(not(feature = "profiling"))]
    let path = format!("{}/deps/static-libs/linux/vanilla", out_dir);

    #[cfg(feature = "profiling")]
    let path = format!("{}/deps/static-libs/linux/profiling", out_dir);

    ["concordium", "dependencies", "ghc"].iter().for_each(|subdir| {
        println!("cargo:rustc-link-search=native={}/{}", path, subdir);
        let walker = walkdir::WalkDir::new(Path::new(&format!("{}/{}", path, subdir)))
            .into_iter()
            .filter_map(Result::ok);
        for item in walker {
            match item.file_name().to_str() {
                Some(lib_file) => {
                    if lib_file.starts_with("lib") && lib_file.ends_with(".a") {
                        println!(
                            "cargo:rustc-link-lib=static={}",
                            &lib_file[3..lib_file.len() - 2]
                        );
                    }
                }
                _ => panic!("Unable to link ghc libs"),
            }
        }
    });

    println!("cargo:rustc-link-search=native={}/deps/static-libs/linux/rust", out_dir);
    println!("cargo:rustc-link-lib=static=Rcrypto");
    println!("cargo:rustc-link-lib=static=wasm_chain_integration");

    println!("cargo:rustc-link-lib=dylib=pq");
    println!("cargo:rustc-link-lib=dylib=gmp");

    Ok(())
}
