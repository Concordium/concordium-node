use std::path::Path;
#[cfg(windows)]
use std::{process::Command, str};

fn main() {
    // Compile the Cap'n'Proto schema
    #[cfg(feature = "s11n_capnp")]
    capnpc::CompilerCommand::new()
        .src_prefix("src/network/serialization")
        .file("src/network/serialization/p2p.capnp")
        .output_path("target/")
        .run()
        .expect("Can't compile the Cap'n'Proto schema");

    // Compile the flatbuffers schema
    flatc_rust::run(flatc_rust::Args {
        inputs: &[Path::new("src/network/serialization/schema.fbs")],
        out_dir: Path::new("target/"),
        ..Default::default()
    })
    .expect("Can't compile the flatbuffers schema");

    // Build GRPC
    let cargo_dir = env!("CARGO_MANIFEST_DIR");
    let proto_root_input = format!("{}/deps/internal/grpc-api", cargo_dir);
    let proto = format!("{}/concordium_p2p_rpc.proto", proto_root_input);

    println!("cargo:rerun-if-changed={}", proto);

    #[cfg(windows)]
    {
        let stack_library_dirs = str::from_utf8(
            &Command::new("stack").args(&["path", "--extra-library-dirs"]).output().unwrap().stdout,
        )
        .unwrap()
        .trim_end()
        .to_string();
        for dir in stack_library_dirs.split(", ") {
            println!("cargo:rustc-link-search=native={}", dir);
        }
        println!(r"cargo:rustc-link-search=native=.\deps\internal\consensus\Concordium");
    }

    tonic_build::configure()
        .build_server(true)
        .build_client(true)
        .compile(&[&proto], &[&proto_root_input])
        .expect("Failed to compile gRPC definitions!");
}
