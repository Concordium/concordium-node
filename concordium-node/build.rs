extern crate flatc_rust;
extern crate protoc_grpcio;

#[cfg(feature = "s11n_capnp")]
extern crate capnpc;

use std::path::Path;

fn main() {
    // Compile the Cap'n'Proto schema
    #[cfg(feature = "s11n_capnp")]
    capnpc::CompilerCommand::new()
        .edition(::capnpc::RustEdition::Rust2018)
        .src_prefix("src/network/serialization")
        .file("src/network/serialization/p2p.capnp")
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
    let proto_root_output = format!("{}/src/proto", cargo_dir);
    let proto_root_input = format!("{}/deps/internal/grpc-api", cargo_dir);
    println!(
        "cargo:rerun-if-changed={}",
        format!("{}/concordium_p2p_rpc.proto", proto_root_output)
    );
    protoc_grpcio::compile_grpc_protos(
        &["concordium_p2p_rpc.proto"],
        &[proto_root_input],
        &proto_root_output,
        None,
    )
    .expect("Failed to compile gRPC definitions!");
}
