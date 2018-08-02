extern crate walkdir;
extern crate protoc_grpcio;

fn main() {
    let proto_root = "src/proto";
    println!("cargo:rerun-if-changed={}", proto_root);
    protoc_grpcio::compile_grpc_protos(
        &["concordium_p2p_rpc.proto"],
        &[proto_root],
        &proto_root
    ).expect("Failed to compile gRPC definitions!");
}