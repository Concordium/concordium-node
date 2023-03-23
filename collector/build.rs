use std::env;

fn main() -> std::io::Result<()> {
    let cargo_dir = env!("CARGO_MANIFEST_DIR");
    let proto_root_input = format!("{}/../concordium-base/concordium-grpc-api", cargo_dir);
    let service = format!("{}/v2/concordium/service.proto", proto_root_input);
    let types = format!("{}/v2/concordium/types.proto", proto_root_input);

    tonic_build::configure()
        .build_server(false)
        .build_client(true)
        .compile(&[&service, &types], &[proto_root_input])
        .expect("Failed to compile gRPC client definitions!");

    Ok(())
}
