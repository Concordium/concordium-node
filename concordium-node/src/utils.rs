use hacl_star::sha2;

pub fn sha256(input: &str) -> [u8;32] {
    let mut output = [0; 32];
    sha2::Sha256::hash(&mut output, input.as_bytes());
    output
}

pub fn to_hex_string(bytes: [u8;32]) -> String {
  let strs: Vec<String> = bytes.iter()
                               .map(|b| format!("{:02X}", b))
                               .collect();
  strs.join("")
}