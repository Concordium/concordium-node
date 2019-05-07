use failure::Fail;

#[derive(Debug, Fail)]
#[fail(display = "Can't parse <{}>", input)]
pub struct HostPortParseError {
    input: String,
}

impl HostPortParseError {
    pub fn new(input: String) -> HostPortParseError { HostPortParseError { input } }
}

#[derive(Debug, Fail)]
#[fail(display = "No DNS resolvers available")]
pub struct NoDNSResolversAvailable;
