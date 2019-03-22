use rustls::{ ServerCertVerifier, RootCertStore, Certificate, TLSError, ServerCertVerified };
use webpki::DNSNameRef;

//Disable certificate verification
pub struct NoCertificateVerification;

impl ServerCertVerifier for NoCertificateVerification {
    fn verify_server_cert(&self,
                          _roots: &RootCertStore,
                          _presented_certs: &[Certificate],
                          _dns_name: DNSNameRef,
                          _ocsp: &[u8])
                          -> Result<ServerCertVerified, TLSError> {
        Ok(ServerCertVerified::assertion())
    }
}
