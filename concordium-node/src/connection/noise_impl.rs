use failure::Fallible;
#[cfg(not(feature = "snow_noise"))]
pub use noiseexplorer_xx::{noisesession::NoiseSession, types::Keypair};
#[cfg(feature = "snow_noise")]
use snow::Session;

const PROLOGUE: &[u8] = b"CP2P";
#[cfg(feature = "snow_noise")]
const PROTOCOL_NAME: &str = "Noise_XX_25519_ChaChaPoly_BLAKE2s";
pub const NOISE_MAX_MESSAGE_LEN: usize = 64 * 1024 - 1; // 65535
const NOISE_AUTH_TAG_LEN: usize = 16;
pub const NOISE_MAX_PAYLOAD_LEN: usize = NOISE_MAX_MESSAGE_LEN - NOISE_AUTH_TAG_LEN;

#[cfg(feature = "snow_noise")]
pub struct NoiseSession {
    session: Option<Session>,
    i:       bool,
    mc:      u128,
    buf:     [u8; NOISE_MAX_MESSAGE_LEN],
}

#[cfg(feature = "snow_noise")]
impl NoiseSession {
    pub fn send_message(&mut self, buf: &mut [u8]) -> Fallible<()> {
        let to_write = match self.mc {
            0 | 1 | 2 => &mut [][..],
            _ => buf,
        };
        let pad = match self.mc {
            0 | 1 | 2 => 0,
            _ => 16,
        };

        let written = self
            .session
            .as_mut()
            .unwrap()
            .write_message(&to_write[..to_write.len() - pad], &mut self.buf)?;
        self.mc += 1;
        buf[..written].copy_from_slice(&self.buf[..written]);
        Ok(())
    }

    pub fn recv_message(&mut self, buf: &mut [u8]) -> Fallible<()> {
        let decrypted_len = match self.mc {
            0 | 1 | 2 => buf.len(),
            _ => buf.len() - 16,
        };

        let read = self
            .session
            .as_mut()
            .unwrap()
            .read_message(&buf, &mut self.buf[..decrypted_len])?;
        self.mc += 1;
        buf[..read].copy_from_slice(&self.buf[..read]);
        Ok(())
    }

    pub fn get_message_count(&self) -> u128 { self.mc }

    pub fn is_initiator(&self) -> bool { self.i }
}

#[cfg(not(feature = "snow_noise"))]
pub fn start_noise_session(is_initiator: bool) -> NoiseSession {
    NoiseSession::init_session(is_initiator, PROLOGUE, Keypair::default())
}

#[cfg(feature = "snow_noise")]
pub fn start_noise_session(is_initiator: bool) -> NoiseSession {
    let keypair = snow::Builder::new(PROTOCOL_NAME.to_owned().parse().unwrap())
        .generate_keypair()
        .expect("Can't create a connection handler!");

    let builder = snow::Builder::new(PROTOCOL_NAME.to_owned().parse().unwrap())
        .prologue(PROLOGUE)
        .local_private_key(&keypair.private);

    let session = if is_initiator {
        builder.build_initiator()
    } else {
        builder.build_responder()
    }
    .expect("Can't build a snow session!");

    NoiseSession {
        session: Some(session),
        i:       is_initiator,
        mc:      0,
        buf:     [0; NOISE_MAX_MESSAGE_LEN],
    }
}

#[cfg(feature = "snow_noise")]
pub fn finalize_handshake(noise_session: &mut NoiseSession) -> Fallible<()> {
    let session = noise_session.session.take().unwrap();
    noise_session.session = Some(session.into_transport_mode()?);
    Ok(())
}

#[cfg(not(feature = "snow_noise"))]
pub fn finalize_handshake(_session: &mut NoiseSession) -> Fallible<()> { Ok(()) }
