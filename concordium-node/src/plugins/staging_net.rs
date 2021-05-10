use anyhow::{anyhow, bail};
use jsonwebtoken::dangerous_insecure_decode;
use reqwest::Client;
use serde::{Deserialize, Serialize};

/// The ClientLogin struct holds information about
/// the client needed to start in staging_net mode
#[derive(Serialize, Deserialize)]
struct ClientLogin {
    /// A JSON Web Token containing username,
    /// expiration date and whether a developer
    token: String,
    /// The client version to check if the current
    /// version is allowed to start
    version: String,
}

/// The ClientLoginReturnStatus type contains all valid
/// responses from the authentication server
#[derive(Serialize, Deserialize)]
enum ClientLoginReturnStatus {
    /// Client authenticated correctly and with a allowed version
    OK,
    /// Client authenticated sucessfully, but with a disallowed version
    WrongVersion,
    /// Client entered wrong authentication details
    WrongAuth,
}

/// The ClientLoginResponse struct holds the status of a login
#[derive(Serialize, Deserialize)]
struct ClientLoginResponse {
    /// The response to a login
    status: ClientLoginReturnStatus,
}

/// The Claim struct is a decoded JSON Web Token claim
#[derive(Serialize, Deserialize)]
pub struct Claim {
    /// Issuer of JWT
    pub iss: String,
    /// Username of user
    pub sub: String,
    /// Expiration date of JWT
    pub exp: i64,
    /// If the user is a developer
    pub developer: bool,
}

const AUTH_URL: &str = "https://auth.eu.staging.concordium.com/auth";

/// The get_username_from_jwt method decodes a JWT
/// into a `Claim` and then returns the username

pub fn get_username_from_jwt(token: &str) -> String {
    // Although this particular method is normally unsafe,
    // it is perfectly safe to use here because we are only
    // obtaining the username from the JWT, and not relying
    // on it for actual validation of the token.
    //
    // The expect call is fine because we would have gotten
    // an error response from the authentication server
    // regardless, and therefore would have needed to
    // restart anyway.
    dangerous_insecure_decode::<Claim>(token)
        .map(|s| s.claims.sub)
        .expect("Could not validate JWT. Authentication would have failed anyway!")
}

/// The authenticate method contacts the authentication server
/// supplying the JWT and then responds back if authentication
/// was successful or not

pub async fn authenticate(token: &str) -> anyhow::Result<()> {
    let client = Client::new();
    let login_details = ClientLogin {
        token:   token.to_owned(),
        version: crate::VERSION.to_owned(),
    };
    let response = client
        .post(AUTH_URL)
        .json(&login_details)
        .send()
        .await
        .map_err(|s| anyhow!("Failed to post to authentication server due to {}", s))?
        .json::<ClientLoginResponse>()
        .await
        .map_err(|s| {
            anyhow!("Failed to deserialize response from authentication server {}", s)
        })?;

    match response.status {
        ClientLoginReturnStatus::OK => Ok(()),
        ClientLoginReturnStatus::WrongAuth => {
            bail!("Could not log you in with those details. Please try again");
        }
        ClientLoginReturnStatus::WrongVersion => {
            bail!(
                "You need to redownload the staging net client as the currently installed version \
                 is not allowed"
            );
        }
    }
}
