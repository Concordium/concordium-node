use reqwest::Client;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct ClientLogin {
    username: String,
    password: String,
    version:  String,
}

#[derive(Serialize, Deserialize)]
enum ClientLoginReturnStatus {
    OK,
    WrongVersion,
    WrongAuth,
}

#[derive(Serialize, Deserialize)]
struct ClientLoginResponse {
    status: ClientLoginReturnStatus,
}

const AUTH_URL: &str = "https://auth.eu.test.concordium.com/auth";

pub fn authenticate(username: &str, token: &str) -> bool {
    let client = Client::new();
    let login_details = ClientLogin {
        username: username.to_owned(),
        password: token.to_owned(),
        version:  crate::VERSION.to_owned(),
    };
    let response = client
        .post(AUTH_URL)
        .json(&login_details)
        .send()
        .map_err(|s| {
            error!("Failed to post to authentication server due to {}", s);
        })
        .ok()
        .and_then(|mut s| {
            s.json::<ClientLoginResponse>()
                .map_err(|s| {
                    error!(
                        "Failed to deserialize response from authentication server {}",
                        s
                    );
                })
                .ok()
        });

    if let Some(response) = response {
        match response.status {
            ClientLoginReturnStatus::OK => true,
            ClientLoginReturnStatus::WrongAuth => {
                error!("Could not log you in with those details. Please try again");
                false
            }
            ClientLoginReturnStatus::WrongVersion => {
                error!(
                    "You need to redownload the beta client as the currently installed version is \
                     not allowed"
                );
                false
            }
        }
    } else {
        false
    }
}
