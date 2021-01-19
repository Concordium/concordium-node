#!/usr/bin/env bash

header () {
  echo -e '\033[1m'$@'\033[0m'
  echo
}

header "Let's get Docker installed! :)"

if [ "$(uname)" != "Darwin" ]; then
  echo "This script is made for MacOS, but it doesn't look like that's what you're using."
  echo
  echo "Exiting."
  exit 1
fi

if ! command -v brew >/dev/null 2>&1; then
  header "Homebrew not found. Installing now..."
  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  echo
else
  header "Homebrew found."
fi

header "Ensuring the Docker MacOS app is installed..."
brew cask install docker # This is a normal desktop app. Sadly, this seems to be the only way to run the docker daemon on MacOS.
echo

header "Ensuring the Docker CLI tools are installed..."
brew install docker docker-compose docker-machine # CLI tools for starting containers etc.
echo

if (! docker stats --no-stream > /dev/null 2>&1); then
  header "Starting Docker service..."
  open -a Docker # If this isn't running, _you're gonna have a bad time_
else
  header "Docker service already running."
fi

header "You need to login to Docker Hub else you won't be able to download images during builds..."
docker login
echo

header "Done."
