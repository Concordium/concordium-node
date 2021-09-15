#!/usr/bin/env bash

GROUP_ID=""
USER_ID=""

if [ ! -z "$EXTERNAL_GID" ];
then
  GROUP_ID="$EXTERNAL_GID"
else
  GROUP_ID="61000"
fi

if [ ! -z "$EXTERNAL_UID" ];
then
  USER_ID="$EXTERNAL_UID"
else
  USER_ID="61000"
fi

groupadd -g $GROUP_ID docker
useradd -g $GROUP_ID -l -m -s /bin/false -u $USER_ID docker

mkdir -p /var/lib/concordium
chown -R $USER_ID:$GROUP_ID /var/lib/concordium

if [ -f /var/lib/concordium/data/baker-credentials.json ];
then
    if grep -q cipherText /var/lib/concordium/data/baker-credentials.json; then
        echo "This distribution does not support encrypted baker credentials."
        echo "Please replace baker-credentials.json with unencrypted credentials"
        echo "and restart the baker."
        echo ""
        echo "Unencrypted credentails can be generated with"
        echo "  concordium-client baker generate-keys <keys-file>.json"
        echo "supplying no password at the prompt."
        echo "To replace the baker keys on your account with these, use"
        echo "  concordium-client baker set-keys <keys-file>.json --sender bakerAccount --out <concordium-data-dir>/baker-credentials.json"
        exit 1
    fi
    export CONCORDIUM_NODE_BAKER_CREDENTIALS_FILE="baker-credentials.json"
fi

/usr/bin/supervisord -c /etc/supervisor/supervisord.conf
