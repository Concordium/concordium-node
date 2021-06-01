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
        echo "Please decrypt the baker credentials file before starting the baker."
        exit 1
    fi
    export BAKER_CREDENTIALS_FILENAME="baker-credentials.json"
fi

if [ -f /var/lib/concordium/data/blocks_to_import.dat ];
then
  export IMPORT_BLOCKS_FROM="/var/lib/concordium/data/blocks_to_import.dat"
fi

/usr/bin/supervisord -c /etc/supervisor/supervisord.conf
