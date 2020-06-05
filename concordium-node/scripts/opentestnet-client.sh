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

su -s /bin/bash -c "/usr/local/bin/concordium-client --config /var/lib/concordium/config config init" -g docker docker

export BAKER_CREDENTIALS_FILENAME="baker-credentials.json"
if ! [ -f "/var/lib/concordium/data/$BAKER_CREDENTIALS_FILENAME" ];
then
    su -s /bin/bash -c "/usr/local/bin/concordium-client baker generate-keys /var/lib/concordium/data/baker-credentials.json" -g docker docker
fi

if [ -f /var/lib/concordium/data/blocks_to_import.dat ];
then
  export IMPORT_BLOCKS_FROM="/var/lib/concordium/data/blocks_to_import.dat"
fi

if [ -f /var/lib/concordium/data/node-dashboard-config.json ];
then
  cp /var/lib/concordium/data/node-dashboard-config.json /var/www/html/assets/config.json
fi

# Re-export the proper environment variable for the middleware, as it uses its own naming it seems
export CFG_DIR=$CONFIG_DIR

/usr/bin/supervisord -c /etc/supervisor/supervisord.conf
