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

if ! [ -f /var/lib/concordium/data/baker-0-credentials.json ];
then
    # The ip+port can be fake as they are not used in this specific command
    concordium-client-bin --grpc-ip localhost --grpc-port 1234 baker generate-keys /var/lib/concordium/data/baker-0-credentials.json
fi
export BAKER_ID=node-0

if [ -f /var/lib/concordium/data/blocks_to_import.dat ];
then
  export IMPORT_BLOCKS_FROM="/var/lib/concordium/data/blocks_to_import.dat"
fi

if [ -f /var/lib/concordium/data/node-dashboard-config.json ];
then
  cp /var/lib/concordium/data/node-dashboard-config.json /var/www/html/config.json
fi

/usr/bin/supervisord -c /etc/supervisor/supervisord.conf
