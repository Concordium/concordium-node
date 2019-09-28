#!/usr/bin/env bash

GROUP_ID=""
USER_ID=""

if [ ! -z "$GID" ];
then
  GROUP_ID="$GID"
else
  GROUP_ID="61000"
fi

if [ ! -z "$UID" ];
then
  USER_ID="$UID"
else
  USER_ID="61000"
fi

groupadd -g $GROUP_ID docker
RUN useradd -g $GROUP_ID -l -M -s /bin/false -u $USER_ID docker

/usr/bin/supervisord -c /etc/supervisor/supervisord.conf
