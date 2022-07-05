#! /usr/bin/env bash

# This script allows running any of the project executables,
# primarily used as Docker container's entrypoint.

set -e

if [[ "$1" == "init" ]]; then
    exec="passetto-init"
elif [[ "$1" == "init-interactive" ]]; then
    exec="passetto-init-interactive"
elif [[ "$1" == "server" ]]; then
    exec="passetto-server"
elif [[ "$1" == "demo" ]]; then
    demo=true
else
    echo "Allowed commands: 'init', 'init-interactive', 'server', 'demo'."
    exit 1
fi

shift

/docker-entrypoint.sh postgres &
until pg_isready -U postgres > /dev/null; do sleep 1; done
{ createdb passetto -U postgres 2> /dev/null \
      && psql passetto -U postgres < ./create_schema.sql > /dev/null \
      && echo "Database found"; \
    } \
    || echo "Database creation is not needed"

export PASSETTO_PG_BACKEND_CONN_STRING=postgresql://postgres@/passetto

if [[ ! "$demo" ]]; then
    eval "./$exec" "$@"
else
    password="1"
    keys=3
    ./passetto-init $password $keys
    MASTER_PASSWORD=$password ./passetto-server
fi
