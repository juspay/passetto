#!/usr/bin/env bash

# This scripts runs the tests providing all the necessary environment.
#
# It has to be run from the project root.
# "pg_tmp" utility must be installed.

set -e

conn_str=$(pg_tmp)

psql "$conn_str" -c "create role passetto"
psql "$conn_str" -f ./service/pgsql/create_schema.sql
PASSETTO_PG_BACKEND_CONN_STRING=$conn_str stack test passetto-tests
