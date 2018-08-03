#!/bin/sh
set -eu
cd $(dirname "$0")
bower install
exec pulp server -p 8080
