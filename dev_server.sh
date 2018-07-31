#!/bin/sh
set -eu
bower install
exec pulp server -p 8080
