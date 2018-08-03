#!/bin/sh
set -eu
cd $(dirname "$0")
npm i
exec ./node_modules/.bin/ts-node index.ts
