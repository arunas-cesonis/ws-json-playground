#!/bin/sh
set -eu
npm i
exec ./node_modules/.bin/ts-node index.ts
