#!/bin/bash
set -euo pipefail

if [ -d public-deploy ]; then
    rm -rf public-deploy;
fi

PACK_DIR="$PWD/site/node_modules/csl_web"
(
    cd csl-web
    wasm-pack build --target web --out-dir "${PACK_DIR}"
)
mkdir -p public-deploy/js
cp "${PACK_DIR}"/csl_web.d.ts site/src/
(
    cd site
    node esbuild.mjs
)
cp "${PACK_DIR}"/*.{wasm,js} public-deploy/js
rsync -a site/public/ public-deploy/
rsync -a public-docs/ public-deploy/
