#!/bin/sh

cd less
lessc --include-path="$APP_DIR/../zotonic_mod_artwork/priv/lib-src/font-awesome-4/less:$APP_DIR/../zotonic_mod_artwork/priv/lib-src/material-design/less" \
    less/zotonic-admin.less \
    "$APP_DIR/priv/lib/css/zotonic-admin.css"
