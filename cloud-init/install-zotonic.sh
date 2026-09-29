#!/usr/bin/env bash
# Run the adjacent cloud-config on a fresh Ubuntu 24.04 or 26.04 LTS host.
# Usage: sudo ./install-zotonic.sh
# Requires cloud-init (apt-get install cloud-init). This is not an upgrade script.
set -euo pipefail
trap 'printf "Installation failed at line %s.\n" "$LINENO" >&2' ERR

if (( EUID != 0 )); then
    echo "Run this script as root." >&2
    exit 1
fi

if [[ ! -r /etc/os-release ]]; then
    echo "This installer requires Ubuntu 24.04 or 26.04 LTS." >&2
    exit 1
fi
. /etc/os-release
case "${ID}:${VERSION_ID}" in
    ubuntu:24.04|ubuntu:26.04) ;;
    *) echo "This installer requires Ubuntu 24.04 or 26.04 LTS." >&2; exit 1 ;;
esac
if ! command -v cloud-init >/dev/null; then
    echo "Install cloud-init first: apt-get update && apt-get install -y cloud-init" >&2
    exit 1
fi

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
config="$script_dir/zotonic-cloudinit.yml"
cloud-init schema --config-file "$config"

# Wait for active first-boot provisioning, but also support cloud-init freshly
# installed on an existing host. Propagate reported provisioning errors.
status=$(cloud-init status --format json | /usr/bin/python3 -c \
    'import json, sys; print(json.load(sys.stdin)["status"])')
if [[ "$status" != "not started" ]]; then
    cloud-init status --wait
fi
if id zotonic >/dev/null 2>&1; then
    zotonic_home=$(getent passwd zotonic | cut -d: -f6)
    if [[ -e "$zotonic_home/zotonic" ]]; then
        echo "An installation already exists at $zotonic_home/zotonic; refusing to overwrite it." >&2
        exit 1
    fi
fi

# Generate only this YAML's commands, without replacing the host's saved
# runcmd or executing its other scripts via the scripts_user module.
work_dir=$(mktemp -d /run/zotonic-install.XXXXXX)
trap 'rm -rf -- "$work_dir"' EXIT
/usr/bin/python3 - "$config" "$work_dir/runcmd" <<'PY'
import sys
from pathlib import Path
import yaml
from cloudinit import util

config = yaml.safe_load(Path(sys.argv[1]).read_text())
Path(sys.argv[2]).write_text(util.shellify(config['runcmd']))
PY

export DEBIAN_FRONTEND=noninteractive
for module in users_groups bootcmd write_files package_update_upgrade_install; do
    cloud-init --force single --file "$config" --name "$module" --frequency always
done

# Use a private working directory for downloads such as kerl.
cd "$work_dir"
/bin/sh -e "$work_dir/runcmd"
echo "Zotonic installation commands completed."
