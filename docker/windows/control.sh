#!/bin/bash

# Installed alongside compose.yml in the selected WSL user's home directory.
set -euo pipefail
action=${1:?Missing action}
checkout=${2:?Missing checkout path}
support_dir=$(cd -- "$(dirname -- "$0")" && pwd)

fail() { printf '%s\n' "$*" >&2; exit 1; }

[ "$(id -u)" != 0 ] || fail 'Use a normal Ubuntu user, not root. Run setup again and select that user.'
case "$checkout" in
    "$HOME"/*) ;;
    *) fail 'The checkout must be inside the selected Linux user home directory.' ;;
esac
# GNU Make-based dependencies and cross-platform launchers need a simple path.
[[ "$checkout" =~ ^/[a-zA-Z0-9_./-]+$ ]] || fail 'Use a checkout path without spaces or special characters.'

check_storage() {
    local path=$1
    while [ ! -e "$path" ]; do path=$(dirname -- "$path"); done
    case "$(stat -f -c %T "$path")" in
        ext2/ext3|btrfs|xfs) ;;
        *) fail 'Use the Ubuntu Linux filesystem, not a Windows mount or network share.' ;;
    esac
    # A symlink inside HOME must not redirect the checkout outside it.
    case "$(realpath -m -- "$checkout")" in
        "$HOME"/*) ;;
        *) fail 'The checkout resolves outside the Linux home directory.' ;;
    esac
}

check_storage "$checkout"
checkout=$(realpath -m -- "$checkout")

if [ "$action" = setup ]; then
    command -v git >/dev/null || fail 'In Ubuntu, run: sudo apt update && sudo apt install -y git'
    if [ ! -e "$checkout" ]; then
        mkdir -p -- "$(dirname -- "$checkout")"
        git -c core.autocrlf=false clone https://github.com/zotonic/zotonic.git "$checkout"
    fi
    [ -d "$checkout/.git" ] && [ -f "$checkout/docker-compose.yml" ] &&
        [ -f "$checkout/bin/zotonic" ] || fail 'Existing directory is not a Zotonic Git checkout. It was left unchanged.'
    git -C "$checkout" config core.autocrlf false
    printf 'Using Linux checkout: %s\n' "$checkout"
    exit 0
fi

command -v docker >/dev/null || fail 'Enable Docker Desktop Settings > Resources > WSL Integration for this Ubuntu distribution.'
docker compose version >/dev/null
[ "$(docker version --format '{{.Client.Os}}')" = linux ] || fail 'Use Docker Desktop WSL 2 integration with the Linux Docker CLI, not Docker VMM or docker.exe.'
[ "$(docker info --format '{{.OSType}}')" = linux ] || fail 'Docker Linux engine is unavailable. Check Docker Desktop and WSL integration.'
[ "$action" != check ] || exit 0
[ -f "$checkout/docker-compose.yml" ] || fail 'Checkout not found. Run setup again.'
cd -- "$checkout"

# Always map the selected WSL user, even if the calling environment has stale IDs.
export ZOTONIC_UID=$(id -u) ZOTONIC_GID=$(id -g)
# Isolate these managed containers from earlier Windows bind-mount projects.
project_name=zotonic-wsl-$(printf '%s\n' "${WSL_DISTRO_NAME:-linux}" "$checkout" | sha256sum | cut -c1-12)
compose() {
    docker compose --project-name "$project_name" --project-directory "$checkout" -f "$checkout/docker-compose.yml" \
        -f "$support_dir/compose.yml" "$@"
}

case "$action" in
    start)
        compose up -d --build
        echo 'Waiting for Zotonic. Initial compilation can take several minutes.'
        deadline=$((SECONDS + 1800))
        attempt=0
        while ((SECONDS < deadline)); do
            # Check this container, not an unrelated server on the Windows host.
            if compose exec -T --user zotonic zotonic curl --silent --fail \
                --max-time 3 --output /dev/null http://127.0.0.1:8000/ 2>/dev/null; then
                echo 'Zotonic is ready at http://localhost:8000/'
                exit 0
            fi
            container=$(compose ps -aq zotonic)
            if [ -z "$container" ] || [ "$(docker inspect --format '{{.State.Running}}' "$container")" != true ]; then
                compose logs --tail 80 zotonic
                fail 'Zotonic stopped during startup. See the container log above.'
            fi
            if ((attempt % 15 == 0)); then echo 'Still waiting; use Logs Zotonic to see build progress.'; fi
            attempt=$((attempt + 1))
            sleep 2
        done
        compose logs --tail 80 zotonic
        fail 'Startup timed out. Containers were left running; inspect Logs Zotonic or use Stop Zotonic.'
        ;;
    stop)
        # Also removes old one-off development shells. Never remove named volumes.
        compose down --remove-orphans --timeout 60
        echo 'Zotonic stopped. Database and checkout files were kept.'
        ;;
    shell) exec docker compose --project-name "$project_name" --project-directory "$checkout" \
        -f "$checkout/docker-compose.yml" -f "$support_dir/compose.yml" \
        exec --user zotonic zotonic bash ;;
    logs) compose logs --follow --tail 100 zotonic ;;
    *) fail "Unknown action: $action" ;;
esac
