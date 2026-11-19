#!/bin/sh

# Stop this checkout's Compose project, including one-off development shells.
# Keep named volumes (PostgreSQL data) and the bind-mounted checkout.
set -eu
cd "$(dirname "$0")"

echo "Stopping Zotonic containers. Database data will be kept."
exec docker compose -f ./docker-compose.yml down --remove-orphans --timeout 30
