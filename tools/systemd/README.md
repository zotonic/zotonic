# Systemd Templates for Zotonic

These templates allow running Zotonic as a systemd service managed with Erlang's `heart` failure restart mechanism and an Erlang/OTP version managed via `kerl`.

## Files

- `zotonic-service.sh.in`: Wrapper script template that sources the kerl environment and invokes `./bin/zotonic`.
- `zotonic.service.in`: Systemd unit file template.

## Placeholders

- `@ZOTONIC_DIR@`: Absolute path to the Zotonic installation directory (e.g. `/opt/zotonic`).
- `@USER@`: System user running Zotonic (e.g. `zotonic`).
- `@GROUP@`: System group for the user (e.g. `zotonic`).
- `@KERL_ACTIVATE@`: Absolute path to kerl's `activate` script (e.g. `/home/zotonic/kerl/24.3.4/activate`).
- `@KERL_OTP_PATH@`: Alternative fallback: kerl OTP directory containing `bin/erl` if not using `activate`.

## Setup Example

1. Generate the files using `sed`:

   ```bash
   ZOTONIC_DIR="/opt/zotonic"
   USER="zotonic"
   GROUP="zotonic"
   KERL_ACTIVATE="/home/zotonic/kerl/24.3.4/activate"

   sed -e "s|@ZOTONIC_DIR@|$ZOTONIC_DIR|g" \
       -e "s|@KERL_ACTIVATE@|$KERL_ACTIVATE|g" \
       tools/systemd/zotonic-service.sh.in > "$ZOTONIC_DIR/bin/zotonic-service.sh"

   chmod +x "$ZOTONIC_DIR/bin/zotonic-service.sh"

   sed -e "s|@ZOTONIC_DIR@|$ZOTONIC_DIR|g" \
       -e "s|@USER@|$USER|g" \
       -e "s|@GROUP@|$GROUP|g" \
       tools/systemd/zotonic.service.in > /etc/systemd/system/zotonic.service
   ```

2. Reload systemd and enable the service:

   ```bash
   sudo systemctl daemon-reload
   sudo systemctl enable zotonic
   sudo systemctl start zotonic
   ```
