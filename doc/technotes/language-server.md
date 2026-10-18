# Zotonic template language server

The Zotonic language server provides editor support for `.tpl` files using the
running site's template and module indexes. It complements Erlang language
servers: it does not handle `.erl` files.

Compile Zotonic, start the node normally, and enable `mod_development` on the
sites you want to use. Editors start the stdio transport with:

```sh
/absolute/path/to/zotonic/bin/zotonic lsp
```

The editor process must have Erlang on its `PATH` and use the same OS account,
Erlang cookie and node-name environment as other `bin/zotonic` commands. For a local node, paths refer to the same filesystem in the editor and node.
For a container, run the transport inside the container and configure source
path mappings as described below. There is no browser/HTTP transport.

## One command for local or Docker development

Point your editor at `bin/zotonic-lsp` to hide runtime selection:

```sh
/absolute/path/to/zotonic/bin/zotonic-lsp
```

This small launcher requires Python 3 on the editor host (standard library only).
It first probes the local Zotonic language server. If that succeeds, it replaces
itself with `bin/zotonic lsp` and never contacts Docker. Otherwise it inspects
running Docker containers for a bind mount containing this checkout, verifies
the language server, derives path mappings from directory bind mounts, and
replaces itself with `docker exec -i`. Discovery has bounded timeouts and runs
only at startup. There is no persistent Python process or message-forwarding
proxy, including in local mode. The direct `bin/zotonic lsp` command remains
available to skip discovery entirely.

VS Code configuration:

```json
{
    "zotonic.command": "/absolute/path/to/zotonic/bin/zotonic-lsp",
    "zotonic.args": [],
    "zotonic.site": "mysite"
}
```

Sublime's LSP configuration uses
`"command": ["/absolute/path/to/zotonic/bin/zotonic-lsp"]` with the existing
Zotonic syntax selector and site options. Neither editor needs Docker-specific
arguments or path mappings for the standard development container.

If more than one container qualifies, the command reports their names rather
than choosing arbitrarily. Save the choice in editor arguments:

```sh
bin/zotonic-lsp --container my-zotonic
```

`--mode local` forbids Docker fallback; `--mode docker` skips the local probe.
`--root /host/checkout` selects another checkout, and `--docker /path/to/docker`
selects the Docker executable. `--container` always overrides automatic local
selection. Site selection remains independent of container selection.

For the repository's development image, discovery uses the `zotonic` user,
checkout directory as HOME, and `docker-data` configuration directories, matching
the development entrypoint. Explicit container environment values take precedence.
For other images, the container's configured user is used when present; the
optional `org.zotonic.lsp.user` label overrides it. A container without a bind
mount containing this checkout can opt in with an `org.zotonic.lsp.root` label
naming its absolute Zotonic root. Such containers still need source bind mounts
for navigation. Discovery does not execute guesses based on image/container names.
It does not start stopped containers, rebuild images, or run the container's
setup entrypoint.

Automatic mapping is intended for matching host/container source trees. Named
volumes and image-only sources are not host source mappings. Use the explicit
container setup below for unusual layouts, users, environment or volume mounts.
Nonempty editor `pathMappings` override inferred mappings. The launcher passes
inferred mappings through the container-only `ZOTONIC_LSP_PATH_MAPPINGS`
environment variable; it does not write editor or Zotonic configuration files.

## Features

- Syntax diagnostics from unsaved buffers, using the template scanner/parser.
- Go to definition for literal template references, including `include`,
  `catinclude`, `extends`, `compose`, `catcompose`, `template="…"`, and other
  strings ending in `.tpl`.
- Go to definition for `overrules`, using the next template in module priority
  order, including when the editor opened the real source behind `_build` symlinks.
- Definitions for filters, models, scomps, actions and literal delegates
  (`event/2`), shared with the development template source viewer.
- Hover showing the reference type, site and matching source files. Paths resolve
  directory symlinks (including `_build` links); Erlang locations include line
  numbers. Markdown-capable clients get formatted headings and source lists,
  with effective and fallback templates labelled. Plain-text clients receive
  the same information without markup. Template matches
  follow module priority; category includes list candidates because choosing
  the effective file requires a resource/category context.
- Completion of template names, filters, models and built-in/custom tags.

Syntax diagnostics do not require a selected site. Explicit site selection keeps
all semantic requests in that site. Automatic mode resolves filters, models and
delegates across running sites with `mod_development` enabled. Filters and
literal delegates identify server-wide Erlang modules; custom models still use
the site module indexes, so actual source locations are compared.

Template references first consider sites whose template directories contain the
current file. If none match the reference, all running development sites are
tried. Sites without a target do not create ambiguity. If every matching site
returns the same canonical source locations in the same priority order, the
reference works without choosing a site. Different effective templates or
priority chains require a selection. Symlink aliases count as the same source.
Hover shows the owning module or site beside each source location. Matching site
lists are shown only for up to three sites, keeping shared-source popups compact.
The server honors the client's preferred
`textDocument.hover.contentFormat`, defaulting to plain text when absent. Completion combines suggestions from candidate
sites without choosing a rendering context.

Diagnostics currently report syntax errors, not missing resources or unknown
runtime variables. Completion is contextual and does not evaluate expressions.
There is no formatting, rename, workspace references or resource-aware category
selection in this first version.

## Visual Studio Code

The client is in `apps/zotonic_mod_development/priv/editors/vscode`.
Install the bundled `zotonic-templates.vsix` using **Extensions: Install from VSIX**,
or run `code --install-extension /path/to/zotonic-templates.vsix`. Installation
requires no npm or extension build step.

For extension development only, run `npm ci` in that directory and open VS Code with:

```sh
code --extensionDevelopmentPath=/absolute/path/to/zotonic/apps/zotonic_mod_development/priv/editors/vscode /absolute/path/to/workspace
```

Maintainers regenerate the checked-in VSIX with `npm ci` followed by
`npm run package` in the extension directory. It is not yet published to the Marketplace.

Configure the command path if the workspace is not the Zotonic checkout:

```json
{
    "zotonic.command": "/absolute/path/to/zotonic/bin/zotonic",
    "zotonic.site": "mysite"
}
```

Use **Zotonic: Select Site** or the status-bar item to select a running site.
The choice is saved in workspace settings. A window has one active site, also
for multi-root workspaces. **Zotonic: Restart Language Server** reconnects after
restarting/upgrading the node. The extension runs only in trusted workspaces.
It associates `.tpl` with **Zotonic Template**; change the language mode if
another installed extension already owns the association.

## Sublime Text

Install the **LSP** package with Package Control. Copy
`apps/zotonic_mod_development/priv/editors/sublime/Zotonic.sublime-syntax` into
`Packages/User` (use **Preferences > Browse Packages**). Select **Zotonic
Template** as the syntax for `.tpl` files.

Merge the supplied `LanguageServers.sublime-settings` example into
**Preferences > Package Settings > LSP > Server Configurations**. Replace the
absolute command path. The command is an argument array: do not add shell quotes
around paths containing spaces.

For a project-specific site, add this to the project's `.sublime-project` file:

```json
{
    "folders": [{ "path": "." }],
    "settings": {
        "LSP": {
            "zotonic": {
                "initialization_options": { "site": "mysite" },
                "settings": { "zotonic": { "site": "mysite" } }
            }
        }
    }
}
```

Keep the initial and live site setting consistent. Use **LSP: Restart Server**
after changing initialization options. Sublime's normal LSP definition, hover,
completion and diagnostics commands work without a Zotonic-specific plugin.

## Zotonic in Docker or another container

Keep editing files on the host. Start the same language-server command inside
an already running development container, with stdin attached and **without a
TTY**:

```sh
docker exec -i --user zotonic my-zotonic /opt/zotonic/bin/zotonic lsp
```

Use your container's actual name, user and Zotonic directory. The command must
run as the user whose Erlang cookie can connect to the node. If the node uses a
custom name/environment, pass the corresponding environment with `docker exec
-e`. The container must contain a compiled Zotonic checkout including these LSP
modules and have `mod_development` enabled for the selected site. No ports or
cookies need to be exposed to the editor host. See the
[Docker exec options](https://docs.docker.com/reference/cli/docker/container/exec/).

For VS Code, use these workspace settings:

```json
{
    "zotonic.command": "docker",
    "zotonic.args": [
        "exec", "-i", "--user", "zotonic", "my-zotonic",
        "/opt/zotonic/bin/zotonic", "lsp"
    ],
    "zotonic.site": "mysite",
    "zotonic.pathMappings": [
        { "localRoot": "/Users/me/Sites/zotonic", "remoteRoot": "/opt/zotonic" }
    ]
}
```

Use an absolute path to `docker` if it is not on the editor's PATH. Each argument
is a separate string; paths with spaces need no extra quoting. Changing the
command, arguments or mappings restarts the VS Code language client.

For Sublime's LSP server configuration, use:

```json
{
    "zotonic": {
        "enabled": true,
        "command": [
            "docker", "exec", "-i", "--user", "zotonic", "my-zotonic",
            "/opt/zotonic/bin/zotonic", "lsp"
        ],
        "selector": "text.html.zotonic",
        "initialization_options": {
            "site": "mysite",
            "pathMappings": [
                { "localRoot": "/Users/me/Sites/zotonic", "remoteRoot": "/opt/zotonic" }
            ]
        },
        "settings": { "zotonic": { "site": "mysite" } }
    }
}
```

Restart Sublime's LSP server after changing mappings. Docker Compose can use
`["docker", "compose", "-f", "/absolute/path/compose.yml", "exec", "-T",
"--user", "zotonic", "service-name", "/opt/zotonic/bin/zotonic", "lsp"]`
instead. Other container runtimes can use the equivalent stdio exec command.

Mappings connect absolute host source directories to container source directories.
The host files should be the same checkout as the container's bind-mounted
sources. Add a mapping for each separately mounted source tree, including
external modules/dependencies you want to navigate to. Nested mappings use the
longest matching directory root. Roots match complete path components:
`/project` never matches `/project-other`. Directory traversal, relative roots
and duplicate roots are rejected. Root paths use POSIX syntax (macOS/Linux
editor hosts); mappings are not yet a Windows drive-path translator.

Incoming documents use container paths for site detection and `overrules`;
diagnostics retain the original host URI. Definition results resolve container
symlinks first, then map the real source back to host URIs. Map the real source
root when a linked dependency lives outside the checkout. An unmapped source
produces an explicit error rather than opening a container-only path on the
host. Hover explanations retain runtime paths so the container's selection can
be inspected. This does not read or write host files from inside the container.

Local use remains the default: `zotonic.args` is `["lsp"]` and path mappings are
empty. No container discovery, Docker subprocess, network proxy or path-mapping
filesystem work is added to local sessions. Mappings are validated once at
initialization; the empty-mapping case passes values straight through.

## Protocol and security

The transport implements LSP 3.17 with byte-counted `Content-Length` framing,
UTF-8 JSON, UTF-16 positions and full document synchronization. Buffers belong
to one editor session and are discarded on close/shutdown; the server never
writes them to disk. Request IDs are echoed. Unknown requests receive a
JSON-RPC method error; unknown notifications are ignored. Requests run serially;
cancellation notifications are accepted but cannot interrupt an active RPC.

Limits are 8 MiB per protocol message, 8 KiB per header, 4 MiB per document,
200 open documents and 64 MiB of total buffer text. Semantic RPCs time out after
10 seconds. Restart the client after a broken transport. Unsupported or invalid buffer updates clear cached text and log a warning;
other malformed notifications are ignored. Only local `file:` URIs are accepted.

There is no LSP TCP/HTTP listener or browser callback. The CLI starts a temporary
Erlang distribution node, like other Zotonic CLI commands. Access uses Erlang
distribution and its cookie, just like the local development shell; possession
of that cookie already grants node-level access. Do not expose distribution to
untrusted networks. This is a trusted developer tool, not a site-user API.
Editor strings do not become new Erlang atoms. Unsaved source is parsed, never
compiled, rendered or executed. Returned locations come from the site's index
or existing Erlang source metadata. Browser editor tokens remain confined to
the permission-checked browser API.

`z_development_lsp_backend` is the reusable semantic layer. Its `request/2`
accepts diagnostics, definition, hover, completion and sites operations.
`z_development_lsp` owns framing, lifecycle and buffers; the launcher connects
to the runtime. `zotonic/sites` is a read-only custom LSP request returning
running development site names. Initialization uses `initializationOptions.site`;
live changes use `workspace/didChangeConfiguration` with `settings.zotonic.site`.
`initializationOptions.pathMappings` supplies up to 32 `{localRoot, remoteRoot}`
objects; changing them requires a new session.

## Verification

`z_development_lsp_tests` covers lifecycle, framing, URI encoding, UTF-16
positions, parser diagnostics and buffer versions. The integration test runs
against a live development node, without writing site files:

```sh
python3 apps/zotonic_mod_development/test/lsp_integration.py mysite
python3 apps/zotonic_mod_development/test/lsp_integration.py mysite --mapped
python3 apps/zotonic_mod_development/test/lsp_integration.py mysite --auto
PYTHONDONTWRITEBYTECODE=1 python3 apps/zotonic_mod_development/test/lsp_discovery_test.py
```

The VS Code client also includes `test/integration.js` for an extension-host
run with `--extensionDevelopmentPath` and `--extensionTestsPath`. Set
`ZOTONIC_LSP_TEST_ROOT` and `ZOTONIC_LSP_TEST_SITE`; use a temporary
`--user-data-dir` and `--extensions-dir` to isolate the test's settings.

References: [LSP specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/),
[VS Code language clients](https://code.visualstudio.com/api/language-extensions/language-server-extension-guide),
[Sublime LSP configuration](https://lsp.sublimetext.io/client_configuration/).

Model, filter, and scomp hovers include their Erlang module documentation. Tag-name
hovers use the manuals in `doc/template-tags`, without requiring a selected site.
Documentation uses Markdown when supported; plain-text clients receive its source
text. Missing documentation leaves the source-location hover available.

For Sublime Text, the server converts Markdown tables in hover documentation to
labelled bullet lists because minihtml does not render tables. Client detection
uses LSP `clientInfo.name`; VS Code and other clients retain the original tables.
Fenced code examples and documentation files are unchanged.
