# Zotonic Templates

A VS Code client for Zotonic's site-aware template language server. Requires a
compiled, running Zotonic node with `mod_development` enabled.

Install the bundled `zotonic-templates.vsix` using **Extensions: Install from VSIX**
in VS Code, or run:

```sh
code --install-extension zotonic-templates.vsix
```

No Node.js, npm, or extension build is needed for installation.

- Set `zotonic.command` to the absolute path of `bin/zotonic`.
- Choose **Zotonic: Select Site** and open a `.tpl` file.

For automatic local/Docker selection, set `zotonic.command` to the checkout's
`bin/zotonic-lsp` and `zotonic.args` to `[]` (requires Python 3 on the host).

For explicit containers, set `zotonic.command` to `docker`, supply the `exec -i` command
in `zotonic.args`, and configure `zotonic.pathMappings` for host/container source
roots. See the setup documentation for complete examples.

Includes diagnostics, definitions, hover, completion and a site selector.
The server reads unsaved buffers without compiling or executing them.

See [setup and protocol documentation](../../../../../doc/technotes/language-server.md)
for Sublime Text setup, limitations, security and testing.

Licensed under the Apache License, Version 2.0; see the Zotonic repository license.

## Rebuilding the package

Extension maintainers need Node.js and npm. From this directory, run:

```sh
npm ci
npm run package
```

The packaging command pins `@vscode/vsce` and includes the runtime dependencies.
Check in `zotonic-templates.vsix` whenever extension code, metadata, or dependencies
change. Bump the version in `package.json` and `package-lock.json` for new releases.
Changes to Zotonic's Erlang language server do not require repackaging the client.

For extension development, run `npm ci` and launch VS Code with
`code --extensionDevelopmentPath=/absolute/path/to/this/directory`.
