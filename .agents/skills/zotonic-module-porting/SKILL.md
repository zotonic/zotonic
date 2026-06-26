---
name: zotonic-module-porting
description: Use when creating Zotonic 1.x modules or porting Zotonic 0.x modules. Covers app structure, .app.src, -mod_config, controllers, binary request keys, JSON decoding, logging, specs, SCSS builds, Makefile/Taskfile usage, and payment module conventions.
---

# Zotonic Module Porting

## Module Structure

A Zotonic 1.x module/app usually has:

```text
app_or_module/
├── rebar.config
├── Makefile
├── Taskfile.yml        (optional, delegate to Makefile if present)
├── priv/
│   ├── dispatch/dispatch
│   ├── lib/            (compiled static output)
│   ├── lib-src/        (SCSS/JS source and build Makefile)
│   └── templates/
└── src/
    ├── app_or_module.app.src
    ├── mod_app_or_module.erl
    ├── actions/
    ├── filters/
    ├── scomps/
    ├── validators/
    ├── controllers/
    ├── models/
    └── support/
```

- Add `.app.src` in `src/`.
- In the main module, add `-mod_config([...])` for all configurable options.
- Use dependencies via `-mod_depends([...])` and `.app.src` applications as appropriate.
- Module application names must start with `zotonic_mod_...`.
- The main Erlang module of `zotonic_mod_mymod` is `src/mod_mymod.erl`.
- Actions, validators, and scomps should include the module name in their Erlang module name to support Zotonic override behavior.
- A site app has the same shape but uses a site module such as `src/sitename.erl` and must include `priv/zotonic_site.config`, `.json`, `.yaml`, or `.yml`.
- Keep source files UTF-8 with LF line endings.

## Module Boundaries

- Keep `src/mod_*.erl` small and Zotonic-facing. It should declare module metadata and config, handle lifecycle callbacks, observe notifications, install data, and connect the module to Zotonic.
- Put public module APIs in `src/models/m_*.erl` when templates, other modules, or model lookups need to call them. Models should normalize external/template input, apply ACL checks for model data, read module configuration, and return template-friendly data structures.
- Put worker processes and domain implementation in `src/support/`. This includes `gen_server` workers, parsers, protocol adapters, batching internals, external command/process communication, and pure transformation helpers.
- Let the model mediate support modules when the functionality is exposed to Zotonic. For example, have the model ensure a shared worker is started, pass timeouts/configuration, call the support worker, and normalize `{ok, ...} | {error, ...}` results for callers.
- Move code out of `mod_*.erl` once it stops being module lifecycle or observer glue. Move code out of `m_*.erl` when it becomes reusable implementation detail rather than a Zotonic-facing API.

## Porting From Zotonic 0.x

- Replace Webmachine-style controllers with Zotonic 1.x/Cowmachine callbacks.
- Look at `zotonic_mod_base` controllers and Cowmachine defaults before implementing callbacks.
- Omit callbacks where defaults are correct.
- Replace old request APIs and `wrq` usage with `z_context`, `m_req`, `cowmachine_req`, and Zotonic controller helpers.
- Replace string query keys with binary keys:

```erlang
z_context:get_q(<<"payment_nr">>, Context)
```

- For JSON request bodies, use:

```erlang
z_controller_helper:decode_request_noz(AcceptedCT, Context)
```

- For raw request-body authorization checks, read the body once and store it in the context if it must be reused.
- When porting templates, remove old convenience patterns that do not work well in Zotonic 1.x, such as `{% with m.rsc[id] as r %}` followed by `r.foo`; use `id.foo` directly in admin edit templates.

## Data And Types

- In Zotonic 1.x, request keys, JSON keys, and query keys are generally binaries.
- Use maps for decoded JSON and payment/resource data.
- Avoid atoms for validation of external values unless they are already internal finite-state values.
- Use named `-spec` variables with `when` clauses:

```erlang
-spec callback(Body, Context) -> Result
    when
        Body :: binary(),
        Context :: z:context(),
        Result :: {true, z:context()} | {{halt, pos_integer()}, z:context()}.
```

## Logging Conversion

- Replace older logging with `?LOG_*` structured maps.
- Include `in => module_or_app_name`.
- If `result => error`, include `reason => ...`.
- Branch on operation results and log success/failure after meaningful side effects.
- If `zotonic_core/include/zotonic.hrl` is included, do not also include `kernel/include/logger.hrl`.

## Crypto And JSON

- Prefer Zotonic JSON helpers such as `z_json`.
- Replace deprecated crypto calls:
  - old `crypto:hmac(...)`
  - new `crypto:mac(hmac, sha256, Key, Data)`
- Use binary-safe signing/authorization code. Keep values as binaries when constructing signatures.

## Payment Modules

- For PSP modules, observe payment notifications and return `#payment_psp_handler{}` where expected.
- Use `m_payment:get/2`, `m_payment:get_by_psp/3`, `m_payment_log:log/4`, and `mod_payment:set_payment_status/4`.
- Add `-mod_config` entries for all PSP settings, such as live/test flags, API keys, webhook host, invoice prefix, and selectable/excluded services.
- For PSP redirect controllers, validate signatures before changing payment status and log the result of status updates.
- For webhook controllers, authorize before decoding and return a 500 halt on processing errors that should be retried.

## Asset Builds

- Move SCSS source from `priv/lib/scss` to `priv/lib-src/scss`.
- Output compiled CSS to `priv/lib/css`; a `dist` directory is not required unless the app already uses one.
- Put the SCSS build command in `priv/lib-src/Makefile`.
- Add an app-level `Makefile` that delegates to `priv/lib-src`.
- Update `Taskfile.yml` to run the app-level Makefile and remove obsolete Elm build tasks after migration.
- Do not run PostCSS unless the user asks or the app already requires it.
- If SCSS imports need npm packages, copy only the needed `node_modules` packages into the build directory and verify the CSS build.

## Translations In Modules And Sites

- Use English template source strings and `{_ ... _}` translation tags for user-facing text.
- Do not leave old 0.x Dutch literals in templates; replace them with English msgids and update PO files.
- Do not regenerate or commit POT files during normal feature work. Zotonic POT files are generated on the `master` branch with:

```sh
bin/zotonic pot zotonic
```

- The POT command connects to an already-running Zotonic node. If a feature/test command creates POT diffs, restore or leave them out unless the user explicitly asks to update POT files.
- Merge existing translations:

```sh
msgmerge --backup=none --update priv/translations/nl.po priv/translations/template/site.pot
```

- Create additional languages with `msginit`, for example:

```sh
msginit --no-translator --locale=de --input=priv/translations/template/site.pot --output-file=priv/translations/de.po
```

- Validate PO files with `msgfmt --check --output-file=/dev/null`.
- When creating a new PO file, update default headers such as `Project-Id-Version` and `PO-Revision-Date` so validation warnings do not linger.

## Verification

- Run `make` for asset changes.
- Run `./rebar3 compile` for Erlang/module changes.
- Do not run POT generation for normal translation-related template changes; POT files are generated on `master` with `bin/zotonic pot zotonic`. Run `msgmerge`/`msginit` and `msgfmt --check` only when intentionally updating PO files.
- After compile, check `git diff -- rebar.lock`; remove unrelated generated lockfile dependency churn unless the task intentionally changed dependencies.
- Ignore `erl_crash.dump`; it is already in `.gitignore` and should not be reported as actionable worktree noise.
