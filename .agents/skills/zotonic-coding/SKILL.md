---
name: zotonic-coding
description: Use when working in Zotonic projects, especially Erlang modules, Zotonic template_compiler templates, dispatch rules, site templates, logging, datamodel fixtures, or site/module structure. Provides project conventions for pragmatic Zotonic 1.x coding.
---

# Zotonic Coding

## First Pass

- Read the local app/module before editing. Prefer existing project patterns over inventing new abstractions.
- Keep changes inside the requested app unless the user explicitly expands scope.
- Files use UTF-8 and LF line endings.
- Use `rg`/`rg --files` for discovery.
- Prefer `make` for normal Zotonic builds; use `./rebar3 compile` after Erlang changes when you only need a compile check. Ignore `rebar.lock` changes from normal build/test commands unless the task intentionally changes dependencies.

## Erlang Style

- Prefer pattern matching and small functions over nested conditionals.
- Use maps for structured request/payment/API data in Zotonic 1.x.
- Add `-spec` declarations using named variables and `when` clauses:

```erlang
-spec function_name(Arg, Context) -> Result
    when
        Arg :: binary(),
        Context :: z:context(),
        Result :: ok | {error, term()}.
```

- Use `#trans{ tr = [...] }` records, not old `{trans, ...}` tuples.
- Use Zotonic records such as `#datamodel{}` and `#rsc_tree{}` when fixtures/menu structures require them.
- Use `m_site`/context environment data for environment-dependent behavior instead of duplicating dev/prod fixtures.
- For Zotonic 1.x code, expect request keys, query keys, JSON keys, and most external textual data to be binaries, not strings.

## App Structure

- Zotonic is an Erlang umbrella application. Core modules live in `apps`; user modules and sites live in `apps_user`.
- Each module/site is an Erlang application with a `src/*.app.src`.
- Module application names must start with `zotonic_mod_...`; the main Erlang module is usually `mod_name.erl`, for example `src/mod_payment_buckaroo.erl`.
- Sites use a site module such as `src/nom.erl` and must have a `priv/zotonic_site` config file, using `.config`, `.json`, `.yaml`, or `.yml`.
- Module/site roots should have `rebar.config` unless the local workspace has an established exception.
- Common source directories include `actions`, `filters`, `scomps`, `validators`, `models`, `controllers`, and `support`.
- Actions, validators, and scomps should include the module/site name in their Erlang module name so higher-priority modules can override them cleanly.
- Keep the main `mod_*.erl` focused on Zotonic module concerns: `-mod_*` declarations, lifecycle hooks, observers, dispatch/menu setup, datamodel/install hooks, and small glue code.
- Put template/model-facing APIs and site-aware operations in `src/models/m_*.erl`. Models are the right place to normalize input from templates, check ACLs for model data, read module config, start shared workers when needed, and expose stable functions to other Zotonic code.
- Put reusable implementation details in `src/support/`. Support modules should own focused domain logic, parsing/recombination, protocol handling, worker `gen_server`s, and helpers that are not themselves template APIs.
- Prefer the model as the boundary between Zotonic callers and support processes. Keep process startup, batching, timeouts, and result normalization close to the public model API unless the logic is truly generic.

## Logging

- Prefer structured `?LOG_*` maps.
- In `?LOG_*` maps, `in => ...` is the Zotonic module or Erlang application context, not necessarily the current Erlang `?MODULE` file.
- If logging `?LOG_ERROR` with `result => error`, include `reason => ...`.
- If logging after an operation that returns `ok | {error, Reason}`, branch on the result and log success/failure explicitly.
- If a module already includes `zotonic_core/include/zotonic.hrl`, do not also include `kernel/include/logger.hrl`; `zotonic.hrl` provides the logging macros.

## Templates

- Zotonic templates use `template_compiler` tags, loosely Django-like.
- Documentation for the tags is in doc/template-tags
- Use `{% extends "base.tpl" %}` and blocks for page composition.
- Use `{% catinclude %}` for category-specific page/header variants.
- Prefer resource URLs with `m.rsc.resource_name.page_url` for named resources.
- Prefer `{% url dispatch_name %}` for controller/dispatch URLs.
- Avoid hard-coded internal URLs.
- Resource names do not contain spaces; Zotonic replaces spaces with `_`.
- Use `{% all include "_html_head.tpl" %}` and standard Zotonic body includes instead of directly including module-provided `_html_head*` fragments.
- Do not duplicate Google Tag Manager or SEO head/body snippets that are provided by Zotonic modules such as `mod_seo`.
- Favor semantic HTML and accessibility: use `header`, `nav`, `main`, `section`, `article`, `footer`; add `aria-*` where appropriate; provide useful image `alt` text.
- Use lowercase HTML tags and attributes.
- Avoid excessive wrapper `div`s and inline styles.
- Prefer Zotonic template constructs (`{% block %}`, `{% if %}`, `{% for %}`, etc.) for logic.
- Do not directly display values from `q` in `{{ ... }}` unless they are escaped or sanitized.
- Values from `m.rsc` are sanitized and may be displayed directly. Values from other models are not guaranteed sanitized and must be escaped or otherwise sanitized.
- Files in `priv/templates/static` are served as-is. A `.tpl` file in that directory must still be a valid template.
- `priv/templates/mediaclass.config` defines image mediaclasses usable in `{% image %}` tags and is written in Erlang format.

## Translations

- Keep template source strings in English. Replace Dutch or other source-language literals with English sentences.
- Wrap user-facing static text in translation tags:

```django
{_ English source text _}
```

- For include arguments or template variables that should be translated, use translated values such as `title=_"Important pages"`.
- After changing translatable template strings for a site, regenerate the site POT file:

```sh
bin/zotonic pot sitename
```

- `bin/zotonic pot sitename` connects to the running Zotonic node. If the server is already running, do not start another one; use the CLI command against the running instance.
- Site POT files live under `priv/translations/template`, for example `priv/translations/template/nom.pot`.
- Merge existing PO files with gettext:

```sh
msgmerge --backup=none --update priv/translations/nl.po priv/translations/template/site.pot
```

- Add a new language with `msginit` and then fill useful translations:

```sh
msginit --no-translator --locale=de --input=priv/translations/template/site.pot --output-file=priv/translations/de.po
```

- Validate PO syntax with:

```sh
msgfmt --check --output-file=/dev/null priv/translations/nl.po
msgfmt --check --output-file=/dev/null priv/translations/de.po
```

- A practical final check is an `rg` scan for known Dutch words in `priv/templates`, but avoid matching compiled libraries or unrelated assets.

## Dispatch

- Zotonic handles language prefixes in page paths; do not add language-code-specific dispatch rules for normal pages.
- Keep dispatch rule names language-neutral.
- Do not create dispatch rules for named page resources that can use `page_path`/`m.rsc.name.page_url`.

## Frontend Assets

- Static compiled output belongs in `priv/lib`.
- Source assets such as SCSS belong in `priv/lib-src`.
- CSS builds should be driven by a local Makefile in `priv/lib-src`, with the app-level `Makefile` delegating to it.
- Keep Taskfile targets focused on the app-level Makefile; remove obsolete Elm build targets after migration.
