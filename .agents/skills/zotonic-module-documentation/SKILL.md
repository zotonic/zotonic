---
name: zotonic-module-documentation
description: Write or update Zotonic module documentation and zotonic_keywords for Erlang files published as reference documentation on zotonic.com. Use for documentation coverage and keyword alignment, not as a requirement to document every Erlang file.
---

# Zotonic Module Documentation

## Choose the published documentation surface

Add `-moduledoc` and `zotonic_keywords` only to Erlang modules intended to be imported into zotonic.com documentation. A request to document a Zotonic module does not mean adding these attributes to every `.erl` file in its application.

Normally document the main `mod_*.erl` and its public models, filters, controllers, scomps, actions, and validators. Check the actual import collectors and existing documentation when scope is unclear. Core collection follows application/source-directory conventions; external collection parses source and can fall back to a generic reference category. That fallback is not a reason to expose internal modules.

Do not add publication metadata to internal workers, support helpers, tests, or site implementation files merely because they are Erlang modules. Use ordinary `%% @doc` comments and specs for implementation documentation. Preserve existing documentation unless the user asks to change it or correct an overbroad addition made during the task. An explicit request to publish a particular internal API can expand this scope.

In a checkout containing `zotonicwww2`, inspect these when necessary:

- `src/support/zotonicwww2_beam_doc.erl`: core documentation collectors.
- `src/support/zotonicwww2_external_import.erl`: external page classification and manifest construction.
- `priv/bin/parse_external_docs.escript`: supported literal source metadata.

## Write documentation from the implementation

Read the module, model callbacks, configuration declarations, and relevant templates before documenting behavior. Preserve useful existing `-moduledoc` text; missing keywords do not imply missing documentation.

For the main module, explain purpose, enabling/setup, permissions, configuration, integration points, and important runtime behavior. Describe `-mod_config` settings accurately. Keep system configuration distinct from per-site module configuration. Do not add or change runtime settings merely to document them.

For public components, document the actual calling interface and a short usable example. For models, distinguish template/HTTP paths from direct Erlang APIs, explain return shapes and errors that matter, and state which entry points enforce ACLs. Do not imply that a configuration-presence check verifies service availability. Use actual template call sites to validate filter examples.

Prefer concise Markdown in literal `-moduledoc` attributes. Retain existing author/license headers. Documentation changes should not alter runtime behavior.

## Select controlled keywords

Use only `keyword_slug` values present in the current checkout's `doc/zotonic_subject_topics.csv`. Read relevant rows, including their meanings; do not invent slugs or substitute labels or aliases.

Compare equivalent components when available, for example Argos with DeepL, or a translation model with `m_translation`. Reuse applicable audience, component, domain, and data-type keywords. Do not copy unrelated implementation-specific topics just to make lists identical.

Keep keyword metadata separate from the prose:

```erlang
-moduledoc(#{
    zotonic_keywords => [
        "reference", "backend_developer", "model",
        "localization_and_translation", "translated_text", "language_code"
    ]
}).
-moduledoc("
Describe the model's public behavior and calling interface here.
").
```

The component keyword must match the page: `module`, `model`, `template_filter`, `controller`, `scomp`, `wire_action`, or `template_validator`, subject to validation against the CSV. Choose audience keywords according to the documentation's readers.

When the task includes notification callback documentation, check the core notification collector and use `-doc` plus `zotonic_keywords` metadata on the relevant callback. Do not create notification documentation from an observer function solely because it handles a notification.

## Verify and report

- Check every added keyword against the CSV and inspect the diff for unintended runtime changes.
- Compile changed trusted local Erlang files as a syntax check when the local build is available. Do not install dependencies or run translation services for documentation-only validation.
- For an external documentation import, use the source parser; never compile or execute fetched repository code to extract documentation.
- If parser compatibility is in question, verify the literal attributes with the external parser in a temporary output location.
- Report what was documented and what was checked. Source changes do not update zotonic.com until deployed and reimported; do not publish, push, or trigger a production import unless requested.
