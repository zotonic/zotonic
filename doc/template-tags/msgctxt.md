---
keywords:
  - reference
  - frontend_developer
  - template
  - localization_and_translation
  - translated_text
---

Set a template's default gettext message context:

```django
{% msgctxt "shop: checkout" %}
<h2>{_ Order _}</h2>
{% button text=_"Continue" %}
```

Declare it once, before output or blocks. An `extends` or `overrules` tag may
precede it. The context must be a literal string and is extracted as `msgctxt`
in the POT file. Use stable semantic labels, rather than template filenames.

The default applies to `{_ ... _}`, `_"..."`, and `{% trans ... %}` messages.
Override it with `tag#with_msgctxt` or an explicit `msgctxt` argument on
`tag#trans_ext`. Included and inherited templates use their own declarations.

For each language, lookup tries the contextual translation and then the
contextless translation. It then tries fallback languages, finally using the
English source. An empty context string is distinct from no message context.

Adding a context creates new catalog identities. Existing contextless
translations remain usable through fallback while contextual entries are translated.

A module can supply the default for all its templates using an attribute on
its module Erlang file, alongside `mod_title` and `mod_description`:

```erlang
-mod_msgctxt("shop: checkout").
```

The selected template file's owning module supplies this default, including
when templates override another module's templates. Includes and inherited
templates each use their own module's default. A template's `msgctxt` declaration
overrides the module default; scopes and explicit `trans` arguments override it
in turn. An explicitly empty context remains an override.

POT extraction uses the same default. The attribute applies to templates;
Erlang translation calls and macros continue to take their message context
explicitly. Modules without the attribute retain their existing behavior.
