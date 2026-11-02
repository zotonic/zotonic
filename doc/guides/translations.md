---
keywords:
  - explanation
  - frontend_developer
  - backend_developer
  - localization_and_translation
  - translated_text
---

# Translating interface messages

Zotonic uses English source messages and gettext PO catalogs for interface
translations. Language configuration and selection are provided by
`module#mod_translation`; Erlang code looks up messages through
[the z_trans API](reference#z_trans).

## Mark messages for translation

In templates, use `{_ Save _}` for a simple label, `_"Save"` for a translated
argument, or `tag#trans_ext` for a message with named placeholders:

```django
{% trans "Hello {name}" name=user_name %}
{% trans "{n} item" "{n} items" n=count %}
```

Keep source messages, message contexts, and both plural source strings literal
so POT extraction can discover them. Preserve placeholders such as `{name}` and
`{n}` exactly in every translation. Translate complete messages instead of
concatenating words: surrounding text can change word order and grammatical case.

## Choose a message context

Prefer clear English source text, such as “First name” instead of “First”. When
identical English text needs different translations, use a stable semantic
`msgctxt`, such as “action” versus “status” for “Open”. A message context is part
of its gettext identity, not a language or an interpolation argument.

Templates can set a default with `tag#msgctxt`, override it inside
`tag#with_msgctxt`, or pass an explicit `msgctxt` to `tag#trans_ext`.
A module's `-mod_msgctxt` attribute supplies a default for its templates.
See [template message contexts](tag#msgctxt) for precedence and
how includes and inheritance behave. Erlang calls supply their context explicitly.

Adding a context creates a new catalog entry. Template lookup tries contextual
then contextless translations in each language before moving to the next fallback
language. This keeps existing general translations usable during migration.
The final fallback is the English source.

Contextual Erlang APIs are strict by default: they do not borrow a contextless
translation, which might have a different meaning or grammatical case. The
[Erlang API reference](reference#z_trans#contextless-fallback) describes
how to opt into the template policy.

## Translate plural messages

The two source strings describe the English singular and plural. They do not
limit other languages to two forms: each PO catalog's `Plural-Forms` header
specifies its own rule and number of forms. The count `n` must be a non-negative
integer.

Within one message context, use the same singular/plural source pair consistently.
POT extraction reports conflicting singular/plural definitions for the same
`msgid`. Give messages with different meanings distinct source text or contexts.
Use separate complete messages for “{n} hours ago” and “in {n} hours”.

Template compilation precompiles the available translated patterns. At rendering
time the count selects a form using the selected catalog's rule. Empty forms and
invalid rule results are skipped during fallback; each fallback catalog uses its
own rule. A catalog without a usable plural header supplies no plural translations.
Translation reloads invalidate compiled templates.

See [the extended trans tag](tag#trans_ext) for template syntax
and [the Erlang translation API](reference#z_trans) for resolving plural
translations and substituting arguments in Erlang.
