---
keywords:
  - reference
  - frontend_developer
  - template
  - localization_and_translation
  - translated_text
  - render
---

::: aside
See also

`tag#trans`.
:::

Translate a text value using gettext and substitute variables.

Example:


```django
{% trans "Hello {foo} World" foo=1234 %}
```

This translates the text using the available po files. The variables in the translated text will be substituted with the value of the arguments.

If the active language is “en” then the example above will output “Hello 1234 World”, in “nl” it will be “Hallo 1234 wereld”. Of course depending on the available translations.

If a translation is not available then the text is output as-is without any translation.

If a “\{” is needed in the string, then repeat it:


```django
{% trans "Hello {{foo}}, and this is {foo}." foo=1234 %}
```

Will echo “Hello \{foo\}, and this is 1234.”

## Plural forms

Supply two English source strings and an `n` argument to translate a count:

```django
{% trans "{n} hour ago" "{n} hours ago" n=hours %}
```

The first string is the English singular; the second is the English plural.
The second string is optional. Without it, the tag keeps its existing behavior.
With it, `n` is required and must evaluate to a non-negative integer. The count
expression is evaluated once and reused for both plural selection and `{n}`.

POT generation extracts these as `msgid` and `msgid_plural`. Each language's PO
file supplies its own `Plural-Forms` header and indexed `msgstr[0]`, `msgstr[1]`,
and further forms as needed. Keep `{n}` and other placeholders unchanged.

All available forms are precompiled. Each candidate uses its catalog's plural
rule; missing forms or invalid rule results are skipped during fallback.
The final fallback is the English source with the English plural rule. A catalog without
a usable plural header does not supply plural translations. Translation reloads
invalidate compiled templates as usual.

Use complete messages such as “{n} hours ago” and “in {n} hours” separately:
the surrounding words can change the grammatical form of the time unit.

For Erlang code, use `z_trans:trans_plural(Singular, Plural, N, Context)` with
literal English binaries or strings for extraction. The function returns the
selected text; callers substitute placeholders separately, just as with `trans/2`.

Within a message context, use the same source pair consistently. POT extraction reports
conflicting singular/plural definitions for the same `msgid`; give messages with
different meanings distinct English source text.

Erlang callers can distinguish identical source pairs with gettext message context:

```erlang
z_trans:trans_plural(<<"{n} hour">>, <<"{n} hours">>, N, <<"relative time: past">>, Context).
```

The five-argument form and `plural_translations/4` use `msgctxt` in PO/POT files.
Supply a literal message context for extraction. Missing contextual translations
fall back to the English source pair, never to a different message context.

## Message context in templates

Supply a literal `msgctxt` to distinguish otherwise identical labels:

```django
{% trans "Open" msgctxt="action" %}
{% trans "Open" msgctxt="status" %}
{% trans "{n} item" "{n} items" n=count msgctxt="basket" %}
```

Without an explicit argument, the message inherits the enclosing
`tag#with_msgctxt` scope or the template's `tag#msgctxt` declaration.
An explicit argument applies to the source message; translated argument values
still use their own enclosing scope. `msgctxt` is metadata, not an interpolation argument.

Template lookup tries the contextual and then contextless translation in each
language, before moving to the next fallback language. The final fallback is the
English source. Each plural candidate retains its own plural rule.

## Erlang singular messages

```erlang
z_trans:trans_msgctxt(<<"Open">>, <<"action">>, Context).
z_trans:trans_msgctxt(<<"Open">>, <<"action">>, nl, Context).
z_trans:translations(<<"Open">>, <<"action">>, Context).
?__(<<"Open">>, <<"action">>, Context).
?___(<<"Open">>, <<"action">>, Context).
```

Erlang APIs are strict by default: they do not borrow a contextless translation.
`translations(Text, MsgCtxt, true, Context)` and
`plural_translations(Singular, Plural, MsgCtxt, true, Context)` explicitly enable
the template fallback policy. The plural result can contain ordered candidates.

Use `lookup` or `lookup_fallback` to select a language from the resolved singular
`#trans{}` record. Passing it to `trans/3` can re-fetch translations using only the
English text, losing the message context. Supply literal source text and `MsgCtxt`
for POT extraction. Existing contextless calls and macros remain unchanged.
