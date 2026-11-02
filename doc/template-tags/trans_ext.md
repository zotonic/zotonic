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

## Related documentation

See the [translation guide](guide#translations) for choosing message
contexts and writing translatable messages, and the
[Erlang translation API](reference#z_trans) for calls from Erlang code.
