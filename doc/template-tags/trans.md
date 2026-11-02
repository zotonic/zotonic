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

`tag#trans_ext`.
:::

Translate a text value using gettext.

Translate the text contained in the tag into the currently selected language.

Example:


```django
{_ translate me _}
```

If the active language is “nl” then this will output “vertaal mij”. Of course depending on the available translations.

If a translation is not available then the text is output as-is without any translation.

This tag and translated literals such as `_"Open"` inherit the template's
`tag#msgctxt` declaration or enclosing `tag#with_msgctxt` scope. For a single
message override, use `{% trans "Open" msgctxt="action" %}`.
