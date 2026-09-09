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
