See also

[trans (variable substitution)](/id/doc_template_tag_tag_trans_ext).

Translate a text value using gettext.

Translate the text contained in the tag into the currently selected language.

Example:


```django
{_ translate me _}
```

If the active language is “nl” then this will output “vertaal mij”. Of course depending on the available translations.

If a translation is not available then the text is output as-is without any translation.