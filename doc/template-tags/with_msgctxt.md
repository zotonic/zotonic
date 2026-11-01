---
keywords:
  - reference
  - frontend_developer
  - template
  - localization_and_translation
  - translated_text
---

Override gettext message context for a lexical scope:

```django
{% with_msgctxt "order: sorting" %}
    {_ Order _}
    {{ _"Position" }}
{% endwith_msgctxt %}
```

The context must be a literal string. Scopes may nest; closing a scope restores
the previous context. This applies to translated literals, short translation tags,
and `trans` tags. An explicit `msgctxt` argument on a `trans` tag takes precedence.

Included templates use their own contexts. See `tag#msgctxt` for template defaults
and the contextual/contextless fallback rules.
