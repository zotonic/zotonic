See also

[template block](/id/doc_template_tag_tag_block), [inherit](/id/doc_template_tag_tag_inherit), [fragment](/id/doc_template_tag_tag_fragment) and [overrules](/id/doc_template_tag_tag_overrules).

Inherit markup from another template.

Note

A template that extends another template contains only the extends tag and template [template block](/id/doc_template_tag_tag_block) tags.

Signal that this template extends another template. The extends tag must be the first tag in a template that inherits from another template.

Example:


```django
{% extends "base.tpl" %}
```

All named template blocks in this template will replace the similar named template blocks in the template base.tpl.

Unlike Django the template name must be a string literal, variables are not allowed.

Arguments
---------

The `extends` tag can pass extra variables to the extended template and inherited fragments:


```django
{% extends "base.tpl" section="news" %}
```

For compatibility with other tags, the `with` keyword is also accepted:


```django
{% extends "base.tpl" with section="news" %}
```

These arguments extend the current template context while the extended template is rendered.
