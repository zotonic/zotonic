---
keywords:
  - reference
  - frontend_developer
  - template
  - categorization
  - maintainability
  - render
---

::: aside
See also

`tag#compose`, `tag#catinclude` and `tag#block`.
:::

Compose another template based on the category of a resource.

The `catcompose` tag is the category-aware variant of `tag#compose`. It selects the template using category lookup, like `tag#catinclude`, and also lets the caller override named blocks in the selected template.

Example:


```django
{% catcompose "_teaser.tpl" id %}
    {% block body %}
        <p>Custom teaser body</p>
    {% endblock %}
{% endcompose %}
```

The selected template depends on the category hierarchy of `id`, using the same lookup rules as `tag#catinclude`.

Arguments
---------

Extra arguments can be passed to the composed template:


```django
{% catcompose "_teaser.tpl" id title="Summary" %}
    {% block body %}Custom teaser body{% endblock %}
{% endcompose %}
```

The `with` keyword is optional here as well:


```django
{% catcompose "_teaser.tpl" id with title="Summary" %}
    {% block body %}Custom teaser body{% endblock %}
{% endcompose %}
```

The resource id is also available in the composed template as `id`.
