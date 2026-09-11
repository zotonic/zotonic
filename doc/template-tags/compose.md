---
keywords:
  - reference
  - frontend_developer
  - template
  - maintainability
  - render
---

::: aside
See also

`tag#catcompose`, `tag#block`, `tag#fragment` and `tag#useblock`.
:::

Compose another template and override its named blocks.

The `compose` tag renders another template, like `tag#include`, but also lets the caller define `tag#block` overrides for the composed template.

Example:


```django
{% compose "_panel.tpl" title="Summary" %}
    {% block body %}
        <p>Hello world</p>
    {% endblock %}
{% endcompose %}
```

The composed template can define blocks such as `body`, and the `compose` caller can override those blocks.

Arguments
---------

Like the `tag#include` tag, the `with` keyword is optional:


```django
{% compose "_panel.tpl" title="Summary" %}
    {% block body %}Hello{% endblock %}
{% endcompose %}

{% compose "_panel.tpl" with title="Summary" %}
    {% block body %}Hello{% endblock %}
{% endcompose %}
```

The supplied arguments extend the current template context for the composed template render.

::: note
The template name can be an expression. If it is not a string literal then the template is selected at runtime.
:::
