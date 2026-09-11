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

`tag#fragment` and `tag#useblock`.
:::

Render a named fragment defined with `tag#fragment`.

The `use` tag renders a fragment that was defined with the `tag#fragment` tag.

Example:


```django
{% fragment answer_row %}
    <div class="row">
        <label>{{ label }}</label>
        <span>{{ value }}</span>
    </div>
{% endfragment %}

{% use answer_row label="Email" value=user.email %}
```

::: note
Like the `tag#include` tag, the `with` keyword is optional:


```django
{% use answer_row label="Email" value=user.email %}
{% use answer_row with label="Phone" value=user.phone %}
```
:::

The fragment is rendered with the current template context, extended with the supplied arguments.
