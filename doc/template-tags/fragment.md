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

`tag#use`, `tag#useblock`, `tag#block`, `tag#extends` and `tag#overrules`.
:::

Define a named reusable template fragment.

A fragment is a named piece of template output that can be reused with the `tag#use` and `tag#useblock` tags.

Fragments can also contain `tag#block` definitions. Those blocks can be addressed from a `tag#useblock` call, similar to `tag#compose`.

Example:


```django
{% fragment answer_row %}
    <div class="row">
        <label>{{ label }}</label>
        <span>{{ value }}</span>
    </div>
{% endfragment %}

{% use answer_row label="Email" value=user.email %}
{% use answer_row with label="Phone" value=user.phone %}
```

The fragment definition itself does not render any output at the place where it is defined.

Fragments are available in the template where they are defined, and in templates that use `tag#extends` or `tag#overrules` with that template.

Fragments use the current template context. Extra variables can be supplied by the `tag#use` or `tag#useblock` tags.
