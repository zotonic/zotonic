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

`tag#fragment` and `tag#use`.
:::

Render a named `tag#fragment`, pass it enclosed body content, and optionally override its named blocks.

The `useblock` tag is like `tag#use`, but also passes the enclosed rendered content to the fragment in the special variable `_body`.

If the fragment contains named `tag#block` definitions, then the `useblock` body can also define blocks with the same names. Those caller blocks override the fragment’s block definitions, similar to `tag#compose`.

Example:


```django
{% fragment panel %}
    <section class="panel">
        <h2>{{ title }}</h2>
        <div class="panel-body">{{ _body }}</div>
    </section>
{% endfragment %}

{% useblock panel title="Summary" %}
    <p>Hello world</p>
{% enduseblock %}
```

Example with block overrides:


```django
{% fragment panel %}
    <section>
        {% block title %}Default title{% endblock %}
        <div>{{ _body }}</div>
    </section>
{% endfragment %}

{% useblock panel %}
    {% block title %}Custom title{% endblock %}
    Body text
{% enduseblock %}
```

::: note
Like the `tag#include` tag, the `with` keyword is optional:


```django
{% useblock panel title="Summary" %}
    <p>Hello world</p>
{% enduseblock %}

{% useblock panel with title="Summary" %}
    <p>Hello world</p>
{% enduseblock %}
```
:::
