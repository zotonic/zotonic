---
keywords:
  - reference
  - frontend_developer
  - template
  - html
  - performance
  - transform
---

Removes whitespace between HTML tags.

::: note
spaceless does not remove non breaking spaces and other whitespace.
:::

Example:


```erlang
{% spaceless %}
<div>
    <p>Test test test</p>
</div>
{% endspaceless %}
```

After rendering:


```erlang
<div><p>Test test test</p></div>
```

New in version 0.8.
