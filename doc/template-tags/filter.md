Filter the contents of a block through variable filters.

Filters can also be piped through to each other.

Example:


```django
{% filter escape | lower %}
The text will be lowered and escaped. So you can use <, > and & without any problems.
{% endfilter %}
```

New in version 0.8.