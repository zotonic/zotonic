Ignore part of a template.

Everything between a `{% comment %}` and a `{% endcomment %}` tag is not output.

Example:


```django
This will show.
{% comment %} And this will not show {% endcomment %}
```

This will output:


```django
This will show.
```

An alternative to the `{% comment %}` tag is to use the `{# ... #}` construct:


```django
This will show.
{# And this will not show #}
```

The big advantage of this notation is that the contents of the `{# ... #}` construct don’t need to be grammatically correct, as they will not be parsed. The contents of a `{% comment %}` tag must be correct as they *will* be parsed by the template compiler.