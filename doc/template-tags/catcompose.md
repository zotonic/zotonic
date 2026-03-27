See also

[compose](/id/doc_template_tag_tag_compose), [catinclude](/id/doc_template_tag_tag_catinclude) and [block](/id/doc_template_tag_tag_block).

Compose another template based on the category of a resource.

The `catcompose` tag is the category-aware variant of [compose](/id/doc_template_tag_tag_compose). It selects the template using category lookup, like [catinclude](/id/doc_template_tag_tag_catinclude), and also lets the caller override named blocks in the selected template.

Example:


```django
{% catcompose "_teaser.tpl" id %}
    {% block body %}
        <p>Custom teaser body</p>
    {% endblock %}
{% endcompose %}
```

The selected template depends on the category hierarchy of `id`, using the same lookup rules as [catinclude](/id/doc_template_tag_tag_catinclude).

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
