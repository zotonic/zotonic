See also

[catcompose](/id/doc_template_tag_tag_catcompose), [block](/id/doc_template_tag_tag_block), [fragment](/id/doc_template_tag_tag_fragment) and [useblock](/id/doc_template_tag_tag_useblock).

Compose another template and override its named blocks.

The `compose` tag renders another template, like [include](/id/doc_template_tag_tag_include), but also lets the caller define [template block](/id/doc_template_tag_tag_block) overrides for the composed template.

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

Like the [include](/id/doc_template_tag_tag_include) tag, the `with` keyword is optional:


```django
{% compose "_panel.tpl" title="Summary" %}
    {% block body %}Hello{% endblock %}
{% endcompose %}

{% compose "_panel.tpl" with title="Summary" %}
    {% block body %}Hello{% endblock %}
{% endcompose %}
```

The supplied arguments extend the current template context for the composed template render.

Note

The template name can be an expression. If it is not a string literal then the template is selected at runtime.
