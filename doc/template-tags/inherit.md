See also

[template block](/id/doc_template_tag_tag_block), [extends](/id/doc_template_tag_tag_extends), [fragment](/id/doc_template_tag_tag_fragment) and [overrules](/id/doc_template_tag_tag_overrules).

Include the markup of an extended template into the extending template.

Say you have a template `hello.tpl` containing:

templates/hello.tpl
```django
{% block test %}
This is content from hello.tpl
{% endblock %}
```

And in your site you have a `world.tpl` template, defined as:

templates/world.tpl
```django
{% extends "hello.tpl" %}
{% block test %}
    First line
    {% inherit %}
    This is more content from world.tpl
{% endblock %}
```

Then, the result of rendering the template `world.tpl` will be:


```erlang
First line
This is content from hello.tpl
This is more content from world.tpl
```

Arguments
---------

The `inherit` tag can pass extra variables when rendering the inherited block:


```django
{% block test %}
    {% inherit who="world" %}
{% endblock %}
```

The `with` keyword is optional:


```django
{% inherit with who="world" %}
```

The supplied arguments extend the current template context for the inherited block render.
