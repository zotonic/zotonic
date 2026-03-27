See also

[fragment](/id/doc_template_tag_tag_fragment) and [useblock](/id/doc_template_tag_tag_useblock).

Render a named [fragment](/id/doc_template_tag_tag_fragment).

The `use` tag renders a fragment that was defined with the [fragment](/id/doc_template_tag_tag_fragment) tag.

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

Note

Like the [include](/id/doc_template_tag_tag_include) tag, the `with` keyword is optional:


```django
{% use answer_row label="Email" value=user.email %}
{% use answer_row with label="Phone" value=user.phone %}
```

The fragment is rendered with the current template context, extended with the supplied arguments.
