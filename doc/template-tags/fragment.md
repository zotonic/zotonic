See also

[use](/id/doc_template_tag_tag_use), [useblock](/id/doc_template_tag_tag_useblock), [block](/id/doc_template_tag_tag_block), [extends](/id/doc_template_tag_tag_extends) and [overrules](/id/doc_template_tag_tag_overrules).

Define a named reusable template fragment.

A fragment is a named piece of template output that can be reused with the [use](/id/doc_template_tag_tag_use) and [useblock](/id/doc_template_tag_tag_useblock) tags.

Fragments can also contain [template block](/id/doc_template_tag_tag_block) definitions. Those blocks can be addressed from a [useblock](/id/doc_template_tag_tag_useblock) call, similar to [compose](/id/doc_template_tag_tag_compose).

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

Fragments are available in the template where they are defined, and in templates that [extend](/id/doc_template_tag_tag_extends) or [overrule](/id/doc_template_tag_tag_overrules) that template.

Fragments use the current template context. Extra variables can be supplied by the [use](/id/doc_template_tag_tag_use) or [useblock](/id/doc_template_tag_tag_useblock) tags.
