See also

[template block](/id/doc_template_tag_tag_block), [inherit](/id/doc_template_tag_tag_inherit), [fragment](/id/doc_template_tag_tag_fragment) and [extends](/id/doc_template_tag_tag_extends).

Inherit markup from like named template in another module.

Signal that this template extends a template with the same name in a module with lower priority.

Note

A template that overrules another template contains only the [overrules](#tag-overrules) tag and [template block](/id/doc_template_tag_tag_block) tags.

The overrules tag must be the first tag in the template.

Example, say a template “page.tpl” contains the following:


```django
{% overrules %}
{% block title %} My new title {% endblock %}
```

All named template blocks will replace the similar named template blocks in the overruled page.tpl template that is “next in line” to be used.

This is useful if you want to use a template from a module, and the template is mentioned in (for example) a dispatch rule. Now you can overrule and extend that template in your own modules without changing the dispatch rules or the original module.

Make sure your module has a higher priority (lower number) than the module containing the overruled template.

Arguments
---------

The `overrules` tag can pass extra variables to the overruled template and inherited fragments:


```django
{% overrules site_name="Example" %}
```

The `with` keyword is optional:


```django
{% overrules with site_name="Example" %}
```
