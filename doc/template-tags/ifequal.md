See also

[if](/id/doc_template_tag_tag_if) and [ifnotequal](/id/doc_template_tag_tag_ifnotequal).

Show something if two values are equal.

The `{% ifequal %}` tag tests if its two arguments are equal. If so then the contents of the `{% ifequal %}` block are output, otherwise the contents of the optional `{% else %}` tag are output.

For example:


```erlang
{% ifequal value 5 %}
  Value is equal to 5.
{% else %}
  Value is {{ value }} which is not 5.
{% endifequal %}
```

It is only possible to compare arguments that are variables (with optional filters) or constants. Examples of constants are numbers, strings or lists.