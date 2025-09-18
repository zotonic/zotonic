Rotate through a list of values.

Rotates through a list of values and outputs them. `{% cycle %}` Is used within a `{% for %}` loop.

Note

You can not apply filters to the cycle values.

Example:


```erlang
{% for a in [1,2,3,4] %}{% cycle "bleu" "blanc" "rouge" %} {% endfor %}
```

Will output “bleu blanc rouge bleu ”.

The list values can be either a string literal, a number literal, a variable or an automatic id (`#name`).