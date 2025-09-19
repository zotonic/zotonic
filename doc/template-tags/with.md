Assign a complex value to a variable.

The `{% with %}` tag assigns the result of a variable expression to a new variable. This is useful when accessing an “expensive” method (e.g., one that hits the database) multiple times. The `{% with %}` tag is often used in conjunction with the search model [m.search](/id/doc_model_model_search).

For example:


```erlang
{% with m.search[{latest cat="news"}] as latest_news %}
  The latest {% latest_news|length %} news articles:
  {% for id in latest_news %}
     {{ m.rsc[id].title }}
  {% endfor %}
{% endwith %}
```

This outputs the number of latest news articles and also the titles of the news articles. The search is only done once when the `{% with %}` tag assigns the variable “latest\_news”.



Multiple assignments
--------------------

It is also possible to assign multiple variables with a single with statement, like this:


```erlang
{% with "value1", "value2" as foo, bar %}
  {{ foo }}
  {{ bar }}
{% endwith %}
```

This will output `value1 value2`.