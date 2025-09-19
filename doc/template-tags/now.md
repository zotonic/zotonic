See also

the [date](/id/doc_template_filter_filter_date) filter for the possible format characters.

Show the current date and time.

Displays the current local date and time, formatted according to the given date format string.

Example:


```erlang
{% now "Y-m-d" %}
```

Did output “2008-12-10” on december 10, 2008.

There is also a variable called `now`, which holds the current date:


```erlang
{{ now|date:"Y-m-d" }}
```

Is equivalent to using the `{% now %}` tag.